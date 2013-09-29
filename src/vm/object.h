/*
 * object.h
 */

#ifndef OBJECT_H
#define OBJECT_H

#include <climits>
#include <cstdint>
#include <cassert>
#include <cstddef>
#include <cstdio>

#include "internal.h"

namespace rhein {

typedef int Int;
typedef uint8_t Byte;
typedef uint32_t Char;

extern State* current_state_;

class Value {
public:
    enum class Type {
        Nil,
        Bool,
        Undef,
        Int,
        Char,
        Object,
        Record,
    };

    static Value k_nil() { return Value(Type::Nil); }
    static Value k_true() { return Value(true); }
    static Value k_false() { return Value(false); }
    static Value k_undef() { return Value(Type::Undef); }
    static Value by_bool(bool value) { return Value(value); }
    static Value by_int(Int value) { return Value(value); }
    static Value by_char(uint32_t ch) { return Value(ch); }
    static Value by_object(const Object* obj) {
        if (obj == nullptr) { return k_nil(); }
        return Value(const_cast<Object*>(obj));
    }
    static Value by_record(Record* rec) { return Value(rec); }

    bool is(Type t) const { return type_id_ == t; }
    bool like_true() const { return !like_false(); }
    bool like_false() const { return (type_id_ == Type::Bool && !u_.v_bool_); }
    Int get_int() const { assert(type_id_ == Type::Int); return u_.v_int_; }
    uint32_t get_char() const { assert(type_id_ == Type::Char); return u_.v_char_; }
    bool get_bool() const { assert(type_id_ == Type::Bool); return u_.v_bool_; }
    template <class T>
    T *get_obj() const {
        if (type_id_ == Type::Object) {
            return reinterpret_cast<T*>(u_.v_obj_);
        } else if (type_id_ == Type::Nil) {
            return nullptr;
        }
        assert(false);
    }
    Record* get_rec() const { assert(type_id_ == Type::Record); return u_.v_rec_; }

    Value::Type get_type() const { return type_id_; }

    unsigned long get_hash() const;

    Class* get_class() const;

    bool eq(const Value& rht) const {
        if (type_id_ != rht.type_id_) {
            return false;
        }
        switch (type_id_) {
        case Type::Nil:
        case Type::Undef: // FALLTHROUGH
            return true;
        case Type::Bool:
            return u_.v_bool_ == rht.u_.v_bool_;
        case Type::Int:
            return u_.v_int_ == rht.u_.v_int_;
        case Type::Char:
            return u_.v_char_ == rht.u_.v_char_;
        case Type::Object:
            return u_.v_obj_ == rht.u_.v_obj_;
        case Type::Record:
            return u_.v_rec_ == rht.u_.v_rec_;
        }
        return false;
    }

    Value() : type_id_(Type::Undef) { }
private:
    Type type_id_;
    union Body {
        Int v_int_;
        Object *v_obj_;
        Record *v_rec_;
        uint32_t v_char_;
        bool v_bool_;
        Class *v_class_;

        Body() : v_obj_(nullptr) { }
        Body(bool value) : v_bool_(value) { }
        Body(Int value) : v_int_(value) { }
        Body(uint32_t ch) : v_char_(ch) { }
        Body(Object* obj) : v_obj_(obj) { }
        Body(Record* rec) : v_rec_(rec) { }
        Body(Class* klass) : v_class_(klass) { }
    } u_;

    Value(Type type) : type_id_(type) { }
    Value(bool value) : type_id_(Type::Bool), u_(value) { }
    Value(Int value) : type_id_(Type::Int), u_(value) { }
    Value(uint32_t ch) : type_id_(Type::Char), u_(ch) { }
    Value(Object* obj) : type_id_(Type::Object), u_(obj) { }
    Value(Record* rec) : type_id_(Type::Record), u_(rec) { }
};

class Object : public PlacementNewObj {
protected:
    Class* klass_;
    Object(Class* c) : klass_(c) { }

    virtual ~Object() = default;

public:
    virtual unsigned long get_hash() {
        return reinterpret_cast<unsigned long>(this);
    }

    virtual Class* get_class() { return klass_; }

    virtual String* get_string_representation();

    // Bytecode level object interface
    // Always fails by default
    virtual bool index_ref(Value /* index */, Value& /* dest */) {
        return false;
    }

    virtual bool index_set(Value /* index */, Value /* value */) {
        return false;
    }

    virtual bool slot_ref(Symbol* /* id */, Value& /* dest */) {
        return false;
    }

    virtual bool slot_set(Symbol* /* id */, Value /* value */) {
        return false;
    }
};

class Class : public Object {
protected:
    Class(Symbol* name, Class* parent, RecordInfo* record_info);

public:
    static Class* create(Symbol* name, Class* parent) {
        return create(name, parent, 0, nullptr);
    }

    static Class* create(Symbol* name, Class* parent,
            unsigned slot_num, Symbol** slot_ids);

    Symbol* get_id() const { return id_; }
    Class* get_parent() const { return parent_; }
    bool has_record_info() const { return (record_info_ != nullptr); }
    const RecordInfo* get_record_info() const { return record_info_; }

    bool is_subclass_of(Class* k) const;
private:
    Symbol* id_;
    Class* parent_;
    RecordInfo* record_info_;

    friend class State;

    // After id setter for State
    void set_id(Symbol* id) { id_ = id; }
};

inline unsigned long
Value::get_hash() const {
    switch (type_id_){
    case Type::Nil:
        return 12345;
    case Type::Bool:
        return 34567;
    case Type::Undef:
        return 0;
    case Type::Int:
        return u_.v_int_;
    case Type::Char:
        return u_.v_char_;
    case Type::Object:
        return u_.v_obj_->get_hash();
    case Type::Record:
        return reinterpret_cast<unsigned long>(u_.v_rec_);
    }
    return 0;
}

class Symbol : public Object {
    friend class SymbolProvider;

    Symbol(unsigned long hash_value, const char* body, size_t length);

    const char* body_;
    size_t length_;
    unsigned long hash_value_;

    static Symbol* create(unsigned long hash_value, const char* body, size_t length);
public:
    unsigned long get_hash() { return hash_value_; }

    String* get_string_representation();
    void get_cstr(const char*& body, size_t& length) const;

    // Override
    bool index_ref(Value index, Value& value);

    Int get_length() const { return length_; }

    String* to_string() const;

    // For debugging
    void dump() const;
};

class SymbolHashTable;

class SymbolProvider {
    static void* operator new (size_t /* size */, void* p) { return p; }

    State* owner;
    SymbolHashTable* string_hash_table;

    SymbolProvider();

public:
    static SymbolProvider* create();
    Symbol* get_symbol(const char* buffer, size_t length);
    Symbol* get_symbol(const char* cstr);
};

class String : public Object {
public:
    static String* create(const char* str);
    static String* create(const char* cstr, size_t len);

    unsigned long get_hash() { return hash_value_; }
    String* get_string_representation();
    void get_cstr(const char*& body, size_t& length) const;

    char elt_ref(Int index) {
        assert (index >= 0 && static_cast<unsigned>(index) < length_);
        return body_[index];
        return body_[index];
    }

    // Override
    bool index_ref(Value index, Value& value);

    Int get_length() const { return length_; }

    String* append(String* rht) const;
    String* head(size_t end) const;
    String* tail(size_t begin) const;
    String* sub(size_t begin, size_t end) const;

    bool to_array(Array*& array) const;
    Symbol* to_symbol() const;
private:
    const char* body_;
    size_t length_;
    unsigned long hash_value_;

    String(const char* str, size_t length);
};

class Array : public Object {
public:
    unsigned long get_hash() { return reinterpret_cast<unsigned long>(this); }

    static Array* create(Int size);
    static Array* literal(Array* array) { return array; }

    Int get_length() const { return size_; }

    Value elt_ref(Int index) {
        assert(bound_ok(index));
        return body_[index];
    }

    void elt_set(Int index, Value value) {
        assert(bound_ok(index));
        body_[index] = value;
    }

    // Override
    bool index_ref(Value index, Value& dest) {
        if (!(index.is(Value::Type::Int) && bound_ok(index.get_int()))) {
            return false;
        }
        dest = elt_ref(index.get_int());
        return true;
    }

    // Override
    bool index_set(Value index, Value value) {
        if (!index.is(Value::Type::Int)) { return false; }
        elt_set(index.get_int(), value);
        return true;
    }

    void append(Value value);

    void drop(int length);

    bool to_string(String*& dest) const;
private:
    Value* body_;
    Int size_;
    Int allocated_size_;

    Array(Int size_);

    bool bound_ok(Int index) { return (0 <= index && index < size_); }
};

class RestArguments : public Object {
public:
    unsigned long get_hash() { return reinterpret_cast<unsigned long>(this); }

    static RestArguments* create(Int size);

    Int get_length() const { return size_; }

    Value elt_ref(Int index) {
        assert(bound_ok(index));
        return body_[index];
    }

    void elt_set(Int index, Value value) {
        assert(bound_ok(index));
        body_[index] = value;
    }

    // Override
    bool index_ref(Value index, Value& dest) {
        if (!(index.is(Value::Type::Int) && bound_ok(index.get_int()))) {
            return false;
        }
        dest = elt_ref(index.get_int());
        return true;
    }

    // Override
    bool index_set(Value index, Value value) {
        if (!(index.is(Value::Type::Int) && bound_ok(index.get_int()))) {
            return false;
        }
        elt_set(index.get_int(), value);
        return true;
    }


    bool to_array(Array*& dest) const;
private:
    Value* body_;
    Int size_;
    Int allocated_size_;

    RestArguments(Int size_);

    bool bound_ok(Int index) { return (0 <= index && index < size_); }
};

struct HashTableNode;

class HashTable : public Object {
public:
    static HashTable* create();
    static HashTable* literal(Array* key, Array* value);

    unsigned get_num_entries() const { return num_entries_; }

    bool find(Value key, Value& result) const;
    bool insert_if_absent(Value key, Value value);
    bool insert(Value key, Value value);
    bool assign(Value key, Value value);
    bool remove(Value key);
    bool import(HashTable *table);

    // Override
    bool index_ref(Value index, Value& dest);
    // Override
    bool index_set(Value index, Value value);

    // For debugging
    void dump();

private:
    const unsigned kDefaultTableSize = 16;
    const double kRehashRatio = 0.75;

    HashTableNode* table_;
    unsigned table_size_;
    unsigned num_entries_;

    HashTable();

    void rehash();
};

class Record : public PlacementNewObj {
public:
    static Record* create(Class* klass);

    bool slot_ref(Symbol* slot_id, Value& value);
    bool slot_set(Symbol* slot_id, Value value);

    Class* get_class() const { return klass_; }
private:
    Class* klass_;
    Value inherit_instance_;
    Value* member_slots_;

    Record(Class* klass);
};

typedef Value (*NativeFunctionBody)(unsigned, Value*);

class Function : public Object {
protected:
    FunctionInfo *info_;
    Closure* closure_;

    Function(Class* klass, FunctionInfo *info)
        : Object(klass), info_(info), closure_(nullptr) { }

public:
    FunctionInfo* get_info() const { return info_; }
    Closure* get_closure() const { return closure_; }

    bool resolve() { return info_->resolve(); }
};

class NativeFunction : public Function {
public:
    static NativeFunction* create(FunctionInfo* name,
            NativeFunctionBody body);

    NativeFunctionBody get_body() const { return body_; }

    NativeFunction* copy();
    NativeFunction* enclose(Closure* closure);

private:
    NativeFunctionBody body_;
    bool copied_;

    NativeFunction(FunctionInfo* info, NativeFunctionBody body);
};

class BytecodeFunction : public Function {
public:
    static BytecodeFunction* create(FunctionInfo *info,
        unsigned func_slot_size, unsigned var_slot_size, unsigned stack_size,
        unsigned constant_table_size, Value* constant_table,
        unsigned bytecode_size, uint32_t* bytecode);

    unsigned get_stack_size() const { return stack_size_; }
    unsigned get_function_slot_num() const { return func_slot_size_; }
    unsigned get_variable_slot_num() const { return var_slot_size_; }
    const uint32_t* get_bytecode() const { return bytecode_; }
    const Value* get_constant_table() const { return constant_table_; }

    BytecodeFunction* copy();
    BytecodeFunction* enclose(Closure* closure);

private:
    bool copied_;
    unsigned stack_size_;
    unsigned func_slot_size_;
    unsigned var_slot_size_;
    unsigned constant_table_size_;
    Value* constant_table_;
    unsigned bytecode_size_;
    uint32_t* bytecode_;

    BytecodeFunction(FunctionInfo* info,
        unsigned func_slot_size, unsigned var_slot_size, unsigned stack_size,
        unsigned constant_table_size, Value* constant_table,
        unsigned bytecode_size, uint32_t* bytecode);
};

class Method : public Object {
    bool copied_;
    DispatcherNode* node_;
    Closure* closure_;

    Method();

public:
    static Method* create();

    bool has_closure() const { return (closure_ != nullptr); }
    Closure* get_closure() const { return closure_; }

    bool dispatch(unsigned argc, Value* args, Value& result);
    bool add_function(Function* func);

    Method* copy();
    Method* enclose(Closure* closure);
};

}

#endif /* OBJECT_H_ */
