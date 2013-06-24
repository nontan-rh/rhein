/*
 * object.h
 */

#ifndef OBJECT_H
#define OBJECT_H

#include <climits>
#include <tr1/cstdint>
#include <cassert>
#include <cstddef>

#include "internal.h"

namespace rhein {

typedef int Int;

class Value {
public:
	enum class Type {
		Nil,
		Bool,
		Undef,
		Int,
		Char,
		Object,
	};

	static Value k_nil() { return Value(Type::Nil); }
	static Value k_true() { return Value(true); }
	static Value k_false() { return Value(false); }
	static Value k_undef() { return Value(Type::Undef); }
	static Value by_bool(bool value) { return Value(value); }
	static Value by_int(Int value) { return Value(value); }
	static Value by_char(uint32_t ch) { return Value(ch); }
	static Value by_object(const Object *obj) {
		return Value(const_cast<Object *>(obj));
	}

	bool is(Type t) { return type_id == t; }
	bool like_true() { return !like_false(); }
	bool like_false() { return (type_id == Type::Bool && !u.v_bool); }
	Int get_int() { assert(type_id == Type::Int); return u.v_int; }
	uint32_t get_char() { assert(type_id == Type::Char); return u.v_char; }
	bool get_bool() { assert(type_id == Type::Bool); return u.v_bool; }
	template <class T>
	T *get_obj() {
		assert(type_id == Type::Object);
		return reinterpret_cast<T*>(u.v_obj);
	}

	unsigned long get_hash() const;

	Klass *get_klass(State *R);

	bool eq(const Value& rht) {
		if (type_id != rht.type_id) {
			return false;
		}
		switch (type_id) {
		case Type::Nil:
		case Type::Undef: // FALLTHROUGH
			return true;
		case Type::Bool:
			return u.v_bool == rht.u.v_bool;
		case Type::Int:
			return u.v_int == rht.u.v_int;
		case Type::Char:
			return u.v_char == rht.u.v_char;
		case Type::Object:
			return u.v_obj == rht.u.v_obj;
		}
		return false;
	}

	Value() : type_id(Type::Undef) { }
private:
	Type type_id;
	union Body {
		Int v_int;
		Object *v_obj;
		uint32_t v_char;
		bool v_bool;

		Body() : v_obj(nullptr) { }
		Body(bool value) : v_bool(value) { }
		Body(Int value) : v_int(value) { }
		Body(uint32_t ch) : v_char(ch) { }
		Body(Object *obj) : v_obj(obj) { }
	} u;

	Value(Type type) : type_id(type) { }
	Value(bool value) : type_id(Type::Bool), u(value) { }
	Value(Int value) : type_id(Type::Int), u(value) { }
	Value(uint32_t ch) : type_id(Type::Char), u(ch) { }
	Value(Object *obj) : type_id(Type::Object), u(obj) { }
};

class Object : public PlacementNewObj {
protected:
    Klass* klass;
    Object(Klass* klass_) : klass(klass_) { }

    virtual ~Object() = default;

public:
    virtual unsigned long get_hash() { return reinterpret_cast<unsigned long>(this); }

    virtual Klass* get_class() { return klass; }

    virtual String* get_string_representation(State* R);

    // Bytecode level object interface
    // Always fails by default
    virtual bool index_ref(State* /* R */, Value /* index */, Value& /* dest */) const {
        return false;
    }

    virtual bool index_set(State* /* R */, Value /* index */, Value /* value */) {
        return false;
    }

    virtual bool slot_ref(State* /* R */, Symbol* /* id */, Value& /* dest */) const {
        return false;
    }

    virtual bool slot_set(State* /* R */, Symbol* /* id */, Value /* value */) {
        return false;
    }
};

class RecordInfo;

class Klass : public Object {
    Klass() = delete;

    friend class State;

    void set_name(Symbol* name_) {
        name = name_;
    }

protected:
    Symbol* name;
    Klass* parent;
    RecordInfo* record_info;

    Klass(State* R, Symbol* name_, Klass* parent_, RecordInfo* record_info_);

public:
    static Klass* create(State* R, Symbol* name, Klass* parent) {
        return create(R, name, parent, 0, nullptr);
    }

    static Klass* create(State* R, Symbol* name, Klass* parent, unsigned slot_num, Symbol** slot_ids);

    Symbol* get_name() const { return name; }
    Klass* get_parent() const { return parent; }
    bool has_record_info() const { return record_info != nullptr; }
    const RecordInfo* get_record_info() const { return record_info; }
};

inline unsigned long
Value::get_hash() const {
	switch (type_id){
	case Type::Nil:
		return 12345;
	case Type::Bool:
		return 34567;
	case Type::Undef:
		return 0;
	case Type::Int:
		return u.v_int;
	case Type::Char:
		return u.v_char;
	case Type::Object:
		return u.v_obj->get_hash();
	}
	return 0;
}

class Symbol : public Object {
    friend class SymbolProvider;
    friend class SymbolHashTable;

    Symbol(State* R, const char* body_, size_t length_);

    const char* body;
    size_t length;
    unsigned long hash_value;

    static Symbol* create(State* R, const char* body, size_t length);
public:
    unsigned long get_hash() { return hash_value; }

    String* get_string_representation(State* R);
    void get_cstr(const char*& body, size_t& length) const;

    // Override
    bool index_ref(State* R, Value index, Value& value) const;

    Int get_length() const { return length; }

    String* to_string(State* R);

    // For debugging
    void dump() const;
};

class SymbolHashTable;

class SymbolProvider {
    static void* operator new (size_t /* size */, void* p) { return p; }

    State* owner;
    SymbolHashTable* string_hash_table;

    SymbolProvider(State* R);

public:
    static SymbolProvider* create(State* R);
    Symbol* get_symbol(const char* buffer, size_t length);
    Symbol* get_symbol(const char* cstr);
};

class String : public Object {
public:
	static String* create(State* R, const char* str);
	static String* create(State* R, const char* cstr, size_t len);

	unsigned long get_hash() { return hash_value; }
	String* get_string_representation(State* R);
    void get_cstr(const char*& body, size_t& length) const;

    // Override
    bool index_ref(State* R, Value index, Value& value) const;

    Int get_length() const { return length; }

    String* append(State* R, String* rht);
    String* head(State* R, size_t end);
    String* tail(State* R, size_t begin);
    String* sub(State* R, size_t begin, size_t end);

    bool to_array(State* R, Array*& array);
    Symbol* to_symbol(State* R);
private:
	const char* body;
	size_t length;
	unsigned long hash_value;

	String(State* R,const char* str, size_t len);
};

class Array : public Object {
    Value* body;
    Int size;
    Int allocated_size;

    Array(State* R, Int size_);
public:
    unsigned long get_hash() { return reinterpret_cast<unsigned long>(this); }

    static Array* create(State* R, Int size);
    static Array* literal(State* /* R */, Array* array) { return array; }

    Int get_length() const { return size; }

    bool elt_ref(Int index, Value& dest) const {
        if (0 <= index && index < size) {
            dest = body[index];
            return true;
        }
        return false;
    }

    bool elt_set(Int index, Value value) {
        if (0 <= index && index < size) {
            body[index] = value;
            return true;
        }
        return false;
    }

    // Override
    bool index_ref(State* /* R */, Value index, Value& dest) const {
        if (!index.is(Value::Type::Int)) { return false; }
        return elt_ref(index.get_int(), dest);
    }

    // Override
    bool index_set(State* /* R */, Value index, Value value) {
        if (!index.is(Value::Type::Int)) { return false; }
        return elt_set(index.get_int(), value);
    }

    bool to_string(State* R, String*& dest);
};

struct HashTableNode;

class HashTable : public Object {
    const unsigned default_table_size = 16;
    const double rehash_ratio = 0.75;

    HashTableNode* table;
    unsigned table_size;
    unsigned num_entries;

    HashTable(State* R);

    void rehash(State* R);

public:
    static HashTable* create(State* R);
    static HashTable* literal(State* R, Array* key, Array* value);

    unsigned get_num_entries() const { return num_entries; }

    bool find(Value key, Value& result) const;
    bool insert_if_absent(State* R, Value key, Value value);
    bool insert(State* R, Value key, Value value);
    bool assign(Value key, Value value);
    bool remove(State* R, Value key);
    bool import(State* R, HashTable *table);

    // Override
    bool index_ref(State* R, Value index, Value& dest) const;
    // Override
    bool index_set(State* R, Value index, Value value);

    // For debugging
    void dump();
};

class RecordInfo {
    unsigned slot_num;
    HashTable* id_index_table;

    static void* operator new(size_t /* size */, void *p) { return p; }

    RecordInfo(State* R, RecordInfo* parent, unsigned slot_num_,
        Symbol** slot_ids);

public:
    static RecordInfo* create(State* R, RecordInfo* parent, unsigned slot_num,
        Symbol** slot_ids);

    unsigned getSlotNum() const { return slot_num; }
    bool getSlotIndex(Symbol* slot_id, unsigned& index) const;
};

class Record : public Object {
    Value* member_slots;

    Record(State* R, Klass* klass);

public:
    static Record* create(State* R, Klass* klass);

    // Override
    bool slot_ref(State* /* R */, Symbol* slot_id, Value& value) const;
    // Override
    bool slot_set(State* /* R */, Symbol* slot_id, Value value);
};
typedef Value (*NativeFunctionBody)(State*, unsigned, Value*);

class Function : public Object {
protected:
	FunctionInfo *info_;
    Frame* closure;

    Function(Klass* klass, FunctionInfo *info)
        : Object(klass), info_(info), closure(nullptr) { }

public:
    FunctionInfo* info() const { return info_; }
    Frame* get_closure() const { return closure; }

    bool resolve(State* R) { return info_->resolve(R); }
};

class NativeFunction : public Function {
    NativeFunctionBody body;
    bool copied;

    NativeFunction(State* R, FunctionInfo* info, NativeFunctionBody body);

public:
    static NativeFunction* create(State* R, FunctionInfo* name,
    		NativeFunctionBody body);

    NativeFunctionBody get_body() const { return body; }

    NativeFunction* copy(State* R);
    NativeFunction* enclose(State* R, Frame* closure);
};

class BytecodeFunction : public Function {
    bool copied;
    unsigned stack_size;
    unsigned func_slot_size;
    unsigned var_slot_size;
    unsigned constant_table_size;
    Value* constant_table;
    unsigned bytecode_size;
    uint32_t* bytecode;

    BytecodeFunction(State* R, FunctionInfo* info,
        unsigned func_slot_size, unsigned var_slot_size_, unsigned stack_size_,
        unsigned constant_table_size_, Value* constant_table_,
        unsigned bytecode_size_, uint32_t* bytecode_);

public:
    static BytecodeFunction* create(State* R, FunctionInfo *info,
        unsigned func_slot_size, unsigned var_slot_size, unsigned stack_size,
        unsigned constant_table_size, Value* constant_table,
        unsigned bytecode_size, uint32_t* bytecode);

    unsigned get_stack_size() const { return stack_size; }
    unsigned get_function_slot_num() const { return func_slot_size; }
    unsigned get_variable_slot_num() const { return var_slot_size; }
    const uint32_t* get_bytecode() const { return bytecode; }
    const Value* get_constant_table() const { return constant_table; }

    BytecodeFunction* copy(State* R);
    BytecodeFunction* enclose(State* R, Frame* closure);
};

class DispatcherNode;

class Method : public Object {
    bool copied;
    DispatcherNode* node;
    Frame* closure;

    Method(State* R);

public:
    static Method* create(State* R);

    bool has_closure() const { return (closure != nullptr); }
    Frame* get_closure() const { return closure; }

    bool dispatch(State* R, unsigned argc, Value* args, Value& result);
    bool add_function(State* R, Function* func);

    Method* copy(State* R);
    Method* enclose(State* R, Frame* closure);
};

}

#endif /* OBJECT_H_ */
