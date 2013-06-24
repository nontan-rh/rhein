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

	bool is(Type t) const { return type_id_ == t; }
	bool like_true() const { return !like_false(); }
	bool like_false() const { return (type_id_ == Type::Bool && !u_.v_bool_); }
	Int get_int() const { assert(type_id_ == Type::Int); return u_.v_int_; }
	uint32_t get_char() const { assert(type_id_ == Type::Char); return u_.v_char_; }
	bool get_bool() const { assert(type_id_ == Type::Bool); return u_.v_bool_; }
	template <class T>
	T *get_obj() const {
		assert(type_id_ == Type::Object);
		return reinterpret_cast<T*>(u_.v_obj_);
	}

	Value::Type get_type() const { return type_id_; }

	unsigned long get_hash() const;

	Class *get_class(State *R) const;

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
		}
		return false;
	}

	Value() : type_id_(Type::Undef) { }
private:
	Type type_id_;
	union Body {
		Int v_int_;
		Object *v_obj_;
		uint32_t v_char_;
		bool v_bool_;

		Body() : v_obj_(nullptr) { }
		Body(bool value) : v_bool_(value) { }
		Body(Int value) : v_int_(value) { }
		Body(uint32_t ch) : v_char_(ch) { }
		Body(Object *obj) : v_obj_(obj) { }
	} u_;

	Value(Type type) : type_id_(type) { }
	Value(bool value) : type_id_(Type::Bool), u_(value) { }
	Value(Int value) : type_id_(Type::Int), u_(value) { }
	Value(uint32_t ch) : type_id_(Type::Char), u_(ch) { }
	Value(Object *obj) : type_id_(Type::Object), u_(obj) { }
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

class Class : public Object {
protected:
    Class(State* R, Symbol* name, Class* parent, RecordInfo* record_info);

public:
    static Class* create(State* R, Symbol* name, Class* parent) {
        return create(R, name, parent, 0, nullptr);
    }

    static Class* create(State* R, Symbol* name, Class* parent,
    		unsigned slot_num, Symbol** slot_ids);

    Symbol* get_id() const { return id_; }
    Class* get_parent() const { return parent_; }
    bool has_record_info() const { return (record_info_ != nullptr); }
    const RecordInfo* get_record_info() const { return record_info_; }
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
	}
	return 0;
}

class Symbol : public Object {
    friend class SymbolProvider;
    friend class SymbolHashTable;

    Symbol(State* R, const char* body, size_t length);

    const char* body_;
    size_t length_;
    unsigned long hash_value_;

    static Symbol* create(State* R, const char* body, size_t length);
public:
    unsigned long get_hash() { return hash_value_; }

    String* get_string_representation(State* R);
    void get_cstr(const char*& body, size_t& length) const;

    // Override
    bool index_ref(State* R, Value index, Value& value) const;

    Int get_length() const { return length_; }

    String* to_string(State* R) const;

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

	unsigned long get_hash() { return hash_value_; }
	String* get_string_representation(State* R);
    void get_cstr(const char*& body, size_t& length) const;

    // Override
    bool index_ref(State* R, Value index, Value& value) const;

    Int get_length() const { return length_; }

    String* append(State* R, String* rht) const;
    String* head(State* R, size_t end) const;
    String* tail(State* R, size_t begin) const;
    String* sub(State* R, size_t begin, size_t end) const;

    bool to_array(State* R, Array*& array) const;
    Symbol* to_symbol(State* R) const;
private:
	const char* body_;
	size_t length_;
	unsigned long hash_value_;

	String(State* R, const char* str, size_t length);
};

class Array : public Object {
public:
    unsigned long get_hash() { return reinterpret_cast<unsigned long>(this); }

    static Array* create(State* R, Int size);
    static Array* literal(State* /* R */, Array* array) { return array; }

    Int get_length() const { return size_; }

    bool elt_ref(Int index, Value& dest) const {
        if (0 <= index && index < size_) {
            dest = body_[index];
            return true;
        }
        return false;
    }

    bool elt_set(Int index, Value value) {
        if (0 <= index && index < size_) {
            body_[index] = value;
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

    bool to_string(State* R, String*& dest) const;
private:
    Value* body_;
    Int size_;
    Int allocated_size_;

    Array(State* R, Int size_);
};

struct HashTableNode;

class HashTable : public Object {
public:
    static HashTable* create(State* R);
    static HashTable* literal(State* R, Array* key, Array* value);

    unsigned get_num_entries() const { return num_entries_; }

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

private:
    const unsigned kDefaultTableSize = 16;
    const double kRehashRatio = 0.75;

    HashTableNode* table_;
    unsigned table_size_;
    unsigned num_entries_;

    HashTable(State* R);

    void rehash(State* R);
};

class Record : public Object {
public:
    static Record* create(State* R, Class* klass);

    // Override
    bool slot_ref(State* /* R */, Symbol* slot_id, Value& value) const;
    // Override
    bool slot_set(State* /* R */, Symbol* slot_id, Value value);
public:
    Value* member_slots_;

    Record(State* R, Class* klass);

};

typedef Value (*NativeFunctionBody)(State*, unsigned, Value*);

class Function : public Object {
protected:
	FunctionInfo *info_;
    Frame* closure_;

    Function(Class* klass, FunctionInfo *info)
        : Object(klass), info_(info), closure_(nullptr) { }

public:
    FunctionInfo* get_info() const { return info_; }
    Frame* get_closure() const { return closure_; }

    bool resolve(State* R) { return info_->resolve(R); }
};

class NativeFunction : public Function {
public:
    static NativeFunction* create(State* R, FunctionInfo* name,
    		NativeFunctionBody body);

    NativeFunctionBody get_body() const { return body_; }

    NativeFunction* copy(State* R);
    NativeFunction* enclose(State* R, Frame* closure);

private:
    NativeFunctionBody body_;
    bool copied_;

    NativeFunction(State* R, FunctionInfo* info, NativeFunctionBody body);
};

class BytecodeFunction : public Function {
public:
    static BytecodeFunction* create(State* R, FunctionInfo *info,
        unsigned func_slot_size, unsigned var_slot_size, unsigned stack_size,
        unsigned constant_table_size, Value* constant_table,
        unsigned bytecode_size, uint32_t* bytecode);

    unsigned get_stack_size() const { return stack_size_; }
    unsigned get_function_slot_num() const { return func_slot_size_; }
    unsigned get_variable_slot_num() const { return var_slot_size_; }
    const uint32_t* get_bytecode() const { return bytecode_; }
    const Value* get_constant_table() const { return constant_table_; }

    BytecodeFunction* copy(State* R);
    BytecodeFunction* enclose(State* R, Frame* closure);

private:
    bool copied_;
    unsigned stack_size_;
    unsigned func_slot_size_;
    unsigned var_slot_size_;
    unsigned constant_table_size_;
    Value* constant_table_;
    unsigned bytecode_size_;
    uint32_t* bytecode_;

    BytecodeFunction(State* R, FunctionInfo* info,
        unsigned func_slot_size, unsigned var_slot_size, unsigned stack_size,
        unsigned constant_table_size, Value* constant_table,
        unsigned bytecode_size, uint32_t* bytecode);
};

class Method : public Object {
    bool copied_;
    DispatcherNode* node_;
    Frame* closure_;

    Method(State* R);

public:
    static Method* create(State* R);

    bool has_closure() const { return (closure_ != nullptr); }
    Frame* get_closure() const { return closure_; }

    bool dispatch(State* R, unsigned argc, Value* args, Value& result);
    bool add_function(State* R, Function* func);

    Method* copy(State* R);
    Method* enclose(State* R, Frame* closure);
};

}

#endif /* OBJECT_H_ */
