//
// object.h
//

#ifndef OBJECT_H
#define OBJECT_H

#include <climits>
#include <tr1/cstdint>
#include <cassert>
#include <cstddef>

namespace rhein {

class Object;
class String;
class Klass;
class State;

typedef int Int;

class PlacementNewObj {
protected:
    static void* operator new (size_t /* size */, void* p) { return p; }
};

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

    virtual bool slot_ref(State* /* R */, String* /* id */, Value& /* dest */) const {
        return false;
    }

    virtual bool slot_set(State* /* R */, String* /* id */, Value /* value */) {
        return false;
    }
};

class RecordInfo;

class Klass : public Object {
    Klass() = delete;

    friend class State;

    void set_name(String* name_) {
        name = name_;
    }

protected:
    String* name;
    Klass* parent;
    RecordInfo* record_info;

    Klass(String* name_, Klass* parent_, RecordInfo* record_info_)
        : Object(nullptr), name(name_), parent(parent_), record_info(record_info_) { }

public:
    static Klass* create(State* R, String* name, Klass* parent) {
        return create(R, name, parent, 0, nullptr);
    }

    static Klass* create(State* R, String* name, Klass* parent, unsigned slot_num, String** slot_ids);

    String* get_name() const { return name; }
    // Override
    Klass* get_class() { return this; }
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
}

#endif // OBJECT_H

