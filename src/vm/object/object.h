//
// object.h
//

#ifndef OBJECT_H
#define OBJECT_H

#include <climits>
#include <cstdint>
#include <cassert>
#include <cstddef>

namespace rhein {

class String;
class Klass;
class State;

typedef uintptr_t Value;

class Object {
    Object() = delete;
    Object& operator=(const Object& /* rht */) = delete;

protected:
    Object(const Object& /* rht */) = default;
    ~Object() = default;

    Klass* klass;

    Object(Klass* klass_) : klass(klass_) { }

    static void* operator new (size_t /* size */, void* p) { return p; }

public:
    virtual unsigned long hash() { return reinterpret_cast<unsigned long>(this); }

    virtual Klass* getKlass() { return klass; }

    virtual String* stringRepr(State* state);

    // Bytecode level object interface
    // Always fails by default
    virtual bool indexRef(State* /* state */, Value /* index */, Value& /* dest */) const {
        return false;
    }

    virtual bool indexSet(State* /* state */, Value /* index */, Value /* value */) {
        return false;
    }

    virtual bool slotRef(State* /* state */, const String* /* id */, Value& /* dest */) const {
        return false;
    }

    virtual bool slotSet(State* /* state */, const String* /* id */, Value /* value */) {
        return false;
    }
};

class RecordInfo;

class Klass : public Object {
    Klass() = delete;

    friend class State;

    void setName(String* name_) {
        name = name_;
    }

protected:
    String* name;
    Klass* parent;
    RecordInfo* record_info;

    Klass(String* name_, Klass* parent_, RecordInfo* record_info_)
        : Object(nullptr), name(name_), parent(parent_), record_info(record_info_) { }

public:
    static Klass* create(State* state, String* name, Klass* parent) {
        return create(state, name, parent, 0, nullptr);
    }

    static Klass* create(State* state, String* name, Klass* parent, unsigned slot_num, String** slot_ids);

    String* getName() const { return name; }
    // Override
    Klass* getKlass() { return this; }
    Klass* getParent() const { return parent; }
    bool hasRecordInfo() const { return record_info != nullptr; }
    const RecordInfo* getRecordInfo() const { return record_info; }
};

typedef intptr_t Int;

// Value tag structure
//
// ---> LSB
// -------1 : Integer
// ------00 : Object
// 00000010 : False
// 00000110 : True
// 00001010 : Null
// 00001110 : Undef

const uintptr_t Cfalse      = 0x00000002;
const uintptr_t Ctrue       = 0x00000006;
const uintptr_t Cnull       = 0x0000000a;
const uintptr_t Cundef      = 0x0000000e;

const uintptr_t int_mask    = 0x00000001;
const uintptr_t obj_mask    = 0x00000003;

static inline bool is_int(Value v)      { return ((v & int_mask) == 1); }
static inline bool is_obj(Value v)      { return ((v & obj_mask) == 0); }
static inline bool is_bool(Value v)     { return (v == Ctrue || v == Cfalse); }
static inline bool is_true(Value v)     { return (v == Ctrue); }
static inline bool is_false(Value v)    { return (v == Cfalse); }
static inline bool is_null(Value v)     { return (v == Cnull); }
static inline bool is_undef(Value v)    { return (v == Cundef); }

static inline bool like_false(Value v)  { return v == Cfalse; }
static inline bool like_true(Value v)   { return !like_false(v); }

const uintptr_t msb_mask    = (uintptr_t)1 << ((sizeof(intptr_t) * CHAR_BIT) - 1);

static inline Int get_int(Value v) {
    assert(is_int(v));
    return static_cast<intptr_t>((v & msb_mask) | (v >> 1));
}

static inline Object* get_obj(Value v) {
    assert(is_obj(v));
    return reinterpret_cast<Object*>(v);
}

Klass* get_klass(State* state, Value v);

static inline unsigned long get_hash(Value v) {
    if (is_obj(v)) {
        return get_obj(v)->hash();
    }
    return static_cast<unsigned long>(v);
}

static inline Value make_value(bool v)      { return (v ? Ctrue : Cfalse); }
static inline Value make_value(const Object* v) { return reinterpret_cast<Value>(v); }
static inline Value make_value(Object* v)   { return reinterpret_cast<Value>(v); }
static inline Value make_value(Int v)       { return static_cast<Value>(((v << 1) | 1)); }
static inline Value make_value(unsigned v)  { return static_cast<Value>(((v << 1) | 1)); }

static inline bool equal(Value lft, Value rht) { return (lft == rht); }

}

#endif // OBJECT_H

