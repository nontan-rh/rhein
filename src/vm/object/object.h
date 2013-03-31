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
    //Object() = delete;
    //Object& operator=(const Object& /* rht */) = delete;

protected:
    //Object(const Object& /* rht */) = default;
    //~Object() = default;

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
// Special constant prefix
// -------- 11111110 : 8bit and more are reserved
//
// ---> LSB
// -------- -------1 : Integer
// -------- ------00 : Object
// -------- 00000010 : Character
// 00000000 11111110 : False
// 00000001 11111110 : True
// 00000010 11111110 : Null
// 00000011 11111110 : Undef

#define CONST_ID(x) (((x) << 8) | ConstSign)

const uintptr_t ConstSign   = 0x000000fe;
const uintptr_t Cfalse      = CONST_ID(0);
const uintptr_t Ctrue       = CONST_ID(1);
const uintptr_t Cnull       = CONST_ID(2);
const uintptr_t Cundef      = CONST_ID(3);

const uintptr_t int_mask    = 0x00000001;
const uintptr_t int_masked  = 0x00000001;
const uintptr_t obj_mask    = 0x00000003;
const uintptr_t obj_masked  = 0x00000000;
const uintptr_t char_mask   = 0x000000ff;
const uintptr_t char_masked = 0x00000002;

static inline bool is_int(Value v)      { return ((v & int_mask) == int_masked); }
static inline bool is_obj(Value v)      { return ((v & obj_mask) == obj_masked); }
static inline bool is_char(Value v)     { return ((v & char_mask) == char_masked); }
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
    return (Int)((v & msb_mask) | (v >> 1));
}

static inline char get_char(Value v) {
    assert(is_char(v));
    return (char)(v >> 8);
}

template <class T>
static inline T* get_obj(Value v) {
    assert(is_obj(v));
    return (T*)v;
}

Klass* get_klass(State* state, Value v);

static inline unsigned long get_hash(Value v) {
    if (is_obj(v)) {
        return get_obj<Object>(v)->hash();
    }
    return (unsigned long)v;
}

static inline Value bool2value(bool v)      { return (v ? Ctrue : Cfalse); }
static inline Value obj2value(const Object* v)  { return (Value)v; }
static inline Value int2value(Int v)        { return (Value)((v << 1) | 1); }
static inline Value uint2value(Int v)       { return (Value)((v << 1) | 1); }
static inline Value char2value(char v)      { return (Value)(((uintptr_t)v << 8) | 2); }

static inline bool equal(Value lft, Value rht) { return (lft == rht); }

}

#endif // OBJECT_H

