//
// array.h
//

#ifndef ARRAY_H
#define ARRAY_H

#include <tr1/cstdint>

#include "object/object.h"

using namespace std;

namespace rhein {

class State;

class String;

class Array : public Object {
    Value* body;
    Int size;
    Int allocated_size;

    Array(State* state, Int size_);
public:
    unsigned long hash() { return reinterpret_cast<unsigned long>(this); }

    static Array* create(State* state, Int size);
    static Array* literal(State* /* state */, Array* array) { return array; }
    
    Int getLength() const { return size; }

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
    bool indexRef(State* /* state */, Value index, Value& dest) const {
        if (!index.is(Value::Type::Int)) { return false; }
        return elt_ref(index.get_int(), dest);
    }

    // Override
    bool indexSet(State* /* state */, Value index, Value value) {
        if (!index.is(Value::Type::Int)) { return false; }
        return elt_set(index.get_int(), value);
    }

    bool toString(State* state, String*& dest);
};

};

#endif // ARRAY_H

