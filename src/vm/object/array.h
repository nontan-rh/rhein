//
// array.h
//

#ifndef ARRAY_H
#define ARRAY_H

#include <cstdint>

#include "object/object.h"

using namespace std;

namespace rhein {

class State;

class Array : public Object {
    Array() = delete;
    Array(const Array& /* rht */) = delete;
    Array& operator=(const Array& /* rht */) = delete;

    Value* body;
    Int size;
    Int allocated_size;

    Array(State* state, Int size_);
    
public:
    unsigned long hash() { return reinterpret_cast<unsigned long>(this); }

    static Array* create(State* state, Int size);
    static Array* literal(State* /* state */, Array* array) { return array; }
    
    Int getLength() const { return size; }

    bool eltRef(Int index, Value& dest) const {
        if (0 <= index && index < size) {
            dest = body[index];
            return true;
        }
        return false;
    }

    bool eltSet(Int index, Value value) {
        if (0 <= index && index < size) {
            body[index] = value;
            return true;
        }
        return false;
    }

    // Override
    bool indexRef(State* /* state */, Value index, Value& dest) const {
        if (!is_int(index)) {
            return false;
        }
        return eltRef(get_int(index), dest);
    }

    // Override
    bool indexSet(State* /* state */, Value index, Value value) {
        if (!is_int(index)) {
            return false;
        }
        return eltSet(get_int(index), value);
    }
};

};

#endif // ARRAY_H

