//
// array.cc
//

#include "object/object.h"
#include "object/array.h"
#include "allocator.h"
#include "vm.h"

using namespace rhein;

Array*
Array::create(State* state, Int size) {
    if (size < 0) {
        return nullptr;
    }
    void* p = state->ator->allocateObject<Array>();
    return new (p) Array(state, size);
}

Array::Array(State* state, Int size_) : Object(state->array_klass), size(size_) {
    allocated_size = 1;
    while (size > allocated_size) {
        allocated_size *= 2;
    }

    body = state->ator->allocateRawArray(allocated_size);
}

