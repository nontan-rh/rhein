//
// array.cc
//

#include "common.h"

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

bool
Array::toString(State* state, String*& dest) {
    char* buf = state->ator->allocateBlock<char>(size);
    
    for (Int i = 0; i < size; i++) {
        if (!is_char(body[i])) {
            state->ator->releaseBlock(buf);
            return false;
        }

        buf[i] = get_char(body[i]);
    }

    dest = state->s_prv->getString(buf, size);
    state->ator->releaseBlock(buf);
    return true;
}

