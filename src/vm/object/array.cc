//
// array.cc
//

#include "object/object.h"
#include "object/array.h"
#include "allocator.h"
#include "vm.h"

using namespace rhein;

Array*
Array::create(State* R, Int size) {
    if (size < 0) {
        return nullptr;
    }
    void* p = R->ator->allocateObject<Array>();
    return new (p) Array(R, size);
}

Array::Array(State* R, Int size_) : Object(R->array_klass), size(size_) {
    allocated_size = 1;
    while (size > allocated_size) {
        allocated_size *= 2;
    }

    body = R->ator->allocateRawArray(allocated_size);
}

bool
Array::to_string(State* R, String*& dest) {
    char* buf = R->ator->allocateBlock<char>(size);
    
    for (Int i = 0; i < size; i++) {
        if (!body[i].is(Value::Type::Char)) {
            R->ator->releaseBlock(buf);
            return false;
        }

        buf[i] = body[i].get_char();
    }

    dest = R->s_prv->get_string(buf, size);
    R->ator->releaseBlock(buf);
    return true;
}

