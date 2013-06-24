//
// array.cc
//

#include "object.h"
#include "allocator.h"
#include "vm.h"

namespace rhein {

Array*
Array::create(State* R, Int size) {
    if (size < 0) {
        return nullptr;
    }
    void* p = R->allocate_object<Array>();
    return new (p) Array(R, size);
}

Array::Array(State* R, Int size) : Object(R->get_array_class()), size_(size) {
    allocated_size_ = 1;
    while (size_ > allocated_size_) {
        allocated_size_ *= 2;
    }

    body_ = R->allocate_raw_array(allocated_size_);
}

bool
Array::to_string(State* R, String*& dest) const {
    char* buf = R->allocate_block<char>(size_);
    
    for (Int i = 0; i < size_; i++) {
        if (!body_[i].is(Value::Type::Char)) {
            R->release_block(buf);
            return false;
        }

        buf[i] = body_[i].get_char();
    }

    dest = String::create(R, buf, size_);
    R->release_block(buf);
    return true;
}

}
