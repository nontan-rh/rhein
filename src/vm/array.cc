//
// array.cc
//

#include "object.h"
#include "allocator.h"
#include "vm.h"

namespace rhein {

Array*
Array::create(Int size) {
    if (size < 0) {
        return nullptr;
    }
    void* p = get_current_state()->allocate_object<Array>();
    return new (p) Array(size);
}

Array::Array(Int size) : Object(get_current_state()->get_array_class()), size_(size) {
    allocated_size_ = 1;
    while (size_ > allocated_size_) {
        allocated_size_ *= 2;
    }

    body_ = get_current_state()->allocate_raw_array(allocated_size_);
}

void
Array::append(Value value) {
    ++size_;
    if (size_ > allocated_size_) {
        unsigned newallocated_size = allocated_size_ * 2;
        Value* newbody = get_current_state()->allocate_raw_array(newallocated_size);
        for (Int i = 0; i < allocated_size_; i++) {
            newbody[i] = body_[i];
        }
        body_ = newbody;
    }
    body_[size_ - 1] = value;
}

bool
Array::to_string(String*& dest) const {
    State* R = get_current_state();
    char* buf = R->allocate_block<char>(size_);
    
    for (Int i = 0; i < size_; i++) {
        if (!body_[i].is(Value::Type::Char)) {
            R->release_block(buf);
            return false;
        }

        buf[i] = body_[i].get_char();
    }

    dest = String::create(buf, size_);
    R->release_block(buf);
    return true;
}

RestArguments*
RestArguments::create(Int size) {
    if (size < 0) {
        return nullptr;
    }
    void* p = get_current_state()->allocate_object<RestArguments>();
    return new (p) RestArguments(size);
}

RestArguments::RestArguments(Int size)
    : Object(get_current_state()->get_rest_arguments_class()), size_(size) {

    allocated_size_ = 1;
    while (size_ > allocated_size_) {
        allocated_size_ *= 2;
    }

    body_ = get_current_state()->allocate_raw_array(allocated_size_);
}

bool
RestArguments::to_array(Array*& dest) const {
    dest = Array::create(size_);

    for (Int i = 0; i < size_; i++) {
        dest->elt_set(i, body_[i]);
    }

    return true;
}

}

