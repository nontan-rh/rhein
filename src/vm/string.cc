/*
 * string.cc
 */

#include <cstring>

#include "object.h"
#include "vm.h"

namespace rhein {

String::String(const char* str, size_t len)
    : Object(get_current_state()->get_string_class()), length_(len), hash_value_(0) {

    char* buf = get_current_state()->allocate_block<char>(length_ + 1);
    memcpy(buf, str, length_ + 1);
    body_ = buf;
}

String*
String::create(const char *str) {
    return String::create(str, strlen(str));
}

String*
String::create(const char *str, size_t len) {
    void* p = get_current_state()->allocate_object<String>();
    return new (p) String(str, len);
}

String*
String::create(char ch) {
    char buf[2];
    buf[0] = ch;
    buf[1] = '\0';
    return String::create(buf);
}

bool
String::index_ref(Value index, Value& value) {
    value = Value::by_char(body_[index.get_int()]);
    return true;
}

String*
String::get_string_representation() {
    return this;
}

void
String::get_cstr(const char*& body, size_t& length) const {
    body = body_;
    length = length_;
}

String*
String::append(String* rht) const {
    State* R = get_current_state();
    size_t newlength = this->length_ + rht->length_;
    char* buffer = R->allocate_block<char>(newlength);
    memcpy(buffer, this->body_, this->length_);
    memcpy(buffer + this->length_, rht->body_, rht->length_);
    String* ret = String::create(buffer, newlength);
    R->release_block(buffer);
    return ret;
}

String*
String::head(size_t end) const {
    assert(end <= length_);
    return String::create(body_, end);
}

String*
String::tail(size_t begin) const {
    assert(begin < length_);
    return String::create(body_ + begin, length_ - begin);
}

String*
String::sub(size_t begin, size_t end) const {
    assert(end <= length_ && begin < length_ && begin <= end);
    return String::create(body_ + begin, end - begin);
}

bool
String::to_array(Array*& array) const {
    array = Array::create(length_);

    for (unsigned i = 0; i < length_; i++) {
        array->elt_set(i, Value::by_char(body_[i]));
    }
    return true;
}

Symbol*
String::to_symbol() const {
     return get_current_state()->get_symbol(body_, length_);
}

}




