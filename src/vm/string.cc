/*
 * string.cc
 */

#include <cstring>

#include "object.h"
#include "vm.h"

namespace rhein {

String::String(State* R, const char* str, size_t len)
	: Object(R->string_class), length_(len), hash_value_(0) {

	char* buf = R->ator->allocateBlock<char>(length_ + 1);
	memcpy(buf, str, length_ + 1);
	body_ = buf;
}

String*
String::create(State* R, const char *str) {
	return String::create(R, str, strlen(str));
}

String*
String::create(State *R, const char *str, size_t len) {
	void* p = R->ator->allocateObject<String>();
	return new (p) String(R, str, len);
}

bool
String::index_ref(State* /* R */, Value index, Value& value) const {
	value = Value::by_char(body_[index.get_int()]);
	return true;
}

String*
String::get_string_representation(State* /* R */) {
	return this;
}

void
String::get_cstr(const char*& body, size_t& length) const {
	body = body_;
	length = length_;
}

String*
String::append(State* R, String* rht) const {
    size_t newlength = this->length_ + rht->length_;
    char* buffer = R->ator->allocateBlock<char>(newlength);
    memcpy(buffer, this->body_, this->length_);
    memcpy(buffer + this->length_, rht->body_, rht->length_);
    String* ret = String::create(R, buffer, newlength);
    R->ator->releaseBlock(buffer);
    return ret;
}

String*
String::head(State* R, size_t end) const {
    if (end > length_) {
        throw;
    }
    return String::create(R, body_, end);
}

String*
String::tail(State* R, size_t begin) const {
    if (begin >= length_) {
        throw;
    }
    return String::create(R, body_ + begin, length_ - begin);
}

String*
String::sub(State* R, size_t begin, size_t end) const {
    if (end > length_ || begin >= length_ || end < begin) {
        throw;
    }
    return String::create(R, body_ + begin, end - begin);
}

bool
String::to_array(State* R, Array*& array) const {
    array = Array::create(R, length_);

    for (unsigned i = 0; i < length_; i++) {
        array->elt_set(i, Value::by_char(body_[i]));
    }
    return true;
}

Symbol*
String::to_symbol(State* R) const {
	 return R->s_prv->get_symbol(body_, length_);
}

}




