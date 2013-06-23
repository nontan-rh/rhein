/*
 * string.cc
 */

#include <cstring>

#include "object.h"
#include "vm.h"

namespace rhein {

String::String(State* R, const char* str, size_t len)
	: Object(R->string_klass), length(len), hash_value(0) {

	char* body_ = R->ator->allocateBlock<char>(length + 1);
	memcpy(body_, str, length + 1);
	body = body_;
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
	value = Value::by_char(body[index.get_int()]);
	return true;
}

String*
String::get_string_representation(State* /* R */) {
	return this;
}

void
String::get_cstr(const char*& body_, size_t& length_) const {
	body_ = body;
	length_ = length;
}

String*
String::append(State* R, String* rht) {
    size_t newlength = this->length + rht->length;
    char* buffer = R->ator->allocateBlock<char>(newlength);
    memcpy(buffer, this->body, this->length);
    memcpy(buffer + this->length, rht->body, rht->length);
    String* ret = String::create(R, buffer, newlength);
    R->ator->releaseBlock(buffer);
    return ret;
}

String*
String::head(State* R, size_t end) {
    if (end > length) {
        throw;
    }
    return String::create(R, body, end);
}

String*
String::tail(State* R, size_t begin) {
    if (begin >= length) {
        throw;
    }
    return String::create(R, body + begin, length - begin);
}

String*
String::sub(State* R, size_t begin, size_t end) {
    if (end > length || begin >= length || end < begin) {
        throw;
    }
    return String::create(R, body + begin, end - begin);
}

bool
String::to_array(State* R, Array*& array) {
    array = Array::create(R, length);

    for (unsigned i = 0; i < length; i++) {
        array->elt_set(i, Value::by_char(body[i]));
    }
    return true;
}

Symbol*
String::to_symbol(State* R) {
	 return R->s_prv->get_symbol(body, length);
}

}




