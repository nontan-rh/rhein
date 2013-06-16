//
// imstring.h - Immutable & unique string
//

#ifndef IMSTRING_H
#define IMSTRING_H

#include <cstddef>

#include "object/object.h"

namespace rhein {

class State;

class Array;

class String : public Object {
    friend class StringProvider;
    friend class StringHashTable;

    String(State* R, const char* body_, size_t length_);
    
    const char* body;
    size_t length;
    unsigned long hash_value;

    static String* create(State* R, const char* body, size_t length);
public:
    unsigned long hash() { return hash_value; }

    String* get_string_representation(State* R);
    void get_cstr(const char*& body, size_t& length) const;

    // Override
    bool index_ref(State* R, Value index, Value& value) const;

    Int get_length() const { return length; }

    String* append(State* R, String* rht);
    String* head(State* R, size_t end);
    String* tail(State* R, size_t begin);
    String* sub(State* R, size_t begin, size_t end);

    bool to_array(State* R, Array*& array);

    // For debugging
    void dump() const;
};

class StringHashTable;

class StringProvider {
    static void* operator new (size_t /* size */, void* p) { return p; }

    State* owner;
    StringHashTable* string_hash_table;

    StringProvider(State* R);

public:
    static StringProvider* create(State* R);
    String* get_string(const char* buffer, size_t length);
    String* get_string(const char* cstr);
};

};

#endif // IMSTRING_H

