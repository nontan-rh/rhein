//
// imstring.h - Immutable & unique string
//

#ifndef IMSTRING_H
#define IMSTRING_H

#include <cstddef>

#include "object/object.h"

namespace rhein {

class State;

class String : public Object {
    friend class StringProvider;
    friend class StringHashTable;

    String() = delete;
    String(const String& /* rht */) = delete;
    String& operator=(const String& /* rht */) = delete;

    String(State* state, const char* body_, size_t length_);
    
    const char* body;
    size_t length;
    unsigned long hash_value;

    static String* create(State* state, const char* body, size_t length);
public:
    unsigned long hash() { return hash_value; }

    String* stringRepr(State* state);
    void getCStr(const char*& body, size_t& length) const;

    // Override
    bool indexRef(State* state, Value index, Value& value) const;

    String* append(State* state, String* rht);
    String* head(State* state, size_t end);
    String* tail(State* state, size_t begin);
    String* substring(State* state, size_t begin, size_t end);

    // For debugging
    void dump();
};

class StringHashTable;

class StringProvider {
    StringProvider() = delete;
    StringProvider(const StringProvider& /* rht */) = delete;
    StringProvider& operator=(const StringProvider& /* rht */) = delete;

    static void* operator new (size_t /* size */, void* p) { return p; }

    State* owner;
    StringHashTable* string_hash_table;

    StringProvider(State* state);

public:
    static StringProvider* create(State* state);
    String* getString(const char* buffer, size_t length);
    String* getString(const char* cstr);
};

};

#endif // IMSTRING_H

