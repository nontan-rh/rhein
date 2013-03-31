//
// imstring.cc - Immutable & unique string
//

#include <cstdio>
#include <cstring>
#include <cassert>

#include "object/object.h"
#include "object/imstring.h"
#include "object/array.h"
#include "allocator.h"
#include "vm.h"

using namespace rhein;

// Dumb hashing function
unsigned long calcHash(const char* cstr, size_t length) {
    unsigned long hash_value = 0x1f2e3d4c;
    for (size_t i=0; i<length; i++) {
        // xor
        hash_value ^= cstr[i];
        // left rotate by 1
        unsigned long top = (hash_value >> (sizeof(unsigned long) * 8 - 1)) & 1;
        hash_value <<= 1;
        hash_value |= top;
    }
    return hash_value;
}

struct StringHashTableNode {
    StringHashTableNode *next;
    unsigned long key_hash;
    size_t length;
    const char* key;
    String* value;

    static void* operator new (size_t /* size */, void* p) { return p; }

    StringHashTableNode()
        : next(nullptr), key_hash(0), length(0), key(nullptr), value(nullptr) { }

    StringHashTableNode(StringHashTableNode *next_, unsigned long key_hash_,
        size_t length_, const char* key_, String* value_)
        : next(next_), key_hash(key_hash_), length(length_), key(key_), value(value_) { }
    
    static StringHashTableNode* create(
        State* state, StringHashTableNode* next, unsigned long key_hash,
        size_t length, const char* key, String* value) {

        void *p = state->ator->allocateStruct<StringHashTableNode>();
        return new (p) StringHashTableNode(next, key_hash, length, key, value);
    }
};

class rhein::StringHashTable {
    //StringHashTable() = delete;
    //StringHashTable(const StringHashTable& /* rht */) = delete;
    //StringHashTable& operator=(const StringHashTable& /* rht */) = delete;

    static void* operator new (size_t /* size */, void* p) { return p; }

    const double rehash_ratio = 0.75;
    const unsigned default_table_size = 16;
    StringHashTableNode* table;
    unsigned table_size;
    unsigned item_num;

    StringHashTable(State* state) : table_size(default_table_size), item_num(0) {
        table = state->ator->allocateBlock<StringHashTableNode>(default_table_size);
        for (unsigned i = 0; i < default_table_size ; i++) {
            table[i].next = nullptr;
        }
    }

public:
    static StringHashTable* create(State* state) {
        void* p = state->ator->allocateStruct<StringHashTable>();
        return new (p) StringHashTable(state);
    }

    bool find(const char* key, size_t length, String*& result) {
        unsigned long hash_value = calcHash(key, length);
        auto node = table[hash_value % table_size].next;

        for(; node != nullptr; node = node->next) {
            if (hash_value == node->key_hash && length == node->length) {
                if (key == node->key || memcmp(key, node->key, length) == 0) {
                    result = node->value;
                    return true;
                }
            }
        }
        result = nullptr;
        return false;
    }

    void insert(State* state, String* value) {
        // Check if there is no collision
        String* dummy; assert(!find(value->body, value->length, dummy));
        unsigned long hash_value = calcHash(value->body, value->length);
        value->hash_value = hash_value;
        auto n = table[hash_value % table_size].next;
        table[hash_value % table_size].next = StringHashTableNode::create(
            state, n, hash_value, value->length, value->body, value);

        item_num++;
        if (item_num > rehash_ratio * table_size) {
            rehash(state);
        }
    }

    void remove(State* state, String* value) {
        // Check if there is
        String* dummy; assert(find(value->body, value->length, dummy));
        auto prev = &table[value->hash_value % table_size];
        auto curr = prev->next;
        for(;/* curr != nullptr */; ) {
            if (value->hash_value == curr->key_hash && value->length == curr->length) {
                if (value->body == curr->key
                    || memcmp(value->body, curr->key, value->length) == 0) {

                    prev->next = curr->next;
                    state->ator->releaseStruct(curr);
                    return;
                }
            }
            prev = curr;
            curr = curr->next;
        }
        // NOTREACHED
    }

    void rehash(State* state) {
        unsigned newtable_size = table_size * 2 + 1;
        StringHashTableNode* newtable = state->ator->allocateBlock<StringHashTableNode>(newtable_size);
        for(unsigned i = 0; i < table_size; i++) {
            StringHashTableNode* node = table[i].next;
            for(; node != nullptr; ) {
                StringHashTableNode* oldnext = node->next;
                StringHashTableNode* newnext = newtable[node->key_hash % newtable_size].next;
                newtable[node->key_hash % newtable_size].next = node;
                node->next = newnext;
                node = oldnext;
            }
        }
        state->ator->releaseBlock(table);
        table_size = newtable_size;
        table = newtable;
    }
};

StringProvider::StringProvider(State* state)
    : owner(state) {
    assert(!owner->hasStringProvider());
    string_hash_table = StringHashTable::create(state);
    owner->setStringProvider(this);
}

StringProvider*
StringProvider::create(State* state) {
    void *p = state->ator->allocateStruct<StringProvider>();
    return new (p) StringProvider(state);
}

String::String(State* state, const char* body_, size_t length_)
    : Object(state->string_klass), body(body_), length(length_) { }

String*
String::create(State* state, const char* body, size_t length) {
    void* p = state->ator->allocateObject<String>();
    return new (p) String(state, body, length);
}

String*
StringProvider::getString(const char* buffer, size_t length) {
    String *registered;
    if (string_hash_table->find(buffer, length, registered)) {
        return registered;
    }

    char* copybuffer = owner->ator->allocateBlock<char>(length);
    memcpy(copybuffer, buffer, length);
    String* new_string = String::create(owner, copybuffer, length);
    string_hash_table->insert(owner, new_string);
    return new_string;
}

String*
StringProvider::getString(const char* cstr) {
    return getString(cstr, strlen(cstr));
}

void
String::getCStr(const char*& b, size_t& l) const {
    b = body;
    l = length;
}

bool
String::indexRef(State* /* state */, Value vindex, Value& dest) const {
    if (!is_int(vindex)) {
        return false;
    }

    Int index = get_int(vindex);
    if (index < 0 || (Int)length <= index) {
        return false;
    }

    dest = char2value(body[index]);
    return true;
}

String*
String::stringRepr(State* /* state */) {
    return this;
}

String*
String::append(State* state, String* rht) {
    size_t newlength = this->length + rht->length;
    char* buffer = state->ator->allocateBlock<char>(newlength);
    memcpy(buffer, this->body, this->length);
    memcpy(buffer + this->length, rht->body, rht->length);
    String* ret = state->s_prv->getString(buffer, newlength);
    state->ator->releaseBlock(buffer);
    return ret;
}

String*
String::head(State* state, size_t end) {
    if (end > length) {
        throw;
    }
    return state->s_prv->getString(body, end);
}

String*
String::tail(State* state, size_t begin) {
    if (begin >= length) {
        throw;
    }
    return state->s_prv->getString(body + begin, length - begin);
}

String*
String::sub(State* state, size_t begin, size_t end) {
    if (end > length || begin >= length || end < begin) {
        throw;
    }
    return state->s_prv->getString(body + begin, end - begin);
}

bool
String::toArray(State* state, Array*& array) {
    array = Array::create(state, length);

    for (unsigned i = 0; i < length; i++) {
        array->eltSet(i, char2value(body[i]));
    }
    return true;
}

void
String::dump() const {
    for (unsigned i = 0; i < length; i++) {
        fprintf(stderr, "%c", body[i]);
    }
    fprintf(stderr, "\n");
}

