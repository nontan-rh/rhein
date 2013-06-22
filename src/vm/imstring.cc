//
// imstring.cc - Immutable & unique string
//

#include <cstdio>
#include <cstring>
#include <cassert>

#include "object.h"
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
        State* R, StringHashTableNode* next, unsigned long key_hash,
        size_t length, const char* key, String* value) {

        void *p = R->ator->allocateStruct<StringHashTableNode>();
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

    StringHashTable(State* R) : table_size(default_table_size), item_num(0) {
        table = R->ator->allocateBlock<StringHashTableNode>(default_table_size);
        for (unsigned i = 0; i < default_table_size ; i++) {
            table[i].next = nullptr;
        }
    }

public:
    static StringHashTable* create(State* R) {
        void* p = R->ator->allocateStruct<StringHashTable>();
        return new (p) StringHashTable(R);
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

    void insert(State* R, String* value) {
        // Check if there is no collision
        String* dummy; assert(!find(value->body, value->length, dummy));
        unsigned long hash_value = calcHash(value->body, value->length);
        value->hash_value = hash_value;
        auto n = table[hash_value % table_size].next;
        table[hash_value % table_size].next = StringHashTableNode::create(
            R, n, hash_value, value->length, value->body, value);

        item_num++;
        if (item_num > rehash_ratio * table_size) {
            rehash(R);
        }
    }

    void remove(State* R, String* value) {
        // Check if there is
        String* dummy; assert(find(value->body, value->length, dummy));
        auto prev = &table[value->hash_value % table_size];
        auto curr = prev->next;
        for(;/* curr != nullptr */; ) {
            if (value->hash_value == curr->key_hash && value->length == curr->length) {
                if (value->body == curr->key
                    || memcmp(value->body, curr->key, value->length) == 0) {

                    prev->next = curr->next;
                    R->ator->releaseStruct(curr);
                    return;
                }
            }
            prev = curr;
            curr = curr->next;
        }
        // NOTREACHED
    }

    void rehash(State* R) {
        unsigned newtable_size = table_size * 2 + 1;
        StringHashTableNode* newtable = R->ator->allocateBlock<StringHashTableNode>(newtable_size);
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
        R->ator->releaseBlock(table);
        table_size = newtable_size;
        table = newtable;
    }
};

StringProvider::StringProvider(State* R)
    : owner(R) {
    assert(!owner->hasStringProvider());
    string_hash_table = StringHashTable::create(R);
    owner->setStringProvider(this);
}

StringProvider*
StringProvider::create(State* R) {
    void *p = R->ator->allocateStruct<StringProvider>();
    return new (p) StringProvider(R);
}

String::String(State* R, const char* body_, size_t length_)
    : Object(R->string_klass), body(body_), length(length_),
      hash_value(0) { }

String*
String::create(State* R, const char* body, size_t length) {
    void* p = R->ator->allocateObject<String>();
    return new (p) String(R, body, length);
}

String*
StringProvider::get_string(const char* buffer, size_t length) {
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
StringProvider::get_string(const char* cstr) {
    return get_string(cstr, strlen(cstr));
}

void
String::get_cstr(const char*& b, size_t& l) const {
    b = body;
    l = length;
}

bool
String::index_ref(State* /* R */, Value vindex, Value& dest) const {
    if (!vindex.is(Value::Type::Int)) {
        return false;
    }

    Int index = vindex.get_int();
    if (index < 0 || (Int)length <= index) {
        return false;
    }

    dest = Value::by_char(body[index]);
    return true;
}

String*
String::get_string_representation(State* /* R */) {
    return this;
}

String*
String::append(State* R, String* rht) {
    size_t newlength = this->length + rht->length;
    char* buffer = R->ator->allocateBlock<char>(newlength);
    memcpy(buffer, this->body, this->length);
    memcpy(buffer + this->length, rht->body, rht->length);
    String* ret = R->s_prv->get_string(buffer, newlength);
    R->ator->releaseBlock(buffer);
    return ret;
}

String*
String::head(State* R, size_t end) {
    if (end > length) {
        throw;
    }
    return R->s_prv->get_string(body, end);
}

String*
String::tail(State* R, size_t begin) {
    if (begin >= length) {
        throw;
    }
    return R->s_prv->get_string(body + begin, length - begin);
}

String*
String::sub(State* R, size_t begin, size_t end) {
    if (end > length || begin >= length || end < begin) {
        throw;
    }
    return R->s_prv->get_string(body + begin, end - begin);
}

bool
String::to_array(State* R, Array*& array) {
    array = Array::create(R, length);

    for (unsigned i = 0; i < length; i++) {
        array->elt_set(i, Value::by_char(body[i]));
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

