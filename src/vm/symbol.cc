//
// imstring.cc - Immutable & unique string
//

#include <cstdio>
#include <cstring>
#include <cassert>

#include "object.h"
#include "allocator.h"
#include "vm.h"

namespace rhein {

struct SymbolHashTableNode : public PlacementNewObj {
    SymbolHashTableNode *next;
    unsigned long key_hash;
    size_t length;
    const char* key;
    Symbol* value;

    SymbolHashTableNode()
        : next(nullptr), key_hash(0), length(0), key(nullptr), value(nullptr) { }

    SymbolHashTableNode(SymbolHashTableNode *next_, unsigned long key_hash_,
        size_t length_, const char* key_, Symbol* value_)
        : next(next_), key_hash(key_hash_), length(length_), key(key_),
          value(value_) { }
    
    static SymbolHashTableNode* create(
        State* R, SymbolHashTableNode* next, unsigned long key_hash,
        size_t length, const char* key, Symbol* value) {

        void *p = R->ator->allocateStruct<SymbolHashTableNode>();
        return new (p) SymbolHashTableNode(next, key_hash, length, key, value);
    }
};

class SymbolHashTable : public PlacementNewObj {
public:
    static SymbolHashTable* create(State* R) {
        void* p = R->ator->allocateStruct<SymbolHashTable>();
        return new (p) SymbolHashTable(R);
    }

    bool find(const char* key, size_t length, Symbol*& result) {
        unsigned long hash_value = calc_string_hash(key, length);
        auto node = table_[hash_value % table_size_].next;

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

    void insert(State* R, Symbol* value) {
        // Check if there is no collision
        Symbol* dummy; assert(!find(value->body_, value->length_, dummy));
        unsigned long hash_value = calc_string_hash(value->body_, value->length_);
        value->hash_value_ = hash_value;
        auto n = table_[hash_value % table_size_].next;
        table_[hash_value % table_size_].next = SymbolHashTableNode::create(
            R, n, hash_value, value->length_, value->body_, value);

        num_items_++;
        if (num_items_ > kRehashRatio * table_size_) {
            rehash(R);
        }
    }

    void remove(State* R, Symbol* value) {
        // Check if there is
        Symbol* dummy; assert(find(value->body_, value->length_, dummy));
        auto prev = &table_[value->hash_value_ % table_size_];
        auto curr = prev->next;
        for(; /* curr != nullptr */; ) {
            if (value->hash_value_ == curr->key_hash && value->length_ == curr->length) {
                if (value->body_ == curr->key
                    || memcmp(value->body_, curr->key, value->length_) == 0) {

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
        unsigned newtable_size = table_size_ * 2 + 1;
        SymbolHashTableNode* newtable = R->ator->allocateBlock<SymbolHashTableNode>(newtable_size);
        for(unsigned i = 0; i < table_size_; i++) {
            SymbolHashTableNode* node = table_[i].next;
            for(; node != nullptr; ) {
                SymbolHashTableNode* oldnext = node->next;
                SymbolHashTableNode* newnext = newtable[node->key_hash % newtable_size].next;
                newtable[node->key_hash % newtable_size].next = node;
                node->next = newnext;
                node = oldnext;
            }
        }
        R->ator->releaseBlock(table_);
        table_size_ = newtable_size;
        table_ = newtable;
    }

private:
    const double kRehashRatio = 0.75;
    const unsigned kDefaultTableSize = 16;
    SymbolHashTableNode* table_;
    unsigned table_size_;
    unsigned num_items_;

    SymbolHashTable(State* R) : table_size_(kDefaultTableSize), num_items_(0) {
        table_ = R->ator->allocateBlock<SymbolHashTableNode>(kDefaultTableSize);
        for (unsigned i = 0; i < kDefaultTableSize ; i++) {
            table_[i].next = nullptr;
        }
    }
};

SymbolProvider::SymbolProvider(State* R)
    : owner(R) {
    assert(!owner->has_symbol_provider());
    string_hash_table = SymbolHashTable::create(R);
    owner->set_symbol_provider(this);
}

SymbolProvider*
SymbolProvider::create(State* R) {
    void *p = R->ator->allocateStruct<SymbolProvider>();
    return new (p) SymbolProvider(R);
}

Symbol::Symbol(State* R, const char* body_, size_t length_)
    : Object(R->symbol_class), body_(body_), length_(length_),
      hash_value_(0) { }

Symbol*
Symbol::create(State* R, const char* body, size_t length) {
    void* p = R->ator->allocateObject<Symbol>();
    return new (p) Symbol(R, body, length);
}

Symbol*
SymbolProvider::get_symbol(const char* buffer, size_t length) {
    Symbol *registered;
    if (string_hash_table->find(buffer, length, registered)) {
        return registered;
    }

    char* copybuffer = owner->ator->allocateBlock<char>(length + 1);
    memcpy(copybuffer, buffer, length + 1);
    Symbol* new_string = Symbol::create(owner, copybuffer, length);
    string_hash_table->insert(owner, new_string);
    return new_string;
}

Symbol*
SymbolProvider::get_symbol(const char* cstr) {
    return get_symbol(cstr, strlen(cstr));
}

void
Symbol::get_cstr(const char*& b, size_t& l) const {
    b = body_;
    l = length_;
}

bool
Symbol::index_ref(State* /* R */, Value vindex, Value& dest) const {
    if (!vindex.is(Value::Type::Int)) {
        return false;
    }

    Int index = vindex.get_int();
    if (index < 0 || (Int)length_ <= index) {
        return false;
    }

    dest = Value::by_char(body_[index]);
    return true;
}

String*
Symbol::get_string_representation(State* R) {
    return String::create(R, body_, length_);
}

String*
Symbol::to_string(State* R) const {
	return String::create(R, body_, length_);
}

void
Symbol::dump() const {
    for (unsigned i = 0; i < length_; i++) {
        fprintf(stderr, "%c", body_[i]);
    }
    fprintf(stderr, "\n");
}

}
