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
        SymbolHashTableNode* next, unsigned long key_hash,
        size_t length, const char* key, Symbol* value) {

        void *p = get_current_state()->allocate_struct<SymbolHashTableNode>();
        return new (p) SymbolHashTableNode(next, key_hash, length, key, value);
    }
};

class SymbolHashTable : public PlacementNewObj {
public:
    static SymbolHashTable* create() {
        void* p = get_current_state()->allocate_struct<SymbolHashTable>();
        return new (p) SymbolHashTable();
    }

    bool find(unsigned long hash_value, const char* key, size_t length,
            Symbol*& result) {
        auto node = table_[hash_value % table_size_].next;

        for(; node; node = node->next) {
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

    void insert(unsigned long hash_value,
            const char* body, size_t length, Symbol* value) {
        auto node = table_[hash_value % table_size_].next;
        table_[hash_value % table_size_].next =
            SymbolHashTableNode::create(node, hash_value, length, body, value);

        num_items_++;
        if (num_items_ > kRehashRatio * table_size_) {
            rehash();
        }
    }

    void rehash() {
        State* R = get_current_state();
        unsigned newtable_size = table_size_ * 2 + 1;
        SymbolHashTableNode* newtable = R->allocate_block<SymbolHashTableNode>(newtable_size);
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
        R->release_block(table_);
        table_size_ = newtable_size;
        table_ = newtable;
    }

private:
    const double kRehashRatio = 0.75;
    const unsigned kDefaultTableSize = 17;
    SymbolHashTableNode* table_;
    unsigned table_size_;
    unsigned num_items_;

    SymbolHashTable() : table_size_(kDefaultTableSize), num_items_(0) {
        table_ = get_current_state()->allocate_block<SymbolHashTableNode>(kDefaultTableSize);
        for (unsigned i = 0; i < kDefaultTableSize ; i++) {
            table_[i].next = nullptr;
        }
    }
};

SymbolProvider::SymbolProvider()
    : owner(get_current_state()) {
    assert(!owner->has_symbol_provider()); // Not to duplicate
    string_hash_table = SymbolHashTable::create();
    owner->set_symbol_provider(this);
}

SymbolProvider*
SymbolProvider::create() {
    void *p = get_current_state()->allocate_struct<SymbolProvider>();
    return new (p) SymbolProvider();
}

Symbol::Symbol(unsigned long hash_value, const char* body, size_t length)
    : Object(get_current_state()->get_symbol_class()), body_(body), length_(length),
      hash_value_(hash_value) { }

Symbol*
Symbol::create(unsigned long hash_value, const char* body, size_t length) {
    void* p = get_current_state()->allocate_object<Symbol>();
    return new (p) Symbol(hash_value, body, length);
}

Symbol*
SymbolProvider::get_symbol(const char* buffer, size_t length) {
    unsigned long hash_value = calc_string_hash(buffer, length);
    Symbol *registered;

    if (string_hash_table->find(hash_value, buffer, length, registered)) {
        return registered;
    }

    char* copybuffer = owner->allocate_block<char>(length + 1);
    copybuffer[length] = '\0';
    memcpy(copybuffer, buffer, length + 1);

    Symbol* new_symbol = Symbol::create(hash_value, copybuffer, length);

    string_hash_table->insert(hash_value, copybuffer, length, new_symbol);
    return new_symbol;
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
Symbol::index_ref(Value vindex, Value& dest) {
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
Symbol::get_string_representation() {
    return String::create(body_, length_);
}

String*
Symbol::to_string() const {
    return String::create(body_, length_);
}

void
Symbol::dump() const {
    for (unsigned i = 0; i < length_; i++) {
        fprintf(stderr, "%c", body_[i]);
    }
    fprintf(stderr, "\n");
}

}

