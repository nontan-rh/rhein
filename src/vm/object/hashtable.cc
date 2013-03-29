//
// hashtable.cc
//

#include "common.h"

#include <cstdio>

#include "object/object.h"
#include "object/hashtable.h"
#include "object/array.h"
#include "allocator.h"
#include "error.h"
#include "vm.h"

using namespace rhein;

struct rhein::HashTableNode {
    HashTableNode* next;
    Value key;
    Value value;

    static void* operator new (size_t /* size */, void* p) { return p; }

    HashTableNode(HashTableNode* next_, Value key_, Value value_)
        : next(next_), key(key_), value(value_) { }

    static HashTableNode* create(State* state, HashTableNode* next, Value key, Value value) {
        void *p = state->ator->allocateStruct<HashTableNode>();
        return new (p) HashTableNode(next, key, value);
    }
};

HashTable::HashTable(State* state)
    : Object(state->hashtable_klass), table_size(default_table_size), item_num(0) {
    table = state->ator->allocateBlock<HashTableNode>(default_table_size);
    for (unsigned i = 0; i < item_num; i++) {
        table[i].next = nullptr;
    }
}

HashTable*
HashTable::create(State* state) {
    void *p = state->ator->allocateObject<HashTable>();
    return new (p) HashTable(state);
}

HashTable*
HashTable::literal(State* state, Array* keys, Array* values) {
    HashTable* ht = HashTable::create(state);
    if (keys->getLength() != values->getLength()) {
        fatal("Length mismatch");
    }

    for (Int i = 0; i < keys->getLength(); i++) {
        Value key, value;
        keys->eltRef(i, key);
        values->eltRef(i, value);
        ht->insert(state, key, value);
    }
    return ht;
}

void
HashTable::rehash(State* state) {
    unsigned newtable_size = table_size * 2 + 1;
    HashTableNode* newtable = state->ator->allocateBlock<HashTableNode>(newtable_size);

    for (unsigned i = 0; i < table_size; i++) {
        HashTableNode* node = table[i].next;
        
        for (; node != nullptr; ) {
            HashTableNode* oldnext = node->next;
            unsigned newtable_index = get_hash(node->key) % newtable_size;
            HashTableNode* newnext = newtable[newtable_index].next;
            newtable[newtable_index].next = node;
            node->next = newnext;
            node = oldnext;
        }
    }

    state->ator->releaseBlock(table);
    table_size = newtable_size;
    table = newtable;
}

bool
HashTable::find(Value key, Value& result) const {
    HashTableNode* node = table[get_hash(key) % table_size].next;

    for (; node != nullptr; node = node->next) {
        if (equal(key, node->key)) {
            result = node->value;
            return true;
        }
    }

    return false;
}

bool
HashTable::insert(State* state, Value key, Value value) {
    Value dummy;
    if (find(key, dummy)) {
        return false;
    }

    unsigned table_index = get_hash(key) % table_size;
    HashTableNode* node = table[table_index].next;
    table[table_index].next = HashTableNode::create(state, node, key, value);

    item_num++;

    if (item_num > table_size * rehash_ratio) {
        rehash(state);
    }
    return true;
}

bool
HashTable::insertAnyway(State* state, Value key, Value value) {
    unsigned table_index = get_hash(key) % table_size;
    HashTableNode* head = table[table_index].next;
    HashTableNode* node = head;
    
    for (; node != nullptr; node = node->next) {
        if (equal(key, node->key)) {
            node->value = value;
            return false;
        }
    }

    head->next = HashTableNode::create(state, head->next, key, value);

    item_num++;

    if (item_num > table_size * rehash_ratio) {
        rehash(state);
    }
    return true;
}

bool
HashTable::assign(Value key, Value value) {
    unsigned table_index = get_hash(key) % table_size;
    HashTableNode* node = table[table_index].next;

    for (; node != nullptr; node = node->next) {
        if (equal(key, node->key)) {
            node->value = value;
            return true;
        }
    }
    return false;
}

bool
HashTable::remove(State* state, Value key) {
    unsigned table_index = get_hash(key) % table_size;
    HashTableNode* prev = &table[table_index];
    HashTableNode* node = table[table_index].next;

    for (; node != nullptr; ) {
        if (equal(key, node->key)) {
            prev->next = node->next;
            state->ator->releaseBlock(node);
            return true;
        }

        prev = node;
        node = node->next;
    }

    return false;
}

bool
HashTable::import(State* state, HashTable *rht) {
    for (unsigned i = 0; i < rht->table_size; i++) {
        HashTableNode* node = rht->table[i].next;
        
        for (; node != nullptr; node = node->next) {
            this->insert(state, node->key, node->value);
        }
    }
    return true;
}

// Override
bool
HashTable::indexRef(State* /* state */, Value index, Value& dest) const {
    return find(index, dest);
}

// Override
bool
HashTable::indexSet(State* state, Value index, Value value) {
    return insertAnyway(state, index, value);
}

void
HashTable::dump() {
    fprintf(stderr, "Dumping hashtable: %p\n", this);
    fprintf(stderr, "table_size: %u, item_num: %u\n", table_size, item_num);
    for (unsigned i = 0; i < table_size; i++) {
        HashTableNode* node = table[i].next;
        for (; node != nullptr; node = node->next) {
            fprintf(stderr, "%p -> %p\n", (void*)node->key, (void*)node->value);
        }
    }
}

