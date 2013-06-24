//
// hashtable.cc
//

#include <cstdio>

#include "object.h"
#include "allocator.h"
#include "error.h"
#include "vm.h"

namespace rhein {

using namespace std;

struct HashTableNode : public PlacementNewObj {
    HashTableNode* next;
    Value key;
    Value value;

    HashTableNode(HashTableNode* next_, Value key_, Value value_)
        : next(next_), key(key_), value(value_) { }

    static HashTableNode* create(State* R, HashTableNode* next, Value key, Value value) {
        void *p = R->ator->allocateStruct<HashTableNode>();
        return new (p) HashTableNode(next, key, value);
    }
};

HashTable::HashTable(State* R)
    : Object(R->hashtable_class), table_size_(kDefaultTableSize), num_entries_(0) {
    table_ = R->ator->allocateBlock<HashTableNode>(kDefaultTableSize);
    for (unsigned i = 0; i < num_entries_; i++) {
        table_[i].next = nullptr;
    }
}

HashTable*
HashTable::create(State* R) {
    void *p = R->ator->allocateObject<HashTable>();
    return new (p) HashTable(R);
}

HashTable*
HashTable::literal(State* R, Array* keys, Array* values) {
    HashTable* ht = HashTable::create(R);
    if (keys->get_length() != values->get_length()) {
        fatal("Length mismatch");
    }

    for (Int i = 0; i < keys->get_length(); i++) {
        Value key, value;
        keys->elt_ref(i, key);
        values->elt_ref(i, value);
        ht->insert_if_absent(R, key, value);
    }
    return ht;
}

void
HashTable::rehash(State* R) {
    unsigned newtable_size = table_size_ * 2 + 1;
    HashTableNode* newtable = R->ator->allocateBlock<HashTableNode>(newtable_size);

    for (unsigned i = 0; i < table_size_; i++) {
        HashTableNode* node = table_[i].next;
        
        for (; node != nullptr; ) {
            HashTableNode* oldnext = node->next;
            unsigned newtable_index = node->key.get_hash() % newtable_size;
            HashTableNode* newnext = newtable[newtable_index].next;
            newtable[newtable_index].next = node;
            node->next = newnext;
            node = oldnext;
        }
    }

    R->ator->releaseBlock(table_);
    table_size_ = newtable_size;
    table_ = newtable;
}

bool
HashTable::find(Value key, Value& result) const {
    HashTableNode* node = table_[key.get_hash() % table_size_].next;

    for (; node != nullptr; node = node->next) {
        if (key.eq(node->key)) {
            result = node->value;
            return true;
        }
    }

    return false;
}

bool
HashTable::insert_if_absent(State* R, Value key, Value value) {
    Value dummy;
    if (find(key, dummy)) {
        return false;
    }

    unsigned table_index = key.get_hash() % table_size_;
    HashTableNode* node = table_[table_index].next;
    table_[table_index].next = HashTableNode::create(R, node, key, value);

    num_entries_++;

    if (num_entries_ > table_size_ * kRehashRatio) {
        rehash(R);
    }
    return true;
}

bool
HashTable::insert(State* R, Value key, Value value) {
    unsigned table_index = key.get_hash() % table_size_;
    HashTableNode* head = &table_[table_index];
    HashTableNode* node = head->next;
    
    for (; node != nullptr; node = node->next) {
        if (key.eq(node->key)) {
            node->value = value;
            return false;
        }
    }

    head->next = HashTableNode::create(R, head->next, key, value);

    num_entries_++;

    if (num_entries_ > table_size_ * kRehashRatio) {
        rehash(R);
    }
    return true;
}

bool
HashTable::assign(Value key, Value value) {
    unsigned table_index = key.get_hash() % table_size_;
    HashTableNode* node = table_[table_index].next;

    for (; node != nullptr; node = node->next) {
        if (key.eq(node->key)) {
            node->value = value;
            return true;
        }
    }
    return false;
}

bool
HashTable::remove(State* R, Value key) {
    unsigned table_index = key.get_hash() % table_size_;
    HashTableNode* prev = &table_[table_index];
    HashTableNode* node = table_[table_index].next;

    for (; node != nullptr; ) {
        if (key.eq(node->key)) {
            prev->next = node->next;
            R->ator->releaseBlock(node);
            return true;
        }

        prev = node;
        node = node->next;
    }

    return false;
}

bool
HashTable::import(State* R, HashTable *rht) {
    for (unsigned i = 0; i < rht->table_size_; i++) {
        HashTableNode* node = rht->table_[i].next;
        
        for (; node != nullptr; node = node->next) {
            this->insert_if_absent(R, node->key, node->value);
        }
    }
    return true;
}

// Override
bool
HashTable::index_ref(State* /* R */, Value index, Value& dest) const {
    return find(index, dest);
}

// Override
bool
HashTable::index_set(State* R, Value index, Value value) {
    return insert(R, index, value);
}

void
HashTable::dump() {
    fprintf(stderr, "Dumping hashtable: %p\n", this);
    fprintf(stderr, "table_size: %u, item_num: %u\n", table_size_, num_entries_);
    for (unsigned i = 0; i < table_size_; i++) {
        HashTableNode* node = table_[i].next;
        for (; node != nullptr; node = node->next) {
            //fprintf(stderr, "%p -> %p\n", (void*)node->key, (void*)node->value);
        }
    }
}

}
