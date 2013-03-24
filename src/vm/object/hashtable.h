//
// hashtable.h
//

#ifndef HASHTABLE_H
#define HASHTABLE_H

#include "object/object.h"

namespace rhein {

class State;
class Array;

struct HashTableNode;

class HashTable : public Object {
    HashTable() = delete;
    HashTable(const HashTable& /* rht */) = delete;
    HashTable& operator=(const HashTable& /* rht */) = delete;
    
    const unsigned default_table_size = 16;
    const double rehash_ratio = 0.75;
    
    HashTableNode* table;
    unsigned table_size;
    unsigned item_num;

    HashTable(State* state);

    void rehash(State* state);

public:
    static HashTable* create(State* state);
    static HashTable* literal(State* state, Array* key, Array* value);

    unsigned getItemNumber() const { return item_num; }

    bool find(Value key, Value& result) const;
    bool insert(State* state, Value key, Value value);
    bool insertAnyway(State* state, Value key, Value value);
    bool assign(Value key, Value value);
    bool remove(State* state, Value key);
    bool import(State* state, HashTable *table);

    // Override
    bool indexRef(State* state, Value index, Value& dest) const;
    // Override
    bool indexSet(State* state, Value index, Value value);

    // For debugging
    void dump();
};

}

#endif

