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
    const unsigned default_table_size = 16;
    const double rehash_ratio = 0.75;
    
    HashTableNode* table;
    unsigned table_size;
    unsigned num_entries;

    HashTable(State* R);

    void rehash(State* R);

public:
    static HashTable* create(State* R);
    static HashTable* literal(State* R, Array* key, Array* value);

    unsigned get_num_entries() const { return num_entries; }

    bool find(Value key, Value& result) const;
    bool insert_if_absent(State* R, Value key, Value value);
    bool insert(State* R, Value key, Value value);
    bool assign(Value key, Value value);
    bool remove(State* R, Value key);
    bool import(State* R, HashTable *table);

    // Override
    bool index_ref(State* R, Value index, Value& dest) const;
    // Override
    bool index_set(State* R, Value index, Value value);

    // For debugging
    void dump();
};

}

#endif

