//
// record.h
//

#ifndef RECORD_H
#define RECORD_H

#include "object/object.h"
#include "object/hashtable.h"

namespace rhein {

class State;

class RecordInfo {
    unsigned slot_num;
    HashTable* id_index_table;

    static void* operator new(size_t /* size */, void *p) { return p; }

    RecordInfo(State* state, RecordInfo* parent, unsigned slot_num_,
        String** slot_ids);

public:
    static RecordInfo* create(State* state, RecordInfo* parent, unsigned slot_num,
        String** slot_ids);

    unsigned getSlotNum() const { return slot_num; }
    bool getSlotIndex(String* slot_id, unsigned& index) const;
};

class Record : public Object {
    Value* member_slots;

    Record(State* state, Klass* klass);

public:
    static Record* create(State* state, Klass* klass);

    // Override
    bool slotRef(State* /* state */, String* slot_id, Value& value) const;
    // Override
    bool slotSet(State* /* state */, String* slot_id, Value value);
};

};

#endif // RECORD_H

