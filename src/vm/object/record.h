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

    RecordInfo(State* R, RecordInfo* parent, unsigned slot_num_,
        String** slot_ids);

public:
    static RecordInfo* create(State* R, RecordInfo* parent, unsigned slot_num,
        String** slot_ids);

    unsigned getSlotNum() const { return slot_num; }
    bool getSlotIndex(String* slot_id, unsigned& index) const;
};

class Record : public Object {
    Value* member_slots;

    Record(State* R, Klass* klass);

public:
    static Record* create(State* R, Klass* klass);

    // Override
    bool slot_ref(State* /* R */, String* slot_id, Value& value) const;
    // Override
    bool slot_set(State* /* R */, String* slot_id, Value value);
};

};

#endif // RECORD_H

