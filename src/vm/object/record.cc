//
// record.cc
//

#include <cstring>
#include <cassert>

#include "object/object.h"
#include "object/imstring.h"
#include "object/record.h"
#include "vm.h"

using namespace rhein;

RecordInfo::RecordInfo(State* state, RecordInfo* parent, unsigned slot_num_,
    String** slot_ids) : slot_num(slot_num_) {

    if (!(parent == nullptr || parent->id_index_table == nullptr)) {
        id_index_table = HashTable::create(state);
        id_index_table->import(state, parent->id_index_table);
    }

    if (slot_num == 0) {
        return;
    }

    if (id_index_table == nullptr) {
        id_index_table = HashTable::create(state);
    }

    unsigned base = id_index_table->getItemNumber();
    for (unsigned i = 0; i < slot_num; i++) {
        id_index_table->insert(state, Value::by_object(slot_ids[i]),
        		Value::by_int(base + i));
    }
}

RecordInfo*
RecordInfo::create(State* state, RecordInfo* parent, unsigned slot_num,
    String** slot_ids) {

    void* p = state->ator->allocateStruct<RecordInfo>();
    return new (p) RecordInfo(state, parent, slot_num, slot_ids);
}

bool
RecordInfo::getSlotIndex(String* slot_id, unsigned& index) const {
    Value vindex;
    if (!id_index_table->find(Value::by_object(slot_id), vindex)) {
        return false;
    }

    assert(0 <= vindex.get_int() && vindex.get_int() < slot_num);
    index = vindex.get_int();
    return true;
}

Record::Record(State* state, Klass* klass)
    : Object(klass),
      member_slots(state->ator->allocateRawArray(klass->getRecordInfo()->getSlotNum())) { }

Record*
Record::create(State* state, Klass* klass) {
    assert(klass->hasRecordInfo());
    void* p = state->ator->allocateObject<Record>();

    return new(p) Record(state, klass);
}

bool
Record::slotRef(State* /* state */, String* slot_id, Value& value) const {
    unsigned index;
    if (!klass->getRecordInfo()->getSlotIndex(slot_id, index)) {
        return false;
    }

    value = member_slots[index];
    return true;
}

bool
Record::slotSet(State* /* state */, String* slot_id, Value value) {
    unsigned index;
    if (!klass->getRecordInfo()->getSlotIndex(slot_id, index)) {
        return false;
    }

    member_slots[index] = value;
    return true;
}

