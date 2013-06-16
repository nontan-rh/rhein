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

RecordInfo::RecordInfo(State* R, RecordInfo* parent, unsigned slot_num_,
    String** slot_ids) : slot_num(slot_num_) {

    if (!(parent == nullptr || parent->id_index_table == nullptr)) {
        id_index_table = HashTable::create(R);
        id_index_table->import(R, parent->id_index_table);
    }

    if (slot_num == 0) {
        return;
    }

    if (id_index_table == nullptr) {
        id_index_table = HashTable::create(R);
    }

    unsigned base = id_index_table->get_num_entries();
    for (unsigned i = 0; i < slot_num; i++) {
        id_index_table->insert_if_absent(R, Value::by_object(slot_ids[i]),
        		Value::by_int(base + i));
    }
}

RecordInfo*
RecordInfo::create(State* R, RecordInfo* parent, unsigned slot_num,
    String** slot_ids) {

    void* p = R->ator->allocateStruct<RecordInfo>();
    return new (p) RecordInfo(R, parent, slot_num, slot_ids);
}

bool
RecordInfo::getSlotIndex(String* slot_id, unsigned& index) const {
    Value vindex;
    if (!id_index_table->find(Value::by_object(slot_id), vindex)) {
        return false;
    }

    assert(0 <= vindex.get_int() && (size_t)vindex.get_int() < slot_num);
    index = vindex.get_int();
    return true;
}

Record::Record(State* R, Klass* klass)
    : Object(klass),
      member_slots(R->ator->allocateRawArray(klass->get_record_info()->getSlotNum())) { }

Record*
Record::create(State* R, Klass* klass) {
    assert(klass->has_record_info());
    void* p = R->ator->allocateObject<Record>();

    return new(p) Record(R, klass);
}

bool
Record::slot_ref(State* /* R */, String* slot_id, Value& value) const {
    unsigned index;
    if (!klass->get_record_info()->getSlotIndex(slot_id, index)) {
        return false;
    }

    value = member_slots[index];
    return true;
}

bool
Record::slot_set(State* /* R */, String* slot_id, Value value) {
    unsigned index;
    if (!klass->get_record_info()->getSlotIndex(slot_id, index)) {
        return false;
    }

    member_slots[index] = value;
    return true;
}

