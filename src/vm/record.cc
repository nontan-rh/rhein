//
// record.cc
//

#include <cstring>
#include <cassert>

#include "object.h"
#include "vm.h"

using namespace rhein;

RecordInfo::RecordInfo(State* R, RecordInfo* parent, unsigned slot_num_,
    Symbol** slot_ids) : num_slots_(slot_num_) {

    if (!(parent == nullptr || parent->id_index_table_ == nullptr)) {
        id_index_table_ = HashTable::create(R);
        id_index_table_->import(R, parent->id_index_table_);
    }

    if (num_slots_ == 0) {
        return;
    }

    if (id_index_table_ == nullptr) {
        id_index_table_ = HashTable::create(R);
    }

    unsigned base = id_index_table_->get_num_entries();
    for (unsigned i = 0; i < num_slots_; i++) {
        id_index_table_->insert_if_absent(R, Value::by_object(slot_ids[i]),
        		Value::by_int(base + i));
    }
}

RecordInfo*
RecordInfo::create(State* R, RecordInfo* parent, unsigned slot_num,
    Symbol** slot_ids) {

    void* p = R->ator->allocateStruct<RecordInfo>();
    return new (p) RecordInfo(R, parent, slot_num, slot_ids);
}

bool
RecordInfo::get_slot_index(Symbol* slot_id, unsigned& index) const {
    Value vindex;
    if (!id_index_table_->find(Value::by_object(slot_id), vindex)) {
        return false;
    }

    assert(0 <= vindex.get_int() && (size_t)vindex.get_int() < num_slots_);
    index = vindex.get_int();
    return true;
}

Record::Record(State* R, Class* klass)
    : Object(klass),
      member_slots_(R->ator->allocateRawArray(klass->get_record_info()->num_slots())) { }

Record*
Record::create(State* R, Class* klass) {
    assert(klass->has_record_info());
    void* p = R->ator->allocateObject<Record>();

    return new(p) Record(R, klass);
}

bool
Record::slot_ref(State* /* R */, Symbol* slot_id, Value& value) const {
    unsigned index;
    if (!klass_->get_record_info()->get_slot_index(slot_id, index)) {
        return false;
    }

    value = member_slots_[index];
    return true;
}

bool
Record::slot_set(State* /* R */, Symbol* slot_id, Value value) {
    unsigned index;
    if (!klass_->get_record_info()->get_slot_index(slot_id, index)) {
        return false;
    }

    member_slots_[index] = value;
    return true;
}

