//
// record.cc
//

#include <cstring>
#include <cassert>

#include "systable.h"
#include "object.h"
#include "vm.h"

namespace rhein {

RecordInfo::RecordInfo(RecordInfo* parent, unsigned slot_num_,
    Symbol** slot_ids) : num_slots_(slot_num_) {

    if (!(parent == nullptr || parent->id_index_table_ == nullptr)) {
        id_index_table_ = SysTable<const Symbol*, unsigned>::create();
        id_index_table_->import(parent->id_index_table_);
    }

    if (num_slots_ == 0) { return; }

    if (id_index_table_ == nullptr) {
        id_index_table_ = SysTable<const Symbol*, unsigned>::create();
    }

    unsigned base = id_index_table_->get_num_entries();
    for (unsigned i = 0; i < num_slots_; i++) {
        id_index_table_->insert_if_absent(slot_ids[i], base + i);
    }
}

RecordInfo*
RecordInfo::create(RecordInfo* parent, unsigned slot_num,
    Symbol** slot_ids) {

    void* p = get_current_state()->allocate_struct<RecordInfo>();
    return new (p) RecordInfo(parent, slot_num, slot_ids);
}

bool
RecordInfo::get_slot_index(Symbol* slot_id, unsigned& index) const {
    if (!id_index_table_->exists(slot_id)) { return false; }
    index = id_index_table_->find(slot_id);
    return true;
}

Record::Record(Class* klass)
    : klass_(klass),
      inherit_instance_(Value::k_undef()),
      member_slots_(get_current_state()->allocate_raw_array(klass->get_record_info()->num_slots())) { }

Record*
Record::create(Class* klass) {
    assert(klass->has_record_info());
    void* p = get_current_state()->allocate_object<Record>();

    return new(p) Record(klass);
}

bool
Record::slot_ref(Symbol* slot_id, Value& value) {
    unsigned index;
    if (!klass_->get_record_info()->get_slot_index(slot_id, index)) {
        return false;
    }

    value = member_slots_[index];
    return true;
}

bool
Record::slot_set(Symbol* slot_id, Value value) {
    unsigned index;
    if (!klass_->get_record_info()->get_slot_index(slot_id, index)) {
        return false;
    }

    member_slots_[index] = value;
    return true;
}

}
