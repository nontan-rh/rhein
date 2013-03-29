//
// object.cc
//

#include "common.h"

#include "object/object.h"
#include "object/imstring.h"
#include "object/record.h"
#include "vm.h"

using namespace rhein;

String*
Object::stringRepr(State* state) {
    return state->s_prv->getString("#<obj>");
}

Klass*
Klass::create(State* state, String* name, Klass* parent, unsigned slot_num, String** slot_ids) {
    void* p = state->ator->allocateObject<Klass>();
    RecordInfo* record_info;

    if (parent != nullptr) {
        record_info = RecordInfo::create(state, parent->record_info, slot_num, slot_ids);
    } else {
        record_info = RecordInfo::create(state, nullptr, slot_num, slot_ids);
    }

    return new (p) Klass(name, parent, record_info);
}

Klass*
rhein::get_klass(State* state, Value v) {
    if (is_obj(v)) {
        return get_obj<Object>(v)->getKlass();
    } else if (is_bool(v)) {
        return state->bool_klass;
    } else if (is_int(v)) {
        return state->int_klass;
    } else if (is_char(v)) {
        return state->char_klass;
    } else if (is_null(v)) {
        return state->null_klass;
    }
    return nullptr;
}

