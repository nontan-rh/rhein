//
// object.cc
//

#include "object/object.h"
#include "object/imstring.h"
#include "object/record.h"
#include "vm.h"

using namespace rhein;

String*
Object::get_string_representation(State* R) {
    return R->s_prv->get_string("#<obj>");
}

Klass*
Klass::create(State* R, String* name, Klass* parent, unsigned slot_num, String** slot_ids) {
    void* p = R->ator->allocateObject<Klass>();
    RecordInfo* record_info;

    if (parent != nullptr) {
        record_info = RecordInfo::create(R, parent->record_info, slot_num, slot_ids);
    } else {
        record_info = RecordInfo::create(R, nullptr, slot_num, slot_ids);
    }

    return new (p) Klass(name, parent, record_info);
}

Klass*
Value::get_klass(State *R) {
	switch (type_id) {
	case Type::Nil:
		return R->null_klass;
	case Type::Bool:
		return R->bool_klass;
	case Type::Int:
		return R->int_klass;
	case Type::Undef:
		return R->null_klass;
	case Type::Char:
		return R->char_klass;
	case Type::Object:
		return u.v_obj->get_class();
	}
	return nullptr;
}

