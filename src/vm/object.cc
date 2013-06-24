//
// object.cc
//

#include "object.h"
#include "vm.h"

using namespace rhein;

String*
Object::get_string_representation(State* R) {
    return String::create(R, "#<obj>");
}
Class::Class(State* R, Symbol* name_, Class* parent_, RecordInfo* record_info_)
    : Object(R->class_class), id_(name_), parent_(parent_), record_info_(record_info_) { }

Class*
Class::create(State* R, Symbol* name, Class* parent, unsigned slot_num, Symbol** slot_ids) {
    void* p = R->ator->allocateObject<Class>();
    RecordInfo* record_info;

    if (parent != nullptr) {
        record_info = RecordInfo::create(R, parent->record_info_, slot_num, slot_ids);
    } else {
        record_info = RecordInfo::create(R, nullptr, slot_num, slot_ids);
    }

    return new (p) Class(R, name, parent, record_info);
}

Class*
Value::get_class(State *R) const {
	switch (type_id_) {
	case Type::Nil:
		return R->nil_class;
	case Type::Bool:
		return R->bool_class;
	case Type::Int:
		return R->int_class;
	case Type::Undef:
		return R->nil_class;
	case Type::Char:
		return R->char_class;
	case Type::Object:
		return u_.v_obj_->get_class();
	}
	return nullptr;
}

