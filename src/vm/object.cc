//
// object.cc
//

#include "object.h"
#include "vm.h"

namespace rhein {

String*
Object::get_string_representation(State* R) {
    return String::create(R, "#<obj>");
}
Class::Class(State* R, Symbol* name_, Class* parent_, RecordInfo* record_info_)
    : Object(R->get_class_class()), id_(name_), parent_(parent_), record_info_(record_info_) { }

Class*
Class::create(State* R, Symbol* name, Class* parent, unsigned slot_num, Symbol** slot_ids) {
    void* p = R->allocate_object<Class>();
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
        return R->get_nil_class();
    case Type::Bool:
        return R->get_bool_class();
    case Type::Int:
        return R->get_int_class();
    case Type::Undef:
        return R->get_nil_class();
    case Type::Char:
        return R->get_char_class();
    case Type::Object:
        return u_.v_obj_->get_class();
    }
    return nullptr;
}


bool
Class::is_subclass_of(Class* k) const {
    for (const Class* s = this; s; s = s->parent_) {
        if (s == k) {
            return true;
        }
    }
    return false;
}

}
