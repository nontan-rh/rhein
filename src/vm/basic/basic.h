//
// basic.h
//

#ifndef BASIC_H
#define BASIC_H

#include "vm.h"
#include "internal.h"

namespace rhein {
namespace basic {

class BasicModule : public Module, public PlacementNewObj {
public:
    static BasicModule* create();
    bool initialize();
};

class List : public Object {
public:
    virtual Value get_head() = 0;
    virtual Value get_tail() = 0;

    bool slot_ref(Symbol* id, Value& dest) {
        State* R = get_current_state();
        if (id == R->get_symbol("head")) {
            dest = get_head();
            return true;
        } else if (id == R->get_symbol("tail")) {
            dest = get_tail();
            return true;
        }
        return false;
    }

protected:
    List(Class* klass) : Object(klass) { }
};

class SingleList : public List {
public:
    Value get_head() { return head_; }
    Value get_tail() {
        if (tail_ == nullptr) {
            return Value::k_nil(get_current_state()->get_class("List"));
        }
        return Value::by_object(tail_);
    }

    static SingleList* create(Value head, SingleList* tail) {
        void* p = get_current_state()->allocate_object<SingleList>();
        return new (p) SingleList(head, tail);
    }

private:
    Value head_;
    SingleList* tail_;

    SingleList(Value head, SingleList* tail)
        : List(get_current_state()->get_class("List")),
          head_(head), tail_(tail) { }
};

}

}

#endif // BASIC_H

