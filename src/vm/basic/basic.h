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

}

}

#endif // BASIC_H

