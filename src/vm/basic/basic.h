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
    static BasicModule* create(State* R);
    bool initialize(State* R);
};

class List : public Object {
public:
    virtual Value get_head(State* R) = 0;
    virtual Value get_tail(State* R) = 0;

    bool slot_ref(State* R, Symbol* id, Value& dest) {
        if (id == R->get_symbol("head")) {
            dest = get_head(R);
            return true;
        } else if (id == R->get_symbol("tail")) {
            dest = get_tail(R);
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

