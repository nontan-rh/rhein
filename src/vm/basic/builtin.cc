//
// builtin.cc
//

#include "vm.h"
#include "error.h"
#include "object.h"

#include "basic/builtin.h"

namespace rhein {
namespace builtin {

static Value
register_function(State* R, unsigned /* argc */, Value* args) {
    Value fn = args[0];
    Value name = args[1];

    fn.get_obj<Function>()->resolve(R);

    R->add_function(fn.get_obj<Function>(), name.get_obj<Symbol>());
    return Value::k_nil();
}

static Value
register_class(State* R, unsigned /* argc */, Value* args) {
    Class* klass = args[0].get_obj<Class>();
    Symbol* name = args[1].get_obj<Symbol>();
    R->add_class(klass, name);
    return Value::k_nil();
}

static Value
register_variable(State* R, unsigned /* argc */, Value* args) {
    R->add_variable(args[0].get_obj<Symbol>(), args[1]);
    return Value::k_nil();
}

BuiltinModule*
BuiltinModule::create(State* R) {
    void* p = R->allocate_struct<BuiltinModule>();
    return new (p) BuiltinModule;
}

bool
BuiltinModule::initialize(State* R) {
    R->add_native_function("!!register_function", false, 2, {"any", "symbol"},
            register_function);
    R->add_native_function("!!register_class", false, 2, {"class", "symbol"},
            register_class);
    R->add_native_function("!!register_variable", false, 2, {"symbol", "any"},
            register_variable);
    return false;
}

}

}

