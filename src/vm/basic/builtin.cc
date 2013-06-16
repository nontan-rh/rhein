//
// builtin.cc
//

#include "vm.h"
#include "error.h"
#include "object/object.h"

#include "basic/builtin.h"

namespace rhein {
namespace builtin {

static Value
register_function(State* state, unsigned argc, Value* args) {
    if (!(argc == 2)) {
        fatal("Invalid arguments");
    }

    Value fn = args[0];
    Value name = args[1];

    if (!((fn.get_klass(state) == state->bytecode_function_klass
           || fn.get_klass(state) == state->native_function_klass)
          && name.get_klass(state) == state->string_klass)) {
        fatal("Invalid arguments");
    }

    fn.get_obj<Function>()->resolve(state);

    state->addFunction(fn.get_obj<Function>(), name.get_obj<String>());
    return Value::k_nil();
}

static Value
register_class(State* state, unsigned argc, Value* args) {
    if (!(argc == 2)) {
        fatal("Invalid arguments");
    }

    Klass* klass = args[0].get_klass(state);
    Value name = args[1];

    if (name.get_klass(state) != state->string_klass) {
        fatal("Invalid arguments");
    }

    state->addKlass(klass, name.get_obj<String>());
    return Value::k_nil();
}

static Value
register_variable(State* state, unsigned argc, Value* args) {
    if (!((argc == 2)
          && args[0].get_klass(state) == state->string_klass)) {
        fatal("Invalid arguments");
    }

    state->addVariable(args[0].get_obj<String>(), args[1]);
    return Value::k_nil();
}

BuiltinModule*
BuiltinModule::create(State* state) {
    void* p = state->ator->allocateStruct<BuiltinModule>();
    return new (p) BuiltinModule;
}


bool
BuiltinModule::initialize(State* state) {
#define ADD_FUNC(n, x) state->addFunction(NativeFunction::create(state, \
    state->s_prv->getString(n), x))
    ADD_FUNC("!!register_function", register_function);
    ADD_FUNC("!!register_class", register_class);
    ADD_FUNC("!!register_variable", register_variable);
#undef ADD_FUNC
    return false;
}

}

}

