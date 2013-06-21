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
register_function(State* R, unsigned argc, Value* args) {
    if (!(argc == 2)) {
        fatal("Invalid arguments");
    }

    Value fn = args[0];
    Value name = args[1];

    if (!((fn.get_klass(R) == R->bytecode_function_klass
           || fn.get_klass(R) == R->native_function_klass)
          && name.get_klass(R) == R->string_klass)) {
        fatal("Invalid arguments");
    }

    fn.get_obj<Function>()->resolve(R);

    R->addFunction(fn.get_obj<Function>(), name.get_obj<String>());
    return Value::k_nil();
}

static Value
register_class(State* R, unsigned argc, Value* args) {
    if (!(argc == 2)) {
        fatal("Invalid arguments");
    }

    Klass* klass = args[0].get_klass(R);
    Value name = args[1];

    if (name.get_klass(R) != R->string_klass) {
        fatal("Invalid arguments");
    }

    R->addKlass(klass, name.get_obj<String>());
    return Value::k_nil();
}

static Value
register_variable(State* R, unsigned argc, Value* args) {
    if (!((argc == 2)
          && args[0].get_klass(R) == R->string_klass)) {
        fatal("Invalid arguments");
    }

    R->addVariable(args[0].get_obj<String>(), args[1]);
    return Value::k_nil();
}

BuiltinModule*
BuiltinModule::create(State* R) {
    void* p = R->ator->allocateStruct<BuiltinModule>();
    return new (p) BuiltinModule;
}


bool
BuiltinModule::initialize(State* R) {
#define ADD_FUNC(n, x) R->addFunction(NativeFunction::create(R, \
    R->s_prv->get_string(n), x))
    ADD_FUNC("!!register_function", register_function);
    ADD_FUNC("!!register_class", register_class);
    ADD_FUNC("!!register_variable", register_variable);
#undef ADD_FUNC
    return false;
}

}

}

