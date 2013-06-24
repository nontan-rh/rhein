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

    if (!((fn.get_class(R) == R->bytecode_function_class
           || fn.get_class(R) == R->native_function_class)
          && name.get_class(R) == R->symbol_class)) {
        fatal("Invalid arguments");
    }

    fn.get_obj<Function>()->resolve(R);

    R->addFunction(fn.get_obj<Function>(), name.get_obj<Symbol>());
    return Value::k_nil();
}

static Value
register_class(State* R, unsigned argc, Value* args) {
    if (!(argc == 2)) {
        fatal("Invalid arguments");
    }

    Class* klass = args[0].get_class(R);
    Value name = args[1];

    if (name.get_class(R) != R->symbol_class) {
        fatal("Invalid arguments");
    }

    R->addClass(klass, name.get_obj<Symbol>());
    return Value::k_nil();
}

static Value
register_variable(State* R, unsigned argc, Value* args) {
    if (!((argc == 2)
          && args[0].get_class(R) == R->symbol_class)) {
        fatal("Invalid arguments");
    }

    R->addVariable(args[0].get_obj<Symbol>(), args[1]);
    return Value::k_nil();
}

BuiltinModule*
BuiltinModule::create(State* R) {
    void* p = R->ator->allocateStruct<BuiltinModule>();
    return new (p) BuiltinModule;
}

static inline void
add_function(State* R, const char* name, NativeFunctionBody fn) {
	R->addFunction(NativeFunction::create(R,
			FunctionInfo::create(R, R->s_prv->get_symbol(name)), fn));
}

bool
BuiltinModule::initialize(State* R) {
    add_function(R, "!!register_function", register_function);
    add_function(R, "!!register_class", register_class);
    add_function(R, "!!register_variable", register_variable);
    return false;
}

}

}

