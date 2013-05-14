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

    if (!((get_klass(state, fn) == state->bytecode_function_klass
           || get_klass(state, fn) == state->native_function_klass)
          && get_klass(state, name) == state->string_klass)) {
        fatal("Invalid arguments");
    }

    state->addFunction((Function*)fn, (String*)name);
    return Cnull;
}

static Value
register_class(State* /* state */, unsigned /* argc */, Value* /* args */) {
    return Cnull;
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
#undef ADD_FUNC
    return false;
}

}

}

