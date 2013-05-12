//
// builtin.cc
//

namespace rhein {
namespace builtin {

BasicModule*
BasicModule::create(State* state) {
    void* p = state->ator->allocateStruct<BasicModule>();
    return new (p) BasicModule;
}


bool
BasicModule::initialize(State* state) {
#define ADD_FUNC(n, x) state->addFunction(NativeFunction::create(state, \
    state->s_prv->getString(n), x))
    ADD_FUNC("!!register_function", register_function)
#undef ADD_FUNC
    return false;
}

}

}

