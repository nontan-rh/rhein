//
// vmmain.cc
//

#include <cstdio>
#include <gc.h>

#include "vm.h"
#include "basic/basic.h"

using namespace rhein;
using namespace rhein::basic;

int main() {
    GC_init();
    State *state = new (GC_malloc(sizeof(State))) State();
    state->loadFile(stdin);
    state->loadModule(BasicModule::create(state));
    execute(state, state->string_provider->getString("entry"), 0, nullptr);
    return 0;
}

