//
// main.cc
//

#include <cstdio>
#include <gc.h>

#include "vm.h"
#include "loader.h"
#include "basic/basic.h"
#include "basic/builtin.h"

using namespace rhein;
using namespace rhein::basic;
using namespace rhein::builtin;

int main(int argc, char** argv) {
    GC_init();

    if (argc != 2) {
        fprintf(stderr, "Usage: rhein <bytecode>\n");
        return 1;
    }

    State *state = new (GC_malloc(sizeof(State))) State();
    state->loadModule(BasicModule::create(state));
    state->loadModule(BuiltinModule::create(state));

    load_script(state, argv[1]);

    execute(state, state->s_prv->getString("main"), 0, nullptr);
    return 0;
}

