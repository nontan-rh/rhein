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

    State *R = new (GC_malloc(sizeof(State))) State();
    R->loadModule(BasicModule::create(R));
    R->loadModule(BuiltinModule::create(R));

    load_script(R, argv[1]);

    execute(R, R->s_prv->get_string("main"), 0, nullptr);
    return 0;
}

