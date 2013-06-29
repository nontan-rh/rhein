//
// main.cc
//

#include <cstdio>
#include <gc.h>

#include "vm.h"
#include "loader.h"
#include "basic/basic.h"
#include "basic/builtin.h"
#include "basic/port.h"

using namespace rhein;
using namespace rhein::basic;
using namespace rhein::builtin;
using namespace rhein::port;

int main(int argc, char** argv) {
    GC_init();

    if (argc != 2) {
        fprintf(stderr, "Usage: rhein <bytecode>\n");
        return 1;
    }

    State *R = new (GC_malloc(sizeof(State))) State();
    R->load_module(BasicModule::create(R));
    R->load_module(BuiltinModule::create(R));
    R->load_module(FileModule::create(R));

    load_script(R, argv[1]);

    execute(R, R->get_symbol("main"), 0, nullptr);
    return 0;
}

