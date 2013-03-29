//
// vmmain.cc
//

#include "common.h"

#include <cstdio>
#include <gc.h>

#include "vm.h"
#include "basic/basic.h"

using namespace rhein;
using namespace rhein::basic;

int main(int argc, char** argv) {
    GC_init();

    if (argc != 2) {
        fprintf(stderr, "Usage: rheinvm <bytecode>\n");
        return 1;
    }

    State *state = new (GC_malloc(sizeof(State))) State();
    state->loadModule(BasicModule::create(state));

    FILE* fp = fopen(argv[1] ,"r");
    if (fp == nullptr) {
        fprintf(stderr, "Cannot open bytecode file\n");
        return 1;
    }
    state->loadFile(fp);
    fclose(fp);

    execute(state, state->s_prv->getString("main"), 0, nullptr);
    return 0;
}

