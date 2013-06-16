//
// basic.h
//

#include "vm.h"

namespace rhein {
namespace basic {

class BasicModule : public Module {
    static void* operator new (size_t /* size */, void* p) { return p; }
    //BasicModule() = default;

public:
    static BasicModule* create(State* R);
    bool initialize(State* R);
};

Value fn_load(State* R, unsigned argc, Value* args);

}

}

