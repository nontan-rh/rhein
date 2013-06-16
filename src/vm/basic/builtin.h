//
// builtin.h
//

#include "vm.h"

namespace rhein {
namespace builtin {

class BuiltinModule : public Module {
    static void* operator new (size_t /* size */, void* p) { return p; }
    //BasicModule() = default;

public:
    static BuiltinModule* create(State* R);
    bool initialize(State* R);
};

}

}

