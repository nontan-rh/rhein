//
// builtin.h
//

#include "vm.h"
#include "internal.h"

namespace rhein {
namespace builtin {

class BuiltinModule : public Module, public PlacementNewObj {
public:
    static BuiltinModule* create();
    bool initialize();
};

}

}

