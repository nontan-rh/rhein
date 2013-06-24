//
// basic.h
//

#include "vm.h"
#include "internal.h"

namespace rhein {
namespace basic {

class BasicModule : public Module, public PlacementNewObj {
public:
    static BasicModule* create(State* R);
    bool initialize(State* R);
};

}

}

