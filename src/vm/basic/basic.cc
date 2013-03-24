//
// basic.cc
//

#include <cstdio>

#include "object/object.h"
#include "object/imstring.h"
#include "object/function.h"
#include "object/record.h"
#include "object/array.h"
#include "object/hashtable.h"
#include "vm.h"
#include "basic/basic.h"
#include "error.h"

namespace rhein {
namespace basic {

void
print_value(State* state, Value v) {
    if (is_int(v)) {
        printf("%ld", get_int(v));
    } else if (v == Ctrue) {
        printf("true");
    } else if (v == Cfalse) {
        printf("false");
    } else if (v == Cnull) {
        printf("null");
    } else if (is_obj(v)) {
        String* str = get_obj(v)->stringRepr(state);
        const char* cstr;
        size_t len;
        str->getCStr(cstr, len);
        for (unsigned i = 0; i < len; i++) {
            printf("%c", cstr[i]);
        }
    } else {
        fatal("Cannot print");
    }
}

Value
fn_print(State* state, unsigned argc, Value* args) {
    if (argc >= 1) {
        print_value(state, args[0]);
        for (unsigned i = 1; i < argc; i++) {
            printf(" ");
            print_value(state, args[i]);
        }
    }
    printf("\n");
    return Cnull;
}

Value
fn_new(State* state, unsigned argc, Value* args) {
    if (argc != 1) {
        fatal("Too many arguments for new");
    }

    Klass* k = get_klass(state, args[0]);
    return make_value(Record::create(state, k));
}

Value
fn_literal(State* state, unsigned argc, Value* args) {
    if (argc == 0) {
        fatal("Class required");
    }

    Klass* k = get_klass(state, args[0]);
    if (k == state->array_klass) {
        if (!(argc == 2 && get_klass(state, args[1]) == state->array_klass)) {
            fatal("Lack of argument");
        }

        return make_value(Array::literal(state, (Array*)get_obj(args[1])));
    } else if (k == state->hashtable_klass) {
        if (!(argc == 3
            && get_klass(state, args[1]) == state->array_klass
            && get_klass(state, args[2]) == state->array_klass)) {

            fatal("Lack of argument");
        }

        return make_value(HashTable::literal(state,
            (Array*)get_obj(args[1]),
            (Array*)get_obj(args[2])));
    } else {
        fatal("Not supported class");
    }
}

BasicModule*
BasicModule::create(State* state) {
    void* p = state->ator->allocateStruct<BasicModule>();
    return new (p) BasicModule;
}


bool
BasicModule::initialize(State* state) {
#define ADD_FUNC(x) state->addFunction(NativeFunction::create(state, \
    state->string_provider->getString(#x), fn_ ## x));
    ADD_FUNC(print);
    ADD_FUNC(new);
    ADD_FUNC(literal);
#undef ADD_FUNC
    return false;
}

}

}
