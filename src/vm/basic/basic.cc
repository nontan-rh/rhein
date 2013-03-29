//
// basic.cc
//

#include "common.h"

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
    } else if (is_char(v)) {
        printf("?%c", get_char(v));
    } else if (v == Ctrue) {
        printf("true");
    } else if (v == Cfalse) {
        printf("false");
    } else if (v == Cnull) {
        printf("null");
    } else if (is_obj(v)) {
        String* str = get_obj<Object>(v)->stringRepr(state);
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
fn_input(State* state, unsigned argc, Value* args) {
    if (argc >= 2) {
        fatal("Invalid arguments");
    }

    if (argc == 1) {
        print_value(state, args[0]);
    }

    char buf[256];
    scanf("%256s", buf);

    return obj2value(state->s_prv->getString(buf));
}

Value
fn_new(State* state, unsigned argc, Value* args) {
    if (argc != 1) {
        fatal("Too many arguments for new");
    }

    Klass* k = get_klass(state, args[0]);
    return obj2value(Record::create(state, k));
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

        return obj2value(Array::literal(state, get_obj<Array>(args[1])));
    } else if (k == state->hashtable_klass) {
        if (!(argc == 3
            && get_klass(state, args[1]) == state->array_klass
            && get_klass(state, args[2]) == state->array_klass)) {

            fatal("Lack of argument");
        }

        return obj2value(HashTable::literal(state,
            get_obj<Array>(args[1]),
            get_obj<Array>(args[2])));
    } else {
        fatal("Not supported class");
    }
}

Value
fn_to_array(State* state, unsigned argc, Value* args) {
    if (!(argc == 1 && get_klass(state, args[0]) == state->string_klass)) {
        fatal("Invalid arguments");
    }

    Array* array;
    if (!get_obj<String>(args[0])->toArray(state, array)) {
        fatal("Error occured");
    }

    return obj2value(array);
}

Value
fn_to_string(State* state, unsigned argc, Value* args) {
    if (!(argc == 1 && get_klass(state, args[0]) == state->array_klass)) {
        fatal("Invalid arguments");
    }

    String* string;
    if (!get_obj<Array>(args[0])->toString(state, string)) {
        fatal("Error occured");
    }

    return obj2value(string);
}

Value
fn_append(State* state, unsigned argc, Value* args) {
    if (argc == 0) {
        return obj2value(state->s_prv->getString(""));
    }

    if (get_klass(state, args[0]) != state->string_klass) {
        fatal("Cannot append");
    }

    String* result = get_obj<String>(args[0]);
    for (unsigned i = 1; i < argc; i++) {
        if (get_klass(state, args[i]) != state->string_klass) {
            fatal("Cannot append");
        }

        result = result->append(state, get_obj<String>(args[i]));
    }
    return obj2value(result);
}

Value
fn_head(State* state, unsigned argc, Value* args) {
    if (!(argc == 2
        && get_klass(state, args[0]) == state->string_klass
        && get_klass(state, args[1]) == state->int_klass)) {

        fatal("Invalid arguments");
    }

    return obj2value(get_obj<String>(args[0])->head(state, get_int(args[1])));
}

Value
fn_tail(State* state, unsigned argc, Value* args) {
    if (!(argc == 2
        && get_klass(state, args[0]) == state->string_klass
        && get_klass(state, args[1]) == state->int_klass)) {

        fatal("Invalid arguments");
    }

    return obj2value(get_obj<String>(args[0])->tail(state, get_int(args[1])));
}

Value
fn_sub(State* state, unsigned argc, Value* args) {
    if (!(argc == 3
        && get_klass(state, args[0]) == state->string_klass
        && get_klass(state, args[1]) == state->int_klass
        && get_klass(state, args[2]) == state->int_klass)) {

        fatal("Invalid arguments");
    }

    return obj2value(get_obj<String>(args[0])->sub(state, get_int(args[1]), get_int(args[2])));
}

Value
fn_length(State* state, unsigned argc, Value* args) {
    if (argc != 1) {
        fatal("Invalid arguments");
    }

    if (get_klass(state, args[0]) == state->string_klass) {
        return int2value(get_obj<String>(args[0])->getLength());
    } else if (get_klass(state, args[0]) == state->array_klass) {
        return int2value(get_obj<Array>(args[0])->getLength());
    } else {
        fatal("Cannot get length");
    }
    return Cnull;
}

Value
fn_die(State* state, unsigned argc, Value* args) {
    if (argc >= 1) {
        fn_print(state, argc, args);
        fflush(stdout);
    }
    exit(1);
    // NOTREACHED
}

BasicModule*
BasicModule::create(State* state) {
    void* p = state->ator->allocateStruct<BasicModule>();
    return new (p) BasicModule;
}


bool
BasicModule::initialize(State* state) {
#define ADD_FUNC(x) state->addFunction(NativeFunction::create(state, \
    state->s_prv->getString(#x), fn_ ## x));
    ADD_FUNC(print);
    ADD_FUNC(input);
    ADD_FUNC(new);
    ADD_FUNC(literal);
    ADD_FUNC(to_array);
    ADD_FUNC(to_string);
    ADD_FUNC(append);
    ADD_FUNC(head);
    ADD_FUNC(tail);
    ADD_FUNC(sub);
    ADD_FUNC(length);
    ADD_FUNC(die);
#undef ADD_FUNC
    return false;
}

}

}

