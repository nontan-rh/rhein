//
// basic.cc
//

#include <cstdio>
#include <cstring>

#include "object.h"
#include "vm.h"
#include "loader.h"
#include "basic/basic.h"
#include "error.h"

namespace rhein {
namespace basic {

void
print_value(State* R, Value v) {
    if (v.is(Value::Type::Int)) {
        printf("%d", v.get_int());
    } else if (v.is(Value::Type::Char)) {
        printf("?%c", v.get_char());
    } else if (v.is(Value::Type::Bool)) {
    	if (v.get_bool()) {
    		printf("true");
    	} else {
    		printf("false");
    	}
    } else if (v.is(Value::Type::Nil)) {
        printf("nil");
    } else if (v.is(Value::Type::Object)) {
        String* str = v.get_obj<Object>()->get_string_representation(R);
        const char* cstr;
        size_t len;
        str->get_cstr(cstr, len);
        for (unsigned i = 0; i < len; i++) {
            printf("%c", cstr[i]);
        }
    } else {
        fatal("Cannot print");
    }
}

Value
fn_print(State* R, unsigned argc, Value* args) {
    if (argc >= 1) {
        print_value(R, args[0]);
        for (unsigned i = 1; i < argc; i++) {
            printf(" ");
            print_value(R, args[i]);
        }
    }
    printf("\n");
    return Value::k_nil();
}

Value
fn_write(State* R, unsigned argc, Value* args) {
    if (argc != 1) {
        fatal("Invalid arguments");
    }

    Value v = args[0];
    if (v.is(Value::Type::Int)) {
        printf("%d", v.get_int());
    } else if (v.is(Value::Type::Char)) {
        printf("%c", v.get_char());
    } else if (v.is(Value::Type::Bool)) {
    	if (v.get_bool()) {
            printf("true");
    	} else {
    		printf("false");
    	}
    } else if (v.is(Value::Type::Nil)) {
        printf("nil");
    } else if (v.is(Value::Type::Object)) {
        String* str = v.get_obj<Object>()->get_string_representation(R);
        const char* cstr;
        size_t len;
        str->get_cstr(cstr, len);
        for (unsigned i = 0; i < len; i++) {
            printf("%c", cstr[i]);
        }
    } else {
        fatal("Cannot print");
    }
    return Value::k_nil();
}

Value
fn_input(State* R, unsigned argc, Value* args) {
    if (argc >= 2) {
        fatal("Invalid arguments");
    }

    if (argc == 1) {
        print_value(R, args[0]);
    }

    char buf[256];
    scanf("%255s", buf);

    return Value::by_object(String::create(R, buf));
}

Value
fn_new(State* R, unsigned argc, Value* args) {
    if (argc != 1) {
        fatal("Too many arguments for new");
    }

    if (args[0].get_klass(R) != R->class_class) {
    	return Value::k_nil();
    }
    return Value::by_object(Record::create(R, args[0].get_obj<Klass>()));
}

Value
fn_literal(State* R, unsigned argc, Value* args) {
    if (argc == 0 || args[0].get_klass(R) != R->class_class) {
        fatal("Class required");
    }

    Klass* k = args[0].get_obj<Klass>();
    if (k == R->array_class) {
        if (!(argc == 2 && args[1].get_klass(R) == R->array_class)) {
            fatal("Lack of argument");
        }

        return Value::by_object(Array::literal(R,
        		args[1].get_obj<Array>()));
    } else if (k == R->hashtable_class) {
        if (!(argc == 3
            && args[1].get_klass(R) == R->array_class
            && args[2].get_klass(R) == R->array_class)) {

            fatal("Lack of argument");
        }

        return Value::by_object(HashTable::literal(R,
            args[1].get_obj<Array>(),
            args[2].get_obj<Array>()));
    }
    fatal("Not supported class");
    return Value::k_nil();
}

Value
fn_to_array(State* R, unsigned argc, Value* args) {
    if (!(argc == 1 && args[0].get_klass(R) == R->string_class)) {
        fatal("Invalid arguments");
    }

    Array* array;
    if (!args[0].get_obj<String>()->to_array(R, array)) {
        fatal("Error occurred");
    }

    return Value::by_object(array);
}

Value
fn_to_string(State* R, unsigned argc, Value* args) {
    if (!(argc == 1 && args[0].get_klass(R) == R->array_class)) {
        fatal("Invalid arguments");
    }

    String* string;
    if (!args[0].get_obj<Array>()->to_string(R, string)) {
        fatal("Error occurred");
    }

    return Value::by_object(string);
}

Value
fn_append(State* R, unsigned argc, Value* args) {
    if (argc == 0) {
        return Value::by_object(String::create(R, ""));
    }

    if (args[0].get_klass(R) != R->string_class) {
        fatal("Cannot append");
    }

    String* result = args[0].get_obj<String>();
    for (unsigned i = 1; i < argc; i++) {
        if (args[i].get_klass(R) != R->string_class) {
            fatal("Cannot append");
        }

        result = result->append(R, args[i].get_obj<String>());
    }
    return Value::by_object(result);
}

Value
fn_head(State* R, unsigned argc, Value* args) {
    if (!(argc == 2
        && args[0].get_klass(R) == R->string_class
        && args[1].get_klass(R) == R->int_class)) {

        fatal("Invalid arguments");
    }

    return Value::by_object(args[0].get_obj<String>()->head(R, args[1].get_int()));
}

Value
fn_tail(State* R, unsigned argc, Value* args) {
    if (!(argc == 2
        && args[0].get_klass(R) == R->string_class
        && args[1].get_klass(R) == R->int_class)) {

        fatal("Invalid arguments");
    }

    return Value::by_object(args[0].get_obj<String>()->tail(R, args[1].get_int()));
}

Value
fn_sub(State* R, unsigned argc, Value* args) {
    if (!(argc == 3
        && args[0].get_klass(R) == R->string_class
        && args[1].get_klass(R) == R->int_class
        && args[2].get_klass(R) == R->int_class)) {

        fatal("Invalid arguments");
    }

    return Value::by_object(args[0].get_obj<String>()->sub(R, args[1].get_int(), args[2].get_int()));
}

Value
fn_length(State* R, unsigned argc, Value* args) {
    if (argc != 1) {
        fatal("Invalid arguments");
    }

    if (args[0].get_klass(R) == R->symbol_class) {
        return Value::by_int(args[0].get_obj<Symbol>()->get_length());
    } else if (args[0].get_klass(R) == R->string_class) {
    	return Value::by_int(args[0].get_obj<String>()->get_length());
    } else if (args[0].get_klass(R) == R->array_class) {
        return Value::by_int(args[0].get_obj<Array>()->get_length());
    } else {
        fatal("Cannot get length");
    }
    return Value::k_nil();
}

Value
fn_die(State* R, unsigned argc, Value* args) {
    if (argc >= 1) {
        fn_print(R, argc, args);
        fflush(stdout);
    }
    exit(1);
    // NOTREACHED
}

Value
fn_is_a(State* R, unsigned argc, Value* args) {
    if (!(argc == 2 && args[1].get_klass(R) == R->class_class)) {
        fatal("Invalid arguments");
    }

    Klass* objklass = args[0].get_klass(R);
    Klass* cmpklass = args[1].get_obj<Klass>();
    for (; objklass != nullptr; objklass = objklass->get_parent()) {
        if (objklass == cmpklass) {
            return Value::k_true();
        }
    }
    return Value::k_false();
}

Value
fn_load(State* R, unsigned argc, Value* args) {
    if (!(argc == 1 && args[0].get_klass(R) == R->string_class)) {
        fatal("Invalid arguments");
    }

    char *fn;
    const char *buf;
    size_t len;
    args[0].get_obj<String>()->get_cstr(buf, len);
    fn = (char*)malloc(sizeof(char) * (len + 1));
    memcpy(fn, buf, len);
    fn[len] = '\0';

    load_script(R, fn);

    free(fn);

    return Value::k_true();
}

BasicModule*
BasicModule::create(State* R) {
    void* p = R->ator->allocateStruct<BasicModule>();
    return new (p) BasicModule;
}

static inline void
add_function(State* R, const char* name, NativeFunctionBody fn) {
	R->addFunction(NativeFunction::create(R,
			FunctionInfo::create(R, R->s_prv->get_symbol(name)), fn));
}

bool
BasicModule::initialize(State* R) {
	add_function(R, "print", fn_print);
	add_function(R, "input", fn_input);
	add_function(R, "new", fn_new);
	add_function(R, "literal", fn_literal);
	add_function(R, "to_array", fn_to_array);
	add_function(R, "to_string", fn_to_string);
	add_function(R, "append", fn_append);
	add_function(R, "head", fn_head);
	add_function(R, "tail", fn_tail);
	add_function(R, "sub", fn_sub);
	add_function(R, "length", fn_length);
	add_function(R, "die", fn_die);
	add_function(R, "is_a", fn_is_a);
	add_function(R, "print", fn_load);
    return false;
}

}

}

