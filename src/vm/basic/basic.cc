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
print_value(Value v) {
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
        String* str = v.get_obj<Object>()->get_string_representation();
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
fn_print(unsigned argc, Value* args) {
    if (argc >= 1) {
        print_value(args[0]);
        for (unsigned i = 1; i < argc; i++) {
            printf(" ");
            print_value(args[i]);
        }
    }
    printf("\n");
    return Value::k_nil();
}

Value
fn_write(unsigned /* argc */, Value* args) {
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
        String* str = v.get_obj<Object>()->get_string_representation();
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
fn_input_0(unsigned /* argc */, Value* /* args */) {
    char buf[256];
    scanf("%255s", buf);

    return Value::by_object(String::create(buf));
}

Value
fn_input_1(unsigned /* argc */, Value* args) {
    print_value(args[0]);

    char buf[256];
    scanf("%255s", buf);

    return Value::by_object(String::create(buf));
}

Value
fn_new(unsigned /* argc */, Value* args) {
    return Value::by_record(Record::create(args[0].get_obj<Class>()));
}

Value
fn_literal(unsigned argc, Value* args) {
    State* R = get_current_state();
    if (argc == 0 || args[0].get_class() != R->get_class_class()) {
        fatal("Class required");
    }

    Class* k = args[0].get_obj<Class>();
    if (k == R->get_array_class()) {
        if (!(argc == 2 && args[1].get_class() == R->get_array_class())) {
            fatal("Lack of argument");
        }

        return Value::by_object(Array::literal(args[1].get_obj<Array>()));
    } else if (k == R->get_hashtable_class()) {
        if (!(argc == 3
            && args[1].get_class() == R->get_array_class()
            && args[2].get_class() == R->get_array_class())) {

            fatal("Lack of argument");
        }

        return Value::by_object(HashTable::literal(
            args[1].get_obj<Array>(),
            args[2].get_obj<Array>()));
    }
    fatal("Not supported class");
    return Value::k_nil();
}

Value
fn_to_array(unsigned /* argc */, Value* args) {
    Array* array;
    if (!args[0].get_obj<String>()->to_array(array)) {
        fatal("Error occurred");
    }

    return Value::by_object(array);
}

Value
fn_to_string(unsigned /* argc */, Value* args) {
    String* string;
    if (!args[0].get_obj<Array>()->to_string(string)) {
        fatal("Error occurred");
    }

    return Value::by_object(string);
}

Value
fn_list_to_string(unsigned /* argc */, Value* args) {
    size_t len;
    List* list = args[0].get_obj<List>();

    List* p = list;
    for (len = 0; p != nullptr; p = p->get_tail().get_obj<List>()) {
        if (!p->get_head().is(Value::Type::Char)) { throw ""; }
        len++;
    }
    char* buf = new char[len + 1];

    List* q = list;
    for (int i = 0; q != nullptr; i++, q = q->get_tail().get_obj<List>()) {
        buf[i] = q->get_head().get_char();
    }
    buf[len] = '\0';

    Value s = Value::by_object(String::create(buf, len));
    delete[] buf;
    return s;
}

Value
fn_nil_to_string(unsigned /* argc */, Value* /* args */) {
    return Value::by_object(String::create(""));
}

Value
fn_append(unsigned argc, Value* args) {
    State* R = get_current_state();
    String* result = args[0].get_obj<String>();
    for (unsigned i = 1; i < argc; i++) {
        if (args[i].get_class() != R->get_string_class()) {
            fatal("Cannot append");
        }

        result = result->append(args[i].get_obj<String>());
    }
    return Value::by_object(result);
}

Value
fn_head(unsigned argc, Value* args) {
    State* R = get_current_state();
    if (!(argc == 2
        && args[0].get_class() == R->get_string_class()
        && args[1].get_class() == R->get_int_class())) {

        fatal("Invalid arguments");
    }

    return Value::by_object(args[0].get_obj<String>()->head(args[1].get_int()));
}

Value
fn_tail(unsigned argc, Value* args) {
    State* R = get_current_state();
    if (!(argc == 2
        && args[0].get_class() == R->get_string_class()
        && args[1].get_class() == R->get_int_class())) {

        fatal("Invalid arguments");
    }

    return Value::by_object(args[0].get_obj<String>()->tail(args[1].get_int()));
}

Value
fn_sub(unsigned argc, Value* args) {
    State* R = get_current_state();
    if (!(argc == 3
        && args[0].get_class() == R->get_string_class()
        && args[1].get_class() == R->get_int_class()
        && args[2].get_class() == R->get_int_class())) {

        fatal("Invalid arguments");
    }

    return Value::by_object(args[0].get_obj<String>()->sub(args[1].get_int(), args[2].get_int()));
}

Value
fn_length(unsigned argc, Value* args) {
    State* R = get_current_state();
    if (argc != 1) {
        fatal("Invalid arguments");
    }

    if (args[0].get_class() == R->get_symbol_class()) {
        return Value::by_int(args[0].get_obj<Symbol>()->get_length());
    } else if (args[0].get_class() == R->get_string_class()) {
        return Value::by_int(args[0].get_obj<String>()->get_length());
    } else if (args[0].get_class() == R->get_array_class()) {
        return Value::by_int(args[0].get_obj<Array>()->get_length());
    } else {
        fatal("Cannot get length");
    }
    return Value::k_nil();
}

Value
fn_die(unsigned argc, Value* args) {
    if (argc >= 1) {
        fn_print(argc, args);
        fflush(stdout);
    }
    exit(1);
    // NOTREACHED
}

Value
fn_is_a(unsigned /* argc */, Value* args) {
    Class* objklass = args[0].get_class();
    Class* cmpklass = args[1].get_obj<Class>();
    for (; objklass != nullptr; objklass = objklass->get_parent()) {
        if (objklass == cmpklass) {
            return Value::k_true();
        }
    }
    return Value::k_false();
}

Value
fn_load(unsigned /* argc */, Value* args) {
    char *fn;
    const char *buf;
    size_t len;
    args[0].get_obj<String>()->get_cstr(buf, len);
    fn = (char*)malloc(sizeof(char) * (len + 1));
    memcpy(fn, buf, len);
    fn[len] = '\0';

    load_script(fn);

    free(fn);

    return Value::k_true();
}

Value
fn_callback(unsigned, Value* args) {
    return execute(args[0].get_obj<BytecodeFunction>(), 0, nullptr);
}

Value
fn_cons(unsigned, Value* args) {
    SingleList* tail;

    if (args[1].is(Value::Type::Nil)) {
        tail = nullptr;
    } else if (args[1].get_class()
            == get_current_state()->get_class("List")) {
        tail = args[1].get_obj<SingleList>();
    } else {
        return Value::k_nil();
    }

    return Value::by_object(SingleList::create(args[0], tail));
}

Value
fn_to_list(unsigned, Value* args) {
    String* s = args[0].get_obj<String>();
    SingleList* x = nullptr;

    if (s->get_length() == 0) { return Value::k_nil(); }

    for (int i = s->get_length() - 1; i >= 0; i--) {
        x = SingleList::create(Value::by_char(s->elt_ref(i)), x);
    }

    return Value::by_object(x);
}

BasicModule*
BasicModule::create() {
    State* R = get_current_state();
    void* p = R->allocate_struct<BasicModule>();
    return new (p) BasicModule;
}

bool
BasicModule::initialize() {
    State* R = get_current_state();
    R->add_class("List", "any");
    R->add_native_function("print", true, 0, {}, fn_print);
    R->add_native_function("write", false, 1, {"any"}, fn_write);
    R->add_native_function("input", false, 0, {}, fn_input_0);
    R->add_native_function("input", false, 1, {"any"}, fn_input_1);
    R->add_native_function("new", false, 1, {"class"}, fn_new);
    R->add_native_function("literal", true, 0, {}, fn_literal);
    R->add_native_function("to_array", false, 1, {"string"}, fn_to_array);
    R->add_native_function("to_string", false, 1, {"array"}, fn_to_string);
    R->add_native_function("to_string", false, 1, {"List"}, fn_list_to_string);
    R->add_native_function("to_string", false, 1, {"nil"}, fn_nil_to_string);
    R->add_native_function("append", true, 1, {"string"}, fn_append);
    R->add_native_function("head", false, 2, {"string", "int"}, fn_head);
    R->add_native_function("tail", false, 2, {"string", "int"}, fn_tail);
    R->add_native_function("sub", false, 3, {"string", "int", "int"}, fn_sub);
    R->add_native_function("length", false, 1, {"any"}, fn_length);
    R->add_native_function("die", true, 0, {}, fn_die);
    R->add_native_function("is_a", false, 2, {"any", "class"}, fn_is_a);
    R->add_native_function("load", false, 1, {"string"}, fn_load);
    R->add_native_function("cons", false, 2, {"any", "any"}, fn_cons);
    R->add_native_function("to_list", false, 1, {"string"}, fn_to_list);
    R->add_native_function("callback", false, 1, {"bytecode_function"}, fn_callback);
    return false;
}

}

}

