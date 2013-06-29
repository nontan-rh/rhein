/*
 * port.cc
 */

#include <cstdio>
#include <cstdlib>

using namespace std;

#include "error.h"
#include "internal.h"
#include "object.h"
#include "port.h"

namespace rhein {
namespace port {

File::File(State* R, String* name, RWFlags rw, PosFlags pos)
    : Port(R->get_class("File")) {

    if (rw == RWFlags::Read) {
        if (pos == PosFlags::Head) {
            const char* cstr;
            size_t len;
            name->get_cstr(cstr, len);
            fp = fopen(cstr, "r");
            if (fp == nullptr) {
                throw "";
            }
        } else if (pos == PosFlags::Tail) {
            fatal("not supported");
        }
    } else {
        fatal("not supported");
    }
}

File*
File::create(State* R, String* name, RWFlags rw, PosFlags pos) {
    return new (R->allocate_object<File>()) File(R, name, rw, pos);
}

Byte
File::read_byte() {
    return fgetc(fp);
}

Char
File::read_char() {
    return fgetc(fp);
}

bool
File::eof() {
    return feof(fp);
}

Value
fn_open(State* R, unsigned argc, Value* args) {
    if (!(argc == 1 && args[0].get_class(R) == R->get_string_class())) {
        fatal("Invalid arguments");
    }

    return Value::by_object(
            File::create(R, args[0].get_obj<String>(),
                    File::RWFlags::Read, File::PosFlags::Head));
}

Value
fn_read_char(State* R, unsigned argc, Value* args) {
    if (!(argc == 1 && args[0].get_class(R) == R->get_class("File"))) {
        fatal("Invalid arguments");
    }
    return Value::by_char(args[0].get_obj<Port>()->read_char());
}

Value
fn_read_byte(State* R, unsigned argc, Value* args) {
    if (!(argc == 1 && args[0].get_class(R) == R->get_class("File"))) {
        fatal("Invalid arguments");
    }
    return Value::by_int(args[0].get_obj<Port>()->read_byte());
}

Value
fn_eof(State* R, unsigned argc, Value* args) {
    if (!(argc == 1 && args[0].get_class(R) == R->get_class("File"))) {
        fatal("Invalid arguments");
    }
    return Value::by_bool(args[0].get_obj<Port>()->eof());
}

FileModule*
FileModule::create(State* R) {
    void* p = R->allocate_struct<FileModule>();
    return new (p) FileModule;
}

static inline void
add_function(State* R, const char* name, NativeFunctionBody fn) {
    R->add_function(NativeFunction::create(R,
            FunctionInfo::create(R, R->get_symbol(name)), fn));
}

static inline void
add_class(State* R, const char* name, const char* parent) {
    R->add_class(Class::create(R,
            R->get_symbol(name), R->get_class(parent)));
}

bool
FileModule::initialize(State* R) {
    add_class(R, "File", "any");
    add_function(R, "open", fn_open);
    add_function(R, "read_char", fn_read_char);
    add_function(R, "read_byte", fn_read_byte);
    add_function(R, "eof", fn_eof);
    return false;
}

}
}




