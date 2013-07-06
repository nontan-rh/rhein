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

size_t
File::read_bytes_as_possible_to_buffer(char* buf, size_t size) {
    return fread(buf, 1, size, fp);
}

bool
File::eof() {
    return feof(fp);
}

Value
fn_open(State* R, unsigned /* argc */, Value* args) {
    return Value::by_object(
            File::create(R, args[0].get_obj<String>(),
                    File::RWFlags::Read, File::PosFlags::Head));
}

Value
fn_read_char(State* /* R */, unsigned /* argc */, Value* args) {
    return Value::by_char(args[0].get_obj<Port>()->read_char());
}

Value
fn_read_byte(State* /* R */, unsigned /* argc */, Value* args) {
    return Value::by_int(args[0].get_obj<Port>()->read_byte());
}

Value
fn_eof(State* /* R */, unsigned /* argc */, Value* args) {
    return Value::by_bool(args[0].get_obj<Port>()->eof());
}

FileModule*
FileModule::create(State* R) {
    void* p = R->allocate_struct<FileModule>();
    return new (p) FileModule;
}

bool
FileModule::initialize(State* R) {
    R->add_class("File", "any");
    R->add_native_function("open", false, 1, {"string"}, fn_open);
    R->add_native_function("read_char", false, 1, {"File"}, fn_read_char);
    R->add_native_function("read_byte", false, 1, {"File"}, fn_read_byte);
    R->add_native_function("eof", false, 1, {"File"}, fn_eof);
    return false;
}

}

}

