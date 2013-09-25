/*
 * port.cc
 */

#include <cstdio>
#include <cstdlib>
#include <cstring>

using namespace std;

#include "error.h"
#include "internal.h"
#include "object.h"
#include "port.h"

namespace rhein {
namespace port {

File::File(String* name, RWFlags rw, PosFlags pos)
    : Port(get_current_state()->get_class("File")) {

    if (rw == RWFlags::Read) {
        if (pos == PosFlags::Head) {
            const char* cstr;
            size_t len;
            name->get_cstr(cstr, len);
            char* buf = new char[len + 1];
            strncpy(buf, cstr, len);
            buf[len] = '\0';
            fp = fopen(buf, "r");
            if (fp == nullptr) {
                assert(false);
            }
            delete[] buf;
        } else if (pos == PosFlags::Tail) {
            fatal("not supported");
        }
    } else {
        fatal("not supported");
    }
}

File*
File::create(String* name, RWFlags rw, PosFlags pos) {
    State* R = get_current_state();
    return new (R->allocate_object<File>()) File(name, rw, pos);
}

Byte
File::read_byte() {
    int x = fgetc(fp);
    if (x == EOF) { assert(false); }
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
    int ch = fgetc(fp);
    if (!feof(fp) && ch != EOF) {
        ungetc(ch, fp);
        return false;
    }
    return true;
}

Value
fn_open(unsigned /* argc */, Value* args) {
    return Value::by_object(
            File::create(args[0].get_obj<String>(),
                    File::RWFlags::Read, File::PosFlags::Head));
}

Value
fn_read_char(unsigned /* argc */, Value* args) {
    return Value::by_char(args[0].get_obj<Port>()->read_char());
}

Value
fn_read_byte(unsigned /* argc */, Value* args) {
    return Value::by_int(args[0].get_obj<Port>()->read_byte());
}

Value
fn_eof(unsigned /* argc */, Value* args) {
    return Value::by_bool(args[0].get_obj<Port>()->eof());
}

Value
fn_make_portseq(unsigned /* argc */, Value* args) {
    PortSeq* p = PortSeq::create(args[0].get_obj<Port>());
    if (p == nullptr) { return Value::k_nil(); }
    return Value::by_object(p);
}

FileModule*
FileModule::create() {
    State* R = get_current_state();
    void* p = R->allocate_struct<FileModule>();
    return new (p) FileModule;
}

bool
FileModule::initialize() {
    State* R = get_current_state();
    R->add_class("File", "any");
    R->add_class("PortSeq", "List");
    R->add_native_function("open", false, 1, {"string"}, fn_open);
    R->add_native_function("read_char", false, 1, {"File"}, fn_read_char);
    R->add_native_function("read_byte", false, 1, {"File"}, fn_read_byte);
    R->add_native_function("eof", false, 1, {"File"}, fn_eof);
    R->add_native_function("make_portseq", false, 1, {"File"}, fn_make_portseq);
    return false;
}

}

}

