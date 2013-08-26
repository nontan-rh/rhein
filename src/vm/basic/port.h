/*
 * port.h
 */

#ifndef PORT_H
#define PORT_H

#include <cstdio>

#include "object.h"
#include "vm.h"
#include "basic/basic.h"

namespace rhein {
namespace port {

using namespace rhein::basic;

class Port : public Object {
public:
    Port(Class* klass, NativeClass* nc) : Object(klass, nc) {}
    virtual Byte read_byte() = 0;
    virtual Char read_char() = 0;
    virtual size_t read_bytes_as_possible_to_buffer(char* buf, size_t size) = 0;
    virtual bool eof() = 0;
};

class File : public Port {
public:
    enum class RWFlags {
        Read = 0x01,
        Write = 0x02,
    };

    enum class PosFlags {
        Head,
        Tail,
    };

    static File* create(String* name, RWFlags rw, PosFlags pos);
    Byte read_byte();
    Char read_char();
    size_t read_bytes_as_possible_to_buffer(char* buf, size_t size);
    bool eof();

private:
    File(String* name, RWFlags rw, PosFlags pos);
    FILE* fp;
};

class FileModule : public Module, public PlacementNewObj {
public:
    static FileModule* create();
    bool initialize();
};

class PortSeq : public List {
public:
    static PortSeq* create(Port* p) {
        State* R = get_current_state();
        if (p->eof()) { return nullptr; }
        return new (R->allocate_object<PortSeq>()) PortSeq(p);
    }

    Value get_head() { return Value::by_char(get_head_char()); }
    Value get_tail() {
        PortSeq* t = get_tail_native();
        if (!t) { return Value::k_nil(); }
        return Value::by_object(get_tail_native());
    }

    Char get_head_char() {
        if (!already_read_) { read_next(); }
        return head_;
    }

    PortSeq* get_tail_native() {
        if (!already_read_) { read_next(); }
        return u_.tail_;
    }

    void read_next() {
        assert(!already_read_);
        head_ = u_.port_->read_char();
        u_.tail_ = create(u_.port_);
        already_read_ = true;
    }

private:
    PortSeq(Port* port)
        : List(get_current_state()->get_class("PortSeq")),
          already_read_(false), head_(0) {
        u_.port_ = port;
    }

    bool already_read_;
    Char head_;

    union {
        PortSeq* tail_;
        Port* port_;
    } u_;
};

}
}

#endif // PORT_H
