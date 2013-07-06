/*
 * port.h
 */

#ifndef PORT_H
#define PORT_H

#include <cstdio>

#include "object.h"
#include "vm.h"

namespace rhein {
namespace port {

class Port : public Object {
public:
    Port(Class* klass) : Object(klass) {}
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

    static File* create(State* R, String* name, RWFlags rw, PosFlags pos);
    Byte read_byte();
    Char read_char();
    size_t read_bytes_as_possible_to_buffer(char* buf, size_t size);
    bool eof();

private:
    File(State* R, String* name, RWFlags rw, PosFlags pos);
    FILE* fp;
};

class FileModule : public Module, public PlacementNewObj {
public:
    static FileModule* create(State* R);
    bool initialize(State* R);
};

}
}

#endif // PORT_H
