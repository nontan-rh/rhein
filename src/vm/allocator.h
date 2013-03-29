//
// allocator.h
//

#ifndef ALLOCATOR_H
#define ALLOCATOR_H

#include "common.h"

#include <gc.h>

#include "object/object.h"

namespace rhein {

class Allocator {
public:
    static void* operator new (size_t /* size */, void* p) { return p; }

    template <class T>
    void* allocateObject() {
        return GC_malloc(sizeof(T));
    }

    Value* allocateRawArray(unsigned size) {
        return (Value*)GC_malloc(sizeof(Value) * size);
    }

    template <class T>
    T* allocateBlock(unsigned size) {
        return (T*)GC_malloc(sizeof(T) * size);
    }

    void releaseBlock(void* p) {
        return GC_free(p);
    }

    template <class T>
    void* allocateStruct() {
        return GC_malloc(sizeof(T));
    }

    void releaseStruct(void* p) {
        return GC_free(p);
    }
};

}

#endif // ALLOCATOR_H

