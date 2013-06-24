//
// allocator.h
//

#ifndef ALLOCATOR_H
#define ALLOCATOR_H

#include <gc.h>

#include "object.h"

namespace rhein {

class Allocator {
public:
    static void* operator new (size_t /* size */, void* p) { return p; }

    template <class T>
    void* allocate_object() {
        return GC_malloc(sizeof(T));
    }

    Value* allocate_raw_array(unsigned size) {
        return (Value*)GC_malloc(sizeof(Value) * size);
    }

    template <class T>
    T* allocate_block(unsigned size) {
        return (T*)GC_malloc(sizeof(T) * size);
    }

    void release_block(void* p) {
        return GC_free(p);
    }

    template <class T>
    void* allocate_struct() {
        return GC_malloc(sizeof(T));
    }

    void release_struct(void* p) {
        return GC_free(p);
    }
};

}

#endif // ALLOCATOR_H

