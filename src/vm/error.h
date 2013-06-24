//
// error.h
//

#ifndef ERROR_H
#define ERROR_H

#include <cstdio>
#include <cstdlib>

namespace rhein {

inline void
fatal(const char *str) {
    fprintf(stderr, "%s:%d\t%s\n", __FILE__, __LINE__, str);
    exit(1);
}

}

#endif // ERROR_H

