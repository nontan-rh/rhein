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
    throw str;
}

}

#endif // ERROR_H

