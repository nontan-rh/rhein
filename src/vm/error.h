//
// error.h
//

#ifndef ERROR_H
#define ERROR_H

#include <cstdio>
#include <cstdlib>

#define fatal(str) do { fprintf(stderr, str "\n"); exit(1); } while(0)

#endif // ERROR_H

