//
// error.h
//

#ifndef ERROR_H
#define ERROR_H

#include <cstdio>
#include <cstdlib>

#define fatal(str) do { fprintf(stderr, "%s:%d\t%s\n", __FILE__, __LINE__, str); exit(1); } while(0)

#endif // ERROR_H

