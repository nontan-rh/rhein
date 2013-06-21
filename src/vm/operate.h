//
// arithmetic.h
//

#ifndef ARITHMETIC_H
#define ARITHMETIC_H

#include "object.h"

namespace rhein {

bool op_inc(Value v, Value& dest);
bool op_dec(Value v, Value& dest);
bool op_neg(Value v, Value& dest);
bool op_not(Value v, Value& dest);
bool op_add(Value lft, Value rht, Value& dest);
bool op_sub(Value lft, Value rht, Value& dest);
bool op_mul(Value lft, Value rht, Value& dest);
bool op_div(Value lft, Value rht, Value& dest);
bool op_mod(Value lft, Value rht, Value& dest);
bool op_eq(Value lft, Value rht, Value& dest);
bool op_ne(Value lft, Value rht, Value& dest);
bool op_gt(Value lft, Value rht, Value& dest);
bool op_lt(Value lft, Value rht, Value& dest);
bool op_ge(Value lft, Value rht, Value& dest);
bool op_le(Value lft, Value rht, Value& dest);

}

#endif // ARITHMETIC_H

