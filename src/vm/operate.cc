//
// operate.cc
//

#include "object.h"
#include "operate.h"

namespace rhein {

bool op_inc(Value v, Value& dest) {
    if (v.is(Value::Type::Int)) {
        dest = Value::by_int(v.get_int() + 1);
        return true;
    }
    return false;
}

bool op_dec(Value v, Value& dest) {
    if (v.is(Value::Type::Int)) {
        dest = Value::by_int(v.get_int() - 1);
        return true;
    }
    return false;
}

bool op_neg(Value v, Value& dest) {
    if (v.is(Value::Type::Int)) {
        dest = Value::by_int(-v.get_int());
        return true;
    }
    return false;
}

bool op_not(Value v, Value& dest) {
    if (v.like_false()) {
        dest = Value::k_true();
        return true;
    }
	dest = Value::k_false();
	return true;
}


bool op_add(Value lft, Value rht, Value& dest) {
    if (lft.is(Value::Type::Int) && rht.is(Value::Type::Int)) {
        dest = Value::by_int(lft.get_int() + rht.get_int());
        return true;
    }
    return false;
}

bool op_sub(Value lft, Value rht, Value& dest) {
    if (lft.is(Value::Type::Int) && rht.is(Value::Type::Int)) {
        dest = Value::by_int(lft.get_int() - rht.get_int());
        return true;
    }
    return false;
}

bool op_mul(Value lft, Value rht, Value& dest) {
    if (lft.is(Value::Type::Int) && rht.is(Value::Type::Int)) {
        dest = Value::by_int(lft.get_int() * rht.get_int());
        return true;
    }
    return false;
}

bool op_div(Value lft, Value rht, Value& dest) {
    if (lft.is(Value::Type::Int) && rht.is(Value::Type::Int)) {
        dest = Value::by_int(lft.get_int() / rht.get_int());
        return true;
    }
    return false;
}

bool op_mod(Value lft, Value rht, Value& dest) {
    if (lft.is(Value::Type::Int) && rht.is(Value::Type::Int)) {
        dest = Value::by_int(lft.get_int() % rht.get_int());
        return true;
    }
    return false;
}

bool op_eq(Value lft, Value rht, Value& dest) {
    dest = Value::by_bool(lft.eq(rht));
    return true;
}

bool op_ne(Value lft, Value rht, Value& dest) {
    dest = Value::by_bool(!lft.eq(rht));
    return true;
}

bool op_gt(Value lft, Value rht, Value& dest) {
    if (lft.is(Value::Type::Int) && rht.is(Value::Type::Int)) {
        dest = Value::by_bool(lft.get_int() > rht.get_int());
        return true;
    } else if (lft.is(Value::Type::Char) && rht.is(Value::Type::Char)) {
        dest = Value::by_bool(lft.get_char() > rht.get_char());
        return true;
    }
    return false;
}

bool op_lt(Value lft, Value rht, Value& dest) {
    if (lft.is(Value::Type::Int) && rht.is(Value::Type::Int)) {
        dest = Value::by_bool(lft.get_int() < rht.get_int());
        return true;
    } else if (lft.is(Value::Type::Char) && rht.is(Value::Type::Char)) {
        dest = Value::by_bool(lft.get_char() < rht.get_char());
        return true;
    }
    return false;
}

bool op_ge(Value lft, Value rht, Value& dest) {
    if (lft.is(Value::Type::Int) && rht.is(Value::Type::Int)) {
        dest = Value::by_bool(lft.get_int() >= rht.get_int());
        return true;
    } else if (lft.is(Value::Type::Char) && rht.is(Value::Type::Char)) {
        dest = Value::by_bool(lft.get_char() >= rht.get_char());
        return true;
    }
    return false;
}

bool op_le(Value lft, Value rht, Value& dest) {
    if (lft.is(Value::Type::Int) && rht.is(Value::Type::Int)) {
        dest = Value::by_bool(lft.get_int() <= rht.get_int());
        return true;
    } else if (lft.is(Value::Type::Char) && rht.is(Value::Type::Char)) {
        dest = Value::by_bool(lft.get_char() <= rht.get_char());
        return true;
    }
    return false;
}

}

