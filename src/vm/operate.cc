//
// operate.cc
//

#include "object/object.h"
#include "operate.h"

using namespace rhein;

bool rhein::op_inc(Value v, Value& dest) {
    if (is_int(v)) {
        dest = int2value(get_int(v) + 1);
        return true;
    }
    return false;
}

bool rhein::op_dec(Value v, Value& dest) {
    if (is_int(v)) {
        dest = int2value(get_int(v) - 1);
        return true;
    }
    return false;
}

bool rhein::op_neg(Value v, Value& dest) {
    if (is_int(v)) {
        dest = int2value(-get_int(v));
        return true;
    }
    return false;
}

bool rhein::op_not(Value v, Value& dest) {
    if (is_true(v)) {
        dest = Cfalse;
        return true;
    } else if (is_false(v)) {
        dest = Ctrue;
        return true;
    }
    return false;
}


bool rhein::op_add(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = int2value(get_int(lft) + get_int(rht));
        return true;
    }
    return false;
}

bool rhein::op_sub(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = int2value(get_int(lft) - get_int(rht));
        return true;
    }
    return false;
}

bool rhein::op_mul(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = int2value(get_int(lft) * get_int(rht));
        return true;
    }
    return false;
}

bool rhein::op_div(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = int2value(get_int(lft) / get_int(rht));
        return true;
    }
    return false;
}

bool rhein::op_mod(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = int2value(get_int(lft) % get_int(rht));
        return true;
    }
    return false;
}

bool rhein::op_eq(Value lft, Value rht, Value& dest) {
    dest = bool2value(equal(lft, rht));
    return true;
}

bool rhein::op_ne(Value lft, Value rht, Value& dest) {
    dest = bool2value(!equal(lft, rht));
    return true;
}

bool rhein::op_gt(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = bool2value(get_int(lft) > get_int(rht));
        return true;
    } else if (is_char(lft) && is_char(rht)) {
        dest = bool2value(get_char(lft) > get_char(rht));
        return true;
    }
    return false;
}

bool rhein::op_lt(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = bool2value(get_int(lft) < get_int(rht));
        return true;
    } else if (is_char(lft) && is_char(rht)) {
        dest = bool2value(get_char(lft) < get_char(rht));
        return true;
    }
    return false;
}

bool rhein::op_ge(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = bool2value(get_int(lft) >= get_int(rht));
        return true;
    } else if (is_char(lft) && is_char(rht)) {
        dest = bool2value(get_char(lft) >= get_char(rht));
        return true;
    }
    return false;
}

bool rhein::op_le(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = bool2value(get_int(lft) <= get_int(rht));
        return true;
    } else if (is_char(lft) && is_char(rht)) {
        dest = bool2value(get_char(lft) <= get_char(rht));
        return true;
    }
    return false;
}

