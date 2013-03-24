//
// operate.cc
//

#include "object/object.h"
#include "operate.h"

using namespace rhein;

bool rhein::op_inc(Value v, Value& dest) {
    if (is_int(v)) {
        dest = make_value(get_int(v) + 1);
        return true;
    }
    return false;
}

bool rhein::op_dec(Value v, Value& dest) {
    if (is_int(v)) {
        dest = make_value(get_int(v) - 1);
        return true;
    }
    return false;
}

bool rhein::op_add(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = make_value(get_int(lft) + get_int(rht));
        return true;
    }
    return false;
}

bool rhein::op_sub(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = make_value(get_int(lft) - get_int(rht));
        return true;
    }
    return false;
}

bool rhein::op_mul(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = make_value(get_int(lft) * get_int(rht));
        return true;
    }
    return false;
}

bool rhein::op_div(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = make_value(get_int(lft) / get_int(rht));
        return true;
    }
    return false;
}

bool rhein::op_mod(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = make_value(get_int(lft) % get_int(rht));
        return true;
    }
    return false;
}

bool rhein::op_eq(Value lft, Value rht, Value& dest) {
    dest = make_value(equal(lft, rht));
    return true;
}

bool rhein::op_ne(Value lft, Value rht, Value& dest) {
    dest = make_value(!equal(lft, rht));
    return true;
}

bool rhein::op_gt(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = make_value(get_int(lft) > get_int(rht));
        return true;
    }
    return false;
}

bool rhein::op_lt(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = make_value(get_int(lft) < get_int(rht));
        return true;
    }
    return false;
}

bool rhein::op_ge(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = make_value(get_int(lft) >= get_int(rht));
        return true;
    }
    return false;
}

bool rhein::op_le(Value lft, Value rht, Value& dest) {
    if (is_int(lft) && is_int(rht)) {
        dest = make_value(get_int(lft) <= get_int(rht));
        return true;
    }
    return false;
}
