//
// peg.cc
//

#include "peg.h"

namespace rhein {
namespace peg {

bool
PegString::parse(List* src, Value& obj, List*& next) {
    State* R = get_current_state();
    List* prev = src;
    for (Int i = 0; i < str_->get_length(); i++) {
        if (src == nullptr) {
            next = prev;
            obj = Value::k_nil();
            return false;
        }

        char str_ch = str_->elt_ref(i);
        char src_ch = src->get_head(R).get_char();
        if (str_ch != src_ch) {
            next = prev;
            obj = Value::k_nil();
            return false;
        }

        prev = src;
        src = src->get_tail(R).get_obj<List>();
    }

    next = prev;
    obj = Value::by_object(str_);
    return true;
}

void
PegCharClass::add(char begin, char end) {
    unsigned b_x = begin / kIntBits;
    unsigned b_y = begin % kIntBits;
    unsigned e_x = end / kIntBits;
    unsigned e_y = end % kIntBits;
    if (b_x == e_x) {
        for (unsigned i = b_y; i < e_y; i++) {
            table_[b_x] |= 1 << i;
        }
    } else {
        for (unsigned i = b_y; i < kIntBits; i++) {
            table_[b_x] |= 1 << i;
        }
        for (unsigned i = b_x; i < e_x - 1; i++) {
            table_[i]   =  0xffffffff;
        }
        for (unsigned i = 0; i < e_y; i++) {
            table_[e_x] |= 1 << i;
        }
    }
}

void
PegCharClass::invert() {
    inverted_ = !inverted_;
}

bool
PegCharClass::parse(List* src, Value& obj, List*& next) {
    State* R = get_current_state();
    if (src == nullptr) {
        obj = Value::k_nil();
        next = nullptr;
        return false;
    }

    Value o = src->get_head(R);
    char ch = o.get_char();
    unsigned x = ch / kIntBits;
    unsigned y = ch % kIntBits;

    if (static_cast<bool>((table_[x] >> y) & 1) != inverted_) {
        next = src->get_tail(R).get_obj<List>();
        return true;
    }

    // else
    next = src;
    obj = Value::k_nil();
    return false;
}

bool
PegTimes::parse(List* src, Value& obj, List*& next) {
    State* R = get_current_state();
    Array* ary = Array::create(R, 0);
    Int i = 0;
    List* s = src;
    List* n = src;
    Value buf;

    for (; i < lower_; i++) {
        if (!syn_->parse(s, buf, s)) {
            next = s;
            return false;
        }
        ary->append(R, buf);
    }

    for (; i != upper_; i++) {
        if (!syn_->parse(s, obj, n)) {
            break;
        }
        ary->append(R, buf);
        s = n;
    }

    obj = Value::by_object(ary);
    next = s;
    return true;
}

bool
PegPred::parse(List* src, Value& obj, List*& next) {
    List* dummy_next;
    bool res = syn_->parse(src, obj, dummy_next);
    next = src;
    if (res == if_success_) {
        obj = Value::k_nil();
        return true;
    }

    // else
    obj = Value::k_nil();
    return false;
}

bool
PegSequence::parse(List* src, Value& obj, List*& next) {
    State* R = get_current_state();
    List* s = src;
    Array* ary = Array::create(R, 0);
    Value buf;

    for (Int i = 0; i < num_; i++) {
        if (!syns_[i]->parse(s, buf, next)) {
            obj = Value::k_nil();
            return false;
        }
        ary->append(R, buf);
    }

    obj = Value::by_object(ary);
    return true;
}

bool
PegChoice::parse(List* src, Value& obj, List*& next) {
    List* s = src;

    for (Int i = 0; i < num_; i++) {
        if (syns_[i]->parse(s, obj, next)) {
            return true;
        } else {
            // Check if parser head did not go ahead
            if (s == next) { continue; }
            // else
            break;
        }
    }

    obj = Value::k_nil();
    return false;
}

bool
PegAction::parse(List* src, Value& obj, List*& next) {
    State* R = get_current_state();
    Value syn_obj[1];
    if (syn_->parse(src, syn_obj[0], next)) {
        obj = execute(R, action_, 1, syn_obj);
        return true;
    }

    return false;
}

bool
PegAny::parse(List* src, Value& obj, List*& next) {
    State* R = get_current_state();
    if (src != nullptr) {
        obj = Value::k_nil();
        return false;
    }
    obj = src->get_head(R);
    next = src->get_tail(R).get_obj<List>();
    return true;
}

bool
PegTry::parse(List* src, Value& obj, List*& next) {
    if (!syn_->parse(src, obj, next)) {
        next = src;
        return false;
    }

    return true;
}

}
}

