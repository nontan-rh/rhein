//
// peg.cc
//

#include "peg.h"
#include <iostream>

namespace rhein {
namespace peg {

bool
PegString::parse(List* src, Value&, Value& obj, List*& next) {
    List* prev = src;
    for (Int i = 0; i < str_->get_length(); i++) {
        if (src == end_of_list) {
            next = prev;
            obj = Value::k_nil();
            return false;
        }

        char str_ch = str_->elt_ref(i);
        char src_ch = src->get_head().get_char();
        if (str_ch != src_ch) {
            next = prev;
            obj = Value::k_nil();
            return false;
        }

        prev = src;

        Value tail_v = src->get_tail();
        src = tail_v.get_obj<List>();
    }

    next = src;
    obj = Value::by_object(str_);
    return true;
}

void
PegCharClass::add(char begin, char end) {
    end++;
    int b_x = begin / kIntBits;
    int b_y = begin % kIntBits;
    int e_x = end / kIntBits;
    int e_y = end % kIntBits;
    if (b_x == e_x) {
        for (int i = b_y; i < e_y; i++) {
            table_[b_x] |= 1 << i;
        }
    } else {
        for (int i = b_y; i < static_cast<int>(kIntBits); i++) {
            table_[b_x] |= 1 << i;
        }
        for (int i = b_x + 1; i < e_x - 1; i++) {
            table_[i]   =  0xffffffff;
        }
        for (int i = 0; i < e_y; i++) {
            table_[e_x] |= 1 << i;
        }
    }
}

void
PegCharClass::invert() {
    inverted_ = !inverted_;
}

bool
PegCharClass::parse(List* src, Value&, Value& obj, List*& next) {
    if (src == end_of_list) {
        obj = Value::k_nil();
        next = end_of_list;
        return false;
    }

    Value o = src->get_head();
    char ch = o.get_char();
    unsigned x = ch / kIntBits;
    unsigned y = ch % kIntBits;

    if (static_cast<bool>((table_[x] >> y) & 1) != inverted_) {
        next = src->get_tail().get_obj<List>();
        obj = Value::by_char(ch);
        return true;
    }

    // else
    next = src;
    obj = Value::k_nil();
    return false;
}

bool
PegTimes::parse(List* src, Value& ctx, Value& obj, List*& next) {
    Array* ary = Array::create(0);
    Int i = 0;
    List* s = src;
    List* n = src;
    Value buf;

    for (; i < lower_; i++) {
        if (!syn_->parse(s, ctx, buf, s)) {
            next = s;
            obj = Value::k_nil();
            return false;
        }
        ary->append(buf);
    }

    for (; i != upper_; i++) {
        if (!syn_->parse(s, ctx, buf, n)) {
            break;
        }
        ary->append(buf);
        s = n;
    }

    obj = Value::by_object(ary);
    next = s;
    return true;
}

bool
PegPred::parse(List* src, Value& ctx, Value& obj, List*& next) {
    List* dummy_next;
    bool res = syn_->parse(src, ctx, obj, dummy_next);
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
PegSequence::parse(List* src, Value& ctx, Value& obj, List*& next) {
    List* s = src;
    Array* ary = Array::create(0);
    Value buf;

    next = src;
    for (Int i = 0; i < num_; i++) {
        if (!syns_[i]->parse(s, ctx, buf, next)) {
            obj = Value::k_nil();
            return false;
        }
        ary->append(buf);
        s = next;
    }

    obj = Value::by_object(ary);
    return true;
}

bool
PegChoice::parse(List* src, Value& ctx, Value& obj, List*& next) {
    List* s = src;
    next = src;
    obj = Value::k_nil();

    for (Int i = 0; i < num_; i++) {
        if (syns_[i]->parse(s, ctx, obj, next)) {
            return true;
        } else {
            // Automatic backtrack enabled
#if 0
            // Check if parser head did not go ahead
            if (s == next) { continue; }
            // else
            break;
#endif
        }
    }

    return false;
}

bool
PegAction::parse(List* src, Value& ctx, Value& obj, List*& next) {
    Value syn_obj[1];
    if (syn_->parse(src, ctx, syn_obj[0], next)) {
        obj = execute(action_, 1, syn_obj);
        return true;
    }

    return false;
}

bool
PegAny::parse(List* src, Value&, Value& obj, List*& next) {
    if (src == end_of_list) {
        obj = Value::k_nil();
        next = end_of_list;
        return false;
    }
    obj = src->get_head();
    next = src->get_tail().get_obj<List>();
    return true;
}

bool
PegDynamic::parse(List* src, Value& ctx, Value& obj, List*& next) {
    Value args[2];
    args[0] = ctx;
    args[1] = Value::by_object(src);
    obj = execute(fn_, 2, args);
    if (obj.get_class() != get_current_state()->get_array_class()) {
        fprintf(stderr, "Type error\n");
        return false;
    }

    Array* ary = obj.get_obj<Array>();

    Value v_succ, v_next;
    v_succ = ary->elt_ref(0);
    obj = ary->elt_ref(1);
    v_next = ary->elt_ref(2);
    next = v_next.get_obj<List>();

    return v_succ.get_bool();
}

bool
PegTry::parse(List* src, Value& ctx, Value& obj, List*& next) {
    if (!syn_->parse(src, ctx, obj, next)) {
        next = src;
        return false;
    }

    return true;
}

bool
PegDrop::parse(List* src, Value& ctx, Value& obj, List*& next) {
    if (!syn_->parse(src, ctx, obj, next)) {
        return false;
    }

    if (obj.get_class() != get_current_state()->get_array_class()) {
        fprintf(stderr, "Type error\n");
        return false;
    }

    obj.get_obj<Array>()->drop(num_);
    return true;
}

void
PegPermute::add(int index) {
    permute_table_->append(Value::by_int(index));
}

bool
PegPermute::parse(List* src, Value& ctx, Value& obj, List*& next) {
    if (!syn_->parse(src, ctx, obj, next)) {
        return false;
    }

    if (obj.get_class() != get_current_state()->get_array_class()) {
        fprintf(stderr, "Type error\n");
        return false;
    }

    Array* obj_ary = obj.get_obj<Array>();
    Array* res = Array::create(permute_table_->get_length());

    for (int i = 0; i < permute_table_->get_length(); i++) {
        int x = permute_table_->elt_ref(i).get_int();
        res->elt_set(i, obj_ary->elt_ref(x));
    }

    obj = Value::by_object(res);
    return true;
}

bool
PegNull::parse(List* src, Value& /* ctx */, Value& obj, List*& next) {
    obj = Value::k_nil();
    next = src;
    return true;
}

bool
PegConstant::parse(List* src, Value& ctx, Value& obj, List*& next) {
    if (!syn_->parse(src, ctx, obj, next)) {
        return false;
    }

    obj = value_;
    return true;
}

bool
PegSpacing::parse(List* src, Value& ctx, Value& obj, List*& next) {
    List* s = src;
    Array* ary = Array::create(0);
    Value buf;

    next = src;
    for (Int i = 0; i < num_; i++) {
        if (begin_ <= i && i <= end_) {
            if (space_->parse(s, ctx, buf, next)) {
                s = next;
            }
        }

        if (!syns_[i]->parse(s, ctx, buf, next)) {
            obj = Value::k_nil();
            return false;
        }

        ary->append(buf);
        s = next;
    }

    if (num_ <= end_) {
        if (space_->parse(s, ctx, buf, next)) {
            s = next;
        }
    }

    obj = Value::by_object(ary);
    return true;
}

Value
fn_parse(unsigned /* argc */, Value* args) {
    Value res;
    bool succ;
    List* rest;
    PegSyntax* syn = args[0].get_obj<PegSyntax>();
    Value ctx = args[1];
    List* src = args[2].get_obj<List>();
    Array* ary = Array::create(3);

    succ = syn->parse(src, ctx, res, rest);

    ary->elt_set(0, Value::by_bool(succ));
    ary->elt_set(1, res);
    if (rest != end_of_list) {
        ary->elt_set(2, Value::by_object(rest));
    } else {
        ary->elt_set(2, Value::by_object(end_of_list));
    }
    return Value::by_object(ary);
}

Value
fn_pstr(unsigned /* argc */, Value* args) {
    return Value::by_object(PegString::create(args[0].get_obj<String>()));
}

Value
fn_pchar(unsigned /* argc */, Value* /* args */) {
    return Value::by_object(PegCharClass::create());
}

Value
fn_pchar_add1(unsigned, Value* args) {
    args[0].get_obj<PegCharClass>()->add(args[1].get_char(), args[1].get_char());
    return args[0];
}

Value
fn_pchar_add(unsigned /* argc */, Value* args) {
    args[0].get_obj<PegCharClass>()->add(args[1].get_char(),
            args[2].get_char());
    return args[0];
}

Value
fn_pchar_inv(unsigned /* argc */, Value* args) {
    args[0].get_obj<PegCharClass>()->invert();
    return args[0];
}

Value
fn_star(unsigned /* argc */, Value* args) {
    return Value::by_object(PegTimes::create( 0, -1,
                args[0].get_obj<PegSyntax>()));
}

Value
fn_plus(unsigned /* argc */, Value* args) {
    return Value::by_object(PegTimes::create( 1, -1,
                args[0].get_obj<PegSyntax>()));
}

Value
fn_times(unsigned /* argc */, Value* args) {
    return Value::by_object(
            PegTimes::create(
                args[1].get_int(), args[2].get_int(),
                args[0].get_obj<PegSyntax>()));
}

Value
fn_opt(unsigned /* argc */, Value* args) {
    return Value::by_object(
            PegTimes::create( 0, 1,
                args[0].get_obj<PegSyntax>()));
}

Value
fn_andp(unsigned /* argc */, Value* args) {
    return Value::by_object(
            PegPred::create(true, args[0].get_obj<PegSyntax>()));
}

Value
fn_notp(unsigned /* argc */, Value* args) {
    return Value::by_object(
            PegPred::create(false, args[0].get_obj<PegSyntax>()));
}

Value
fn_pseq(unsigned argc, Value* args) {
    State* R = get_current_state();
    PegSyntax** syns = R->allocate_block<PegSyntax*>(argc);
    for (unsigned i = 0; i < argc; i++) {
        syns[i] = args[i].get_obj<PegSyntax>();
    }
    return Value::by_object(PegSequence::create(argc, syns));
}

Value
fn_pchoice(unsigned argc, Value* args) {
    State* R = get_current_state();
    PegSyntax** syns = R->allocate_block<PegSyntax*>(argc);
    for (unsigned i = 0; i < argc; i++) {
        syns[i] = args[i].get_obj<PegSyntax>();
    }
    return Value::by_object(PegChoice::create(argc, syns));
}

Value
fn_paction(unsigned /* argc */, Value* args) {
    return Value::by_object(PegAction::create(args[1],
                args[0].get_obj<PegSyntax>()));
}

Value
fn_pany(unsigned /* argc */, Value* /* args */) {
    return Value::by_object(PegAny::create());
}

Value
fn_pdynamic(unsigned /* argc */, Value* args) {
    return Value::by_object(PegDynamic::create(args[0]));
}

Value
fn_ptry(unsigned /* argc */, Value* args) {
    return Value::by_object(PegTry::create(args[0].get_obj<PegSyntax>()));
}

Value
fn_pdrop(unsigned /* argc */, Value* args) {
    return Value::by_object(PegDrop::create(args[0].get_obj<PegSyntax>(),
                args[1].get_int()));
}

Value
fn_pperm(unsigned argc, Value* args) {
    PegPermute* perm = PegPermute::create(args[0].get_obj<PegSyntax>());
    for (unsigned i = 1; i < argc; i++) {
        Value index_v = args[i];
        if (!index_v.is(Value::Type::Int)) {
            fprintf(stderr, "pperm: Integer required\n");
            return Value::k_nil();
        }
        perm->add(index_v.get_int());
    }
    return Value::by_object(perm);
}

Value
fn_pconst(unsigned /* argc */, Value* args) {
    return Value::by_object(PegConstant::create(args[0].get_obj<PegSyntax>(),
            args[1]));
}

Value
fn_psseq(unsigned argc, Value* args) {
    State* R = get_current_state();
    PegSyntax** syns = R->allocate_block<PegSyntax*>(argc - 3);
    for (unsigned i = 0; i + 3 < argc; i++) {
        syns[i] = args[i + 3].get_obj<PegSyntax>();
    }
    return Value::by_object(PegSpacing::create(argc - 3, syns, args[0].get_obj<PegSyntax>(), args[1].get_int(), args[2].get_int()));
}

PegModule*
PegModule::create() {
    State* R = get_current_state();
    void* p = R->allocate_struct<PegModule>();
    return new (p) PegModule;
}

bool
PegModule::initialize() {
    State* R = get_current_state();
    R->add_class("PegSyntax", "any");
    R->add_class("PegString", "PegSyntax");
    R->add_class("PegCharClass", "PegSyntax");
    R->add_class("PegTimes", "PegSyntax");
    R->add_class("PegPred", "PegSyntax");
    R->add_class("PegSequence", "PegSyntax");
    R->add_class("PegChoice", "PegSyntax");
    R->add_class("PegAction", "PegSyntax");
    R->add_class("PegAny", "PegSyntax");
    R->add_class("PegDynamic", "PegSyntax");
    R->add_class("PegTry", "PegSyntax");
    R->add_class("PegDrop", "PegSyntax");
    R->add_class("PegPermute", "PegSyntax");
    R->add_class("PegNull", "PegSyntax");
    R->add_class("PegConstant", "PegSyntax");
    R->add_class("PegSpacing", "PegSyntax");
    R->add_native_function("parse", false, 3, {"PegSyntax", "any", "List"}, fn_parse);
    R->add_native_function("pstr", false, 1, {"string"}, fn_pstr);
    R->add_native_function("pchar", false, 0, { }, fn_pchar);
    R->add_native_function("add", false, 3, {"PegCharClass", "char", "char"}, fn_pchar_add);
    R->add_native_function("add", false, 2, {"PegCharClass", "char"}, fn_pchar_add1);
    R->add_native_function("inv", false, 1, {"PegCharClass"}, fn_pchar_inv);
    R->add_native_function("star", false, 1, {"PegSyntax"}, fn_star);
    R->add_native_function("plus", false, 1, {"PegSyntax"}, fn_plus);
    R->add_native_function("times", false, 3, {"PegSyntax", "int", "int"}, fn_times);
    R->add_native_function("opt", false, 1, {"PegSyntax"}, fn_opt);
    R->add_native_function("andp", false, 1, {"PegSyntax"}, fn_andp);
    R->add_native_function("notp", false, 1, {"PegSyntax"}, fn_notp);
    R->add_native_function("pseq", true, 0, {}, fn_pseq);
    R->add_native_function("pchoice", true, 0, {}, fn_pchoice);
    R->add_native_function("paction", false, 2, {"PegSyntax", "any"}, fn_paction);
    R->add_native_function("pdynamic", false, 1, {"any"}, fn_pdynamic);
    R->add_native_function("pany", false, 0, {}, fn_pany);
    R->add_native_function("ptry", false, 1, {"PegSyntax"}, fn_ptry);
    R->add_native_function("pdrop", false, 2, {"PegSyntax", "int"}, fn_pdrop);
    R->add_native_function("pperm", true, 1, {"PegSyntax"}, fn_pperm);
    R->add_native_function("pconst", false, 2, {"PegSyntax", "any"}, fn_pconst);
    R->add_native_function("psseq", true, 3, {"PegSyntax", "int", "int"}, fn_psseq);

    PegNull* pnull = PegNull::create();
    R->add_variable(R->get_symbol("pnull"), Value::by_object(pnull));
    return false;
}

}
}

