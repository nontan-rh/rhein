//
// vm.cc
//

#include <iostream>
#include <cstring>
#include <cstdint>
#include <type_traits>
#include <initializer_list>

using namespace std;

#include "systable.h"
#include "object.h"
#include "error.h"
#include "operate.h"
#include "vm.h"

namespace rhein {

State* current_state_;

struct Insn {
    enum {
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        Inc,
        Dec,
        Neg,
        Not,
        Eq,
        Ne, // 10
        Gt,
        Lt,
        Ge,
        Le,
        Jump,
        IfJump,
        UnlessJump,
        Call,
        Ret,
        Ranew, // 20
        Raref,
        Raset,
        Iref,
        Iset,
        Mref,
        Mset,
        Lfref,
        Lfset,
        Lvref,
        Lvset, // 30
        Laref,
        Laset,
        Gfref,
        Gvref,
        Gvset,
        Load,
        LoadClass,
        LoadUndef,
        LoadNull,
        LoadTrue, // 40
        LoadFalse,
        Enclose,
        Dup,
        Pop,
        Escape,
        Break,
    };
};

void
Closure::copy_slots(BytecodeFunction* fn) {
    State* R = get_current_state();
    // Arguments
    Value* new_args = R->allocate_raw_array(arg_count_);
    for (unsigned i = 0; i < arg_count_; i++) { new_args[i] = args_[i]; }

    // Function slots
    Value* new_func_slots = R->allocate_raw_array(arg_count_);
    for (unsigned i = 0; i < fn->get_function_slot_num(); i++) {
        new_func_slots[i] = func_slots_[i];
    }

    // Variable slots
    Value* new_var_slots = R->allocate_raw_array(arg_count_);
    for (unsigned i = 0; i < fn->get_variable_slot_num(); i++) {
        new_var_slots[i] = var_slots_[i];
    }

    args_ = new_args;
    func_slots_ = new_func_slots;
    var_slots_ = new_var_slots;
}

Frame::Frame(Value* stack_ptr, BytecodeFunction* fn_, Frame* parent_,
        Closure* closure_, unsigned argc_, Value* args_, Value*& next_stack_ptr)
    : fn(fn_), parent(parent_), should_save_closure(false),
      pc(nullptr), restore_stack_ptr(stack_ptr), sp(nullptr) {
    State* R = get_current_state();
    const size_t frame_size = sizeof(Frame) - sizeof(Value);
    unsigned required_value_slots = fn->get_function_slot_num()
            + fn->get_variable_slot_num() + fn->get_stack_size();
    uintptr_t istack_ptr = reinterpret_cast<uintptr_t>(stack_ptr);
    Value* local_area_begin = reinterpret_cast<Value*>(istack_ptr + frame_size);
    Value* func_slots = local_area_begin;
    Value* var_slots = local_area_begin + fn->get_function_slot_num();
    closure = Closure::create(closure_, argc_, args_, func_slots, var_slots);
    stack = local_area_begin + required_value_slots;
    uintptr_t istack = reinterpret_cast<uintptr_t>(stack);
    size_t align = alignment_of<Frame>::value;
    if (istack % align) {
        next_stack_ptr = reinterpret_cast<Value*>(
                    istack + align - (istack % align));
    } else {
        next_stack_ptr = stack;
    }

    if (next_stack_ptr > R->get_stack_end()) {
        fatal("Stack overflow");
    }
}

bool
Frame::local_func_ref(unsigned depth, unsigned offset, Value& value) {
    Closure* clos = closure;
    for (; depth > 0; depth--, clos = clos->get_parent()) {
        if (clos == nullptr) { return false; }
    }

    value = clos->get_func_slots()[offset];
    return true;
}

bool
Frame::local_func_set(unsigned depth, unsigned offset, Value value) {
    Closure* clos = closure;
    for (; depth > 0; depth--, clos = clos->get_parent()) {
        if (clos == nullptr) { return false; }
    }

    clos->get_func_slots()[offset] = value;
    return true;
}

bool
Frame::local_var_ref(unsigned depth, unsigned offset, Value& value) {
    Closure* clos = closure;
    for (; depth > 0; depth--, clos = clos->get_parent()) {
        if (clos == nullptr) { return false; }
    }

    value = clos->get_var_slots()[offset];
    return true;
}

bool
Frame::local_var_set(unsigned depth, unsigned offset, Value value) {
    Closure* clos = closure;
    for (; depth > 0; depth--, clos = clos->get_parent()) {
        if (clos == nullptr) { return false; }
    }

    clos->get_var_slots()[offset] = value;
    return true;
}

bool
Frame::local_arg_ref(unsigned depth, unsigned offset, Value& value) {
    Closure* clos = closure;
    for (; depth > 0; depth--, clos = clos->get_parent()) {
        if (clos == nullptr) { return false; }
    }

    value = clos->get_args()[offset];
    return true;
}

bool
Frame::local_arg_set(unsigned depth, unsigned offset, Value value) {
    Closure* clos = closure;
    for (; depth > 0; depth--, clos = clos->get_parent()) {
        if (clos == nullptr) { return false; }
    }

    clos->get_args()[offset] = value;
    return true;
}

State::State() : symbol_provider_(nullptr) {
    SwitchState ss(this); // Hack

    // Bootstrap type system
    allocator_ = new (GC_malloc(sizeof(Allocator))) Allocator();

    initialize_class1();
    initialize_symbol();
    initialize_class2();

    func_slots_ = SysTable<const Symbol*, Object*>::create();
    var_slots_ = SysTable<const Symbol*, Value>::create();
    klass_slots_ = SysTable<const Symbol*, Class*>::create();

    initialize_class3();

    //func_slots->dump();
    //var_slots->dump();
    //klass_slots->dump();

    stack_top_ = stack_begin_ = allocate_raw_array(kStackSize);
    stack_end_ = stack_begin_ + kStackSize;
}

State::~State() {
}

void
State::initialize_class1() {
    any_class_ = Class::create(nullptr, nullptr);
    class_class_ = Class::create(nullptr, any_class_);
    any_class_->klass_ = class_class_;
    nil_class_ = Class::create(nullptr, any_class_);
    bool_class_ = Class::create(nullptr, any_class_);
    int_class_ = Class::create(nullptr, any_class_);
    char_class_ = Class::create(nullptr, any_class_);
    symbol_class_ = Class::create(nullptr, any_class_);
    string_class_ = Class::create(nullptr, any_class_);
    array_class_ = Class::create(nullptr, any_class_);
    hashtable_class_ = Class::create(nullptr, any_class_);
    method_class_ = Class::create(nullptr, any_class_);
    bytecode_function_class_ = Class::create(nullptr, any_class_);
    native_function_class_ = Class::create(nullptr, any_class_);
    rest_arguments_class_ = Class::create(nullptr, any_class_);
}

void
State::initialize_class2() {
    any_class_->set_id(get_symbol("any"));
    class_class_->set_id(get_symbol("class"));
    nil_class_->set_id(get_symbol("nil"));
    bool_class_->set_id(get_symbol("bool"));
    int_class_->set_id(get_symbol("int"));
    char_class_->set_id(get_symbol("char"));
    symbol_class_->set_id(get_symbol("symbol"));
    string_class_->set_id(get_symbol("string"));
    array_class_->set_id(get_symbol("array"));
    hashtable_class_->set_id(get_symbol("hashtable"));
    method_class_->set_id(get_symbol("method"));
    bytecode_function_class_->set_id(get_symbol("bytecode_function"));
    native_function_class_->set_id(get_symbol("native_function"));
    rest_arguments_class_->set_id(get_symbol("rest_arguments"));
}

void
State::set_class_hash(Class* klass){
    klass_slots_->insert(klass->get_id(), klass);
}

void
State::initialize_class3() {
    set_class_hash(any_class_);
    set_class_hash(class_class_);
    set_class_hash(nil_class_);
    set_class_hash(bool_class_);
    set_class_hash(int_class_);
    set_class_hash(char_class_);
    set_class_hash(symbol_class_);
    set_class_hash(string_class_);
    set_class_hash(array_class_);
    set_class_hash(hashtable_class_);
    set_class_hash(method_class_);
    set_class_hash(bytecode_function_class_);
    set_class_hash(native_function_class_);
    set_class_hash(rest_arguments_class_);
}

void
State::initialize_symbol() {
    symbol_provider_ = SymbolProvider::create();
}

bool
State::get_class(Symbol* id, Value& klass) const {
    klass = Value::by_object(klass_slots_->find(id));
    return true;
}

Class*
State::get_class(const char* id) const {
    return klass_slots_->find(get_symbol(id));
}

bool
State::global_func_ref(Symbol* id, Value& func) const {
    func = Value::by_object(func_slots_->find(id));
    return true;
}

bool
State::global_var_ref(Symbol* id, Value& value) const {
    value = var_slots_->find(id);
    return true;
}

bool
State::global_var_set(Symbol* id, Value value) {
    var_slots_->insert(id, value);
    return true;
}

bool
State::add_native_function(const char* id, bool variadic,
        unsigned num_args, std::initializer_list<const char*> arg_class_ids,
        NativeFunctionBody body) {
    return this->add_function(NativeFunction::create(
                FunctionInfo::create(
                    this->get_symbol(id),
                    variadic,
                    num_args,
                    arg_class_ids),
                body));
}

bool
State::add_function(Function* func) {
    return add_function(func, func->get_info()->name());
}

bool
State::add_function(Function* func, const Symbol* name) {
    if (func_slots_->exists(name)) {
        Object* old = func_slots_->find(name);
        if (old->get_class() == native_function_class_
                || old->get_class() == bytecode_function_class_) {
            Method* method = Method::create();
            method->add_function(static_cast<Function*>(old));
            method->add_function(func);
            func_slots_->assign(name, method);
            return true;
        } else if (old->get_class() == method_class_) {
            return static_cast<Method*>(old)->add_function(func);
        } else {
            throw "";
        }
    }

    func_slots_->insert_if_absent(name, func);
    return true;
}

Class*
State::add_class(const char* name, const char* parent) {
    Class* klass = Class::create(get_symbol(name), get_class(parent));
    this->add_class(klass);
    return klass;
}

bool
State::add_class(Class* klass) {
    return add_class(klass, klass->get_id());
}

bool
State::add_class(Class* klass, const Symbol* name) {
    klass_slots_->insert_if_absent(name, klass);
    return true;
}

bool
State::add_variable(Symbol* id, Value val) {
    var_slots_->insert_if_absent(id, val);
    return true;
}

bool
State::load_module(Module* module) {
    return module->initialize();
}

void
State::dump_classes() {
    //klass_slots_->dump();
}

void
State::dump_functions() {
    //func_slots_->dump();
}

void
State::dump_variables() {
    //var_slots_->dump();
}

Value
execute(Symbol* entry_point, unsigned argc, Value* args) {
    Value fn;
    if (!get_current_state()->global_func_ref(entry_point, fn)) {
        fatal("No such function");
    }
    return execute(fn, argc, args);
}

Value execute(Value fn, unsigned argc, Value* args) {
    State* R = get_current_state();
    if (!fn.is(Value::Type::Object)) {
        fatal("Not excutable object");
    }

    if (fn.get_obj<Object>()->get_class() == R->get_method_class()) {
        fn.get_obj<Method>()->dispatch(argc, args, fn);
    }

    if (!fn.is(Value::Type::Object)) {
        fatal("Not excutable object");
    }

    Object* ofn = fn.get_obj<Object>();
    if (ofn->get_class() == R->get_bytecode_function_class()) {
        return execute((BytecodeFunction*)ofn, argc, args);
    } else if (ofn->get_class() == R->get_native_function_class()) {
        return ((NativeFunction*)ofn)->get_body()(argc, args);
    } else {
        fatal("Not excutable object");
    }
    // NOTREACHED
    return Value::k_nil();
}

inline uint32_t
get_insn_arg_u(uint32_t insn) {
    return insn >> 8;
}

inline uint32_t
get_insn_arg_uu1(uint32_t insn) {
    return (insn >> 8) & 0xff;
}

inline uint32_t
get_insn_arg_uu2(uint32_t insn) {
    return (insn >> 16) & 0xff;
}

#define UNARY_OP(op) \
    if (!op(sp[0], sp[0])) { \
        fatal("Error"); \
    } \
    pc++; \
    break;


#define BINARY_OP(op) \
    if (!op(sp[1], sp[0], sp[1])) { \
        fatal("Error"); \
    } \
    pc++; \
    sp++; \
    break;

#define LOCAL_REFER_OP(op) { \
    uint32_t depth = get_insn_arg_uu1(insn); \
    uint32_t offset = get_insn_arg_uu2(insn); \
    if (!op(depth, offset, *(--sp))) { \
        fprintf(stderr, #op ":%u:%u\n", depth, offset); \
        fatal("Error on local refer"); \
    } \
    pc++; \
    } \
    break;

#define LOCAL_SET_OP(op) { \
    uint32_t depth = get_insn_arg_uu1(insn); \
    uint32_t offset = get_insn_arg_uu2(insn); \
    if (!op(depth, offset, *(sp))) { \
        fprintf(stderr, #op ":%u:%u\n", depth, offset); \
        fatal("Error on local set"); \
    } \
    pc++; \
    } \
    break;

#define GLOBAL_REFER_OP(op) { \
    Value id = fn->get_constant_table()[get_insn_arg_u(insn)]; \
    if (!id.is(Value::Type::Object) \
            || id.get_obj<Object>()->get_class() != R->get_symbol_class()) { \
        fatal("Error on global refer"); \
    } \
    if (!R->op(id.get_obj<Symbol>(), *(--sp))) { \
        fatal("Error on global refer"); \
    } \
    pc++; \
    } \
    break;

#define GLOBAL_SET_OP(op) { \
    Value id = fn->get_constant_table()[get_insn_arg_u(insn)]; \
    if (!id.is(Value::Type::Object) \
            || id.get_obj<Object>()->get_class() != R->get_symbol_class()) { \
        fatal("Error on global set"); \
    } \
    if (!R->op(id.get_obj<Symbol>(), *(sp))) { \
        fatal("Error on global set"); \
    } \
    pc++; \
    } \
    break;

Value
execute(BytecodeFunction* entry_fn, unsigned argc_, Value* args_) {
    State* R = get_current_state();
    Value*& next_stack_ptr = R->get_stack_top();
    Frame* fr = Frame::create(next_stack_ptr, entry_fn, nullptr,
            nullptr, argc_, args_, next_stack_ptr);
    BytecodeFunction* fn = entry_fn;
    Value* sp = fr->stack;
    const uint32_t* pc = fn->get_bytecode();

    for(; ; ){
        uint32_t insn = *pc;
        switch(insn & 0xff) {
            case Insn::Add: BINARY_OP(op_add)
            case Insn::Sub: BINARY_OP(op_sub)
            case Insn::Mul: BINARY_OP(op_mul)
            case Insn::Div: BINARY_OP(op_div)
            case Insn::Mod: BINARY_OP(op_mod)
            case Insn::Inc: UNARY_OP(op_inc)
            case Insn::Dec: UNARY_OP(op_dec)
            case Insn::Neg: UNARY_OP(op_neg)
            case Insn::Not: UNARY_OP(op_not)
            case Insn::Eq: BINARY_OP(op_eq)
            case Insn::Ne: BINARY_OP(op_ne)
            case Insn::Gt: BINARY_OP(op_gt)
            case Insn::Lt: BINARY_OP(op_lt)
            case Insn::Ge: BINARY_OP(op_ge)
            case Insn::Le: BINARY_OP(op_le)
            case Insn::Jump: {
                uint32_t dest = get_insn_arg_u(insn);
                pc = fn->get_bytecode() + dest;
            }
                break;
            case Insn::IfJump: {
                uint32_t dest = get_insn_arg_u(insn);
                if ((*sp++).like_true()) {
                    pc = fn->get_bytecode() + dest;
                } else {
                    pc++;
                }
            }
                break;
            case Insn::UnlessJump: {
                uint32_t dest = get_insn_arg_u(insn);
                if ((*sp++).like_false()) {
                    pc = fn->get_bytecode() + dest;
                } else {
                    pc++;
                }
            }
                break;
            case Insn::Call: {
                uint32_t argc = get_insn_arg_u(insn);
                Value func = *sp++;
                Value* stack_args = sp;

                if (!func.is(Value::Type::Object)) {
                    fatal("Not callable object");
                }

                Closure* closure = nullptr;
                if (func.get_obj<Object>()->get_class() == R->get_method_class()) {
                    if (!func.get_obj<Method>()->dispatch(argc, stack_args, func)) {
                        fatal("Could not dispatch");
                    }
                    closure = func.get_obj<Method>()->get_closure();
                }

                Function* ofunc = func.get_obj<Function>();
                if (!ofunc->get_info()->check_type(argc, stack_args)) {
                    ofunc->get_info()->name()->dump();
                    fatal("Type error");
                }

                if (ofunc->get_class() == R->get_bytecode_function_class()) {
                    fn = (BytecodeFunction*)ofunc;

                    if (fn->get_info()->variadic()) {
                        unsigned vargs_count = argc - fn->get_info()->num_args();
                        RestArguments* vargs = RestArguments::create(vargs_count);
                        for (unsigned i = 0; i < vargs_count; i++) {
                            vargs->elt_set(i, stack_args[i + fn->get_info()->num_args()]);
                        }
                        stack_args[fn->get_info()->num_args()] = Value::by_object(vargs);
                    }

                    if (closure == nullptr) {
                        closure = fn->get_closure();
                    }

                    fr->pc = pc + 1;
                    fr->sp = sp + argc;
                    fr = Frame::create(next_stack_ptr, fn, fr, closure, argc,
                            stack_args, next_stack_ptr);
                    pc = fn->get_bytecode();
                    sp = fr->stack;
                } else if (ofunc->get_class() == R->get_native_function_class()) {
                    Value ret = (*func.get_obj<NativeFunction>()->get_body())(argc, sp);
                    sp += argc;
                    *(--sp) = ret;
                    pc++;
                }
            }
                break;
            case Insn::Ret: {
                Frame* parent = fr->parent;
                if (fr->should_save_closure) {
                    fr->closure->copy_slots(fr->fn);
                }
                if (parent == nullptr) { // if top level
                    goto VMExit;
                }
                next_stack_ptr = fr->restore_stack_ptr;
                *(--parent->sp) = *sp;
                pc = parent->pc;
                sp = parent->sp;
                fr = parent;
                fn = parent->fn;
            }
                break;
            case Insn::Ranew: {
                Value size = *sp++;
                if (!size.is(Value::Type::Int) || size.get_int() < 0) {
                    fatal("Invalid size");
                }

                *(--sp) = Value::by_object(Array::create(size.get_int()));
                pc++;
            }
                break;
            case Insn::Raref: {
           Value index = *sp++;
                Value array = *sp;

                if (!array.is(Value::Type::Object) || array.get_obj<Object>()->get_class() != R->get_array_class()) {
                    fatal("Invalid type");
                }

                if (!array.get_obj<Array>()->index_ref(index, *(--sp))) {
                    fatal("Cannot refer");
                }
                pc++;
            }
                break;
            case Insn::Raset: {
                Value value = *sp++;
                Value index = sp[0];
                Value array = sp[1];

                if (!array.is(Value::Type::Object) || array.get_obj<Object>()->get_class() != R->get_array_class()) {
                    fatal("Invalid type");
                }

                if (!array.get_obj<Array>()->index_set(index, value)) {
                    fatal("Cannot set");
                }
                pc++;
            }
                break;
            case Insn::Iref: {
                Value index = *sp++;
                if (!sp[0].is(Value::Type::Object)) {
                    fatal("Cannot refer");
                }

                if (!sp[0].get_obj<Object>()->index_ref(index, sp[0])) {
                    fatal("Cannot refer");
                }
                pc++;
            }
                break;
            case Insn::Iset: {
                Value index = *sp++;
                Value obj = *sp++;
                Value value = sp[0];

                if (!obj.is(Value::Type::Object)) {
                    fatal("Cannot set");
                }

                if (!obj.get_obj<Object>()->index_set(index, value)) {
                    fatal("Cannot set");
                }
                pc++;
            }
                break;
            case Insn::Mref: {
                Value obj = sp[0];
                Value id = fn->get_constant_table()[get_insn_arg_u(insn)];

                if (!(id.is(Value::Type::Object)
                    && id.get_obj<Object>()->get_class() == R->get_symbol_class())) { 
                    obj.get_class()->get_id()->dump();
                    fatal("Cannot refer");
                }
                Symbol* id_sym = id.get_obj<Symbol>();

                if (obj.is(Value::Type::Object)
                        && !obj.get_obj<Object>()->slot_ref(id_sym, sp[0])) {
                    id.get_obj<Symbol>()->dump();
                    fatal("Cannot refer");
                } else if (obj.is(Value::Type::Record)
                        && !obj.get_rec()->slot_ref(id_sym, sp[0])) {
                    id.get_obj<Symbol>()->dump();
                    fatal("Cannot refer");
                }
                pc++;
            }
                break;
            case Insn::Mset: {
                Value obj = *sp++;
                Value id = fn->get_constant_table()[get_insn_arg_u(insn)];

                if (!(id.is(Value::Type::Object)
                    && id.get_obj<Object>()->get_class() == R->get_symbol_class())) {
                    
                    fatal("Cannot set");
                }
                Symbol* id_sym = id.get_obj<Symbol>();

                if (obj.is(Value::Type::Object)
                        && !obj.get_obj<Object>()->slot_set(id_sym, sp[0])) {
                    fatal("Cannot set");
                } else if (obj.is(Value::Type::Record)
                        && !obj.get_rec()->slot_set(id_sym, sp[0])) {
                    fatal("Cannot set");
                }
                pc++;
            }
                break;
            case Insn::Lfref: LOCAL_REFER_OP(fr->local_func_ref)
            case Insn::Lfset: LOCAL_SET_OP(fr->local_func_set)
            case Insn::Lvref: LOCAL_REFER_OP(fr->local_var_ref)
            case Insn::Lvset: LOCAL_SET_OP(fr->local_var_set)
            case Insn::Laref: LOCAL_REFER_OP(fr->local_arg_ref)
            case Insn::Laset: LOCAL_SET_OP(fr->local_arg_set)
            case Insn::Gfref: GLOBAL_REFER_OP(global_func_ref)
            case Insn::Gvref: GLOBAL_REFER_OP(global_var_ref)
            case Insn::Gvset: GLOBAL_SET_OP(global_var_set)
            case Insn::Load:
                *(--sp) = fn->get_constant_table()[get_insn_arg_u(insn)];
                pc++;
                break;
            case Insn::LoadClass: {
                Value id = fn->get_constant_table()[get_insn_arg_u(insn)];

                if (!(id.is(Value::Type::Object) && id.get_obj<Object>()->get_class() == R->get_symbol_class())) {
                    fatal("Name must be string");
                }

                if (!R->get_class(id.get_obj<Symbol>(), *(--sp))) {
                    fatal("Cannot find klass");
                }
                pc++;
            }
                break;
            case Insn::LoadUndef:
                *(--sp) = Value::k_undef();
                pc++;
                break;
            case Insn::LoadNull:
                *(--sp) = Value::k_nil();
                pc++;
                break;
            case Insn::LoadTrue:
                *(--sp) = Value::k_true();
                pc++;
                break;
            case Insn::LoadFalse:
                *(--sp) = Value::k_false();
                pc++;
                break;
            case Insn::Enclose: {
                Value id = fn->get_constant_table()[get_insn_arg_u(insn)];

                if (!id.is(Value::Type::Object) || id.get_obj<Object>()->get_class() != R->get_symbol_class()) {
                    fatal("Cannot enclose");
                }

                Value func;
                if (!R->global_func_ref(id.get_obj<Symbol>(), func)) {
                    fatal("Cannot enclose");
                }

                if (!func.is(Value::Type::Object)) {
                    fatal("Cannot enclose");
                }

                if (func.get_obj<Object>()->get_class() == R->get_native_function_class()) {
                    *(--sp) = Value::by_object(func.get_obj<NativeFunction>()->enclose(fr->closure));
                } else if (func.get_obj<Object>()->get_class() == R->get_bytecode_function_class()) {
                    *(--sp) = Value::by_object(func.get_obj<BytecodeFunction>()->enclose(fr->closure));
                } else if (func.get_obj<Object>()->get_class() == R->get_method_class()) {
                    *(--sp) = Value::by_object(func.get_obj<Method>()->enclose(fr->closure));
                } else {
                    fatal("Cannot enclose");
                }
                fr->should_save_closure = true;
                pc++;
            }
                break;
            case Insn::Dup:
                --sp;
                sp[0] = sp[1];
                pc++;
                break;
            case Insn::Pop:
                sp++;
                pc++;
                break;
            case Insn::Escape: {
                Value value = *sp++;
                sp += get_insn_arg_u(insn);
                *(--sp) = value;
                pc++;
            }
                break;
            case Insn::Break:
                getchar();
                pc++;
                break;
            default:
                fprintf(stderr, "%u\n", insn);
                fatal("Invalid insn");
                break;
        }
    }
VMExit:
    return *sp;
}

}
