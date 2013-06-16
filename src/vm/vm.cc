//
// vm.cc
//

#include <iostream>
using namespace std;

#include <tr1/cstdint>
#include <cstring>

#include "object/object.h"
#include "object/imstring.h"
#include "object/function.h"
#include "object/record.h"
#include "object/array.h"
#include "error.h"
#include "operate.h"
#include "vm.h"

using namespace rhein;

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
        LoadKlass,
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

Frame::Frame(State* R, BytecodeFunction* fn_, Frame* parent_, Frame* closure_,
    unsigned argc_, Value* args_)
    : fn(fn_), closure(closure_), parent(parent_),
      stack(R->ator->allocateRawArray(fn_->get_stack_size())),
      argc(argc_), args(args_),
      func_slots(R->ator->allocateRawArray(fn_->get_function_slot_num())),
      var_slots(R->ator->allocateRawArray(fn_->get_variable_slot_num())),
      pc(nullptr), sp(nullptr) { }

bool
Frame::lfref(unsigned depth, unsigned offset, Value& value) {
    Frame* frame = this;
    for (; depth > 0; depth--, frame = frame->closure) {
        if (frame == nullptr) {
            return false;
        }
    }

    if (frame->fn->get_function_slot_num() <= offset) {
        return false;
    }
    value = frame->func_slots[offset];
    return true;
}

bool
Frame::lfset(unsigned depth, unsigned offset, Value value) {
    Frame* frame = this;
    for (; depth > 0; depth--, frame = frame->closure) {
        if (frame == nullptr) {
            return false;
        }
    }

    if (frame->fn->get_function_slot_num() <= offset) {
        return false;
    }
    frame->func_slots[offset] = value;
    return true;
}

bool
Frame::lvref(unsigned depth, unsigned offset, Value& value) {
    Frame* frame = this;
    for (; depth > 0; depth--, frame = frame->closure) {
        if (frame == nullptr) {
            return false;
        }
    }

    if (frame->fn->get_variable_slot_num() <= offset) {
        return false;
    }
    value = frame->var_slots[offset];
    return true;
}

bool
Frame::lvset(unsigned depth, unsigned offset, Value value) {
    Frame* frame = this;
    for (; depth > 0; depth--, frame = frame->closure) {
        if (frame == nullptr) {
            return false;
        }
    }

    if (frame->fn->get_variable_slot_num() <= offset) {
        return false;
    }
    frame->var_slots[offset] = value;
    return true;
}

bool
Frame::laref(unsigned depth, unsigned offset, Value& value) {
    Frame* frame = this;
    for (; depth > 0; depth--, frame = frame->closure) {
        if (frame == nullptr) {
            return false;
        }
    }

    if (frame->argc <= offset) {
        return false;
    }
    value = frame->args[offset];
    return true;
}

bool
Frame::laset(unsigned depth, unsigned offset, Value value) {
    Frame* frame = this;
    for (; depth > 0; depth--, frame = frame->closure) {
        if (frame == nullptr) {
            return false;
        }
    }

    if (frame->argc <= offset) {
        return false;
    }
    frame->args[offset] = value;
    return true;
}

State::State() : s_prv(nullptr) {
    // Bootstrap type system
    ator = new (GC_malloc(sizeof(Allocator))) Allocator();

    initializeKlass1();
    initializeString();
    initializeKlass2();

    func_slots = HashTable::create(this);
    var_slots = HashTable::create(this);
    klass_slots = HashTable::create(this);

    initializeKlass3();

    //func_slots->dump();
    //var_slots->dump();
    //klass_slots->dump();
}

State::~State() {
}

void
State::initializeKlass1() {
#define SET_KLASS(name, parent) name ## _klass = Klass::create(this, nullptr, parent ## _klass)
    any_klass = Klass::create(this, nullptr, nullptr);
    SET_KLASS(null, any);
    SET_KLASS(bool, any);
    SET_KLASS(int, any);
    SET_KLASS(char, any);
    SET_KLASS(string, any);
    SET_KLASS(array, any);
    SET_KLASS(hashtable, any);
    SET_KLASS(method, any);
    SET_KLASS(bytecode_function, any);
    SET_KLASS(native_function, any);
#undef SET_KLASS
}

void
State::initializeKlass2() {
#define SET_NAME(name) name ## _klass->set_name(s_prv->get_string(#name))
    SET_NAME(any);
    SET_NAME(null);
    SET_NAME(bool);
    SET_NAME(int);
    SET_NAME(char);
    SET_NAME(string);
    SET_NAME(array);
    SET_NAME(hashtable);
    SET_NAME(method);
    SET_NAME(bytecode_function);
    SET_NAME(native_function);
#undef SET_NAME
}

void
State::initializeKlass3() {
#define HASH_SET(name) klass_slots->insert(this, \
    Value::by_object(s_prv->get_string(#name)), Value::by_object(name ## _klass))
    HASH_SET(any);
    HASH_SET(null);
    HASH_SET(bool);
    HASH_SET(int);
    HASH_SET(char);
    HASH_SET(string);
    HASH_SET(array);
    HASH_SET(hashtable);
    HASH_SET(method);
    HASH_SET(bytecode_function);
    HASH_SET(native_function);
#undef HASH_SET
}

void
State::initializeString() {
    s_prv = StringProvider::create(this);
}

bool
State::addFunction(Function* func) {
    return addFunction(func, func->get_name());
}

bool
State::addFunction(Function* func, const String* name) {
    Value old;
    if (func_slots->find(Value::by_object(name), old)) {
        Object* fold = old.get_obj<Object>();
        if (fold->get_class() == native_function_klass
            || fold->get_class() == bytecode_function_klass) {

            Method* method = Method::create(this);
            method->add_function(this, (Function*)fold);
            method->add_function(this, func);

            return func_slots->assign(Value::by_object(name), Value::by_object(method));
        } else if (fold->get_class() == method_klass) {
            return static_cast<Method*>(fold)->add_function(this, func);
        } else {
            exit(1);
        }
    }
    return func_slots->insert_if_absent(this, Value::by_object(name), Value::by_object(func));
}

bool
State::addKlass(Klass* klass) {
    return addKlass(klass, klass->get_name());
}

bool
State::addKlass(Klass* klass, const String* name) {
    return klass_slots->insert_if_absent(this, Value::by_object(name), Value::by_object(klass));
}

bool
State::addVariable(String* id, Value val) {
    return var_slots->insert_if_absent(this, Value::by_object(id), val);
}

bool
State::loadModule(Module* module) {
    return module->initialize(this);
}

void
State::dumpClasses() {
    klass_slots->dump();
}

void
State::dumpFunctions() {
    func_slots->dump();
}

void
State::dumpVariables() {
    var_slots->dump();
}

Value
rhein::execute(State* R, String* entry_point, unsigned argc, Value* argv) {
    Value fn;
    if (!R->gfref(entry_point, fn)) {
        fatal("No such function");
    }

    if (!fn.is(Value::Type::Object)) {
        fatal("Not excutable object");
    }

    if (fn.get_obj<Object>()->get_class() == R->method_klass) {
        fn.get_obj<Method>()->dispatch(R, argc, argv, fn);
    }

    if (!fn.is(Value::Type::Object)) {
        fatal("Not excutable object");
    }

    Object* ofn = fn.get_obj<Object>();
    if (ofn->get_class() == R->bytecode_function_klass) {
        return execute(R, (BytecodeFunction*)ofn, argc, argv);
    } else if (ofn->get_class() == R->native_function_klass) {
        return ((NativeFunction*)ofn)->get_body()(R, argc, argv);
    } else {
        fatal("Not excutable object");
    }
    // NOTREACHED
    return Value::k_nil();
}

inline uint32_t
getInsnArgU(uint32_t insn) {
    return insn >> 8;
}

inline uint32_t
getInsnArgUU1(uint32_t insn) {
    return (insn >> 8) & 0xff;
}

inline uint32_t
getInsnArgUU2(uint32_t insn) {
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
    uint32_t depth = getInsnArgUU1(insn); \
    uint32_t offset = getInsnArgUU2(insn); \
    if (!fr->op(depth, offset, *(--sp))) { \
        fprintf(stderr, #op ":%u:%u\n", depth, offset); \
        fatal("Error on local refer"); \
    } \
    pc++; \
    } \
    break;

#define LOCAL_SET_OP(op) { \
    uint32_t depth = getInsnArgUU1(insn); \
    uint32_t offset = getInsnArgUU2(insn); \
    if (!fr->op(depth, offset, *(sp))) { \
        fprintf(stderr, #op ":%u:%u\n", depth, offset); \
        fatal("Error on local set"); \
    } \
    pc++; \
    } \
    break;

#define GLOBAL_REFER_OP(op) { \
    Value id = fn->get_constant_table()[getInsnArgU(insn)]; \
    if (!id.is(Value::Type::Object) \
			|| id.get_obj<Object>()->get_class() != R->string_klass) { \
        fatal("Error on global refer"); \
    } \
    if (!R->op(id.get_obj<String>(), *(--sp))) { \
        fatal("Error on global refer"); \
    } \
    pc++; \
    } \
    break;

#define GLOBAL_SET_OP(op) { \
    Value id = fn->get_constant_table()[getInsnArgU(insn)]; \
    if (!id.is(Value::Type::Object) \
    		|| id.get_obj<Object>()->get_class() != R->string_klass) { \
        fatal("Error on global set"); \
    } \
    if (!R->op(id.get_obj<String>(), *(sp))) { \
        fatal("Error on global set"); \
    } \
    pc++; \
    } \
    break;

Value
rhein::execute(State* R, BytecodeFunction* bfn, unsigned argc_, Value* args_) {
    Frame* fr = Frame::create(R, bfn, nullptr, nullptr, argc_, args_);
    BytecodeFunction* fn = bfn;
    Value* sp = fr->stack + bfn->get_stack_size();
    const uint32_t* pc = bfn->get_bytecode();

    for(; ; ){
        uint32_t insn = *pc;
#if 0
        cerr << "stack: ";
        for (int i = 0; i < fn->get_stack_size() - 1; i++) {
            cerr << fr->stack[i] << ":";
        }
        cerr << fr->stack[fn->get_stack_size() - 1] << endl;
        cerr << "pc: " << pc - fn->get_bytecode() << endl;
        cerr << "sp: " << fn->get_stack_size() - (sp - fr->stack) << endl;
        cerr << "code: " << (insn & 0xff) << endl;
        cerr.flush();
#endif
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
                uint32_t dest = getInsnArgU(insn);
                pc = fn->get_bytecode() + dest;
            }
                break;
            case Insn::IfJump: {
                uint32_t dest = getInsnArgU(insn);
                if ((*sp++).like_true()) {
                    pc = fn->get_bytecode() + dest;
                } else {
                    pc++;
                }
            }
                break;
            case Insn::UnlessJump: {
                uint32_t dest = getInsnArgU(insn);
                if ((*sp++).like_false()) {
                    pc = fn->get_bytecode() + dest;
                } else {
                    pc++;
                }
            }
                break;
            case Insn::Call: {
                uint32_t argc = getInsnArgU(insn);
                Value func = *sp++;
                Value* stack_args = sp;

                if (!func.is(Value::Type::Object)) {
                    fatal("Not callable object");
                }

                Frame* closure = nullptr;
                if (func.get_obj<Object>()->get_class() == R->method_klass) {
                    if (!func.get_obj<Method>()->dispatch(R, argc, sp, func)) {
                        fatal("Could not dispatch");
                    }
                    closure = func.get_obj<Method>()->get_closure();
                }

                Function* ofunc = func.get_obj<Function>();

                if (ofunc->get_class() == R->bytecode_function_klass) {
                    fn = (BytecodeFunction*)ofunc;
                    unsigned arg_count = fn->get_num_args() + (fn->is_variadic() ? 1 : 0);
                    Value* args = R->ator->allocateRawArray(arg_count);
                    memcpy(args, stack_args, fn->get_num_args());

                    if (fn->is_variadic()) {
                        unsigned vargs_count = argc - fn->get_num_args();
                        Array* vargs = Array::create(R, vargs_count);
                        for (unsigned i = 0; i < vargs_count; i++) {
                            vargs->elt_set(i, sp[i + fn->get_num_args()]);
                        }
                        args[fn->get_num_args()] = Value::by_object(vargs);
                    }

                    if (closure == nullptr) {
                        closure = fn->get_closure();
                    }

                    fr->pc = pc + 1;
                    fr->sp = sp + argc;
                    fr = Frame::create(R, fn, fr, closure, argc, args);
                    pc = fn->get_bytecode();
                    sp = fr->stack + fn->get_stack_size();
                } else if (ofunc->get_class() == R->native_function_klass) {
                    Value ret = (*func.get_obj<NativeFunction>()->get_body())(R, argc, sp);
                    sp += argc;
                    *(--sp) = ret;
                    pc++;
                }
            }
                break;
            case Insn::Ret: {
                Frame* parent = fr->parent;
                if (parent == nullptr) { // if top level
                    goto VMExit;
                }
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

                *(--sp) = Value::by_object(Array::create(R, size.get_int()));
                pc++;
            }
                break;
            case Insn::Raref: {
           Value index = *sp++;
                Value array = *sp;

                if (!array.is(Value::Type::Object) || array.get_obj<Object>()->get_class() != R->array_klass) {
                    fatal("Invalid type");
                }

                if (!array.get_obj<Array>()->index_ref(R, index, *(--sp))) {
                    fatal("Cannot refer");
                }
                pc++;
            }
                break;
            case Insn::Raset: {
                Value value = *sp++;
                Value index = sp[0];
                Value array = sp[1];

                if (!array.is(Value::Type::Object) || array.get_obj<Object>()->get_class() != R->array_klass) {
                    fatal("Invalid type");
                }

                if (!array.get_obj<Array>()->index_set(R, index, value)) {
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

                if (!sp[0].get_obj<Object>()->index_ref(R, index, sp[0])) {
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

                if (!obj.get_obj<Object>()->index_set(R, index, value)) {
                    fatal("Cannot set");
                }
                pc++;
            }
                break;
            case Insn::Mref: {
                Value obj = sp[0];
                Value id = fn->get_constant_table()[getInsnArgU(insn)];

                if (!obj.is(Value::Type::Object) || !id.is(Value::Type::Object)
                    || id.get_obj<Object>()->get_class() != R->string_klass) {

                    obj.get_klass(R)->get_name()->dump();
                    fatal("Cannot refer");
                }

                if (!obj.get_obj<Object>()->slot_ref(R, id.get_obj<String>(), sp[0])) {
                    id.get_obj<String>()->dump();
                    fatal("Cannot refer");
                }
                pc++;
            }
                break;
            case Insn::Mset: {
                Value obj = *sp++;
                Value id = fn->get_constant_table()[getInsnArgU(insn)];

                if (!obj.is(Value::Type::Object) || !id.is(Value::Type::Object)
                    || id.get_obj<Object>()->get_class() != R->string_klass) {
                    
                    fatal("Cannot set");
                }

                if (!obj.get_obj<Object>()->slot_set(R, id.get_obj<String>(), sp[0])) {
                    fatal("Cannot set");
                }
                pc++;
            }
                break;
            case Insn::Lfref: LOCAL_REFER_OP(lfref)
            case Insn::Lfset: LOCAL_SET_OP(lfset)
            case Insn::Lvref: LOCAL_REFER_OP(lvref)
            case Insn::Lvset: LOCAL_SET_OP(lvset)
            case Insn::Laref: LOCAL_REFER_OP(laref)
            case Insn::Laset: LOCAL_SET_OP(laset)
            case Insn::Gfref: GLOBAL_REFER_OP(gfref)
            case Insn::Gvref: GLOBAL_REFER_OP(gvref)
            case Insn::Gvset: GLOBAL_SET_OP(gvset)
            case Insn::Load:
                *(--sp) = fn->get_constant_table()[getInsnArgU(insn)];
                pc++;
                break;
            case Insn::LoadKlass: {
                Value id = fn->get_constant_table()[getInsnArgU(insn)];

                if (!(id.is(Value::Type::Object) && id.get_obj<Object>()->get_class() == R->string_klass)) {
                    fatal("Name must be string");
                }

                if (!R->getKlass(id.get_obj<String>(), *(--sp))) {
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
                Value id = fn->get_constant_table()[getInsnArgU(insn)];

                if (!id.is(Value::Type::Object) || id.get_obj<Object>()->get_class() != R->string_klass) {
                    fatal("Cannot enclose");
                }

                Value func;
                if (!R->gfref(id.get_obj<String>(), func)) {
                    fatal("Cannot enclose");
                }

                if (!func.is(Value::Type::Object)) {
                    fatal("Cannot enclose");
                }

                if (func.get_obj<Object>()->get_class() == R->native_function_klass) {
                    *(--sp) = Value::by_object(func.get_obj<NativeFunction>()->enclose(R, fr));
                } else if (func.get_obj<Object>()->get_class() == R->bytecode_function_klass) {
                    *(--sp) = Value::by_object(func.get_obj<BytecodeFunction>()->enclose(R, fr));
                } else if (func.get_obj<Object>()->get_class() == R->method_klass) {
                    *(--sp) = Value::by_object(func.get_obj<Method>()->enclose(R, fr));
                } else {
                    fatal("Cannot enclose");
                }
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
                sp += getInsnArgU(insn);
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
        }
    }
VMExit:
    return *sp;
}

