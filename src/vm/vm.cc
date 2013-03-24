//
// vm.cc
//

#include <iostream>
using namespace std;

#include <cstdint>

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
        Eq,
        Ne,
        Gt,
        Lt,
        Ge,
        Le,
        Jump,
        IfJump,
        UnlessJump,
        Call,
        Ret,
        Ranew,
        Raref,
        Raset,
        Iref,
        Iset,
        Mref,
        Mset,
        Lfref,
        Lfset,
        Lvref,
        Lvset,
        Laref,
        Laset,
        Gfref,
        Gvref,
        Gvset,
        Load,
        LoadKlass,
        LoadUndef,
        LoadNull,
        LoadTrue,
        LoadFalse,
        Enclose,
        Dup,
        Pop,
        Escape,
    };
};

Frame::Frame(State* state, BytecodeFunction* fn_, Frame* parent_, Frame* closure_,
    unsigned argc_, Value* args_)
    : fn(fn_), closure(closure_), parent(parent_),
      stack(state->ator->allocateRawArray(fn_->getStackSize())),
      argc(argc_), args(args_),
      func_slots(state->ator->allocateRawArray(fn_->getFunctionSlotSize())),
      var_slots(state->ator->allocateRawArray(fn_->getVariableSlotSize())),
      pc(nullptr), sp(nullptr) { }

bool
Frame::lfref(unsigned depth, unsigned offset, Value& value) {
    Frame* frame = this;
    for (; depth > 0; depth--, frame = frame->closure) {
        if (frame == nullptr) {
            return false;
        }
    }

    if (frame->fn->getFunctionSlotSize() <= offset) {
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

    if (frame->fn->getFunctionSlotSize() <= offset) {
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

    if (frame->fn->getVariableSlotSize() <= offset) {
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

    if (frame->fn->getVariableSlotSize() <= offset) {
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

State::State() : string_provider(nullptr) {
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
#define SET_NAME(name) name ## _klass->setName(string_provider->getString(#name))
    SET_NAME(any);
    SET_NAME(null);
    SET_NAME(bool);
    SET_NAME(int);
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
    make_value(string_provider->getString(#name)), make_value(name ## _klass))
    HASH_SET(any);
    HASH_SET(null);
    HASH_SET(bool);
    HASH_SET(int);
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
    string_provider = StringProvider::create(this);
}

bool
State::addFunction(Function* func) {
    Value name = make_value(func->getName());
    Value old;
    if (func_slots->find(name, old)) {
        Object* fold = get_obj(old);
        if (fold->getKlass() == native_function_klass
            || fold->getKlass() == bytecode_function_klass) {

            Method* method = Method::create(this);
            return (method->addFunction(this, (Function*)fold)
                    && method->addFunction(this, func)
                    && func_slots->assign(name, make_value(method)));
        } else if (fold->getKlass() == method_klass) {
            return static_cast<Method*>(fold)->addFunction(this, func);
        } else {
            exit(1);
        }
    }
    return func_slots->insert(this, make_value(func->getName()), make_value(func));
}

bool
State::addKlass(Klass* klass) {
    return klass_slots->insert(this, make_value(klass->getName()), make_value(klass));
}

bool
State::addVariable(String* id) {
    return var_slots->insert(this, make_value(id), Cnull);
}

bool
State::loadModule(Module* module) {
    return module->initialize(this);
}

Value
rhein::execute(State* state, String* entry_point, unsigned argc, Value* argv) {
    Value fn;
    if (!state->gfref(entry_point, fn)) {
        fatal("No such function");
    }

    if (!is_obj(fn)) {
        fatal("Not excutable object");
    }

    if (get_obj(fn)->getKlass() == state->method_klass) {
        ((Method*)get_obj(fn))->dispatch(state, argc, argv, fn);
    }

    if (!is_obj(fn)) {
        fatal("Not excutable object");
    }

    Object* ofn = get_obj(fn);
    if (ofn->getKlass() == state->bytecode_function_klass) {
        return execute(state, (BytecodeFunction*)ofn, argc, argv);
    } else if (ofn->getKlass() == state->native_function_klass) {
        return ((NativeFunction*)ofn)->getBody()(state, argc, argv);
    } else {
        fatal("Not excutable object");
    }
    // NOTREACHED
    return Cnull;
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
        fatal("Error on local refer"); \
    } \
    pc++; \
    } \
    break;

#define LOCAL_SET_OP(op) { \
    uint32_t depth = getInsnArgUU1(insn); \
    uint32_t offset = getInsnArgUU2(insn); \
    if (!fr->op(depth, offset, *(sp++))) { \
        fatal("Error on local set"); \
    } \
    pc++; \
    } \
    break;

#define GLOBAL_REFER_OP(op) { \
    Value id = fn->getConstantTable()[getInsnArgU(insn)]; \
    if (!is_obj(id) || get_obj(id)->getKlass() != state->string_klass) { \
        fatal("Error on global refer"); \
    } \
    if (!state->op((String*)get_obj(id), *(--sp))) { \
        fatal("Error on global refer"); \
    } \
    pc++; \
    } \
    break;

#define GLOBAL_SET_OP(op) { \
    Value id = fn->getConstantTable()[getInsnArgU(insn)]; \
    if (!is_obj(id) || get_obj(id)->getKlass() != state->string_klass) { \
        fatal("Error on global set"); \
    } \
    if (!state->op((String*)get_obj(id), *(sp++))) { \
        fatal("Error on global set"); \
    } \
    pc++; \
    } \
    break;

Value
rhein::execute(State* state, BytecodeFunction* bfn, unsigned argc_, Value* args_) {
    Frame* fr = Frame::create(state, bfn, nullptr, nullptr, argc_, args_);
    BytecodeFunction* fn = bfn;
    Value* sp = fr->stack + bfn->getStackSize();
    const uint32_t* pc = bfn->getBytecode();

    for(; ; ){
        uint32_t insn = *pc;
        /* cerr << "stack: ";
        for (int i = 0; i < fn->getStackSize() - 1; i++) {
            cerr << fr->stack[i] << ":";
        }
        cerr << fr->stack[fn->getStackSize() - 1] << endl;
        cerr << "pc: " << pc - fn->getBytecode() << endl;
        cerr << "code: " << (insn & 0xff) << endl;
        cerr.flush();
        */
        switch(insn & 0xff) {
            case Insn::Add: BINARY_OP(op_add)
            case Insn::Sub: BINARY_OP(op_sub)
            case Insn::Mul: BINARY_OP(op_mul)
            case Insn::Div: BINARY_OP(op_div)
            case Insn::Mod: BINARY_OP(op_mod)
            case Insn::Inc: UNARY_OP(op_inc)
            case Insn::Dec: UNARY_OP(op_dec)
            case Insn::Eq: BINARY_OP(op_eq)
            case Insn::Ne: BINARY_OP(op_ne)
            case Insn::Gt: BINARY_OP(op_gt)
            case Insn::Lt: BINARY_OP(op_lt)
            case Insn::Ge: BINARY_OP(op_ge)
            case Insn::Le: BINARY_OP(op_le)
            case Insn::Jump: {
                uint32_t dest = getInsnArgU(insn);
                pc = fn->getBytecode() + dest;
            }
                break;
            case Insn::IfJump: {
                uint32_t dest = getInsnArgU(insn);
                if (like_true(*sp++)) {
                    pc = fn->getBytecode() + dest;
                } else {
                    pc++;
                }
            }
                break;
            case Insn::UnlessJump: {
                uint32_t dest = getInsnArgU(insn);
                if (like_false(*sp++)) {
                    pc = fn->getBytecode() + dest;
                } else {
                    pc++;
                }
            }
                break;
            case Insn::Call: {
                uint32_t argc = getInsnArgU(insn);
                Value func = *sp++;
                Value* args = sp;

                if (!is_obj(func)) {
                    fatal("Not callable object");
                }

                Frame* closure = nullptr;
                if (get_obj(func)->getKlass() == state->method_klass) {
                    if (!((Method*)func)->dispatch(state, argc, sp, func)) {
                        fatal("Could not dispatch");
                    }
                    closure = ((Method*)func)->getClosure();
                }

                Object* ofunc = get_obj(func);
                if (ofunc->getKlass() == state->bytecode_function_klass) {
                    fn = (BytecodeFunction*)ofunc;
                    if (closure == nullptr) {
                        closure = fn->getClosure();
                    }

                    fr->pc = pc + 1;
                    fr->sp = sp + argc;
                    fr = Frame::create(state, fn, fr, closure, argc, args);
                    pc = fn->getBytecode();
                    sp = fr->stack + fn->getStackSize();
                } else if (ofunc->getKlass() == state->native_function_klass) {
                    Value ret = (*((NativeFunction*)get_obj(func))->getBody())(state, argc, sp);
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
                if (!is_int(size) || get_int(size) < 0) {
                    fatal("Invalid size");
                }

                *(--sp) = make_value(Array::create(state, get_int(size)));
                pc++;
            }
                break;
            case Insn::Raref: {
                Value index = *sp++;
                Value array = *sp;

                if (!is_obj(array) || get_obj(array)->getKlass() != state->array_klass) {
                    fatal("Invalid type");
                }

                if (!get_obj(array)->indexRef(state, index, *(--sp))) {
                    fatal("Cannot refer");
                }
                pc++;
            }
                break;
            case Insn::Raset: {
                Value value = *sp++;
                Value index = sp[0];
                Value array = sp[1];

                if (!is_obj(array) || get_obj(array)->getKlass() != state->array_klass) {
                    fatal("Invalid type");
                }

                if (!get_obj(array)->indexSet(state, index, value)) {
                    fatal("Cannot set");
                }
                pc++;
            }
                break;
            case Insn::Iref: {
                Value index = *sp++;
                if (!is_obj(sp[0])) {
                    fatal("Cannot refer");
                }

                if (!get_obj(sp[0])->indexRef(state, index, sp[0])) {
                    fatal("Cannot refer");
                }
                pc++;
            }
                break;
            case Insn::Iset: {
                Value index = *sp++;
                Value obj = *sp++;
                Value value = sp[0];

                if (!is_obj(obj)) {
                    fatal("Cannot set");
                }

                if (!get_obj(obj)->indexSet(state, index, value)) {
                    fatal("Cannot set");
                }
                pc++;
            }
                break;
            case Insn::Mref: {
                Value obj = sp[0];
                Value id = fn->getConstantTable()[getInsnArgU(insn)];

                if (!is_obj(obj) || !is_obj(id)
                    || get_obj(id)->getKlass() != state->string_klass) {

                    fatal("Cannot refer");
                }

                if (!get_obj(obj)->slotRef(state, (String*)get_obj(id), sp[0])) {
                    fatal("Cannot refer");
                }
                pc++;
            }
                break;
            case Insn::Mset: {
                Value obj = *sp++;
                Value id = fn->getConstantTable()[getInsnArgU(insn)];

                if (!is_obj(obj) || !is_obj(id)
                    || get_obj(id)->getKlass() != state->string_klass) {
                    
                    fatal("Cannot set");
                }

                if (!get_obj(obj)->slotSet(state, (String*)get_obj(id), sp[0])) {
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
                *(--sp) = fn->getConstantTable()[getInsnArgU(insn)];
                pc++;
                break;
            case Insn::LoadKlass: {
                Value id = fn->getConstantTable()[getInsnArgU(insn)];

                if (!(is_obj(id) && get_obj(id)->getKlass() == state->string_klass)) {
                    fatal("Name must be string");
                }

                if (!state->getKlass((String*)get_obj(id), *(--sp))) {
                    fatal("Cannot find klass");
                }
                pc++;
            }
                break;
            case Insn::LoadUndef:
                *(--sp) = Cundef;
                pc++;
                break;
            case Insn::LoadNull:
                *(--sp) = Cnull;
                pc++;
                break;
            case Insn::LoadTrue:
                *(--sp) = Ctrue;
                pc++;
                break;
            case Insn::LoadFalse:
                *(--sp) = Cfalse;
                pc++;
                break;
            case Insn::Enclose: {
                Value id = fn->getConstantTable()[getInsnArgU(insn)];

                if (!is_obj(id) || get_obj(id)->getKlass() != state->string_klass) {
                    fatal("Cannot enclose");
                }

                Value func;
                if (!state->gfref((String*)id, func)) {
                    fatal("Cannot enclose");
                }

                if (!is_obj(func)) {
                    fatal("Cannot enclose");
                }

                if (get_obj(func)->getKlass() == state->native_function_klass) {
                    *(--sp) = make_value(((NativeFunction*)get_obj(func))->enclose(state, fr));
                } else if (get_obj(func)->getKlass() == state->bytecode_function_klass) {
                    *(--sp) = make_value(((BytecodeFunction*)get_obj(func))->enclose(state, fr));
                } else if (get_obj(func)->getKlass() == state->method_klass) {
                    *(--sp) = make_value(((Method*)get_obj(func))->enclose(state, fr));
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
            default:
                fatal("Invalid insn");
        }
    }
VMExit:
    return *sp;
}

