//
// function.h
//

#ifndef FUNCTION_H
#define FUNCTION_H

#include <tr1/cstdint>

#include "object/object.h"
#include "object/imstring.h"

namespace rhein {

class State;
struct Frame;

typedef Value (*NativeFunctionBody)(State*, unsigned, Value*);

class Function : public Object {
protected:
    String* name;
    bool variable_arg;
    unsigned arg_count;
    String** arg_klass_id;
    Klass** arg_klass;
    Frame* closure;

    Function(Klass* klass, String* name_, bool variable_arg_, unsigned arg_count_,
        String** arg_klass_id_)
        : Object(klass), name(name_), variable_arg(variable_arg_),
          arg_count(arg_count_), arg_klass_id(arg_klass_id_), arg_klass(nullptr),
          closure(nullptr) { }

public:
    Klass** getArgumentKlass() const { return arg_klass; }
    unsigned getArgumentCount() const { return arg_count; }
    bool isVariableArgument() const { return variable_arg; }
    const String* getName() const { return name; }
    Frame* getClosure() const { return closure; }

    bool resolve(State* state);
};

class NativeFunction : public Function {
    NativeFunctionBody body;
    bool copied;

    NativeFunction(State* state, String* name, bool variable_arg,
        unsigned arg_count, String** arg_klass_id, NativeFunctionBody body);

public:
    unsigned long hash() { return reinterpret_cast<unsigned long>(this); }

    static NativeFunction* create(State* state, String* name,
        NativeFunctionBody body) {

        return create(state, name, true, 0, nullptr, body);
    }

    static NativeFunction* create(State* state, String* name,
        bool variable_arg, unsigned arg_count, String** arg_klass_id,
        NativeFunctionBody body);

    NativeFunctionBody getBody() const { return body; }

    NativeFunction* copy(State* state);
    NativeFunction* enclose(State* state, Frame* closure);
};

class BytecodeFunction : public Function {
    bool copied;
    unsigned stack_size;
    unsigned func_slot_size;
    unsigned var_slot_size;
    unsigned constant_table_size;
    Value* constant_table;
    unsigned bytecode_size;
    uint32_t* bytecode;

    BytecodeFunction(State* state, String* name_, bool variable_arg_,
        unsigned argc_, String** arg_klass_id_, unsigned func_slot_size_,
        unsigned var_slot_size_, unsigned stack_size_,
        unsigned constant_table_size_, Value* constant_table_,
        unsigned bytecode_size_, uint32_t* bytecode_);

public:
    unsigned long hash() { return reinterpret_cast<unsigned long>(this); }

    static BytecodeFunction* create(State* state, String* name,
        bool variable_arg, unsigned argc, String** arg_klass_id,
        unsigned func_slot_size, unsigned var_slot_size, unsigned stack_size,
        unsigned constant_table_size, Value* constant_table,
        unsigned bytecode_size, uint32_t* bytecode);

    unsigned getStackSize() const { return stack_size; }
    unsigned getFunctionSlotSize() const { return func_slot_size; }
    unsigned getVariableSlotSize() const { return var_slot_size; }
    const uint32_t* getBytecode() const { return bytecode; }
    const Value* getConstantTable() const { return constant_table; }

    BytecodeFunction* copy(State* state);
    BytecodeFunction* enclose(State* state, Frame* closure);
};

class DispatcherNode;

class Method : public Object {
    bool copied;
    DispatcherNode* node;
    Frame* closure;

    Method(State* state);

public:
    static Method* create(State* state);

    bool hasClosure() const { return (closure != nullptr); }
    Frame* getClosure() const { return closure; }

    bool dispatch(State* state, unsigned argc, Value* args, Value& result);
    bool addFunction(State* state, Function* func);

    Method* copy(State* state);
    Method* enclose(State* state, Frame* closure);
};

}

#endif // FUNCTION_H

