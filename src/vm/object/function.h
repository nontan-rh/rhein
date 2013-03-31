//
// function.h
//

#ifndef FUNCTION_H
#define FUNCTION_H

#include <cstdint>

#include "object/object.h"
#include "object/imstring.h"

namespace rhein {

class State;
struct Frame;

typedef Value (*NativeFunctionBody)(State*, unsigned, Value*);

class Function : public Object {
    //Function() = delete;
    //Function& operator=(const Function& /* rht */) = delete;

protected:
    //Function(const Function& /* rht */) = default;

    String* name;
    bool variable_arg;
    unsigned arg_count;
    Klass** arg_klass;
    Frame* closure;

    Function(Klass* klass, String* name_, bool variable_arg_, unsigned arg_count_,
        Klass** arg_klass_)
        : Object(klass), name(name_), variable_arg(variable_arg_), arg_count(arg_count_),
          arg_klass(arg_klass_), closure(nullptr) { }

public:
    Klass** getArgumentKlass() const { return arg_klass; }
    unsigned getArgumentCount() const { return arg_count; }
    bool isVariableArgument() const { return variable_arg; }
    const String* getName() const { return name; }
    Frame* getClosure() const { return closure; }
};

class NativeFunction : public Function {
    //NativeFunction() = delete;
    //NativeFunction& operator=(const NativeFunction& /* rht */) = delete;

    //NativeFunction(const NativeFunction& /* rht */) = default;

    NativeFunctionBody body;
    bool copied;

    NativeFunction(State* state, String* name, bool variable_arg, unsigned arg_count,
        Klass** arg_klass, NativeFunctionBody body);

public:
    unsigned long hash() { return reinterpret_cast<unsigned long>(this); }

    static NativeFunction* create(State* state, String* name, NativeFunctionBody body) {
        return create(state, name, true, 0, nullptr, body);
    }

    static NativeFunction* create(State* state, String* name, bool variable_arg, unsigned arg_count,
        Klass** arg_klass, NativeFunctionBody body);

    NativeFunctionBody getBody() const { return body; }

    NativeFunction* copy(State* state);
    NativeFunction* enclose(State* state, Frame* closure);
};

class BytecodeFunction : public Function {
    //BytecodeFunction() = delete;
    //BytecodeFunction& operator=(const BytecodeFunction& /* rht */) = delete;

    //BytecodeFunction(const BytecodeFunction& /* rht */) = default;

    bool copied;
    unsigned stack_size;
    unsigned func_slot_size;
    unsigned var_slot_size;
    unsigned constant_table_size;
    Value* constant_table;
    unsigned bytecode_size;
    uint32_t* bytecode;

    BytecodeFunction(State* state, String* name_, bool variable_arg_, unsigned argc_,
        Klass** arg_klass_, unsigned func_slot_size_, unsigned var_slot_size_,
        unsigned stack_size_, unsigned constant_table_size_,
        Value* constant_table_, unsigned bytecode_size_, uint32_t* bytecode_);

public:
    unsigned long hash() { return reinterpret_cast<unsigned long>(this); }

    static BytecodeFunction* create(State* state, String* name,
        bool variable_arg, unsigned argc, Klass** arg_klass,
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
    //Method() = delete;
    //Method& operator=(const Method& /* rht */) = delete;

    //Method(const Method& /* rht */) = default;

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

