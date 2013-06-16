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
    bool variadic;
    unsigned arg_count;
    String** arg_klass_id;
    Klass** arg_klass;
    Frame* closure;

    Function(Klass* klass, String* name_, bool variadic_, unsigned arg_count_,
        String** arg_klass_id_)
        : Object(klass), name(name_), variadic(variadic_),
          arg_count(arg_count_), arg_klass_id(arg_klass_id_), arg_klass(nullptr),
          closure(nullptr) { }

public:
    Klass** get_arg_classes() const { return arg_klass; }
    unsigned get_num_args() const { return arg_count; }
    bool is_variadic() const { return variadic; }
    const String* get_name() const { return name; }
    Frame* get_closure() const { return closure; }

    bool resolve(State* R);
};

class NativeFunction : public Function {
    NativeFunctionBody body;
    bool copied;

    NativeFunction(State* R, String* name, bool variable_arg,
        unsigned arg_count, String** arg_klass_id, NativeFunctionBody body);

public:
    static NativeFunction* create(State* R, String* name,
        NativeFunctionBody body) {

        return create(R, name, true, 0, nullptr, body);
    }

    static NativeFunction* create(State* R, String* name,
        bool variable_arg, unsigned arg_count, String** arg_klass_id,
        NativeFunctionBody body);

    NativeFunctionBody get_body() const { return body; }

    NativeFunction* copy(State* R);
    NativeFunction* enclose(State* R, Frame* closure);
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

    BytecodeFunction(State* R, String* name_, bool variable_arg_,
        unsigned argc_, String** arg_klass_id_, unsigned func_slot_size_,
        unsigned var_slot_size_, unsigned stack_size_,
        unsigned constant_table_size_, Value* constant_table_,
        unsigned bytecode_size_, uint32_t* bytecode_);

public:
    unsigned long hash() { return reinterpret_cast<unsigned long>(this); }

    static BytecodeFunction* create(State* R, String* name,
        bool variable_arg, unsigned argc, String** arg_klass_id,
        unsigned func_slot_size, unsigned var_slot_size, unsigned stack_size,
        unsigned constant_table_size, Value* constant_table,
        unsigned bytecode_size, uint32_t* bytecode);

    unsigned get_stack_size() const { return stack_size; }
    unsigned get_function_slot_num() const { return func_slot_size; }
    unsigned get_variable_slot_num() const { return var_slot_size; }
    const uint32_t* get_bytecode() const { return bytecode; }
    const Value* get_constant_table() const { return constant_table; }

    BytecodeFunction* copy(State* R);
    BytecodeFunction* enclose(State* R, Frame* closure);
};

class DispatcherNode;

class Method : public Object {
    bool copied;
    DispatcherNode* node;
    Frame* closure;

    Method(State* R);

public:
    static Method* create(State* R);

    bool has_closure() const { return (closure != nullptr); }
    Frame* get_closure() const { return closure; }

    bool dispatch(State* R, unsigned argc, Value* args, Value& result);
    bool add_function(State* R, Function* func);

    Method* copy(State* R);
    Method* enclose(State* R, Frame* closure);
};

}

#endif // FUNCTION_H

