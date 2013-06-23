//
// function.cc
//

#include "object.h"
#include "allocator.h"
#include "vm.h"

using namespace rhein;

FunctionInfo*
FunctionInfo::create(State* R, Symbol* id) {
	return create(R, id, true, 0, nullptr);
}

FunctionInfo*
FunctionInfo::create(State* R, Symbol* id, bool variadic, unsigned num_args,
		Symbol** arg_class_ids) {
	void* p = R->ator->allocateStruct<FunctionInfo>();
	return new (p) FunctionInfo(id, variadic, num_args, arg_class_ids);
}

bool
FunctionInfo::resolve(State* R) {
    arg_classes_ = R->ator->allocateBlock<Klass*>(num_args_);

    for (unsigned i = 0; i < num_args_; i++) {
        Value klass;
        if (!R->getKlass(arg_class_ids_[i], klass)) {
            return false;
        }

        arg_classes_[i] = klass.get_obj<Klass>();
    }

    return true;
}

NativeFunction::NativeFunction(State* R, FunctionInfo* info, NativeFunctionBody body_)
    : Function(R->native_function_klass, info), body(body_), copied(false) { }

NativeFunction*
NativeFunction::create(State* R, FunctionInfo* info, NativeFunctionBody body) {
    void* p = R->ator->allocateObject<NativeFunction>();
    return new (p) NativeFunction(R, info, body);
}

NativeFunction*
NativeFunction::copy(State* R) {
    void* p = R->ator->allocateObject<NativeFunction>();
    NativeFunction* copy_func = new (p) NativeFunction(*this);
    copy_func->copied = true;
    return copy_func;
}

NativeFunction*
NativeFunction::enclose(State* R, Frame* closure) {
    NativeFunction* closure_func = copy(R);
    closure_func->closure = closure;
    return closure_func;
}

BytecodeFunction::BytecodeFunction(State* R, FunctionInfo* info,
    unsigned func_slot_size_, unsigned var_slot_size_,
    unsigned stack_size_, unsigned constant_table_size_,
    Value* constant_table_, unsigned bytecode_size_, uint32_t* bytecode_)
    : Function(R->bytecode_function_klass, info),
      copied(false),
      stack_size(stack_size_), func_slot_size(func_slot_size_),
      var_slot_size(var_slot_size_),
      constant_table_size(constant_table_size_),
      constant_table(constant_table_),
      bytecode_size(bytecode_size_),
      bytecode(bytecode_) { }

BytecodeFunction*
BytecodeFunction::create(State* R, FunctionInfo* info,
    unsigned func_slot_size, unsigned var_slot_size, unsigned stack_size,
    unsigned constant_table_size, Value* constant_table,
    unsigned bytecode_size, uint32_t* bytecode) {

    void* p = R->ator->allocateObject<BytecodeFunction>();
    return new (p) BytecodeFunction(R, info,
        func_slot_size, var_slot_size, stack_size, constant_table_size,
        constant_table, bytecode_size, bytecode);
}

BytecodeFunction*
BytecodeFunction::copy(State* R) {
    void* p = R->ator->allocateObject<BytecodeFunction>();
    BytecodeFunction* copy_func = new (p) BytecodeFunction(*this);
    copy_func->copied = true;
    return copy_func;
}

BytecodeFunction*
BytecodeFunction::enclose(State* R, Frame* closure) {
    BytecodeFunction* closure_func = copy(R);
    closure_func->closure = closure;
    return closure_func;
}

class rhein::DispatcherNode {
    Value entry;
    Value variable_entry;
    HashTable* child_table;

    static void* operator new (size_t /* size */, void* p) { return p; }

    DispatcherNode(State* R)
        : entry(Value::k_nil()), variable_entry(Value::k_nil()),
          child_table(HashTable::create(R)) { }

public:
    static DispatcherNode* create(State* R) {
        void* p = R->ator->allocateStruct<DispatcherNode>();
        return new (p) DispatcherNode(R);
    }

    bool dispatch(State* R, unsigned argc, Value* args, unsigned index,
    		Value& func) {
        if (argc == index) {
            if (!entry.is(Value::Type::Nil)) {
                func = entry;
                return true;
            } else if (!variable_entry.is(Value::Type::Nil)) {
                func = variable_entry;
                return true;
            } else {
                return false;
            }
        }

        Klass* klass = args[index].get_klass(R);
        while (true) {
            Value child;
            if (child_table->find(Value::by_object(klass), child)) {
                // Unsafe cast!
                if (child.get_obj<DispatcherNode>()->dispatch(R,
                		argc, args, index + 1, func)) {
                    return true;
                }
            }
            klass = klass->get_parent();
            if (klass == nullptr) {
                break;
            }
        }
        if (!variable_entry.is(Value::Type::Nil)) {
            func = variable_entry;
            return true;
        }
        return false;
    }

    bool addFunction(State* R, Value func, Function* func_body,
    		unsigned index) {
        if (func_body->info()->num_args() == index) {
            if (func_body->info()->variadic()) {
                if (!variable_entry.is(Value::Type::Nil)) {
                    return false;
                }
                variable_entry = func;
                return true;
            } else {
                if (!entry.is(Value::Type::Nil)) {
                    return false;
                }
                entry = func;
                return true;
            }
        }
        Klass* klass = func_body->info()->arg_classes()[index];
        Value child;
        if (!child_table->find(Value::by_object(klass), child)) {
            // Unsafe cast
            child = Value::by_object((Object*)create(R));
            child_table->insert_if_absent(R, Value::by_object(klass), child);
        }
        // Unsafe cast
        return child.get_obj<DispatcherNode>()->addFunction(R, func,
        		func_body, index + 1);
    }
};

Method::Method(State* R)
    : Object(R->method_klass), copied(false),
      node(DispatcherNode::create(R)),
      closure(nullptr) { }

Method*
Method::create(State* R) {
    void* p = R->ator->allocateObject<Method>();
    return new (p) Method(R);
}

bool
Method::dispatch(State* R, unsigned argc, Value* args, Value& result) {
    if (!node->dispatch(R, argc, args, 0, result)) {
        return false;
    }
    return true;
}

bool
Method::add_function(State* R, Function* func) {
    return node->addFunction(R, Value::by_object(func), func, 0);
}

Method*
Method::copy(State* R) {
    void* p = R->ator->allocateObject<Method>();
    Method* copy_method = new (p) Method(*this);

    copy_method->copied = true;
    return copy_method;
}

Method*
Method::enclose(State* R, Frame* closure) {
    Method* closure_method = copy(R);
    closure_method->closure = closure;
    return closure_method;
}

