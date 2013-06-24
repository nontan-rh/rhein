//
// function.cc
//

#include "object.h"
#include "allocator.h"
#include "vm.h"

namespace rhein {

FunctionInfo*
FunctionInfo::create(State* R, Symbol* id) {
	return create(R, id, true, 0, nullptr);
}

FunctionInfo*
FunctionInfo::create(State* R, Symbol* id, bool variadic, unsigned num_args,
		Symbol** arg_class_ids) {
	void* p = R->allocate_struct<FunctionInfo>();
	return new (p) FunctionInfo(id, variadic, num_args, arg_class_ids);
}

bool
FunctionInfo::resolve(State* R) {
    arg_classes_ = R->allocate_block<Class*>(num_args_);

    for (unsigned i = 0; i < num_args_; i++) {
        Value klass;
        if (!R->get_class(arg_class_ids_[i], klass)) {
            return false;
        }

        arg_classes_[i] = klass.get_obj<Class>();
    }

    return true;
}

NativeFunction::NativeFunction(State* R, FunctionInfo* info, NativeFunctionBody body_)
    : Function(R->get_native_function_class(), info), body_(body_), copied_(false) { }

NativeFunction*
NativeFunction::create(State* R, FunctionInfo* info, NativeFunctionBody body) {
    void* p = R->allocate_object<NativeFunction>();
    return new (p) NativeFunction(R, info, body);
}

NativeFunction*
NativeFunction::copy(State* R) {
    void* p = R->allocate_object<NativeFunction>();
    NativeFunction* copy_func = new (p) NativeFunction(*this);
    copy_func->copied_ = true;
    return copy_func;
}

NativeFunction*
NativeFunction::enclose(State* R, Frame* closure) {
    NativeFunction* closure_func = copy(R);
    closure_func->closure_ = closure;
    return closure_func;
}

BytecodeFunction::BytecodeFunction(State* R, FunctionInfo* info,
    unsigned func_slot_size, unsigned var_slot_size,
    unsigned stack_size, unsigned constant_table_size,
    Value* constant_table, unsigned bytecode_size, uint32_t* bytecode)
    : Function(R->get_bytecode_function_class(), info),
      copied_(false),
      stack_size_(stack_size), func_slot_size_(func_slot_size),
      var_slot_size_(var_slot_size),
      constant_table_size_(constant_table_size),
      constant_table_(constant_table),
      bytecode_size_(bytecode_size),
      bytecode_(bytecode) { }

BytecodeFunction*
BytecodeFunction::create(State* R, FunctionInfo* info,
    unsigned func_slot_size, unsigned var_slot_size, unsigned stack_size,
    unsigned constant_table_size, Value* constant_table,
    unsigned bytecode_size, uint32_t* bytecode) {

    void* p = R->allocate_object<BytecodeFunction>();
    return new (p) BytecodeFunction(R, info,
        func_slot_size, var_slot_size, stack_size, constant_table_size,
        constant_table, bytecode_size, bytecode);
}

BytecodeFunction*
BytecodeFunction::copy(State* R) {
    void* p = R->allocate_object<BytecodeFunction>();
    BytecodeFunction* copy_func = new (p) BytecodeFunction(*this);
    copy_func->copied_ = true;
    return copy_func;
}

BytecodeFunction*
BytecodeFunction::enclose(State* R, Frame* closure) {
    BytecodeFunction* closure_func = copy(R);
    closure_func->closure_ = closure;
    return closure_func;
}

class DispatcherNode {
public:
    static DispatcherNode* create(State* R) {
        void* p = R->allocate_struct<DispatcherNode>();
        return new (p) DispatcherNode(R);
    }

    bool dispatch(State* R, unsigned argc, Value* args, unsigned index,
    		Value& func) {
        if (argc == index) {
            if (!entry_.is(Value::Type::Nil)) {
                func = entry_;
                return true;
            } else if (!variable_entry_.is(Value::Type::Nil)) {
                func = variable_entry_;
                return true;
            } else {
                return false;
            }
        }

        Class* klass = args[index].get_class(R);
        while (true) {
            Value child;
            if (child_table_->find(Value::by_object(klass), child)) {
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
        if (!variable_entry_.is(Value::Type::Nil)) {
            func = variable_entry_;
            return true;
        }
        return false;
    }

    bool addFunction(State* R, Value func, Function* func_body,
    		unsigned index) {
        if (func_body->get_info()->num_args() == index) {
            if (func_body->get_info()->variadic()) {
                if (!variable_entry_.is(Value::Type::Nil)) {
                    return false;
                }
                variable_entry_ = func;
                return true;
            } else {
                if (!entry_.is(Value::Type::Nil)) {
                    return false;
                }
                entry_ = func;
                return true;
            }
        }
        Class* klass = func_body->get_info()->arg_classes()[index];
        Value child;
        if (!child_table_->find(Value::by_object(klass), child)) {
            // Unsafe cast
            child = Value::by_object((Object*)create(R));
            child_table_->insert_if_absent(R, Value::by_object(klass), child);
        }
        // Unsafe cast
        return child.get_obj<DispatcherNode>()->addFunction(R, func,
        		func_body, index + 1);
    }

private:
    Value entry_;
    Value variable_entry_;
    HashTable* child_table_;

    static void* operator new (size_t /* size */, void* p) { return p; }

    DispatcherNode(State* R)
        : entry_(Value::k_nil()), variable_entry_(Value::k_nil()),
          child_table_(HashTable::create(R)) { }
};

Method::Method(State* R)
    : Object(R->get_method_class()), copied_(false),
      node_(DispatcherNode::create(R)),
      closure_(nullptr) { }

Method*
Method::create(State* R) {
    void* p = R->allocate_object<Method>();
    return new (p) Method(R);
}

bool
Method::dispatch(State* R, unsigned argc, Value* args, Value& result) {
    if (!node_->dispatch(R, argc, args, 0, result)) {
        return false;
    }
    return true;
}

bool
Method::add_function(State* R, Function* func) {
    return node_->addFunction(R, Value::by_object(func), func, 0);
}

Method*
Method::copy(State* R) {
    void* p = R->allocate_object<Method>();
    Method* copy_method = new (p) Method(*this);

    copy_method->copied_ = true;
    return copy_method;
}

Method*
Method::enclose(State* R, Frame* closure) {
    Method* closure_method = copy(R);
    closure_method->closure_ = closure;
    return closure_method;
}

}
