//
// function.cc
//

#include <initializer_list>
#include <cstring>

#include "systable.h"
#include "object.h"
#include "allocator.h"
#include "vm.h"

namespace rhein {

FunctionInfo*
FunctionInfo::create(State* R, Symbol* id) {
    return create(R, id, true, 0, nullptr, nullptr);
}

FunctionInfo*
FunctionInfo::create(State* R, Symbol* id, bool variadic, unsigned num_args,
        ArgDispatchKind* disp_kinds, Symbol** arg_class_ids) {
    void* p = R->allocate_struct<FunctionInfo>();
    return new (p) FunctionInfo(id, variadic, num_args, disp_kinds, arg_class_ids);
}

FunctionInfo*
FunctionInfo::create(State* R, Symbol* id, bool variadic, unsigned num_args,
        std::initializer_list<const char*> ids) {
    Symbol** s = R->allocate_block<Symbol*>(ids.size());
    ArgDispatchKind* d = R->allocate_block<ArgDispatchKind>(ids.size());
    unsigned c = 0;
    for (auto i = ids.begin(); i != ids.end(); i++) {
        s[c] = R->get_symbol(*i);
        d[c] = ArgDispatchKind::Instance;
        c++;
    }
    return create(R, id, variadic, num_args, d, s);
}

FunctionInfo*
FunctionInfo::create(State* R, Symbol* id, bool variadic, unsigned num_args,
        std::initializer_list<ArgDispatchKind> kinds,
        std::initializer_list<const char*> ids) {
    if (ids.size() != kinds.size()) { throw ""; }

    Symbol** s = R->allocate_block<Symbol*>(ids.size());
    ArgDispatchKind* d = R->allocate_block<ArgDispatchKind>(ids.size());

    unsigned c = 0;
    auto i = ids.begin();
    auto k = kinds.begin();
    for (; c < ids.size() ; i++, k++, c++) {
        s[c] = R->get_symbol(*i);
        d[c] = *k;
    }
    return create(R, id, variadic, num_args, d, s);
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

    resolved_ = true;
    return true;
}

bool
FunctionInfo::check_type(State* R, unsigned argc, Value* args) {
    if (!resolved_) { resolve(R); }
    if (argc == num_args_ || (variadic_ && argc >= num_args_)) {
        for (unsigned i = 0; i < num_args_; i++) {
            switch (disp_kinds_[i]) {
                case ArgDispatchKind::Class:
                    if (!(args[i].get_class(R) == R->get_class_class()
                                && args[i].get_obj<Class>() == arg_classes_[i])) {
                        return false;
                    }
                    break;
                case ArgDispatchKind::Instance:
                    if (!args[i].get_class(R)->is_subclass_of(arg_classes_[i])) {
                        return false;
                    }
                    break;
            }
        }
        return true;
    }
    return false;
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

        if (args[index].get_class(R) == R->get_class_class()) {
            Class *klass = args[index].get_obj<Class>();
            while (klass) {
                if (class_table_->exists(klass)) {
                    DispatcherNode* child = class_table_->find(klass);
                    if (child->dispatch(R, argc, args, index + 1, func)) {
                        return true;
                    }
                }
                klass = klass->get_parent();
            }
        }

        Class* klass = args[index].get_class(R);
        while (klass) {
            if (child_table_->exists(klass)) {
                DispatcherNode* child = child_table_->find(klass);
                if (child->dispatch(R, argc, args, index + 1, func)) {
                    return true;
                }
            }
            klass = klass->get_parent();
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
        DispatcherNode* child;

        switch (func_body->get_info()->disp_kinds()[index]) {
            case FunctionInfo::ArgDispatchKind::Class:
                if (!class_table_->exists(klass)) {
                    child = create(R);
                    class_table_->insert_if_absent(R, klass, child);
                } else {
                    child = class_table_->find(klass);
                }
            case FunctionInfo::ArgDispatchKind::Instance:
                if (!child_table_->exists(klass)) {
                    child = create(R);
                    child_table_->insert_if_absent(R, klass, child);
                } else {
                    child = child_table_->find(klass);
                }
                break;
        }

        return child->addFunction(R, func, func_body, index + 1);
    }

private:
    Value entry_;
    Value variable_entry_;
    SysTable<const Class*, DispatcherNode*>* class_table_;
    SysTable<const Class*, DispatcherNode*>* child_table_;

    static void* operator new (size_t /* size */, void* p) { return p; }

    DispatcherNode(State* R)
        : entry_(Value::k_nil()), variable_entry_(Value::k_nil()),
          class_table_(SysTable<const Class*, DispatcherNode*>::create(R)),
          child_table_(SysTable<const Class*, DispatcherNode*>::create(R)) { }
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
    if (!func->get_info()->is_resolved()) { func->get_info()->resolve(R); }
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
