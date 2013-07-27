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
FunctionInfo::create(Symbol* id) {
    return create(id, true, 0, nullptr, nullptr);
}

FunctionInfo*
FunctionInfo::create(Symbol* id, bool variadic, unsigned num_args,
        ArgDispatchKind* disp_kinds, Symbol** arg_class_ids) {
    void* p = get_current_state()->allocate_struct<FunctionInfo>();
    return new (p) FunctionInfo(id, variadic, num_args, disp_kinds, arg_class_ids);
}

FunctionInfo*
FunctionInfo::create(Symbol* id, bool variadic, unsigned num_args,
        std::initializer_list<const char*> ids) {
    State* R = get_current_state();
    Symbol** s = R->allocate_block<Symbol*>(ids.size());
    ArgDispatchKind* d = R->allocate_block<ArgDispatchKind>(ids.size());
    unsigned c = 0;
    for (auto i = ids.begin(); i != ids.end(); i++) {
        s[c] = R->get_symbol(*i);
        d[c] = ArgDispatchKind::Instance;
        c++;
    }
    return create(id, variadic, num_args, d, s);
}

FunctionInfo*
FunctionInfo::create(Symbol* id, bool variadic, unsigned num_args,
        std::initializer_list<ArgDispatchKind> kinds,
        std::initializer_list<const char*> ids) {
    if (ids.size() != kinds.size()) { throw ""; }

    State* R = get_current_state();
    Symbol** s = R->allocate_block<Symbol*>(ids.size());
    ArgDispatchKind* d = R->allocate_block<ArgDispatchKind>(ids.size());

    unsigned c = 0;
    auto i = ids.begin();
    auto k = kinds.begin();
    for (; c < ids.size() ; i++, k++, c++) {
        s[c] = R->get_symbol(*i);
        d[c] = *k;
    }
    return create(id, variadic, num_args, d, s);
}

bool
FunctionInfo::resolve() {
    State* R = get_current_state();
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
FunctionInfo::check_type(unsigned argc, Value* args) {
    State* R = get_current_state();
    if (!resolved_) { resolve(); }
    if (argc == num_args_ || (variadic_ && argc >= num_args_)) {
        for (unsigned i = 0; i < num_args_; i++) {
            switch (disp_kinds_[i]) {
                case ArgDispatchKind::Class:
                    if (!(args[i].get_class() == R->get_class_class()
                                && args[i].get_obj<Class>() == arg_classes_[i])) {
                        return false;
                    }
                    break;
                case ArgDispatchKind::Instance:
                    if (!args[i].get_class()->is_subclass_of(arg_classes_[i])) {
                        return false;
                    }
                    break;
            }
        }
        return true;
    }
    return false;
}

NativeFunction::NativeFunction(FunctionInfo* info, NativeFunctionBody body_)
    : Function(get_current_state()->get_native_function_class(), info), body_(body_), copied_(false) { }

NativeFunction*
NativeFunction::create(FunctionInfo* info, NativeFunctionBody body) {
    void* p = get_current_state()->allocate_object<NativeFunction>();
    return new (p) NativeFunction(info, body);
}

NativeFunction*
NativeFunction::copy() {
    void* p = get_current_state()->allocate_object<NativeFunction>();
    NativeFunction* copy_func = new (p) NativeFunction(*this);
    copy_func->copied_ = true;
    return copy_func;
}

NativeFunction*
NativeFunction::enclose(Closure* closure) {
    NativeFunction* closure_func = copy();
    closure_func->closure_ = closure;
    return closure_func;
}

BytecodeFunction::BytecodeFunction(FunctionInfo* info,
    unsigned func_slot_size, unsigned var_slot_size,
    unsigned stack_size, unsigned constant_table_size,
    Value* constant_table, unsigned bytecode_size, uint32_t* bytecode)
    : Function(get_current_state()->get_bytecode_function_class(), info),
      copied_(false),
      stack_size_(stack_size), func_slot_size_(func_slot_size),
      var_slot_size_(var_slot_size),
      constant_table_size_(constant_table_size),
      constant_table_(constant_table),
      bytecode_size_(bytecode_size),
      bytecode_(bytecode) { }

BytecodeFunction*
BytecodeFunction::create(FunctionInfo* info,
    unsigned func_slot_size, unsigned var_slot_size, unsigned stack_size,
    unsigned constant_table_size, Value* constant_table,
    unsigned bytecode_size, uint32_t* bytecode) {

    void* p = get_current_state()->allocate_object<BytecodeFunction>();
    return new (p) BytecodeFunction(info,
        func_slot_size, var_slot_size, stack_size, constant_table_size,
        constant_table, bytecode_size, bytecode);
}

BytecodeFunction*
BytecodeFunction::copy() {
    void* p = get_current_state()->allocate_object<BytecodeFunction>();
    BytecodeFunction* copy_func = new (p) BytecodeFunction(*this);
    copy_func->copied_ = true;
    return copy_func;
}

BytecodeFunction*
BytecodeFunction::enclose(Closure* closure) {
    BytecodeFunction* closure_func = copy();
    closure_func->closure_ = closure;
    return closure_func;
}

class DispatcherNode {
public:
    static DispatcherNode* create() {
        void* p = get_current_state()->allocate_struct<DispatcherNode>();
        return new (p) DispatcherNode();
    }

    bool dispatch(unsigned argc, Value* args, unsigned index,
            Value& func) {
        State* R = get_current_state();
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

        if (args[index].get_class() == R->get_class_class()) {
            Class* klass = args[index].get_obj<Class>();
            while (klass) {
                if (class_table_->exists(klass)) {
                    DispatcherNode* child = class_table_->find(klass);
                    if (child->dispatch(argc, args, index + 1, func)) {
                        return true;
                    }
                }
                klass = klass->get_parent();
            }
        }

        Class* klass = args[index].get_class();
        while (klass) {
            if (child_table_->exists(klass)) {
                DispatcherNode* child = child_table_->find(klass);
                if (child->dispatch(argc, args, index + 1, func)) {
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

    bool addFunction(Value func, Function* func_body,
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
                    child = create();
                    class_table_->insert_if_absent(klass, child);
                } else {
                    child = class_table_->find(klass);
                }
                break;
            case FunctionInfo::ArgDispatchKind::Instance:
                if (!child_table_->exists(klass)) {
                    child = create();
                    child_table_->insert_if_absent(klass, child);
                } else {
                    child = child_table_->find(klass);
                }
                break;
        }

        return child->addFunction(func, func_body, index + 1);
    }

private:
    Value entry_;
    Value variable_entry_;
    SysTable<const Class*, DispatcherNode*>* class_table_;
    SysTable<const Class*, DispatcherNode*>* child_table_;

    static void* operator new (size_t /* size */, void* p) { return p; }

    DispatcherNode()
        : entry_(Value::k_nil()), variable_entry_(Value::k_nil()),
          class_table_(SysTable<const Class*, DispatcherNode*>::create()),
          child_table_(SysTable<const Class*, DispatcherNode*>::create()) { }
};

Method::Method()
    : Object(get_current_state()->get_method_class()), copied_(false),
      node_(DispatcherNode::create()),
      closure_(nullptr) { }

Method*
Method::create() {
    void* p = get_current_state()->allocate_object<Method>();
    return new (p) Method();
}

bool
Method::dispatch(unsigned argc, Value* args, Value& result) {
    if (!node_->dispatch(argc, args, 0, result)) {
        return false;
    }
    return true;
}

bool
Method::add_function(Function* func) {
    if (!func->get_info()->is_resolved()) { func->get_info()->resolve(); }
    return node_->addFunction(Value::by_object(func), func, 0);
}

Method*
Method::copy() {
    void* p = get_current_state()->allocate_object<Method>();
    Method* copy_method = new (p) Method(*this);

    copy_method->copied_ = true;
    return copy_method;
}

Method*
Method::enclose(Closure* closure) {
    Method* closure_method = copy();
    closure_method->closure_ = closure;
    return closure_method;
}

}
