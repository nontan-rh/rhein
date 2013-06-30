//
// vm.h
//

#ifndef VM_H
#define VM_H

#include <cstdio>

#include <initializer_list>

#include "allocator.h"
#include "object.h"

namespace rhein {

class ObjectSigniture {
public:
    enum {
        Class,
        Function,
    };
};

class LiteralSigniture {
public:
    enum {
        Int,
        Char,
        Symbol,
        String,
    };
};

class Module;

class State {
public:
    static void* operator new(size_t /* size */, void* p) { return p; }

    // Accessor for builtin classes
    Class* get_any_class() const { return any_class_; }
    Class* get_class_class() const { return class_class_; }
    Class* get_int_class() const { return int_class_; }
    Class* get_char_class() const { return char_class_; }
    Class* get_nil_class() const { return nil_class_; }
    Class* get_bool_class() const { return bool_class_; }
    Class* get_array_class() const { return array_class_; }
    Class* get_method_class() const { return method_class_; }
    Class* get_bytecode_function_class() const {
        return bytecode_function_class_;
    }
    Class* get_native_function_class() const {
        return native_function_class_;
    }
    Class* get_hashtable_class() const {
        return hashtable_class_;
    }
    Class* get_string_class() const { return string_class_; }
    Class* get_symbol_class() const { return symbol_class_; }

    Symbol* get_symbol(const char* cstr) const {
        return symbol_provider_->get_symbol(cstr);
    }

    Symbol* get_symbol(const char* cstr, size_t len) const {
        return symbol_provider_->get_symbol(cstr, len);
    }

    template <class T>
    void* allocate_object() {
        return allocator_->allocate_object<T>();
    }

    Value* allocate_raw_array(unsigned size) {
        return allocator_->allocate_raw_array(size);
    }

    template <class T>
    T* allocate_block(unsigned size) {
        return allocator_->allocate_block<T>(size);
    }

    void release_block(void* p) {
        allocator_->release_block(p);
    }

    template <class T>
    void* allocate_struct() {
        return allocator_->allocate_struct<T>();
    }

    void release_struct(void* p) {
        return allocator_->release_struct(p);
    }

    State();
    ~State();

    void initialize_class1();
    void initialize_class2();
    void initialize_class3();
    void initialize_symbol();

    // Installation
    bool add_native_function(const char* id, bool variadic,
            unsigned num_args, std::initializer_list<const char*> arg_class_ids,
            NativeFunctionBody body);
    bool add_function(Function* func); 
    bool add_function(Function* func, const Symbol* name); 
    bool add_variable(Symbol* id, Value val);
    Class* add_class(const char* name, const char* parent);
    bool add_class(Class* klass, const Symbol* name);
    bool add_class(Class* klass);

    void set_symbol_provider(SymbolProvider* s) { symbol_provider_ = s; } 
    bool has_symbol_provider() const { return (symbol_provider_ != nullptr); }

    bool get_class(Symbol* id, Value& klass) const {
        return klass_slots_->find(Value::by_object(id), klass);
    }

    Class* get_class(const char* id) const {
        Value klass;
        if (!klass_slots_->find(Value::by_object(get_symbol(id)), klass)) {
            throw "No such class";
        }
        return klass.get_obj<Class>();
    }

    // Bytecode level interface
    bool global_func_ref(Symbol* id, Value& func) const {
        if (!func_slots_->find(Value::by_object(id), func)) {
            id->dump();
            return false;
        }
        return true;
    }

    bool global_var_ref(Symbol* id, Value& value) const {
        return var_slots_->find(Value::by_object(id), value);
    }

    bool global_var_set(Symbol* id, Value value) {
        return var_slots_->assign(Value::by_object(id), value);
    }

    // File loading interface
    bool load_file(FILE* fp);
    
    // Module loader API
    bool load_module(Module* module);

    // Stack info
    Value* get_stack_begin() const { return stack_begin_; }
    Value* get_stack_end() const { return stack_end_; }

    // For debugging
    void dump_functions();
    void dump_classes();
    void dump_variables();

private:
    HashTable* func_slots_;
    HashTable* var_slots_;
    HashTable* klass_slots_;

    Class* any_class_;
    Class* class_class_;
    Class* int_class_;
    Class* char_class_;
    Class* nil_class_;
    Class* bool_class_;
    Class* array_class_;
    Class* method_class_;
    Class* bytecode_function_class_;
    Class* native_function_class_;
    Class* hashtable_class_;
    Class* string_class_;
    Class* symbol_class_;

    SymbolProvider* symbol_provider_;

    Allocator* allocator_;

    const unsigned kStackSize = 1 << 24;
    Value* stack_begin_;
    Value* stack_end_;

    bool read_object(FILE* fp);
    bool read_function(FILE* fp);
    bool read_class(FILE* fp);

    void set_class_hash(Class* klass);
};

class Module {
protected:
    virtual ~Module() = default;

public:
    virtual bool initialize(State* R) = 0;
};

// Frame must be a POD
struct Frame : public PlacementNewObj {
    Frame(State* R, Value* stack_ptr, BytecodeFunction* fn_, Frame* parent_,
            Frame* closure_, unsigned argc_, Value* args_,
            Value*& next_stack_ptr);

    BytecodeFunction* fn;
    Frame* closure;
    Frame* parent;
    Value* stack;
    unsigned argc;
    Value* args;
    Value* func_slots;
    Value* var_slots;
    const uint32_t* pc;
    Value* restore_stack_ptr;
    Value* sp;
    Value slots[1];

    static Frame* create(State* R, Value* stack_ptr, BytecodeFunction* fn,
            Frame* parent, Frame* closure, unsigned argc, Value* args,
            Value*& next_stack_ptr) {
        return new (stack_ptr) Frame(R, stack_ptr, fn, parent,
                closure, argc, args, next_stack_ptr);
    }

    // Variables
    bool local_func_ref(unsigned depth, unsigned offset, Value& value);
    bool local_func_set(unsigned depth, unsigned offset, Value value);
    bool local_var_ref(unsigned depth, unsigned offset, Value& value);
    bool local_var_set(unsigned depth, unsigned offset, Value value);
    bool local_arg_ref(unsigned depth, unsigned offset, Value& value);
    bool local_arg_set(unsigned depth, unsigned offset, Value value);
};

Value execute(State* R, Symbol* entry_point, unsigned argc, Value* argv);
Value execute(State* R, BytecodeFunction* bfn, unsigned argc, Value* args);

}

#endif // VM_H

