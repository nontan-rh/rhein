//
// vm.h
//

#ifndef VM_H
#define VM_H

#include <cstdio>

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

    Allocator* ator;
    SymbolProvider* s_prv;

    Class* any_class;
    Class* class_class;
    Class* int_class;
    Class* char_class;
    Class* nil_class;
    Class* bool_class;
    Class* array_class;
    Class* method_class;
    Class* bytecode_function_class;
    Class* native_function_class;
    Class* hashtable_class;
    Class* string_class;
    Class* symbol_class;

    State();
    ~State();

    void initialize_class1();
    void initialize_class2();
    void initialize_class3();
    void initialize_symbol();

    // Installation
    bool add_function(Function* func); 
    bool add_function(Function* func, const Symbol* name); 
    bool add_variable(Symbol* id, Value val);
    bool add_class(Class* klass, const Symbol* name);
    bool add_class(Class* klass);

    void set_symbol_provider(SymbolProvider* s) { s_prv = s; } 
    bool has_symbol_provider() const { return (s_prv != nullptr); }

    bool get_class(Symbol* id, Value& klass) const {
    	return klass_slots_->find(Value::by_object(id), klass);
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

    // For debugging
    void dump_functions();
    void dump_classes();
    void dump_variables();

private:
    HashTable* func_slots_;
    HashTable* var_slots_;
    HashTable* klass_slots_;

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

struct Frame : public PlacementNewObj {
    Frame(State* R, BytecodeFunction* fn_, Frame* parent_, Frame* closure_,
        unsigned argc_, Value* args_);

    BytecodeFunction* fn;
    Frame* closure;
    Frame* parent;
    Value* stack;
    unsigned argc;
    Value* args;
    Value* func_slots;
    Value* var_slots;
    const uint32_t* pc;
    Value* sp;

    static Frame* create(State* R, BytecodeFunction* fn, Frame* parent, Frame* closure,
        unsigned argc, Value* args) {
        void* p = R->ator->allocateStruct<Frame>();
        return new (p) Frame(R, fn, parent, closure, argc, args);
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

