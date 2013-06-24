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
    HashTable* func_slots;
    HashTable* var_slots;
    HashTable* klass_slots;

    bool readObject(FILE* fp);
    bool readFunction(FILE* fp);
    bool readKlass(FILE* fp);

public:
    static void* operator new(size_t /* size */, void* p) { return p; }

    Allocator* ator;
    SymbolProvider* s_prv;

    Klass* any_class;
    Klass* class_class;
    Klass* int_class;
    Klass* char_class;
    Klass* nil_class;
    Klass* bool_class;
    Klass* array_class;
    Klass* method_class;
    Klass* bytecode_function_class;
    Klass* native_function_class;
    Klass* hashtable_class;
    Klass* string_class;
    Klass* symbol_class;

    State();
    ~State();

    void initializeKlass1();
    void initializeKlass2();
    void initializeKlass3();
    void initializeSymbol();

    // Installation
    bool addFunction(Function* func); 
    bool addFunction(Function* func, const Symbol* name); 
    bool addVariable(Symbol* id, Value val);
    bool addKlass(Klass* klass, const Symbol* name);
    bool addKlass(Klass* klass);

    void setSymbolProvider(SymbolProvider* s) { s_prv = s; } 
    bool hasSymbolProvider() const { return (s_prv != nullptr); }

    bool getKlass(Symbol* id, Value& klass) { return klass_slots->find(Value::by_object(id), klass); }

    // Bytecode level interface
    bool gfref(Symbol* id, Value& func) {
        if (!func_slots->find(Value::by_object(id), func)) {
            id->dump();
            return false;
        }
        return true;
    }

    bool gvref(Symbol* id, Value& value) {
    	return var_slots->find(Value::by_object(id), value);
    }

    bool gvset(Symbol* id, Value value) {
    	return var_slots->assign(Value::by_object(id), value);
    }

    // File loading interface
    bool loadFile(FILE* fp);
    
    // Module loader API
    bool loadModule(Module* module);

    // For debugging
    void dumpFunctions();
    void dumpClasses();
    void dumpVariables();
private:
    void set_class_hash(Klass* klass);
};

class Module {
protected:
    virtual ~Module() = default;

public:
    virtual bool initialize(State* R) = 0;
};

struct Frame {
private:
    Frame(State* R, BytecodeFunction* fn_, Frame* parent_, Frame* closure_,
        unsigned argc_, Value* args_);

    static void* operator new (size_t /* size */, void* p) { return p; }
public:
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
    bool lfref(unsigned depth, unsigned offset, Value& value);
    bool lfset(unsigned depth, unsigned offset, Value value);
    bool lvref(unsigned depth, unsigned offset, Value& value);
    bool lvset(unsigned depth, unsigned offset, Value value);
    bool laref(unsigned depth, unsigned offset, Value& value);
    bool laset(unsigned depth, unsigned offset, Value value);
};

Value execute(State* R, Symbol* entry_point, unsigned argc, Value* argv);
Value execute(State* R, BytecodeFunction* bfn, unsigned argc, Value* args);

}

#endif // VM_H

