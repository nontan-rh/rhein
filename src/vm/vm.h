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
    bool readClass(FILE* fp);

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

    void initializeClass1();
    void initializeClass2();
    void initializeClass3();
    void initializeSymbol();

    // Installation
    bool addFunction(Function* func); 
    bool addFunction(Function* func, const Symbol* name); 
    bool addVariable(Symbol* id, Value val);
    bool addClass(Class* klass, const Symbol* name);
    bool addClass(Class* klass);

    void setSymbolProvider(SymbolProvider* s) { s_prv = s; } 
    bool hasSymbolProvider() const { return (s_prv != nullptr); }

    bool getClass(Symbol* id, Value& klass) { return klass_slots->find(Value::by_object(id), klass); }

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
    void set_class_hash(Class* klass);
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

