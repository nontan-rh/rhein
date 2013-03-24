//
// vm.h
//

#ifndef VM_H
#define VM_H

#include <cstdio>

#include "allocator.h"
#include "object/function.h"
#include "object/imstring.h"
#include "object/object.h"
#include "object/hashtable.h"

namespace rhein {

class ObjectSigniture {
public:
    enum {
        Class,
        Function,
        Variable,
    };
};

class LiteralSigniture {
public:
    enum {
        Int,
        Char,
        String,
    };
};

class Module;

class State {
    HashTable* func_slots;
    HashTable* var_slots;
    HashTable* klass_slots;

    State(const State& /* rht */) = delete;
    State& operator=(const State& /* rht */) = delete;

    bool readObject(FILE* fp);
    bool readVariable(FILE* fp);
    bool readFunction(FILE* fp);
    bool readKlass(FILE* fp);

public:
    static void* operator new(size_t /* size */, void* p) { return p; }

    Allocator* ator;
    StringProvider* string_provider;

    Klass* any_klass;
    Klass* int_klass;
    Klass* char_klass;
    Klass* null_klass;
    Klass* bool_klass;
    Klass* array_klass;
    Klass* method_klass;
    Klass* bytecode_function_klass;
    Klass* native_function_klass;
    Klass* hashtable_klass;
    Klass* string_klass;

    State();
    ~State();

    void initializeKlass1();
    void initializeKlass2();
    void initializeKlass3();
    void initializeString();

    // Installation
    bool addFunction(Function* func); 
    bool addVariable(String* id);
    bool addKlass(Klass* klass);

    void setStringProvider(StringProvider* s) { string_provider = s; } 
    bool hasStringProvider() const { return (string_provider != nullptr); }

    bool getKlass(String* id, Value& klass) { return klass_slots->find(make_value(id), klass); }

    // Bytecode level interface
    bool gfref(String* id, Value& func) {
        if (!func_slots->find(make_value(id), func)) {
            id->dump();
            return false;
        }
        return true;
    }

    bool gvref(String* id, Value& value) { return var_slots->find(make_value(id), value); }
    bool gvset(String* id, Value value) { return var_slots->assign(make_value(id), value); }

    // File loading interface
    bool loadFile(FILE* fp);
    
    // Module loader API
    bool loadModule(Module* module);
};

class Module {
protected:
    Module() = default;
public:
    virtual bool initialize(State* state) = 0;
};

struct Frame {
private:
    Frame() = delete;
    Frame(const Frame& /* rht */) = delete;
    Frame& operator=(const Frame& /* rht */) = delete;

    Frame(State* state, BytecodeFunction* fn_, Frame* parent_, Frame* closure_,
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

    static Frame* create(State* state, BytecodeFunction* fn, Frame* parent, Frame* closure,
        unsigned argc, Value* args) {
        void* p = state->ator->allocateStruct<Frame>();
        return new (p) Frame(state, fn, parent, closure, argc, args);
    }

    // Variables
    bool lfref(unsigned depth, unsigned offset, Value& value);
    bool lfset(unsigned depth, unsigned offset, Value value);
    bool lvref(unsigned depth, unsigned offset, Value& value);
    bool lvset(unsigned depth, unsigned offset, Value value);
    bool laref(unsigned depth, unsigned offset, Value& value);
    bool laset(unsigned depth, unsigned offset, Value value);
};

Value execute(State* state, String* entry_point, unsigned argc, Value* argv);
Value execute(State* state, BytecodeFunction* bfn, unsigned argc, Value* args);

}

#endif // VM_H

