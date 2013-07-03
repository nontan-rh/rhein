/*
 * internal.h
 */

#ifndef INTERNAL_H
#define INTERNAL_H

#include <initializer_list>

namespace rhein {

template <typename K, typename V> class SysTable;

class Value;
class Object;
class Symbol;
class String;
class Class;
struct Frame;
class Array;
class State;
class HashTable;
class Record;
class DispatcherNode;

class PlacementNewObj {
protected:
    static void* operator new (size_t /* size */, void* p) { return p; }
};

class FunctionInfo : public PlacementNewObj {
public:
    static FunctionInfo* create(State* R, Symbol* id);
    static FunctionInfo* create(State* R, Symbol* id, bool variadic,
            unsigned num_args, Symbol** arg_class_ids);
    static FunctionInfo* create(State* R, Symbol* id, bool variadic,
            unsigned num_args, std::initializer_list<const char*> arg_class_ids);

    Symbol* name() const { return name_; }
    bool variadic() const { return variadic_; }
    unsigned num_args() const { return num_args_; }
    Symbol** arg_class_ids() const { return arg_class_ids_; }
    Class** arg_classes() const { return arg_classes_; }

    bool is_resolved() const { return resolved_; }
    bool resolve(State* R);

    bool check_type(State* R, unsigned argc, Value* args);
private:
    Symbol* name_;
    bool variadic_;
    unsigned num_args_;
    Symbol** arg_class_ids_;
    Class** arg_classes_;
    bool resolved_;

    FunctionInfo(Symbol* name, bool variadic, unsigned num_args,
            Symbol** arg_class_ids) : name_(name), variadic_(variadic),
            num_args_(num_args), arg_class_ids_(arg_class_ids),
            arg_classes_(nullptr), resolved_(false) { }
};

class RecordInfo {
    unsigned num_slots_;
    SysTable<const Symbol*, unsigned>* id_index_table_;

    static void* operator new(size_t /* size */, void *p) { return p; }

    RecordInfo(State* R, RecordInfo* parent, unsigned slot_num_,
        Symbol** slot_ids);

public:
    static RecordInfo* create(State* R, RecordInfo* parent, unsigned slot_num,
        Symbol** slot_ids);

    unsigned num_slots() const { return num_slots_; }
    bool get_slot_index(Symbol* slot_id, unsigned& index) const;
};

inline unsigned long
calc_string_hash(const char* cstr, size_t length) {
    unsigned long hash_value = 0x1f2e3d4c;
    for (size_t i=0; i<length; i++) {
        // xor
        hash_value ^= cstr[i];
        // left rotate by 1
        unsigned long top = (hash_value >> (sizeof(unsigned long) * 8 - 1)) & 1;
        hash_value <<= 1;
        hash_value |= top;
    }
    return hash_value;
}

}

#endif /* INTERNAL_H_ */

