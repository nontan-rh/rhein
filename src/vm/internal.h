/*
 * internal.h
 */

#ifndef INTERNAL_H
#define INTERNAL_H

namespace rhein {

class Object;
class Symbol;
class String;
class Class;
struct Frame;
class Array;
class State;
class HashTable;
class Record;

class PlacementNewObj {
protected:
    static void* operator new (size_t /* size */, void* p) { return p; }
};

class FunctionInfo : public PlacementNewObj {
public:
	static FunctionInfo* create(State* R, Symbol* id);
	static FunctionInfo* create(State* R, Symbol* id, bool variadic,
			unsigned num_args, Symbol** arg_class_ids);

	Symbol* name() const { return name_; }
	bool variadic() const { return variadic_; }
	unsigned num_args() const { return num_args_; }
	Symbol** arg_class_ids() const { return arg_class_ids_; }
	Class** arg_classes() const { return arg_classes_; }

	bool resolve(State* R);
private:
	Symbol* name_;
	bool variadic_;
	unsigned num_args_;
	Symbol** arg_class_ids_;
	Class** arg_classes_;

	FunctionInfo(Symbol* name, bool variadic, unsigned num_args,
			Symbol** arg_class_ids) : name_(name), variadic_(variadic),
			num_args_(num_args), arg_class_ids_(arg_class_ids),
			arg_classes_(nullptr) { }
};

class RecordInfo {
    unsigned slot_num;
    HashTable* id_index_table;

    static void* operator new(size_t /* size */, void *p) { return p; }

    RecordInfo(State* R, RecordInfo* parent, unsigned slot_num_,
        Symbol** slot_ids);

public:
    static RecordInfo* create(State* R, RecordInfo* parent, unsigned slot_num,
        Symbol** slot_ids);

    unsigned num_slots() const { return slot_num; }
    bool get_slot_index(Symbol* slot_id, unsigned& index) const;
};

}

#endif /* INTERNAL_H_ */
