/*
 * internal.h
 */

#ifndef INTERNAL_H
#define INTERNAL_H

class State;
class Symbol;
class Klass;

class PlacementNewObj {
protected:
    static void* operator new (size_t /* size */, void* p) { return p; }
};

class FunctionInfo : public PlacementNewObj {
public:
	static FunctionInfo* create(State* R, Symbol* name);

	Symbol* name() const { return name_; }
	bool variadic() const { return variadic_; }
	unsigned num_args() const { return num_args_; }
	Symbol** arg_class_ids() const { return arg_class_ids_; }
	Klass** arg_classes() const { return arg_classes_; }

	bool resolve(State* R);
private:
	Symbol* name_;
	bool variadic_;
	unsigned num_args_;
	Symbol* arg_class_ids_;
	Klass** arg_classes_;

	FunctionInfo(Symbol* name, bool variadic, unsigned num_args,
			Symbol** arg_class_ids) : name_(name), variadic_(variadic),
			num_args_(num_args), arg_class_ids_(arg_class_ids),
			arg_classes_(nullptr) { }
};

#endif /* INTERNAL_H_ */
