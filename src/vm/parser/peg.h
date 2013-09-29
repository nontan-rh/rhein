//
// peg.h
//

#ifndef PEG_H
#define PEG_H

#include "object.h"
#include "vm.h"
#include "basic/port.h"

namespace rhein {
namespace peg {

using namespace rhein::port;

class PegModule : public Module, public PlacementNewObj {
public:
    static PegModule* create();
    bool initialize();
};

class PegSyntax : public Object {
public:
    virtual bool parse(List* src, Value& ctx, Value& obj, List*& next) = 0;

protected:
    PegSyntax(Class* klass) : Object(klass) { }
};

class PegString : public PegSyntax {
public:
    static PegString* create(String* str) {
        return new (get_current_state()->allocate_object<PegString>()) PegString(str);
    }
    bool parse(List* src, Value& ctx, Value& obj, List*& next);

private:
    String* str_;

    PegString(String* str)
        : PegSyntax(get_current_state()->get_class("PegString")),
          str_(str) { }
};

class PegCharClass : public PegSyntax {
public:
    static PegCharClass* create() {
        return new (get_current_state()->allocate_object<PegCharClass>()) PegCharClass();
    }


    void add(char begin, char end);
    void invert();

    bool parse(List* src, Value& ctx, Value& obj, List*& next);

private:
    static const size_t kCharKinds = 256;
    static const size_t kIntBits = 32;
    static const size_t kTableSize = kCharKinds / kIntBits;
    uint32_t table_[kTableSize];
    bool inverted_;

    PegCharClass()
        : PegSyntax(get_current_state()->get_class("PegCharClass")), inverted_(false) {
        for (unsigned i = 0; i < kTableSize; i++) { table_[i] = 0; }
    }
};

class PegTimes : public PegSyntax {
public:
    static PegTimes* create(Int lower, Int upper, PegSyntax* syn) {
        if (!(lower <= upper || upper == -1)) {
            return nullptr;
        }
        return new (get_current_state()->allocate_object<PegTimes>()) PegTimes(lower, upper, syn);
    }

    bool parse(List* src, Value& ctx, Value& obj, List*& next);

private:
    Int lower_;
    Int upper_;
    PegSyntax* syn_;

    PegTimes(Int lower, Int upper, PegSyntax* syn)
        : PegSyntax(get_current_state()->get_class("PegTimes")),
          lower_(lower), upper_(upper), syn_(syn) { }
};

class PegPred : public PegSyntax {
public:
    static PegPred* create(bool if_success, PegSyntax* syn) {
        return new (get_current_state()->allocate_object<PegPred>()) PegPred(if_success, syn);
    }

    bool parse(List* src, Value& ctx, Value& obj, List*& next);

private:
    bool if_success_;
    PegSyntax* syn_;

    PegPred(bool if_success, PegSyntax* syn)
        : PegSyntax(get_current_state()->get_class("PegPred")),
          if_success_(if_success), syn_(syn) { }
};

class PegSequence : public PegSyntax {
public:
    static PegSequence* create(Int num, PegSyntax** syns) {
        return new (get_current_state()->allocate_object<PegSequence>()) PegSequence(num, syns);
    }

    bool parse(List* src, Value& ctx, Value& obj, List*& next);

private:
    Int num_;
    PegSyntax** syns_;

    PegSequence(Int num, PegSyntax** syns)
        : PegSyntax(get_current_state()->get_class("PegSequence")),
          num_(num), syns_(syns) { }
};

class PegChoice : public PegSyntax {
public:
    static PegChoice* create(Int num, PegSyntax** syns) {
        return new (get_current_state()->allocate_object<PegChoice>()) PegChoice(num, syns);
    }

    bool parse(List* src, Value& ctx, Value& obj, List*& next);

private:
    Int num_;
    PegSyntax** syns_;

    PegChoice(Int num, PegSyntax** syns)
        : PegSyntax(get_current_state()->get_class("PegChoice")),
          num_(num), syns_(syns) { }
};

class PegAction : public PegSyntax {
public:
    static PegAction* create(Value action, PegSyntax* syn) {
        return new (get_current_state()->allocate_object<PegAction>()) PegAction(action, syn);
    }

    bool parse(List* src, Value& ctx, Value& obj, List*& next);

private:
    Value action_;
    PegSyntax* syn_;

    PegAction(Value action, PegSyntax* syn)
        : PegSyntax(get_current_state()->get_class("PegAction")),
          action_(action), syn_(syn) { }
};

class PegAny : public PegSyntax {
public:
    static PegAny* create() {
        return new (get_current_state()->allocate_object<PegAny>()) PegAny();
    }

    bool parse(List* src, Value& ctx, Value& obj, List*& next);

private:
    PegAny()
        : PegSyntax(get_current_state()->get_class("PegAny")) { }
};

class PegDynamic : public PegSyntax {
public:
    static PegDynamic* create(Value fn) {
        return new (get_current_state()->allocate_object<PegDynamic>())
            PegDynamic(fn);
    }

    bool parse(List* src, Value& ctx, Value& obj, List*& next);

private:
    Value fn_;

    PegDynamic(Value fn)
        : PegSyntax(get_current_state()->get_class("PegDynamic")),
          fn_(fn) { }
};

class PegTry : public PegSyntax {
public:
    static PegTry* create(PegSyntax* syn) {
        return new (get_current_state()->allocate_object<PegTry>()) PegTry(syn);
    }

    bool parse(List* src, Value& ctx, Value& obj, List*& next);

private:
    PegSyntax* syn_;

    PegTry(PegSyntax* syn)
        : PegSyntax(get_current_state()->get_class("PegTry")),
          syn_(syn) { }
};

class PegDrop : public PegSyntax {
public:
    static PegDrop* create(PegSyntax* syn, int num) {
        return new (get_current_state()->allocate_object<PegDrop>()) PegDrop(syn, num);
    }

    bool parse(List* src, Value& ctx, Value& obj, List*& next);

private:
    PegSyntax* syn_;
    int num_;

    PegDrop(PegSyntax* syn, int num)
        : PegSyntax(get_current_state()->get_class("PegDrop")),
          syn_(syn), num_(num) { }
};

}
}

#endif // PEG_H

