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
    static PegModule* create(State* R);
    bool initialize(State* R);
};

class PegSyntax : public Object {
public:
    virtual bool parse(List* src, Value& obj, List*& next) = 0;

protected:
    PegSyntax(Class* klass) : Object(klass) { }
};

class PegString : public PegSyntax {
public:
    static PegString* create(State* R, String* str) {
        return new (R->allocate_object<PegString>()) PegString(R, str);
    }
    bool parse(List* src, Value& obj, List*& next);

private:
    String* str_;

    PegString(State* R, String* str)
        : PegSyntax(R->get_class("PegString")),
          str_(str) { }
};

class PegCharClass : public PegSyntax {
public:
    static PegCharClass* create(State* R) {
        return new (R->allocate_object<PegCharClass>()) PegCharClass(R);
    }


    void add(char begin, char end);
    void invert();

    bool parse(List* src, Value& obj, List*& next);

private:
    static const size_t kCharKinds = 256;
    static const size_t kIntBits = 32;
    static const size_t kTableSize = kCharKinds / kIntBits;
    uint32_t table_[kTableSize];
    bool inverted_;

    PegCharClass(State* R)
        : PegSyntax(R->get_class("PegCharClass")), inverted_(false) {
        for (unsigned i = 0; i < kTableSize; i++) { table_[i] = 0; }
    }
};

class PegTimes : public PegSyntax {
public:
    static PegTimes* create(State* R, Int lower, Int upper, PegSyntax* syn) {
        if (!(lower <= upper || upper == -1)) {
            return nullptr;
        }
        return new (R->allocate_object<PegTimes>()) PegTimes(R, lower, upper, syn);
    }

    bool parse(List* src, Value& obj, List*& next);

private:
    Int lower_;
    Int upper_;
    PegSyntax* syn_;

    PegTimes(State*R , Int lower, Int upper, PegSyntax* syn)
        : PegSyntax(R->get_class("PegTimes")),
          lower_(lower), upper_(upper), syn_(syn) { }
};

class PegPred : public PegSyntax {
public:
    static PegPred* create(State* R, bool if_success, PegSyntax* syn) {
        return new (R->allocate_object<PegPred>()) PegPred(R, if_success, syn);
    }

    bool parse(List* src, Value& obj, List*& next);

private:
    bool if_success_;
    PegSyntax* syn_;

    PegPred(State* R, bool if_success, PegSyntax* syn)
        : PegSyntax(R->get_class("PegPred")),
          if_success_(if_success), syn_(syn) { }
};

class PegSequence : public PegSyntax {
public:
    static PegSequence* create(State* R, Int num, PegSyntax** syns) {
        return new (R->allocate_object<PegSequence>()) PegSequence(R, num, syns);
    }

    bool parse(List* src, Value& obj, List*& next);

private:
    Int num_;
    PegSyntax** syns_;

    PegSequence(State* R, Int num, PegSyntax** syns)
        : PegSyntax(R->get_class("PegSequence")),
          num_(num), syns_(syns) { }
};

class PegChoice : public PegSyntax {
public:
    static PegChoice* create(State* R, Int num, PegSyntax** syns) {
        return new (R->allocate_object<PegChoice>()) PegChoice(R, num, syns);
    }

    bool parse(List* src, Value& obj, List*& next);

private:
    Int num_;
    PegSyntax** syns_;

    PegChoice(State* R, Int num, PegSyntax** syns)
        : PegSyntax(R->get_class("PegChoice")),
          num_(num), syns_(syns) { }
};

class PegAction : public PegSyntax {
public:
    static PegAction* create(State* R, Value action, PegSyntax* syn) {
        return new (R->allocate_object<PegAction>()) PegAction(R, action, syn);
    }

    bool parse(List* src, Value& obj, List*& next);

private:
    Value action_;
    PegSyntax* syn_;

    PegAction(State* R, Value action, PegSyntax* syn)
        : PegSyntax(R->get_class("PegAction")),
          action_(action), syn_(syn) { }
};

class PegAny : public PegSyntax {
public:
    static PegAny* create(State* R) {
        return new (R->allocate_object<PegAny>()) PegAny(R);
    }

    bool parse(List* src, Value& obj, List*& next);

private:
    PegAny(State* R)
        : PegSyntax(R->get_class("PegAny")) { }
};

class PegTry : public PegSyntax {
public:
    static PegTry* create(State* R, PegSyntax* syn) {
        return new (R->allocate_object<PegTry>()) PegTry(R, syn);
    }

    bool parse(List* src, Value& obj, List*& next);

private:
    PegSyntax* syn_;

    PegTry(State* R, PegSyntax* syn)
        : PegSyntax(R->get_class("PegTry")),
          syn_(syn) { }
};

}
}

#endif // PEG_H

