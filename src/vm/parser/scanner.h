//
// scanner.h
//

#ifndef SCANNER_H
#define SCANNER_H

#include "vm.h"
#include "internal.h"
#include "object.h"
#include "basic/port.h"

namespace rhein {

using namespace rhein::port;

class Token : public Object {
public:
    enum class Kind {
        IntLiteral,
        StrLiteral,
        Id,
        Operator,
        LParen,
        RParen,
        LBrace,
        RBrace,
        LBracket,
        RBracket,
        Colon,
        DQuote,
        Question,
        Hat,
        Dot,
        Comma,
        While,
        If,
        Elif,
        Else,
        And,
        Or,
        Break,
        True,
        False,
        Nil,
        Local,
        Def,
        Class,
        Global,
    };

    static Token* int_literal(State* R, Int i);
    static Token* id(State* R, const char* ch);
    static Token* str_literal(State* R, const char* ch);
    static Token* create(State* R, Kind k);
private:
};

class Scanner : public PlacementNewObj {
public:
    static Scanner* create(State* R, Port* p) {
        return new (R->allocate_struct<Scanner>()) Scanner(R, p);
    }

    int get_token();
    bool eof() const { return p_->eof() && re2c_cursor == re2c_limit; };
private:
    static const size_t kBufferSize = 1024;

    bool fill(size_t additional_required_size);

    const char* token() const { return re2c_token; }
    size_t length() const { return re2c_cursor - re2c_token; }

    Scanner() { }
    Scanner(State* R, Port* p) : R_(R), p_(p), re2c_buffer_size(kBufferSize) {
        re2c_buffer = new char[kBufferSize];
        re2c_cursor =
            re2c_limit =
            re2c_token =
            re2c_marker =
            re2c_ctx_marker = re2c_buffer;
    }

    ~Scanner();

    State* R_;
    Port* p_;

    char* re2c_buffer;
    size_t re2c_buffer_size;
    char* re2c_cursor;
    char* re2c_limit;
    char* re2c_token;
    char* re2c_marker;
    char* re2c_ctx_marker;
};

}

#endif // SCANNER_H

