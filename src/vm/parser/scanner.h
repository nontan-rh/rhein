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
        CharLiteral,
        Id,
        Operator,
        LParen,
        RParen,
        LBrace,
        RBrace,
        LBracket,
        RBracket,
        Colon,
        Hat,
        Tilde,
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
        MaybeTerm,
        Term,
        Eof,
    };

    static Token* int_literal(Int i) {
        Token* t = create(R, Kind::IntLiteral);
        t->u_.v_int_ = i;
        return t;
    }

    static Token* id(const char* ch, size_t length) {
        Token* t = create(R, Kind::Id);
        t->u_.v_sym_ = R->get_symbol(ch, length);
        return t;
    }

    static Token* str_literal(const char* ch, size_t length) {
        Token* t = create(R, Kind::StrLiteral);
        t->u_.v_str_ = String::create(R, ch, length);
        return t;
    }

    static Token* char_literal(const char* ch, size_t length) {
        Token* t = create(R, Kind::CharLiteral);
        t->u_.v_sym_ = R->get_symbol(ch, length);
        return t;
    }

    static Token* op(const char* ch, size_t length) {
        Token* t = create(R, Kind::Operator);
        t->u_.v_sym_ = R->get_symbol(ch, length);
        return t;
    }

    static Token* space(int i) {
        if (i == 1) {
            return create(R, Kind::MaybeTerm);
        } else if (i == 2) {
            return create(R, Kind::Term);
        }
        throw "";
    }

    static Token* create(Kind k) {
        return new (R->allocate_object<Token>()) Token(R, k);
    }

    Kind get_kind() const { return kind_; }
private:
    Token(Kind k) : Object(R->get_class("Token")), kind_(k) { }

    Kind kind_;
    union {
        Int v_int_;
        Symbol* v_sym_;
        String* v_str_;
    } u_;
};

class Scanner : public PlacementNewObj {
public:
    static Scanner* create(Port* p) {
        return new (R->allocate_struct<Scanner>()) Scanner(R, p);
    }

    Token* get_token();
    bool eof() const { return p_->eof() && re2c_cursor >= re2c_limit; };
private:
    static const size_t kBufferSize = 1024;

    bool fill(size_t additional_required_size);

    const char* token() const { return re2c_token; }
    size_t length() const { return re2c_cursor - re2c_token; }

    Scanner() { }
    Scanner(Port* p) : R_(R), p_(p), re2c_buffer_size(kBufferSize) {
        re2c_buffer = new char[kBufferSize];
        re2c_cursor =
            re2c_limit =
            re2c_token =
            re2c_marker =
            re2c_ctx_marker = re2c_buffer;
    }

    ~Scanner();

    _;
    Port* p_;

    char* re2c_buffer;
    size_t re2c_buffer_size;
    char* re2c_cursor;
    char* re2c_limit;
    char* re2c_token;
    char* re2c_marker;
    char* re2c_ctx_marker;
};

static inline
Int str2int(const char *p, size_t length) {
    unsigned value = 0;
    for (unsigned i = 0; i < length; i++) {
        value += p[i] - '0';
        value *= 10;
    }
    return value;
}

class ScannerModule : public Module, public PlacementNewObj {
public:
    static ScannerModule* create();
    bool initialize();
};

static inline const char*
to_str(Token::Kind kind) {
    switch (kind) {
        case Token::Kind::IntLiteral: // 0
            return "IntLiteral";
        case Token::Kind::StrLiteral:
            return "StrLiteral";
        case Token::Kind::CharLiteral:
            return "CharLiteral";
        case Token::Kind::Id:
            return "Id";
        case Token::Kind::Operator:
            return "Operator";
        case Token::Kind::LParen:
            return "LParen";
        case Token::Kind::RParen:
            return "RParen";
        case Token::Kind::LBrace:
            return "LBrace";
        case Token::Kind::RBrace:
            return "RBrace";
        case Token::Kind::LBracket:
            return "LBracket";
        case Token::Kind::RBracket:
            return "RBracket";
        case Token::Kind::Colon:      // 10
            return "Colon";
        case Token::Kind::Hat:
            return "Hat";
        case Token::Kind::Tilde:
            return "Tilde";
        case Token::Kind::Dot:
            return "Dot";
        case Token::Kind::Comma:
            return "Comma";
        case Token::Kind::While:
            return "While";
        case Token::Kind::If:
            return "If";
        case Token::Kind::Elif:
            return "Elif";
        case Token::Kind::Else:       // 20
            return "Else";
        case Token::Kind::And:
            return "And";
        case Token::Kind::Or:
            return "Or";
        case Token::Kind::Break:
            return "Break";
        case Token::Kind::True:
            return "True";
        case Token::Kind::False:
            return "False";
        case Token::Kind::Nil:
            return "Nil";
        case Token::Kind::Local:
            return "Local";
        case Token::Kind::Def:
            return "Def";
        case Token::Kind::Class:
            return "Class";
        case Token::Kind::Global:     // 30
            return "Global";
        case Token::Kind::MaybeTerm:
            return "MaybeTerm";
        case Token::Kind::Term:
            return "Term";
        case Token::Kind::Eof:
            return "Eof";
        default:
            return "Unknown";
    }
}

}

#endif // SCANNER_H

