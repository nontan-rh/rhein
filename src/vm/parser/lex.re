
#include <algorithm>

#include "object.h"
#include "basic/port.h"
#include "parser/scanner.h"

namespace rhein {

Token*
Scanner::get_token() {
re2c_token = re2c_cursor;

int space_kind = 0;

std:
/*!re2c
re2c:define:YYCTYPE  = "char";
re2c:define:YYCURSOR = re2c_cursor;
re2c:define:YYMARKER = re2c_marker;
re2c:define:YYCTXMARKER = re2c_ctx_marker;
re2c:define:YYLIMIT = re2c_limit;
re2c:define:YYFILL:naked = 1;
re2c:define:YYFILL@len = #;
re2c:define:YYFILL = "if (!fill(#)) { return 100; }";
re2c:yyfill:enable = 1;

SPACE       = [ ];
NEWLINE     = [\n\r];
TERM        = ";";
ID          = [a-zA-Z_][a-zA-Z0-9_]*;
DIGIT       = [1-9][0-9]*;
OPERATOR    = [!$&-=|+-*/?<>]+;

SPACE       { goto std; }
NEWLINE     { space_kind = 1; goto space_mode; }
TERM        { space_kind = 2; goto space_mode; }
"("         { return Token::create(R, Token::Kind::LParen); }
")"         { return Token::create(R, Token::Kind::RParen); }
"{"         { return Token::create(R, Token::Kind::LBrace); }
"}"         { return Token::create(R, Token::Kind::RBrace); }
"["         { return Token::create(R, Token::Kind::LBracket); }
"]"         { return Token::create(R, Token::Kind::RBracket); }
":"         { return Token::create(R, Token::Kind::Colon); }
"\""        { return Token::create(R, Token::Kind::DQuote); }
"?"         { return Token::create(R, Token::Kind::Question); }
"^"         { return Token::create(R, Token::Kind::Hat); }
"~"         { return Token::create(R, Token::Kind::Tilde); }
"."         { return Token::create(R, Token::Kind::Dot); }
","         { return Token::create(R, Token::Kind::Comma); }
"eq"        { return Token::operator(R, "eq"); }
"ne"        { return Token::operator(R, "ne"); }
"neg"       { return Token::operator(R, "neg"); }
"not"       { return Token::operator(R, "not"); }
"while"     { return Token::create(R, Token::Kind::While); }
"if"        { return Token::create(R, Token::Kind::If); }
"elif"      { return Token::create(R, Token::Kind::Elif); }
"else"      { return Token::create(R, Token::Kind::Else); }
"and"       { return Token::create(R, Token::Kind::And); }
"or"        { return Token::create(R, Token::Kind::Or); }
"break"     { return Token::create(R, Token::Kind::Break); }
"true"      { return Token::create(R, Token::Kind::True); }
"false"     { return Token::create(R, Token::Kind::False); }
"nil"       { return Token::create(R, Token::Kind::Nil); }
"local"     { return Token::create(R, Token::Kind::Local); }
"def"       { return Token::create(R, Token::Kind::Def); }
"class"     { return Token::create(R, Token::Kind::Class); }
"global"    { return Token::create(R, Token::Kind::Global); }
ID          { return Token::id(R, token(), length()); }
"0"         { return Token::int_literal(R, 0); }
DIGIT       { return Token::int_literal(R, str2int(token(), length())); }
OPERATOR    { return Token::operator(R, token(), length()); }
[^]         { return 101; }
*/

space_mode:
/*!re2c
SPACE       { goto space_mode; }
NEWLINE     { space_kind = std::max(space_kind, 1); goto space_mode; }
TERM        { space_kind = 2; goto space_mode; }
[^]         { re2c_cursor--; return space_kind; }
*/
}

}

