%option batch 8bit never-interactive noyywrap nounistd
%option full ecs nodefault

%option noinput nounput
%option noyy_push_state noyy_pop_state noyy_top_state
%option noyy_scan_bytes noyy_scan_string
%option noyyget_extra noyyset_extra noyyget_leng noyyget_text
%option noyyget_lineno noyyset_lineno noyyget_in noyyget_in
%option noyyget_out noyyset_out noyyget_lval noyyget_lval
%option noyyget_lloc noyyset_lloc noyyget_debug noyyget_debug

%top{
#include <stdint.h>
}

%{

// Default version uses unix read(); don't need it.
#define YY_INPUT(buf,result,max_size)  result = 0;  errno = 0;

// Must be included before Bison-generated JoltParser.h.
#include "ParserDecls.h"

#include "Location.h"
#include "Token.h"
#include "JoltParser.h"
#include "util/Integer.h"
#include "util/Message.h"
#include "util/String.h"
#include <vector>

static char *line_start;
static uint32_t line_no;

static vector<char> nesting;

static char *comment_start;
static size_t comment_start_len;

static char *string_start;
static char *string_end;

static void new_token(short lexeme, Object *value);
static Location make_loc(char *p = yytext);
static Token *yytokens;

static void emit(int token);
static void emit_integer(char *p, int base);
static void emit_float();
static void emit_lambdaparm();
static void emit_identifier();
static void emit_label();
static void note_whitespace();
static void note_newline();
static void emit_string();
static void emit_escape(char *p, size_t len, int base);

%}

WS        [ \t\r\f\v]
LET       [a-zA-Z_]
AN        [0-9a-zA-Z_]
HEX       [0-9a-fA-F_]

%x COMMENT
%x LAMBDAPARM
%x STRING

%%

"(" {
  nesting.push_back(' ');
  emit('(');
}

")" {
  if (!nesting.empty()) {
    char delim = nesting.back();
    nesting.pop_back();

    if (delim != ' ') {
      *yytext = delim;
      yyless(0);

      new_token(')', nullptr);
      new_token(op_concat, nullptr);
      yytokens->m_col++;
      YY_BREAK;
    }
  }

  emit(')');
}

{WS}+                   note_whitespace();
{WS}*("//".*)?\n        note_newline();

"/*"[([{<]* {
  BEGIN(COMMENT);
  note_whitespace();
  comment_start = yytext + 2;
  comment_start_len = yyleng - 2;
}

<COMMENT>[^*)\]}>\n]+     ;
<COMMENT>[^*)\]}>\n]*\n   note_newline();
<COMMENT>.                ;

<COMMENT>[)\]}>]*"*/" {
  if (size_t(yyleng) - 2 != comment_start_len)
    YY_BREAK;

  bool match = true;
  for (size_t i = 0; i < comment_start_len; i++) {
    int ch = yytext[i];
    switch (ch) {
      case ')': ch = '('; break;
      case ']': ch = '['; break;
      case '}': ch = '{'; break;
      case '>': ch = '<'; break;
      default: break;
    }

    if (ch != comment_start[comment_start_len - i - 1]) {
      match = false;
      break;
    }
  }

  if (match)
    BEGIN(INITIAL);
}

<COMMENT><<EOF>> {
  EmitError(make_loc()) << "Unterminated comment.";
  BEGIN(INITIAL);
}

\'[^'\n\\]* |
\"[^"\n\\]* {
  BEGIN(STRING);
  string_start = yytext;
  string_end = yytext + yyleng;
}

<STRING>['"] {
  if (*yytext != *string_start)
    *string_end++ = *yytext;
  else
    emit_string();
}

<STRING>[^'"\n\\]+ {
  memmove(string_end, yytext, yyleng);
  string_end += yyleng;
}

<STRING><<EOF>> |
<STRING>\n {
  EmitError(make_loc()) << "Non-terminated string literal.";
  line_start = yytext + 1;
  line_no++;
  emit_string();
}

<STRING>\\a     *string_end++ = 0x07;
<STRING>\\b     *string_end++ = 0x08;
<STRING>\\e     *string_end++ = 0x1b;
<STRING>\\f     *string_end++ = 0x0c;
<STRING>\\n     *string_end++ = 0x0a;
<STRING>\\r     *string_end++ = 0x0d;
<STRING>\\s     *string_end++ = 0x20;
<STRING>\\v     *string_end++ = 0x0b;
<STRING>\\\'    *string_end++ = '\'';
<STRING>\\\"    *string_end++ = '"';

<STRING>\\b[01]+                  emit_escape(yytext + 2, yyleng - 2, 2);
<STRING>\\o[0-7]+                 emit_escape(yytext + 2, yyleng - 2, 8);
<STRING>\\[0-9]+                  emit_escape(yytext + 1, yyleng - 1, 10);
<STRING>\\x[0-9a-fA-F]+           emit_escape(yytext + 2, yyleng - 2, 16);

<STRING>\\b\{[01]+\}              emit_escape(yytext + 3, yyleng - 4, 2);
<STRING>\\o\{[0-7]+\}             emit_escape(yytext + 3, yyleng - 4, 8);
<STRING>\\\{[0-9]+\}              emit_escape(yytext + 2, yyleng - 3, 10);
<STRING>\\x\{[0-9a-fA-F]+\}       emit_escape(yytext + 3, yyleng - 4, 16);

<STRING>\\\n {
  *string_end++ = '\n';
  line_start = yytext + 2;
  line_no++;
}

<STRING>\\\( {
  nesting.push_back(*string_start);
  emit_string();

  new_token(op_concat, nullptr);
  new_token('(', nullptr);
}

<STRING>\\.     *string_end++ = yytext[1];

<STRING>\\ {
  // End-of-file was hit if this rule matches.
  EmitError(make_loc()) << "Non-terminated string literal.";
  emit_string();
}

"/="      emit(op_divassign);
"=>"      emit(op_arrow);
"++"      emit(op_incr);
"+="      emit(op_addassign);
"--"      emit(op_decr);
"-="      emit(op_subassign);
"->"      emit(op_deref);
"->*"     emit(op_derefstar);
"*="      emit(op_multassign);
"%="      emit(op_modassign);
"&="      emit(op_bitandassign);
"|="      emit(op_bitorassign);
"^="      emit(op_bitxorassign);
">="      emit(op_ge);
">>"      emit(op_rshift);
">>="     emit(op_rshiftassign);
"<="      emit(op_le);
"<<"      emit(op_lshift);
"<<="     emit(op_lshiftassign);
"=="      emit(op_eq);
"!="      emit(op_ne);
"<["      emit(op_lexprfrag);
"]>"      emit(op_rexprfrag);
"<{"      emit(op_lstmtfrag);
"}>"      emit(op_rstmtfrag);
".."      emit(op_range);
"..."     emit(op_ellipses);
".*"      emit(op_dotstar);
"&&"      emit(op_and);
"||"      emit(op_or);
"~>"      emit(op_rstream);
"<~"      emit(op_wstream);

":+:"     emit(t_construct);
":-:"     emit(t_destruct);

0[bB]{HEX}*   emit_integer(yytext + 2, 2);
0[oO]{HEX}*   emit_integer(yytext + 2, 8);
0[xX]{HEX}*   emit_integer(yytext + 2, 16);
[0-9]{HEX}*   emit_integer(yytext, 10);

0[xX]{HEX}*/\.\.   emit_integer(yytext + 2, 16);
[0-9]{HEX}*/\.\.   emit_integer(yytext, 10);

[0-9][0-9_]*(\.[0-9_]*)?([eE][+-]?[0-9]+)?   emit_float();
\.[0-9_]+([eE][+-]?[0-9]+)?                  emit_float();
0[xX]{HEX}+(\.{HEX}*)?([pP][+-]?{HEX}+)?     emit_float();
0[xX]\.{HEX}+([pP][+-]?{HEX}+)?              emit_float();

as               emit(kw_as);
"as?"            emit(kw_asopt);
base             emit(kw_base);
case             emit(kw_case);
catch            emit(kw_catch);
class            emit(kw_class);
concept          emit(kw_concept);
concept_map      emit(kw_concept_map);
const            emit(kw_const);
default          emit(kw_default);
deferred         emit(kw_deferred);
delete           emit(kw_delete);
else             emit(kw_else);
enum             emit(kw_enum);
exit             emit(kw_exit);
for              emit(kw_for);
func             emit(kw_func);
if               emit(kw_if);
in               emit(kw_in);
inline           emit(kw_inline);
macro            emit(kw_macro);
mutable          emit(kw_mutable);
namespace        emit(kw_namespace);
new              emit(kw_new);
next             emit(kw_next);
operator         emit(kw_operator);
override         emit(kw_override);
private          emit(kw_private);
protected        emit(kw_protected);
public           emit(kw_public);
requires         emit(kw_requires);
return           emit(kw_return);
static           emit(kw_static);
template         emit(kw_template);
this             emit(kw_this);
~this            emit(kw_t_this);
throw            emit(kw_throw);
throws           emit(kw_throws);
"throws?"        emit(kw_throwsq);
try              emit(kw_try);
"try!"           emit(kw_tryforce);
"try?"           emit(kw_tryopt);
type             emit(kw_type);
union            emit(kw_union);
using            emit(kw_using);
var              emit(kw_var);
virtual          emit(kw_virtual);
while            emit(kw_while);

_[0-9]+          { BEGIN(LAMBDAPARM); yymore(); }

<LAMBDAPARM>{AN}+   { emit_identifier(); BEGIN(INITIAL); }
<LAMBDAPARM>.       { yyless(yyleng - 1); emit_lambdaparm(); BEGIN(INITIAL); }
<LAMBDAPARM>\n      { yyless(yyleng - 1); emit_lambdaparm(); BEGIN(INITIAL); }
<LAMBDAPARM><<EOF>> { emit_lambdaparm(); BEGIN(INITIAL); }

{LET}{AN}*       emit_identifier();

\/{LET}{AN}*\/   emit_label();

.         emit(yytext[0]);

%%

// Tokenize the entire source, creating a chain of tokens.  This is rather
// unusual compiler design.  Tokens are normally produced when asked for by
// the parser.  Isn't this wasteful of memory?
//
//    * Computers now have so much, who really cares anymore?  I'd never do
//      this back in the days when a compiler running on a mainframe was
//      considered a pig if it used a whole megabyte.
//
//    * Most tokens will make it into the parse tree, so the memory is going
//      to be consumed anyway.  The other tokens will disappear at the
//      first garbage collection.
//
//    * It considerably simplifies some tasks.  Scanning a string literal,
//      for example, would require a messy finite state machine if tokens
//      were produced one at a time.
//
// Tokens are chained in reverse order.  They'll be put into the proper
// order later on.
Token *ScanBuffer(char *p, size_t len) {
  // Insert a semicolon at the front, so make statement start detection
  // simpler.
  YY_BUFFER_STATE bs = yy_scan_buffer(p, len);
  line_start = yytext;
  line_no = 1;

  new_token(';', nullptr);
  nesting.resize(0);

  BEGIN(INITIAL);
  yylex();
  yy_delete_buffer(bs);

  // A null token terminates the chain.
  new_token(0, nullptr);
  return yytokens;
}

static void new_token(short lexeme, Object *value) {
  yytokens = new Token(yytokens, lexeme, value);
  yytokens->m_line_no = line_no;
  yytokens->m_col = uint32_t(yytext - line_start);
}

static Location make_loc(char *p) {
  return Location(line_no, uint32_t(p - line_start));
}

static inline void emit(int lexeme) {
  new_token(lexeme, nullptr);
}

static bool ends_in_letter() {
  if ((yy_hold_char >= 'a' && yy_hold_char <= 'z') ||
      (yy_hold_char >= 'A' && yy_hold_char <= 'Z')) {
    EmitError(make_loc()) << "Number ends with a letter: '"
                          << yy_hold_char << "'.";
    return true;
  }

  return false;
}

static void emit_integer(char *token, int base) {
  char *ds = token;
  char *p = token;

  if (ends_in_letter()) {
    new_token(t_integer, Integer::Get(wki_zero));
    return;
  }

  while (char d = *p++) {
    if (d >= '0' && d <= '9')
      *ds = char(d - '0');
    else if (d >= 'a' && d <= 'f')
      *ds = char(d - 'a' + 10);
    else if (d >= 'A' && d <= 'F')
      *ds = char(d - 'A' + 10);
    else
      continue;

    if ((*ds > 9 && base < 16) ||
        (*ds > 7 && base < 10) ||
        (*ds > 1 && base < 8)) {
      new_token(t_integer, Integer::Get(wki_zero));
      EmitError(yytokens) << "Digit '" << d << "' cannot appear in a base "
                          << base << "number.";
      return;
    }

    ds++;
  }

  size_t len = ds - token;
  if (len == 0) {
    new_token(t_integer, Integer::Get(wki_zero));
    EmitError(yytokens) << "Number has no digits.";
    return;
  }

  if ((base == 2 && len < 32) ||
      (base == 8 && len < 11) ||
      (base == 10 && len < 10) ||
      (base == 16 && len < 8)) {
    int v = 0;
    for (size_t i = 0; i < len; i++)
      v = v * base + token[i];
    new_token(t_integer, Integer::Get(v));
  } else {
    Integer *v = Integer::Get(wki_zero);
    Integer *b = Integer::Get(base);

    for (size_t i = 0; i < len; i++)
      v = v->Mul(b)->Add(Integer::Get(token[i]));

    new_token(t_integer, v);
  }
}

static void emit_float() {
  // Need to strip out underlines if any are present.
  if (char *p = strchr(yytext, '_')) {
    char *q = p;
    while (char c = *++p) {
      if (c != '_')
        *q++ = c;
    }
    *q = 0;
  }

  double f = ends_in_letter() ? 0.0 : atof(yytext);
  new_token(t_float, new Float(f));
}

static void emit_lambdaparm() {
  String *ident = String::Get(yytext, yyleng);
  new_token(t_lambdaparm, ident);
}

static void emit_identifier() {
  String *ident = String::Get(yytext, yyleng);
  new_token(t_ident, ident);
}

static void emit_label() {
  String *ident = String::Get(yytext + 1, yyleng - 2);
  new_token(t_label, ident);
}

static void note_whitespace() {
  // Whitespace must be remembered, because they help determine what are infix
  // vs pre- or postfix operators.
  if (yytokens->m_ws_after == 0)
    yytokens->m_ws_after = ' ';
}

static void note_newline() {
  // Newlines must be remembered, though only the first in a sequence of white-
  // space, because some of them may be turned into semicolons or commas.
  yytokens->m_ws_after = '\n';
  yytokens->m_nl_col = uint32_t(yytext - line_start - 1);
  line_start = yytext + yyleng - 1;
  line_no++;
}

static void emit_string() {
  int lexeme = *string_start == '"' ? t_string : t_char;
  string_start++;
  String *s = String::Get(string_start, string_end - string_start);
  new_token(lexeme, s);
  yytokens->m_col = uint32_t(string_start - line_start);
  BEGIN(INITIAL);
}

static void emit_escape(char *p, size_t len, int base) {
  char *p_save = p;
  uint32_t value = 0;
  while (len > 0 && value < 0x0fffffff) {
    value *= base;

    if (*p >= '0' && *p <= '9')
      value += *p - '0';
    else if (*p >= 'a' && *p <= 'f')
      value += *p - 'a' + 10;
    else
      value += *p - 'A' + 10;

    len--;
    p++;
  }

  // Convert code point to UTF-8.
  if (value <= 0x7f) {
    *string_end++ = char(value);
  } else if (value <= 0x7ff) {
    *string_end++ = char(0xC0 + (value >> 6));
    *string_end++ = char(0x80 + (value & 0x3f));
  } else if (value <= 0xffff) {
    *string_end++ = char(0xe0 + (value >> 12));
    *string_end++ = char(0x80 + ((value >> 6) & 0x3f));
    *string_end++ = char(0x80 + (value & 0x3f));
  } else if (value <= 0x10ffff) {
    *string_end++ = char(0xf0 + (value >> 18));
    *string_end++ = char(0x80 + ((value >> 12) & 0x3f));
    *string_end++ = char(0x80 + ((value >> 6) & 0x3f));
    *string_end++ = char(0x80 + (value & 0x3f));
  } else {
    EmitWarning1(make_loc(p_save)) << "Bad Unicode code point in string.";
  }
}
