// Copyright (c) 2014, Jeff Cohen
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
// 1. Redistributions of source code must retain the above copyright notice,
//    this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

/******************************************************************************/

// Must be included before Bison-generated JoltParser.hpp.
#include "ParserDecls.h"

#include "Config.h"
#include "Location.h"
#include "Token.h"
#include "JoltParser.hpp"
#include "util/Integer.h"
#include "util/Message.h"
#include "util/String.h"
#include <cctype>
#include <cstring>
#include <vector>

/******************************************************************************/

enum {
  TT_NO_MAP_BEFORE  = 1 << 0,
  TT_NO_MAP_AFTER   = 1 << 1,
  TT_INFIXOP        = 1 << 2,
  TT_PREFIXOP       = 1 << 3,
  TT_POSTFIXOP      = 1 << 4,
  TT_WHITESPACE     = 1 << 5,
  TT_LPRIMARY       = 1 << 6,
  TT_RPRIMARY       = 1 << 7,
  TT_IGNOREWS       = 1 << 8
};

class TokenTraits {
  struct Traits {
    uint16_t        m_flags;
    uint16_t        m_prefixop;
    uint16_t        m_postfixop;

    Traits() : m_flags(0), m_prefixop(0), m_postfixop(0) { }
  };

  constexpr static const int NTRAITS = 400;
  Traits            m_traits[NTRAITS];

  template<uint16_t idx, uint16_t flags, uint16_t op1 = 0, uint16_t op2 = 0>
  void init() {
    static_assert(idx < NTRAITS, "NTRAITS too small");

    constexpr static const uint16_t prepost = TT_PREFIXOP | TT_POSTFIXOP;
    static_assert(!(flags & prepost) || op1 > 0, "op1 must be specified");
    static_assert((flags & prepost) != prepost || op2 > 0,
      "op2 must be specified");
    static_assert((flags & prepost) || (op1 == 0 && op2 == 0),
      "neither op1 nor op2 can be specified");
    static_assert((flags & prepost) == prepost || op2 == 0,
      "op2 cannot be specified");


    m_traits[idx].m_flags = flags;
    if (flags & TT_PREFIXOP) {
      m_traits[idx].m_prefixop = op1;
      if (flags & TT_POSTFIXOP)
        m_traits[idx].m_postfixop = op2;
    } else if (flags & TT_POSTFIXOP) {
      m_traits[idx].m_postfixop = op1;
    }
  }

public:
  const Traits &operator[](int idx) { return m_traits[idx]; }

  TokenTraits() {
    init<' ',             TT_WHITESPACE>();
    init<'\n',            TT_WHITESPACE>();

    init<'/',             TT_INFIXOP>();
    init<'%',             TT_INFIXOP>();
    init<'^',             TT_INFIXOP>();
    init<'|',             TT_INFIXOP>();
    init<'<',             TT_INFIXOP>();
    init<'>',             TT_INFIXOP>();
    init<'?',             TT_INFIXOP>();
    init<':',             TT_INFIXOP>();
    init<op_le,           TT_INFIXOP>();
    init<op_ge,           TT_INFIXOP>();
    init<op_eq,           TT_INFIXOP>();
    init<op_ne,           TT_INFIXOP>();
    init<op_concat,       TT_INFIXOP>();
    init<op_lshift,       TT_INFIXOP>();
    init<op_rshift,       TT_INFIXOP>();
    init<'=',             TT_INFIXOP>();
    init<op_addassign,    TT_INFIXOP>();
    init<op_subassign,    TT_INFIXOP>();
    init<op_multassign,   TT_INFIXOP>();
    init<op_divassign,    TT_INFIXOP>();
    init<op_modassign,    TT_INFIXOP>();
    init<op_bitandassign, TT_INFIXOP>();
    init<op_bitorassign,  TT_INFIXOP>();
    init<op_bitxorassign, TT_INFIXOP>();
    init<op_lshiftassign, TT_INFIXOP>();
    init<op_rshiftassign, TT_INFIXOP>();
    init<op_dotstar,      TT_INFIXOP>();
    init<op_derefstar,    TT_INFIXOP>();
    init<op_range,        TT_INFIXOP>();
    init<op_or,           TT_INFIXOP>();
    init<kw_as,           TT_INFIXOP>();
    init<kw_asopt,        TT_INFIXOP>();
    init<kw_in,           TT_INFIXOP>();

    init<'-',             TT_INFIXOP | TT_PREFIXOP,   '-'>();
    init<'+',             TT_INFIXOP | TT_PREFIXOP,   '+'>();
    init<'!',             TT_INFIXOP | TT_PREFIXOP,   op_not>();
    init<'.',             TT_INFIXOP | TT_PREFIXOP,   '.'>();
    init<'@',             TT_INFIXOP | TT_PREFIXOP,   '@'>();
    init<'$',             TT_INFIXOP | TT_PREFIXOP,   '$'>();
    init<op_deref,        TT_INFIXOP | TT_PREFIXOP,   op_deref>();
    init<op_arrow,        TT_INFIXOP | TT_PREFIXOP,   op_arrow>();

    init<'*',             TT_INFIXOP | TT_PREFIXOP | TT_POSTFIXOP,
                          '*', op_ptr>();
    init<'&',             TT_INFIXOP | TT_PREFIXOP | TT_POSTFIXOP,
                          '&', op_ref>();
    init<op_and,          TT_INFIXOP | TT_PREFIXOP | TT_POSTFIXOP,
                          op_move, op_r_ref>();

    init<'#',             TT_POSTFIXOP, '#'>();
    init<op_ellipses,     TT_POSTFIXOP, op_ellipses>();

    init<kw_const,        TT_POSTFIXOP | TT_IGNOREWS,  kw_const>();
    init<kw_mutable,      TT_POSTFIXOP | TT_IGNOREWS,  kw_mutable>();

    init<kw_new,          TT_PREFIXOP | TT_IGNOREWS,   kw_new>();
    init<kw_new2,         TT_PREFIXOP | TT_IGNOREWS,   kw_new2>();
    init<kw_delete,       TT_PREFIXOP | TT_IGNOREWS,   kw_delete>();
    init<kw_delete2,      TT_PREFIXOP | TT_IGNOREWS,   kw_delete2>();
    init<kw_try,          TT_PREFIXOP | TT_IGNOREWS,   kw_try>();
    init<kw_tryforce,     TT_PREFIXOP | TT_IGNOREWS,   kw_tryforce>();
    init<kw_tryopt,       TT_PREFIXOP | TT_IGNOREWS,   kw_tryopt>();

    init<op_incr,         TT_PREFIXOP | TT_POSTFIXOP,  op_incr, op_incr>();
    init<op_decr,         TT_PREFIXOP | TT_POSTFIXOP,  op_decr, op_decr>();
    init<'~',             TT_PREFIXOP | TT_POSTFIXOP,  '~', pf_ident>();
    init<op_r_ref,        TT_PREFIXOP | TT_POSTFIXOP,  op_move, op_r_ref>();

    init<kw_throws,       TT_POSTFIXOP | TT_IGNOREWS,  kw_throws>();
    init<kw_throws2,      TT_INFIXOP>();
    init<kw_throwsq,      TT_PREFIXOP | TT_INFIXOP | TT_IGNOREWS, kw_throwsq>();

    init<'(',             TT_RPRIMARY | TT_NO_MAP_AFTER>();
    init<'[',             TT_RPRIMARY | TT_NO_MAP_AFTER>();
    init<'{',             TT_RPRIMARY | TT_NO_MAP_AFTER>();
    init<t_lambda,        TT_RPRIMARY | TT_NO_MAP_AFTER>();
    init<op_lexprfrag,    TT_RPRIMARY | TT_NO_MAP_AFTER>();
    init<op_lstmtfrag,    TT_RPRIMARY | TT_NO_MAP_AFTER>();

    init<')',             TT_LPRIMARY | TT_NO_MAP_BEFORE>();
    init<']',             TT_LPRIMARY | TT_NO_MAP_BEFORE>();
    init<'}',             TT_LPRIMARY | TT_NO_MAP_BEFORE>();
    init<t_rangle,        TT_LPRIMARY | TT_NO_MAP_BEFORE>();
    init<op_rexprfrag,    TT_LPRIMARY | TT_NO_MAP_AFTER>();
    init<op_rstmtfrag,    TT_LPRIMARY | TT_NO_MAP_AFTER>();

    init<',',             TT_NO_MAP_BEFORE | TT_NO_MAP_AFTER>();
    init<';',             TT_NO_MAP_BEFORE | TT_NO_MAP_AFTER>();
    init<':',             TT_NO_MAP_BEFORE | TT_NO_MAP_AFTER>();

    init<t_langle,        TT_NO_MAP_BEFORE | TT_NO_MAP_AFTER | TT_RPRIMARY >();

    init<t_ident,         TT_LPRIMARY | TT_RPRIMARY>();
    init<t_string,        TT_LPRIMARY | TT_RPRIMARY>();
    init<t_integer,       TT_LPRIMARY | TT_RPRIMARY>();
    init<t_float,         TT_LPRIMARY | TT_RPRIMARY>();
    init<kw_base,         TT_LPRIMARY | TT_RPRIMARY>();
    init<kw_class,        TT_LPRIMARY | TT_RPRIMARY>();
    init<kw_deferred,     TT_LPRIMARY | TT_RPRIMARY>();
    init<kw_namespace,    TT_LPRIMARY | TT_RPRIMARY>();
    init<kw_this,         TT_LPRIMARY | TT_RPRIMARY>();
    init<kw_type,         TT_LPRIMARY | TT_RPRIMARY>();

    init<kw_enum,         TT_RPRIMARY | TT_POSTFIXOP | TT_IGNOREWS, kw_enum>();
    init<kw_union,        TT_RPRIMARY | TT_IGNOREWS>();

    // FIXME: are kw_func and kw_template
    // LPRIMARYs?
  }
};

static TokenTraits s_traits;

/******************************************************************************/

static Token *s_tokens = nullptr;

// Called from bison-generated parser.
Token *Token::GetNextToken() {
  Token *t = s_tokens->m_next;
  s_tokens->m_next = t->m_next;
  return t;
}

// We need to keep track of every set of nested brackets and what's in them as
// in a few cases it affects to lexeme that is recognized.  You may wonder
// why bison's GLR mode isn't used as it would make these hacks largely un-
// necessary.  The reason is because bison's GPL exemption vanishes when
// GLR is used

namespace {
  struct Context {
    enum NewlineMapping {
      NewLinesToSemis,
      NewLinesToCommas,
      IgnoreNewLines
        // FIXME: handle attribute list case
    };

    // The token that will close and pop this context.
    short           m_closer;

    // The state of newline mapping.
    NewlineMapping  m_mapping;

    // Track if we're in the declaration list of a class or namespace, so we
    // know how to lexically handle construct/destruct statements.
    bool            m_class;

    // If in a declaration, this notes which type of declaration.
    short           m_decl;

	Context(short closer, NewlineMapping nlm)
		: m_closer(closer),
		  m_mapping(nlm),
		  m_class(false),
		  m_decl(0) { }
  };
}

static vector<Context> s_context;

// Miscellaneous flags to prevent excessive cascading error messages.
static bool g_reportedUnbalancedParen = false;

/******************************************************************************/

// Convert an "operator" keyword followed by an operator into an
// identifier token (t_ident).
static void ConvertOperatorKeyword(Token *t) {
  Token *op = t->m_next;
  WellKnownString wks;

  switch (op->m_lexeme) {
    case '+':             wks = wks_op_add;             break;
    case '-':             wks = wks_op_sub;             break;
    case '*':             wks = wks_op_mul;             break;
    case '/':             wks = wks_op_div;             break;
    case '~':             wks = wks_op_bitnot;          break;
    case '#':             wks = wks_op_count;           break;
    case '!':             wks = wks_op_not;             break;
    case op_deref:        wks = wks_op_deref;           break;
    case '%':             wks = wks_op_rem;             break;
    case op_lshift:       wks = wks_op_lshift;          break;
    case op_rshift:       wks = wks_op_rshift;          break;
    case '&':             wks = wks_op_bitand;          break;
    case '|':             wks = wks_op_bitor;           break;
    case '^':             wks = wks_op_bitxor;          break;
    case '<':             wks = wks_op_lt;              break;
    case '>':             wks = wks_op_gt;              break;
    case op_le:           wks = wks_op_le;              break;
    case op_ge:           wks = wks_op_ge;              break;
    case op_eq:           wks = wks_op_eq;              break;
    case op_ne:           wks = wks_op_ne;              break;
    case op_range:        wks = wks_op_range;           break;
    case '=':             wks = wks_op_assign;          break;
    case op_addassign:    wks = wks_op_add_assign;      break;
    case op_subassign:    wks = wks_op_sub_assign;      break;
    case op_multassign:   wks = wks_op_mul_assign;      break;
    case op_divassign:    wks = wks_op_div_assign;      break;
    case op_modassign:    wks = wks_op_rem_assign;      break;
    case op_bitandassign: wks = wks_op_bitand_assign;   break;
    case op_bitorassign:  wks = wks_op_bitor_assign;    break;
    case op_bitxorassign: wks = wks_op_bitxor_assign;   break;
    case op_lshiftassign: wks = wks_op_lshift_assign;   break;
    case op_rshiftassign: wks = wks_op_rshift_assign;   break;
    case kw_in:           wks = wks_op_in;              break;
    case kw_new:          wks = wks_op_new;             break;
    case kw_delete:       wks = wks_op_delete;          break;
    case kw_const:        wks = wks_op_const;           break;
    case kw_mutable:      wks = wks_op_mutable;         break;

    case op_incr:
      if (op->m_next->m_lexeme == '!') {
        op = op->m_next;
        wks = wks_op_pre_incr;
      } else {
        wks = wks_op_post_incr;
      }
      break;

    case op_decr:
      if (op->m_next->m_lexeme == '!') {
        op = op->m_next;
        wks = wks_op_pre_decr;
      } else {
        wks = wks_op_post_decr;
      }
      break;

    case '(':
      if (op->m_next->m_lexeme != ')')
        goto error;
      op = op->m_next;
      wks = wks_op_apply;
      break;

    case '[':
      if (op->m_next->m_lexeme != ']')
        goto error;
      op = op->m_next;
      wks = wks_op_index;
      break;

    default:
      goto error;
  }

  t->m_lexeme = t_ident;
  t->m_next = op->m_next;
  t->m_ws_after = op->m_ws_after;
  t->SetStringValue(String::Get(wks));
  return;

error:
  EmitError(t) << "Bad use of 'operator'.";
}

static Token *SkipSubexpression(Token *t, short closer = ')') {
  // If we already skipped this subexpression, return memoized result.
  if (Token *x = t->Closer())
    return x;

  Token *start = t;

  // Skip open parenthesis.
  if (t->m_next->m_lexeme == 0) {
    start->SetCloser(t);
    return t;
  }

  short opener = t->m_lexeme;
  t = t->m_next;

  // Look for closing token.
  while (t->m_lexeme != closer) {
    if (t->m_lexeme == opener)
      t = SkipSubexpression(t, closer);

    // If we're looking for a ] and see a ]>, then we need to split it into
    // two tokens.
    if (opener == '[' && t->m_lexeme == op_rexprfrag) {
      t->m_next = new Token(t, '>');
      t->m_lexeme = ']';
      break;
    }

    // If we're looking for a } and see a }>, then we need to split it into
    // two tokens.
    if (opener == '{' && t->m_lexeme == op_rstmtfrag) {
      t->m_next = new Token(t, '>');
      t->m_lexeme = '}';
      break;
    }

    if (t->m_next->m_lexeme == 0) {
      start->SetCloser(t);
      return t;
    }

    t = t->m_next;
  }

  // Return closing token.
  start->SetCloser(t);
  return t;
}

// Is the token an identifier or a name macro yielding an identifier?
// Returns nullptr if not, otherwise returns the following token.
static Token *SkipIdentifier(Token *t, bool backup = false) {
  Token *last = nullptr;
  if (t->m_lexeme == t_ident) {
    last = t;
    t = t->m_next;
  } else if (t->m_lexeme == kw_operator) {
    ConvertOperatorKeyword(t);
    last = t;
    t = t->m_next;
  } else if (t->m_lexeme == t_string) {
    do {
      t = t->m_next;
    } while (t->m_lexeme == t_string || t->m_lexeme == op_concat);

    if (t->m_lexeme != '$')
      return nullptr;

    last = t;
    t = t->m_next;
    return backup ? last : t;
  } else {
    return nullptr;
  }

  if (t->m_lexeme == '$') {
    last = t;
    t = t->m_next;
  }

  return backup ? last : t;
}

static void ScanAttribute(Token *start, String *name) {
  if (s_context.back().m_decl == kw_namespace)
    return;

  Token *t = start;

  // If the attribute was a reserved word, prefix it with @.
  if (t->m_lexeme != '@') {
    t->m_next = new Token(t->m_next, t_ident, name);
    t->m_next->m_ws_after = t->m_ws_after;
    t->m_next->m_line_no = t->m_line_no;
    t->m_next->m_col = t->m_col;
    t->m_lexeme = '@';
    t->m_ws_after = 0;
  } else if (t->m_ws_after != 0) {
    return;
  }

  if (t->m_next->m_lexeme == kw_deferred) {
    t = t->m_next;
    t->m_lexeme = t_ident;
    t->SetStringValue(String::Get("@deferred", 9));
  } else if (Token *u = SkipIdentifier(t->m_next, true)) {
    if (u == t->m_next && !name) {
      // Prepend an @ to the identifier.
      String *s = String::Get("@", 1);
      s = s->Concat(u->StringValue());
      u->SetStringValue(s);
    }
    t = u;
  } else if (t->m_next->m_lexeme == '[') {
    t = SkipSubexpression(t->m_next);
    goto setmark;
  } else if (t->m_next->m_lexeme != '(') {
    return;
  }

  // Look for and scan over arguments.
  if (t->m_ws_after == 0 && t->m_next->m_lexeme == '(') {
    Token *u = t->m_next;
    t = SkipSubexpression(u);
    u->m_lexeme = t_attrargs;
  }

setmark:
  // If this attribute was at a potential start of statement, propogate that
  // to our following token.
  t->m_next->m_stmt_mark = start->m_stmt_mark;

  // Also prohibit newline mapping afterwards.
  if (t->m_ws_after == '\n')
    t->m_ws_after = ' ';

  // If this attribute is followed by a colon and then a newline, within a
  // class or namespace declaration, force the newline to a semicolon.
  if (s_context.back().m_class) {
    t = t->m_next;
    if (t->m_lexeme == ':' && t->m_ws_after == '\n') {
      t->m_next = new Token(t, ';', true);
    }
  }
}

// Scan ahead to determine if a '<' is the start of a generic parameter list.
static bool StartOfGenericParameters(Token *t) {
  // Check if we already scanned passed this '<' when checking an earlier '<'
  // but failed to find a matching '>'.
  if (t->m_lexeme == t_langle)
    return true;
  if (t->Closer() == t)
    return false;

  Token *start = t;
  Token *last = t;
  t = t->m_next;

  // Assume we won't find a matching '>' and mark this '<' as just a '<'.
  start->SetCloser(start);

  while (true) {
    switch (t->m_lexeme) {
      // If we made it this far, then we have a generic parameter list.
      case '>':
        start->SetCloser(t);
        start->m_lexeme = t_langle;
        t->m_lexeme = t_rangle;
        return true;

      // Likewise for '>>'; but split it into two tokens first.
      case op_rshift:
        start->SetCloser(t);
        start->m_lexeme = t_langle;
        t->m_lexeme = t_rangle;
        t->m_next = new Token(t, '>');
        return true;

      // Skip over nested brackets.
      case '(':
        t = SkipSubexpression(t, ')');
        break;
      case '[':
        t = SkipSubexpression(t, ']');
        break;
      case '{':
        t = SkipSubexpression(t, '}');
        break;
      case t_lambda:
        // We're scanning ahead; they shouldn't exist yet.  SkipSubExpression()
        // won't handle them correctly, if their matching '}' haven't been
        // memoized yet.
        verify(false);
      case '<':
        if (!StartOfGenericParameters(t))
          return false;
        t = t->Closer();
        break;
      case op_lexprfrag:
        t = SkipSubexpression(t, op_rexprfrag);
        break;
      case op_lstmtfrag:
        t = SkipSubexpression(t, op_rstmtfrag);
        break;

      // Obviously, hitting one of these means we're not going to find a '>'.
      case ')':
      case ']':
      case '}':
      case op_rexprfrag:
      case op_rstmtfrag:
      case 0:
        return false;

      // These operators are often associated with boolean operands and if we
      // run across one that's it.  They must be in a parenthesized sub-
      // expression to be used as a generic parameter.  Only binary operators
      // need considerng.  (& must be accepted, as it's also used to construct
      // reference types.)
      // FIXME: postfix ops aren't recognized yet, so infix & must be accepted.
      case op_and:
      case op_or:
      case '!':
      case '^':
      case '|':
      case '~':
        return false;

      // End of statement (or start of a new statement) also stops the scan.
      // Again, parenthesizing would be necessary.
      case ';':
      case kw_case:
      case kw_class:
      case kw_exit:
      case kw_for:
      case kw_func:
      case kw_if:
      case kw_macro:
      case kw_next:
      case kw_return:
      case kw_switch:
      case kw_throw:
      case kw_using:
      case kw_while:
      case t_construct:
      case t_destruct:
        return false;

      // Don't be fooled by '<' or '>' preceded by "operator".
      case kw_operator:
        ConvertOperatorKeyword(t);
        break;

      case kw_template:
        if (t->m_stmt_mark || last->m_ws_after == '\n')
          return false;
        break;

      // Const may or may not be the start of a statement.
      case kw_const:
        if (t->m_stmt_mark || last->m_ws_after == '\n') {
          Token *u = SkipIdentifier(t->m_next);
          if (!u || u->m_lexeme != '=')
            return false;
        }
        break;

      default:
        last = t;
        t = t->m_next;
        continue;
    }

    last = t;
    t = t->m_next;
  }
}

static bool IsLambdaClosure(Token *t) {
  // FIXME:  This is a lambda closure and not a statement block if one of the
  // following conditions is true:
  //   * The '{' is immediately followed by a capture list, i.e. '['.
  //   * The '{' is immediately followed by '=>'.
  //   * The '{' is immediately followed by a LambdaParms non-terminal, which
  //     is then followed by '=>'.  In this case, an extra t_lambdahack must
  //     be inserted after the '{' to prevent a grammar ambiguity.
  return false;
}

// There are several tweaks we make to the token stream.  The implicit
// mapping of newlines to semicolons drives many of these tweaks.  Other
// tweaks address the few minor ambiguities in the grammar.
static void AdjustTokens() {
  // Don't bother with empty file.
  if (!s_tokens || !s_tokens->m_next)
    return;

  // The tokens were chained together backwards.  First order of business
  // is to put them in the correct order.
  Token *t = s_tokens;
  s_tokens = nullptr;

  while (t) {
    Token *x = t;
    t = t->m_next;
    x->m_next = s_tokens;
    s_tokens = x;
  }

  // The first token is a dummy token; the last token is the EOF token.
  Token *last = nullptr;
  t = s_tokens;

  // Mark the first, true token as a potential start of statement.
  t->m_next->m_stmt_mark = true;

  // Some keywords get converted to idents under some circumstances.  Cache
  // the String equivalents here.
  String *ident_base        = String::Get("base", 4);
  String *ident_class       = String::Get("class", 5);
  String *ident_deferred    = String::Get("deferred", 8);
  String *ident_namespace   = String::Get("namespace", 9);
  String *ident_inline      = String::Get("@inline", 7);
  String *ident_override    = String::Get("@override", 9);
  String *ident_private     = String::Get("@private", 8);
  String *ident_protected   = String::Get("@protected", 10);
  String *ident_public      = String::Get("@public", 7);
  String *ident_this        = String::Get("this", 4);
  String *ident_type        = String::Get("type", 4);
  String *ident_virtual     = String::Get("@virtual", 8);

top:
  // Advance to next token.
  last = t;
  t = t->m_next;

  switch (t->m_lexeme) {

    case '(':
      if (last->m_lexeme == kw_enum || last->m_lexeme == kw_union) {
        s_context.push_back(Context(')', Context::NewLinesToCommas));
        break;
      }
      [[fallthrough]];
    case t_attrargs:
      s_context.push_back(Context(')', Context::IgnoreNewLines));
      break;

    case '[':
      s_context.push_back(Context(']', Context::IgnoreNewLines));
      break;

    case '{':
      if (IsLambdaClosure(t))
        t->m_lexeme = t_lambda;
      [[fallthrough]];
    case t_lambda:
      if (auto decl = s_context.back().m_decl) {
        s_context.back().m_decl = 0;
        s_context.push_back(Context('}', Context::NewLinesToSemis));
        if (decl == kw_class || decl == kw_namespace)
          s_context.back().m_class = true;
      } else {
        s_context.push_back(Context('}', Context::NewLinesToSemis));
      }
      t->m_next->m_stmt_mark = true;
      break;

    // A '<' may or may not be the start of a generic parameter list.  See
    // which it is.
    case '<':
      if (StartOfGenericParameters(t))
        s_context.push_back(Context(t_rangle, Context::IgnoreNewLines));
      break;

    // Nested <> were converted to t_langle/t_rangle when the outermost pair
    // was converted.
    case t_langle:
      s_context.push_back(Context(t_rangle, Context::IgnoreNewLines));
      break;

    case op_lexprfrag:
      s_context.push_back(Context(op_rexprfrag, Context::IgnoreNewLines));
      break;

    case op_lstmtfrag:
      s_context.push_back(Context(op_rstmtfrag, Context::IgnoreNewLines));
      t->m_next->m_stmt_mark = true;
      break;

    case ')':
    case ']':
    case '}':
    case t_rangle:
    case op_rexprfrag:
    case op_rstmtfrag:
      // Of course, in a legal program brackets would always be balanced...
      if (t->m_lexeme != s_context.back().m_closer || s_context.size() <= 1) {
        // Only report once until we get back in sync...
        if (!g_reportedUnbalancedParen) {
          EmitError(t) << "Unbalanced closing '"
                       << (t->m_lexeme == t_rangle ? '>' : char(t->m_lexeme))
                       << "'.";
          g_reportedUnbalancedParen = true;
        }
      } else {
        s_context.pop_back();
        g_reportedUnbalancedParen = false;
        if (t->m_lexeme == '}')
          s_context.back().m_decl = 0;
      }
      break;

    case kw_class:
      // A class keyword following a '.', '->', '.*' or '->*' is really just
      // an identifier.
      if (last->m_lexeme == '.' || last->m_lexeme == op_deref ||
          last->m_lexeme == op_dotstar || last->m_lexeme == op_derefstar) {
        t->m_lexeme = t_ident;
        t->SetStringValue(ident_class);
        break;
      }

      if (t->m_stmt_mark) {
        s_context.back().m_decl = kw_class;
      }
      break;

    // namespace is a keyword only at the start of a statement.  Otherwise,
    // it's an identifier whose value is the type, namespace.
    case kw_namespace:
      if (t->m_stmt_mark) {
        s_context.back().m_decl = kw_namespace;
      } else if (last->m_lexeme != kw_using) {
        t->m_lexeme = t_ident;
        t->SetStringValue(ident_namespace);
      }
      break;

    // Note presence of other declarations.
    case kw_func:
    case kw_macro:
      if (t->m_stmt_mark) {
        s_context.back().m_decl = t->m_lexeme;
      }
      break;

    // Note presence of template clause preceding certain declarations and
    // propagate start of statement mark.
    case kw_template: {
      Token *u = t->m_next;
      if (StartOfGenericParameters(u))
        u->Closer()->m_next->m_stmt_mark = t->m_stmt_mark;
      break;
    }

    // Outside of a class declaration list, these do not declare a method and
    // hence cannot have proc bodies.
    case kw_this:
    case kw_t_this:
      if (s_context.back().m_class && t->m_stmt_mark)
        s_context.back().m_decl = t->m_lexeme;
      else
        t->SetStringValue(t->m_lexeme == kw_this ? ident_this
                                              : String::Get(wkhs_destructor));
      break;

    // This is really unfortunate.  throws is both a infix and postfix operator,
    // and the presence of white space cannot be used to distinguish those uses
    // because it's a keyword and not a symbol.  So we need this hack: when
    // used as an infix operator, it must be followed by '(' on the same line.
    // Note that the operand does not necessarily end with ')' due to operator
    // precedence; throws is still an operator, not a function.
    case kw_throws:
      if (t->m_next->m_lexeme == '(' && t->m_ws_after != '\n')
        t->m_lexeme = kw_throws2;
      break;

    // If two string literals are separated by whitespace, insert an op_concat.
    case t_string:
      if (t->m_next->m_lexeme == t_string) {
          t->m_ws_after = 0;
          t->m_next = new Token(t, op_concat);
          t->m_next->m_col = t->m_next->m_next->m_col;
      }
      break;

    case op_deref:
      if (s_context.back().m_decl == kw_func && last->m_lexeme == kw_const ) {
        // A const following an formal argument list must bypass whitespace
        // handling logic for operators.
        goto mapnewline;
      }
      break;

    // Convert operator keyword sequences into identifier tokens.
    case kw_operator:
      ConvertOperatorKeyword(t);
      break;

      // a statement.
      break;

    // deferred is a keyword only at the end of a proc declaration.
    case kw_deferred:
      if (s_context.back().m_decl != kw_func ||
          (t->m_ws_after != '\n' && t->m_next->m_lexeme != ';')) {
        t->m_lexeme = t_ident;
        t->SetStringValue(ident_deferred);
        break;
      }
      [[fallthrough]];

    // default, deferred or delete at end of a proc declaration means no
    // proc body.
    case kw_default:
    case kw_delete:
      if (s_context.back().m_decl == kw_func && t->m_ws_after == '\n') {
        s_context.back().m_mapping = Context::NewLinesToSemis;
        s_context.back().m_decl = 0;

        // Note: delete ordinarily inhibits newline mapping, so do it here.
        t->m_next = new Token(t, ';', true);
        last = t;
        t = t->m_next;
      } else if (t->m_lexeme == kw_delete && t->m_next->m_lexeme == '[') {
        // Hack needed to disambiguate delete[...] from set type construction.
        t->m_lexeme = kw_delete2;
      }
      break;

    case kw_new:
      if (t->m_next->m_lexeme == '[') {
        // Hack needed to disambiguate new[...] from set type construction.
        t->m_lexeme = kw_new2;
      }
      break;

    // Type is a keyword only at the start of a statement.
    case kw_type:
      if (!t->m_stmt_mark) {
        t->m_lexeme = t_ident;
        t->SetStringValue(ident_type);
      }
      break;

    // "base" is a pseudo-ident; it needs a value.
    case kw_base:
      t->SetStringValue(ident_base);
      break;

    // Handle lexical considerations for attributes.
    case '@':
      ScanAttribute(t, nullptr);
      break;

    case kw_inline:
      ScanAttribute(t, ident_inline);
      break;

    case kw_override:
      ScanAttribute(t, ident_override);
      break;

    case kw_private:
      ScanAttribute(t, ident_private);
      break;

    case kw_protected:
      ScanAttribute(t, ident_protected);
      break;

    case kw_public:
      ScanAttribute(t, ident_public);
      break;

    case kw_static:
      ScanAttribute(t, String::Get(wks_atstatic));
      break;

    case kw_virtual:
      ScanAttribute(t, ident_virtual);
      break;

    case ';':
      s_context.back().m_decl = 0;
      t->m_next->m_stmt_mark = true;
      break;

    case t_label:
      if (t->m_stmt_mark) {
        t->m_next->m_stmt_mark = true;
        if (t->m_ws_after == '\n')
          t->m_ws_after = ' ';
      }
      break;

    case kw_exit:
    case kw_next:
    case kw_return:
      // A following label is ambiguous because it maybe part of a following
      // expression.  Assuming it's not (parenthesis can be used to force
      // making it part of an expression).
      if (t->m_next->m_lexeme == t_label && t->m_ws_after != '\n')
        t->m_next->m_lexeme = t_label2;
      break;

    case 0:
      return;

    default:
      break;
  }

  // An expression may start with a prefix op; handle that here.
  // FIXME: this may have quadratic behavior on a long sequence of prefix
  // ops, because we'll scan each suffix of the chain.  Shouldn't be a
  // problem, as how many prefix ops can be chained?  If custom prefix ops
  // are ever defined, it will cause the sanity check to fail anyway.
  if (s_traits[t->m_lexeme].m_flags & TT_PREFIXOP) {
    Token *u = t;

    // First, a sanity check:  make sure this chain of prefix ops ends with
    // a right primary, nor can it be preceded by a left primary.
    if ((s_traits[last->m_lexeme].m_flags & TT_LPRIMARY) == 0) {
      do {
        u = u->m_next;
      } while (u && s_traits[u->m_lexeme].m_flags & TT_PREFIXOP);
    }

    if (u && s_traits[u->m_lexeme].m_flags & TT_RPRIMARY) {
      // It does, so process them.
      u = t;
      auto traits = s_traits[u->m_lexeme];

      while (traits.m_flags & TT_PREFIXOP) {
        // Just no.  It cannot be at the end of a line.
        if (u->m_ws_after == '\n')
          break;

        if (u->m_ws_after != 0) {
          // A prefix op isn't supposed to be followed by whitespace.
          // Exception:  operators that are keywords.
          if ((traits.m_flags & TT_IGNOREWS) == 0) {
            EmitWarning1(u) << "Prefix operators may not be followed by "
                               "whitespace.";
            u->m_ws_after = 0;
          }
        }

        u->m_lexeme = traits.m_prefixop;
        u = u->m_next;
        traits = s_traits[u->m_lexeme];
      }
    }
  }

  // Look for a chain of operators following a primary token.  Identify which
  // are prefix or postfix, and whether any newlines on either side of an infix
  // is to be turned into spaces.
  if (s_traits[t->m_lexeme].m_flags & TT_LPRIMARY) {
    char leftws = t->m_ws_after;
    Token *w = t;
    Token *u = t->m_next;

    // Scan postfix operators.
    auto traits = s_traits[u->m_lexeme];
    while (traits.m_flags & TT_POSTFIXOP) {
      if (leftws != 0) {
        // A postfix op isn't supposed to be preceded by whitespace, so if it
        // can be interpreted as an infix op, then do so.
        if (traits.m_flags & TT_INFIXOP)
          break;

        // Just no.  It cannot be at the start of a line.
        if (leftws == '\n')
          break;

        if (leftws == ' ' && (traits.m_flags & TT_IGNOREWS) == 0) {
          EmitWarning1(u) << "Postfix operators may not be preceded by "
                             "whitespace.";
          w->m_ws_after = 0;
        }
      } else if (u->m_ws_after == 0) {
        // If there's no whitespace on either side, it could be interpreted
        // as an infix op also.  What follows must either be a prefix op or a
        // right primary.
        if ((traits.m_flags & TT_INFIXOP) &&
            (u->m_next->m_lexeme != '[') &&
            (!(s_traits[u->m_next->m_lexeme].m_flags & TT_POSTFIXOP)) &&
            (s_traits[u->m_next->m_lexeme].m_flags &
             (TT_PREFIXOP | TT_RPRIMARY)))
          break;
      }

      u->m_lexeme = traits.m_postfixop;
      leftws = u->m_ws_after;
      w = u;
      u = u->m_next;
      traits = s_traits[u->m_lexeme];
    }

    // Scan infix operator.
    if (traits.m_flags & TT_INFIXOP) {
      if (leftws + u->m_ws_after != 0) {
        if (leftws * u->m_ws_after == 0) {
          if (leftws != 0 && (traits.m_flags & TT_PREFIXOP) != 0)
            goto mapnewline;
          if (leftws == 0 && (traits.m_flags & TT_POSTFIXOP) != 0)
            goto mapnewline;

          // Can't be confused with pre- or postfix op, so just emit
          // a warning (with two exceptions).
          if (u->m_lexeme != '.' && u->m_lexeme != ':')
            EmitWarning1(u) << "Infix operator may not have whitespace on "
                               "only one side.";
        }

        // Kill newlines on either side of the operator; no conversion to
        // semi-colons here.
        w->m_ws_after = ' ';
        u->m_ws_after = ' ';

        // A hack:  An infix @ in a namespace statement context means a
        // declaration list will not follow.
        if (u->m_lexeme == '@' && s_context.back().m_decl == kw_namespace)
          s_context.back().m_mapping = Context::NewLinesToSemis;
      }

      u = u->m_next;
      traits = s_traits[u->m_lexeme];

      // Finally, scan prefix operators.
      // FIXME: this may not be needed, because it duplicates the same code
      // below.  The only difference is the lack of the sanity check, which is
      // theoretically not needed because we just scanned a infix op.
      while (traits.m_flags & TT_PREFIXOP) {
        // Just no.  It cannot be at the end of a line.
        if (u->m_ws_after == '\n')
          break;

        if (u->m_ws_after != 0) {
          // A prefix op isn't supposed to be followed by whitespace.
          // Exception:  operators that are keywords.
          if ((traits.m_flags & TT_IGNOREWS) == 0) {
            EmitWarning1(u) << "Prefix operators may not be followed by "
                               "whitespace.";
            u->m_ws_after = 0;
          }
        }

        u->m_lexeme = traits.m_prefixop;
        u = u->m_next;
        traits = s_traits[u->m_lexeme];
      }
    }
  }

mapnewline:
  // Do something with newlines:  either ignore them by turning to spaces,
  // or convert them to semicolons or commas.
  if (t->m_ws_after == '\n') {
    t->m_ws_after = ' ';

    // See if newlines should otherwise be mapped; if not, kill it.
    Context &c = s_context.back();
    if (c.m_mapping == Context::IgnoreNewLines)
      goto postnlmapping;

    if (s_traits[t->m_lexeme].m_flags & TT_NO_MAP_AFTER)
      goto postnlmapping;

    if (s_traits[t->m_next->m_lexeme].m_flags & TT_NO_MAP_BEFORE)
      goto postnlmapping;

    // These tokens need special handling.
    switch (t->m_next->m_lexeme) {
      // These tokens inhibit newline mapping if preceded by a template
      // clause, which we detect by a preceding t_rangle token.
      case kw_class:
      case kw_func:
      case kw_macro:
      case kw_this:
      case kw_t_this:
      case kw_type:
        if (t->m_lexeme == t_rangle && t->m_next->m_stmt_mark)
          goto postnlmapping;
        break;

      default:
        break;
    }

    // Map newline to something not whitespace.
    bool useComma = c.m_mapping == Context::NewLinesToCommas;
    t->m_next = new Token(t, useComma ? ',' : ';', true);
    if (!useComma)
      t->m_next->m_stmt_mark = true;
  }

postnlmapping:
  goto top;
}

void Token::TokenizeFile(bool first, Token *t) {
  if (!first) {
    verify(s_context.size() == 1);
  } else {
    // Pretend every file is enclosed within a pair of parenthesis, giving us
    // a place, when parsing a statement, to track when it's appropriate to map
    // newlines to semicolons.
    s_context.push_back(Context(')', Context::NewLinesToSemis));
    s_context.back().m_mapping = Context::NewLinesToSemis;
  }

  s_tokens = t;
  AdjustTokens();
}
