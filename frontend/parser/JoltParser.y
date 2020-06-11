%{
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

// Jolt's grammar was designed to be context-free -- unlike certain well-known
// languages -- and as close to LALR(1) as practical.  Some deviations from
// LALR(1) were accepted because strict adherence would have made some language
// constructs less pleasant to look at and type in.  After all, LALR(1) is not
// a goal in and of itself but only a means to avoid unnecessary complexity in
// the parser.  It is better to have added complexity in the parser than in the
// Jolt code programmers write.
//
// An interesting question is why, given that this file can only be processed
// by bison, was bison's GLR mode not used?  First, most systems at this time
// do not have a recent enough version of bison for GLR to be available.
// Second, it's not clear it would really make the code simpler.  Look ahead
// code could be eliminated, but only to be replaced with pick-the-correct-
// parse-tree code.  Third and finally, using bison's GLR mode eliminates its
// GPL exception, making it incompatible with Jolt's primary license.  Note
// that if GLR was sufficiently compelling, there do exist BSD-licensed parser
// generators supporting GLR, though none of them can read bison/yacc files.
//
// FIXME: very little syntax error handling, reporting or recovery yet.

#include "ParserDecls.h"
#include "Token.h"
#include "entity/AttributeList.h"
#include "entity/Class.h"
#include "entity/Const.h"
#include "entity/Entity.h"
#include "entity/Field.h"
#include "entity/Method.h"
#include "entity/Scope.h"
#include "entity/Specialization.h"
#include "entity/Var.h"
#include "node/AddrOf.h"
#include "node/Apply.h"
#include "node/Attributes.h"
#include "node/Block.h"
#include "node/CallBuiltin.h"
#include "node/Delete.h"
#include "node/Enum.h"
#include "node/Expr.h"
#include "node/Ident.h"
#include "node/For.h"
#include "node/If.h"
#include "node/Initializer.h"
#include "node/Label.h"
#include "node/List.h"
#include "node/Literal.h"
#include "node/Member.h"
#include "node/NativeOperator.h"
#include "node/New.h"
#include "node/Quote.h"
#include "node/Sequence.h"
#include "node/Transfer.h"
#include "node/TypeOf.h"
#include "node/Union.h"
#include "node/VarDecl.h"
#include "node/While.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"
#include <stdio.h>

using namespace AST;

#ifndef NDEBUG
#define YYDEBUG 1
#endif

#define YYERROR_VERBOSE

BisonArray<DeclInfo> yyparsetree;

int yylex();
void yyerror(const char *s);
static Node *ProcessVarDecls(SafeArray<VarInfo> infos);
static void SetVarDeclAttributes(VarDecl *vd, AttributeList *al);
static BisonArray<Field *> ProcessFieldDecls(SafeArray<VarInfo> infos);
static Node *GenerateBlock(SafeArray<Object *> stmts);
static void ProcessTemplateParams(TemplateClause &tc);
static Node *ProcessTemplateType(Location sl, SafeArray<TemplateInfo> infos);

%}

%union {
  bool                 m_bool;
  AttributeList       *m_attrlist;
  Entity              *m_entity;
  Node                *m_expr;
  Object              *m_object;
  Token               *m_token;
  AST::AttrInfo        m_attr;
  AST::MemberItem      m_member;
  AST::BaseInfo        m_baseinfo;
  AST::DeclInfo        m_declinfo;
  AST::FuncInfo        m_funcinfo;
  AST::IfClause        m_ifclause;
  AST::ImportInfo      m_importinfo;
  AST::ProcBody        m_procbody;
  AST::TemplateClause  m_templateclause;
  AST::TemplateInfo    m_templateinfo;
  AST::NamespaceInfo   m_namespaceinfo;
  AST::VarInfo         m_varinfo;
  AST::VarName         m_varname;
  AST::BisonArray<AST::AttrInfo>      m_attrs;
  AST::BisonArray<AST::MemberItem>    m_members;
  AST::BisonArray<AST::BaseInfo>      m_baseinfos;
  AST::BisonArray<AST::DeclInfo>      m_declinfos;
  AST::BisonArray<AST::IfClause>      m_ifclauses;
  AST::BisonArray<Token *>            m_tokens;
  AST::BisonArray<AST::TemplateInfo>  m_templateinfos;
  AST::BisonArray<AST::VarInfo>       m_varinfos;
  AST::BisonArray<AST::VarName>       m_varnames;
  AST::BisonArray<Node *>             m_exprlist;
  AST::BisonArray<Object *>           m_objlist;
}

// Define keyword tokens (that aren't operators).
%token <m_token> kw_base
%token <m_token> kw_case
%token <m_token> kw_catch
%token <m_token> kw_class
%token <m_token> kw_concept
%token <m_token> kw_concept_map
%token <m_token> kw_default
%token <m_token> kw_deferred
%token <m_token> kw_delete kw_delete2
%token <m_token> kw_exit
%token <m_token> kw_for
%token <m_token> kw_func
%token <m_token> kw_if
%token <m_token> kw_macro
%token <m_token> kw_namespace
%token <m_token> kw_new kw_new2
%token <m_token> kw_next
%token <m_token> kw_operator
%token <m_token> kw_requires
%token <m_token> kw_return
%token <m_token> kw_switch
%token <m_token> kw_template
%token <m_token> kw_this
%token <m_token> kw_t_this
%token <m_token> kw_throw
%token <m_token> kw_type
%token <m_token> kw_union
%token <m_token> kw_using
%token <m_token> kw_var
%token <m_token> kw_while

// Attributes that have been promoted to keywords.
%token <m_token> kw_inline
%token <m_token> kw_override
%token <m_token> kw_private
%token <m_token> kw_protected
%token <m_token> kw_public
%token <m_token> kw_static
%token <m_token> kw_virtual

// Define literal tokens.
%token <m_token> t_char
%token <m_token> t_ident
%token <m_token> t_lambdaparm
%token <m_token> t_string
%token <m_token> t_integer
%token <m_token> t_float

// Define assignment tokens.
%token <m_token> '='
%token <m_token> op_addassign
%token <m_token> op_subassign
%token <m_token> op_multassign
%token <m_token> op_divassign
%token <m_token> op_modassign
%token <m_token> op_bitandassign
%token <m_token> op_bitorassign
%token <m_token> op_bitxorassign
%token <m_token> op_lshiftassign
%token <m_token> op_rshiftassign

// Define pseudo-tokens.  These are either hacks to work around LALR(1)
// limitations, and/or define node opcodes that do not have precedence.
// Some of these are used temporarily in Lexer.cpp and do not appear in
// the grammar.
%token <m_token> op_rexprfrag
%token <m_token> op_rstmtfrag
%token <m_token> t_construct
%token <m_token> t_destruct
%token <m_token> t_lambdahack
%token <m_token> t_rangle
%token <m_token> t_attrargs
%token <m_token> t_label t_label2

// Tokens with operator precedence.
%nonassoc <m_token> op_lowest_precedence
%nonassoc <m_token> kw_else
%left     <m_token> ';'
%nonassoc <m_token> op_rstream op_wstream
%nonassoc <m_token> op_arrow
%left     <m_token> '?' ':'
%left     <m_token> op_or
%left     <m_token> op_and
%left     <m_token> op_not
%nonassoc <m_token> kw_as kw_asopt kw_in
%right    <m_token> op_new_delete
%nonassoc <m_token> op_range
%left     <m_token> '|'
%left     <m_token> '^'
%left     <m_token> '&'
%left     <m_token> op_eq op_ne
%left     <m_token> '<' '>' op_le op_ge
%left     <m_token> op_lshift op_rshift
%left     <m_token> '$'
%left     <m_token> '+' '-'
%left     <m_token> '*' '/' '%'
%right    <m_token> '~' op_move kw_try kw_tryforce kw_tryopt op_unary
%left     <m_token> op_ref op_r_ref op_ptr '(' '[' t_lambda t_langle op_lexprfrag op_lstmtfrag op_incr op_decr pf_ident '#' op_ellipses kw_const kw_mutable kw_enum
%left     <m_token> '.' '!' op_deref op_dotstar op_derefstar kw_throws kw_throws2 kw_throwsq
%right    <m_token> '@'
%left     <m_token> op_concat
%nonassoc <m_token> op_highest_precedence

// Define the types of all non-terminals.
%type <m_attr>           Attribute
%type <m_attrs>          Attributes
%type <m_attrlist>       AttributesOpt
%type <m_baseinfos>      BaseClassList
%type <m_baseinfo>       BaseSpecifier
%type <m_expr>           Block
//%type <m_node>           Capture
//%type <m_nodelist>       CaptureList
//%type <m_nodelist>       CaptureListOpt
//%type <m_nodelist>       Cases
//%type <m_caseclause>     Case
%type <m_expr>           CatchStmt
%type <m_entity>         ClassDecl
%type <m_expr>           CompoundStatement
%type <m_entity>         ConsDestructDecl
%type <m_entity>         ConstStmt
%type <m_expr>           ConstructStmt
%type <m_expr>           ConstructWhat
%type <m_declinfo>       Declaration
%type <m_declinfos>      DeclarationList
%type <m_declinfos>      DeclarationList2
%type <m_expr>           DestructStmt
%type <m_ifclause>       ElseOpt
%type <m_ifclause>       ElseIf
%type <m_ifclauses>      ElseIfList
%type <m_members>        EnumList
%type <m_member>         EnumMember
%type <m_expr>           ExitStmt
%type <m_expr>           Expr
%type <m_exprlist>       ExprList
%type <m_expr>           ExprOpt
%type <m_expr>           ForExtra
%type <m_expr>           ForStmt
%type <m_entity>         FuncDecl
%type <m_funcinfo>       FuncInfo
%type <m_funcinfo>       FuncInfos
%type <m_token>          Identifier
%type <m_token>          IdentifierOpt
%type <m_expr>           IfStmt
%type <m_expr>           IfOpt
%type <m_importinfo>     ImportDecl
%type <m_baseinfos>      InheritanceSpecifier
%type <m_token>          LabelOpt
%type <m_expr>           Lambda
%type <m_expr>           LoopElseOpt
%type <m_entity>         MacroDecl
%type <m_entity>         MethodDecl;
%type <m_namespaceinfo>  NamespaceDecl
%type <m_tokens>         NamespaceSpecifier
%type <m_expr>           NextStmt
%type <m_procbody>       ProcBody
%type <m_object>         Program
%type <m_expr>           ReturnStmt
%type <m_member>         ReturnType
%type <m_member>         ReturnTypeOpt
%type <m_exprlist>       SpecializationOpt
%type <m_object>         Statement
%type <m_object>         StatementNoAttrs
%type <m_objlist>        StatementList
%type <m_expr>           SwitchStmt
%type <m_templateclause> Template
%type <m_templateclause> TemplateClause
%type <m_templateinfo>   TemplateParam
%type <m_templateinfos>  TemplateParams
%type <m_templateinfo>   TemplateParamName
%type <m_expr>           ThrowStmt
%type <m_members>        TupleList
%type <m_member>         TupleMember
%type <m_token>          TupleName
%type <m_members>        TupleValue
%type <m_entity>         TypeStmt
%type <m_object>         UnlabeledStatement
%type <m_expr>           UsingStmt
%type <m_expr>           VarStmt
%type <m_varinfo>        VarDecl
%type <m_varinfos>       VarDeclList
%type <m_varname>        VarName
%type <m_varnames>       VarNames
%type <m_varnames>       VarNames2
%type <m_expr>           WhileStmt

%%

Program:
    DeclarationList2
      { yyparsetree = $1; }
  ;

Block:
    StatementList
      { $$ = GenerateBlock($1); }
  ;

StatementList:
    Statement
      { $$ = BisonArray<Object *>::New($1); }
  | StatementList ';' Statement
      { $$ = $1.Append($3); }
  ;

Statement:
    /* empty */
      { $$ = new Literal(yylval.m_token->GetLocation(),
                         Value::New(Type::Void())); }
  | StatementNoAttrs
      { $$ = $1; }
  | Attributes StatementNoAttrs
      {
        AttributeList *al = new AttributeList($1[0].m_start->GetLocation(), $1);
        if (auto e = dyn_cast<Node *>($2))
          if (auto vd = dyn_cast<VarDecl *>(e))
            SetVarDeclAttributes(vd, al), $$ = $2;
          else
            $$ = new Attributes(al, e);
        else if (auto e = dyn_cast<::Entity *>($2))
          e->SetAttributes(al), $$ = e;
        else
          verify(false);
      }
  | error
      { $$ = new Literal(yylval.m_token->GetLocation(),
                         Value::New(Type::Void())); }
  ;

StatementNoAttrs:
    UnlabeledStatement
      { $$ = $1; }
  | CompoundStatement
      { $$ = new Label($1->m_location, nullptr, $1); }
  | t_label CompoundStatement
      { auto label = safe_cast<String *>($1->m_value);
        $$ = new Label($1->GetLocation(), label, $2); }
  ;

LabelOpt:
    /* empty */
      { $$ = nullptr; }
  | t_label2
      { $$ = $1; }
  ;

UnlabeledStatement:
    ClassDecl
      { $$ = $1; }
  | ConstStmt
      { $$ = $1; }
  | ConstructStmt
      { $$ = $1; }
  | DestructStmt
      { $$ = $1; }
  | ExitStmt
      { $$ = $1; }
  | FuncDecl
      { $$ = $1; }
  | MacroDecl
      { $$ = $1; }
  | NextStmt
      { $$ = $1; }
  | ReturnStmt
      { $$ = $1; }
  | ThrowStmt
      { $$ = $1; }
  | TypeStmt
      { $$ = $1; }
  | UsingStmt
      { $$ = $1; }
  | VarStmt
      { $$ = $1; }
  | Expr
      { $$ = $1; }
  | Expr '=' Expr
      { $$ = new Apply($2->GetLocation(), binop::assign, $1, $3); }
  | Expr op_addassign Expr
      { $$ = new Apply($2->GetLocation(), binop::addassign, $1, $3); }
  | Expr op_subassign Expr
      { $$ = new Apply($2->GetLocation(), binop::subassign, $1, $3); }
  | Expr op_multassign Expr
      { $$ = new Apply($2->GetLocation(), binop::mulassign, $1, $3); }
  | Expr op_divassign Expr
      { $$ = new Apply($2->GetLocation(), binop::divassign, $1, $3); }
  | Expr op_modassign Expr
      { $$ = new Apply($2->GetLocation(), binop::modassign, $1, $3); }
  | Expr op_bitandassign Expr
      { $$ = new Apply($2->GetLocation(), binop::bitandassign, $1, $3); }
  | Expr op_bitorassign Expr
      { $$ = new Apply($2->GetLocation(), binop::bitorassign, $1, $3); }
  | Expr op_bitxorassign Expr
      { $$ = new Apply($2->GetLocation(), binop::bitxorassign, $1, $3); }
  | Expr op_lshiftassign Expr
      { $$ = new Apply($2->GetLocation(), binop::lshiftassign, $1, $3); }
  | Expr op_rshiftassign Expr
      { $$ = new Apply($2->GetLocation(), binop::rshiftassign, $1, $3); }
  ;

CompoundStatement:
    CatchStmt
      { $$ = $1; }
  | ForStmt
      { $$ = $1; }
  | IfStmt
      { $$ = $1; }
  | SwitchStmt
      { $$ = $1; }
  | WhileStmt
      { $$ = $1; }
  ;

ThrowStmt:
    kw_throw ExprOpt
      { verify(false); /* TODO */ }
  ;

ExitStmt:
    kw_exit LabelOpt ExprOpt IfOpt
      { $$ = new Transfer($1->GetLocation(), tk_exit, $3, $2, $4); }
  ;

LoopElseOpt:
    /* empty */ %prec op_lowest_precedence
      { $$ = nullptr; }
  | kw_else '{' Block '}'
      { $$ = $3; }
  ;

CatchStmt:
    kw_catch Identifier '{' Block '}'
      { verify(false); }
  | kw_catch ':' Expr '{' Block '}'
      { verify(false); }
  | kw_catch Identifier ':' Expr '{' Block '}'
      { verify(false); }
  ;

ForStmt:
    kw_for VarNames kw_in Expr ForExtra '{' Block '}' LoopElseOpt
      { $$ = new For($1->GetLocation(), nullptr, $2, $4, $5, $7, $9); }
  | kw_for Identifier VarNames kw_in Expr ForExtra '{' Block '}' LoopElseOpt
      { $$ = new For($1->GetLocation(), $2, $3, $5, $6, $8, $10); }
  ;

ForExtra:
    /* empty */
      { $$ = nullptr; }
  | ':' Expr
      { $$ = $2; }
  ;

IfOpt:
    /* empty */
      { $$ = nullptr; }
  | kw_if Expr
      { $$ = $2; }
  ;

ConstStmt:
    kw_const Identifier
      { $$ = new Const($2, false, nullptr); }
  | kw_const Identifier '=' Expr
      { $$ = new Const($2, false, new Expr($4)); }
  | kw_const '@' Identifier '=' Expr
      { verify(false); } // What if Identifier is a name macro?
  ;

TypeStmt:
    kw_type Identifier
      { $$ = new Const($2, true, nullptr); }
  | kw_type Identifier '=' Expr
      { $$ = new Const($2, true, new Expr($4)); }
  | TemplateClause kw_type Identifier SpecializationOpt
      { Entity *e = new Const($3, true, nullptr);
        $$ = new Specialization(e, $1, $4); }
  | TemplateClause kw_type Identifier SpecializationOpt '=' Expr
      { Entity *e = new Const($3, true, new Expr($6));
        $$ = new Specialization(e, $1, $4); }
  ;

IfStmt:
    kw_if Expr '{' Block '}' ElseIfList ElseOpt
      { IfClause main { $1, $2, $4 };
        $$ = new If(main, $6, $7); }
  ;

ElseIfList:
    /* empty */
      { $$ = BisonArray<IfClause>::New(); }
  | ElseIfList ElseIf
      { $$ = $1.Append($2); }
  ;

ElseIf:
    kw_else kw_if Expr '{' Block '}'
      { $$ = { $1, $3, $5 }; }
  ;

ElseOpt:
    /* empty */ %prec op_lowest_precedence
      { $$ = { nullptr, nullptr, nullptr }; }
  | kw_else '{' Block '}'
      { $$ = { $1, nullptr, $3 }; }
  ;

NextStmt:
    kw_next LabelOpt IfOpt
      { $$ = new Transfer($1->GetLocation(), tk_next, nullptr, $2, $3); }
  ;

ReturnStmt:
    kw_return LabelOpt ExprOpt IfOpt
      { $$ = new Transfer($1->GetLocation(), tk_return, $3, $2, $4); }
  ;

SwitchStmt:
    kw_switch Expr '{' Cases '}'
      { verify(false); /* TODO */ }
  ;

Cases:
    Case
  | Cases Case
  ;

Case:
    kw_case ExprList ':' Block
  | kw_default ':' Block
  ;

UsingStmt:
    kw_using kw_namespace NamespaceSpecifier
      { verify(false); /* TODO */ }
  | kw_using Expr
      { verify(false); /* TODO */ }
  ;

VarStmt:
    kw_var VarDeclList
      { $$ = ProcessVarDecls($2); }
  ;

VarDeclList:
    VarDecl
      { $$ = BisonArray<VarInfo>::New($1); }
  | VarDeclList ',' VarDecl
      { $$ = $1.Append($3); }
  ;

VarDecl:
    VarNames ':' Expr
      { $$ = { $1, $3, nullptr }; }
  | VarNames '=' Expr
      { $$ = { $1, nullptr, $3 }; }
  | VarNames ':' Expr '=' Expr
      { $$ = { $1, $3, $5 }; }
  ;

VarNames:
    VarName
      { $$ = BisonArray<VarName>::New($1); }
  | '(' VarNames2 ')'
      { $$ = $2; }
  ;

VarNames2:
    VarName
      { $$ = BisonArray<VarName>::New($1); }
  | VarNames2 ',' VarName
      { $$ = $1.Append($3); }
  ;

VarName:
    Identifier
      { $$ = { $1, false, false }; }
  | Identifier kw_const
      { $$ = { $1, true, false }; }
  | Identifier op_ref
      { $$ = { $1, false, true }; }
  | Identifier kw_const op_ref
      { $$ = { $1, true, true }; }
  ;

WhileStmt:
    kw_while Expr '{' Block '}' LoopElseOpt
      { $$ = new While($1->GetLocation(), $2, $4, $6); }
  ;

ConstructWhat:
    Identifier
      { $$ = new Ident($1); }
  | kw_this
      { $$ = new Ident($1); }
  | kw_base '[' Expr ']'
      { $$ = new Ident($1);
        $$ = new Apply($2->GetLocation(), binop::subscript, $$, $3); }
  ;

ConstructStmt:
    t_construct ConstructWhat
      { $$ = new VarDecl($2, nullptr, nullptr, dm_not_on_return); }
  | t_construct ConstructWhat '(' TupleValue ')'
      { $$ = new VarDecl($2, nullptr, nullptr, $4, dm_not_on_return); }
  | t_construct ConstructWhat Identifier '(' TupleValue ')'
      { $$ = new VarDecl($2, nullptr, new Ident($3), $5, dm_not_on_return); }
  | t_construct ConstructWhat '=' Expr
      { $$ = new VarDecl($2, nullptr, $4, dm_not_on_return); }
  ;

DestructStmt:
    t_destruct Expr
      { $$ = new VarDecl($2, nullptr, nullptr, dm_is_destructor); }
  ;

ExprOpt:
    /* empty */
      { $$ = nullptr; }
  | Expr
      { $$ = $1; }
  ;

Expr:
    Identifier
      { $$ = new Ident($1); }
  | t_lambdaparm
      { $$ = new Ident($1); }
  | t_char
      { $$ = new Literal($1->GetLocation(),
                         Value::NewPseudo(Type::PseudoChar(), $1->m_value)); }
  | t_string
      { $$ = new Literal($1->GetLocation(),
                         Value::NewPseudo(Type::PseudoString(), $1->m_value)); }
  | t_integer
      { $$ = new Literal($1->GetLocation(),
                         Value::NewPseudo(Type::PseudoInteger(), $1->m_value)); }
  | t_float
      { $$ = new Literal($1->GetLocation(),
                         Value::NewPseudo(Type::PseudoFloat(), $1->m_value)); }
  | '.' Identifier
      { $$ = new Literal($1->GetLocation(),
                         Value::NewPseudo(Type::PseudoEnum(), $2->m_value)); }
  | kw_this
      { $$ = new Ident($1); }
  | kw_base
      { $$ = new Ident($1); }
  | t_label Expr %prec op_unary
      {
        if ($2->Kind() != nk_Block)
          $2 = new Block(new Scope(), $2);
        auto label = safe_cast<String *>($1->m_value);
        $$ = new Label($1->GetLocation(), label, $2);
      }
  | '-' Expr %prec op_unary
      { $$ = new Apply($1->GetLocation(), unop::neg, $2); }
  | '+' Expr %prec op_unary
      { $$ = new Apply($1->GetLocation(), unop::pos, $2); }
  | '*' Expr %prec op_unary
      { $$ = new Apply($1->GetLocation(), unop::deref, $2); }
  | '&' Expr %prec op_unary
      { $$ = new AddrOf($1->GetLocation(), $2); }
  | '~' Expr
      { $$ = new Apply($1->GetLocation(), unop::bitnot, $2); }
  | '#' Expr %prec op_unary
      { $$ = new Apply($1->GetLocation(), unop::count, $2); }
  | op_incr Expr %prec op_unary
      { $$ = new Apply($1->GetLocation(), unop::preincr, $2); }
  | op_decr Expr %prec op_unary
      { $$ = new Apply($1->GetLocation(), unop::predecr, $2); }
  | Expr op_incr
      { $$ = new Apply($2->GetLocation(), unop::postincr, $1); }
  | Expr op_decr
      { $$ = new Apply($2->GetLocation(), unop::postdecr, $1); }
  | '(' TupleValue ')'
      {
        if ($2.size() == 1 && !($2[0].m_trailingComma || $2[0].m_name))
          $$ = $2[0].m_value; // parenthesized subexpression
        else
          $$ = new List($1->GetLocation(), $2); // List value
      }
  | op_lexprfrag Expr op_rexprfrag
      { $$ = new Quote($1->GetLocation(), $2); }
  | op_lstmtfrag Block op_rstmtfrag
      { $$ = new Quote($1->GetLocation(), $2); }
  | Expr '#'
      { $$ = new Apply($2->GetLocation(), unop::macroinsert, $1); }
  | Expr op_ellipses
      { $$ = new Apply($2->GetLocation(), unop::macroiter, $1); }
  | op_range Expr %prec '.'
      { $$ = new Apply($1->GetLocation(), unop::global, $2); }
  | Expr '.' Expr
      { $$ = new Member($2->GetLocation(), $1, $3, false); }
  | Expr op_deref Expr
      { $$ = new Member($2->GetLocation(), $1, $3, true); }
  | Expr op_dotstar Expr
      { $$ = new Apply($2->GetLocation(), binop::dotstar, $1, $3); }
  | Expr op_derefstar Expr
      { $$ = new Apply($2->GetLocation(), binop::derefstar, $1, $3); }
  | Expr '!' Expr
      { $$ = new Apply($2->GetLocation(), binop::select, $1, $3); }
  | Expr '(' TupleValue ')'
      { $$ = new Apply($2->GetLocation(), binop::apply, $1, $3); }
  | Expr t_langle TupleValue t_rangle
      { $$ = new Apply($2->GetLocation(), binop::derive, $1, $3); }
  | Expr '[' TupleValue ']'
      { $$ = new Apply($2->GetLocation(), binop::subscript, $1, $3); }
  | Expr Lambda
      { $$ = new Apply($2->m_location, binop::closure, $1, $2); }
  | Expr '*' Expr
      { $$ = new Apply($2->GetLocation(), binop::mul, $1, $3); }
  | Expr '/' Expr
      { $$ = new Apply($2->GetLocation(), binop::div, $1, $3); }
  | Expr '%' Expr
      { $$ = new Apply($2->GetLocation(), binop::mod, $1, $3); }
  | Expr '+' Expr
      { $$ = new Apply($2->GetLocation(), binop::add, $1, $3); }
  | Expr op_concat Expr
      { $$ = new Apply($2->GetLocation(), binop::add, $1, $3); }
  | Expr '-' Expr
      { $$ = new Apply($2->GetLocation(), binop::sub, $1, $3); }
  | Expr '$' Expr
      { $$ = new Apply($2->GetLocation(), binop::apply, $1, $3); }
  | Expr op_lshift Expr
      { $$ = new Apply($2->GetLocation(), binop::lshift, $1, $3); }
  | Expr op_rshift Expr
      { $$ = new Apply($2->GetLocation(), binop::rshift, $1, $3); }
  | Expr '<' Expr
      { $$ = new Apply($2->GetLocation(), binop::lt, $1, $3); }
  | Expr '>' Expr
      { $$ = new Apply($2->GetLocation(), binop::gt, $1, $3); }
  | Expr op_le Expr
      { $$ = new Apply($2->GetLocation(), binop::le, $1, $3); }
  | Expr op_ge Expr
      { $$ = new Apply($2->GetLocation(), binop::ge, $1, $3); }
  | Expr op_eq Expr
      { $$ = new Apply($2->GetLocation(), binop::eq, $1, $3); }
  | Expr op_ne Expr
      { $$ = new Apply($2->GetLocation(), binop::ne, $1, $3); }
  | Expr '&' Expr
      { $$ = new Apply($2->GetLocation(), binop::bitand_, $1, $3); }
  | Expr '^' Expr
      { $$ = new Apply($2->GetLocation(), binop::bitxor, $1, $3); }
  | Expr '|' Expr
      { $$ = new Apply($2->GetLocation(), binop::bitor_, $1, $3); }
  | kw_new Expr %prec op_new_delete
      { $$ = new New($1->GetLocation(), nullptr, $2); }
  | kw_new2 '[' TupleValue ']' Expr %prec op_new_delete
      { $$ = new New($1->GetLocation(), $3, $5); }
  | kw_delete Expr %prec op_new_delete
      { $$ = new Delete($1->GetLocation(), nullptr, $2); }
  | kw_delete2 '[' TupleValue ']' Expr %prec op_new_delete
      { $$ = new Delete($1->GetLocation(), $3, $5); }
  | Expr '?' Expr ':' Expr
      { $$ = new If($2->GetLocation(), $1, $3, $5); }
  | op_not Expr
      { $$ = new Apply($1->GetLocation(), unop::boolnot, $2); }
  | Expr op_and Expr
      { $$ = new Apply($2->GetLocation(), binop::booland, $1, $3); }
  | Expr op_or Expr
      { $$ = new Apply($2->GetLocation(), binop::boolor, $1, $3); }
  | Expr kw_in Expr
      { $$ = new Apply($2->GetLocation(), binop::in, $1, $3); }
  | '$' Expr %prec op_unary
      { $$ = new TypeOf($1->GetLocation(), $2); }
  | Expr kw_const
      { $$ = new Apply($2->GetLocation(), unop::const_, $1); }
  | Expr kw_mutable
      { $$ = new Apply($2->GetLocation(), unop::mutable_, $1); }
  | Expr op_ptr
      { $$ = new Apply($2->GetLocation(), unop::ptr, $1); }
  | Expr op_ref
      { $$ = new Apply($2->GetLocation(), unop::lref, $1); }
  | Expr op_r_ref
      { $$ = new Apply($2->GetLocation(), unop::rref, $1); }
  | '[' Expr ']'
      { $$ = new Apply($1->GetLocation(), unop::set_, $2); }
  | Expr kw_throws
      { verify(false); }
  | Expr kw_throws2 Expr
      { verify(false); }
  | Expr kw_throwsq Expr
      { verify(false); }
  | kw_throwsq Expr %prec op_unary
      { verify(false); }
  | kw_try Expr
      { verify(false); }
  | kw_tryforce  Expr
      { verify(false); }
  | kw_tryopt Expr
      { verify(false); }
  | op_move ':' Expr
      { $$ = new Apply($1->GetLocation(), unop::move, $3); }
  | op_move Expr ':' Expr
      { $$ = new Apply($1->GetLocation(), binop::forward, $2, $4); }
  | Expr op_range Expr
      { $$ = new Apply($2->GetLocation(), binop::range, $1, $3); }
  | Expr kw_as Expr
      { $$ = new Apply($2->GetLocation(), binop::as, $1, $3); }
  | Expr kw_asopt Expr
      { verify(false); }
  | '{' Block '}'
      { $$ = $2; }  // FIXME?
  | Lambda
      { $$ = $1; }
  | Template
      { $$ = ProcessTemplateType($1.m_keyword->GetLocation(), $1.m_parms); }
  | kw_enum '(' EnumList ')'
      { $$ = new Enum($1->GetLocation(), $3, nullptr); }
  | Expr kw_enum '(' EnumList ')'
      { $$ = new Enum($2->GetLocation(), $4, $1); }
  | kw_union '(' TupleList ')'
      { $$ = new Union($1->GetLocation(), $3); }
  | op_arrow Expr
      { verify(false); }
  | Expr op_arrow Expr
      { verify(false); }
  ;

Lambda:
    t_lambda op_arrow Block '}'
      { verify(false); }
  | t_lambda t_lambdahack LambdaParms op_arrow StatementList '}'
      { verify(false); }
  | t_lambda '[' CaptureList ']' LambdaParms op_arrow StatementList '}'
      { verify(false); }
  ;

LambdaParms:
    '(' TupleValue ')' op_deref ReturnType
      { verify(false); }
  | LambdaIdents
      { verify(false); }
  | /* empty */
  ;

LambdaIdents:
    Identifier
  | LambdaIdents ',' Identifier
  ;

TupleValue:
    /* empty */
      { $$ = BisonArray<MemberItem>::Empty(); }
  | TupleList
      { $$ = $1; }
  | TupleList ','
      { $$ = $1; $$.back().m_trailingComma = true; }
  ;

TupleList:
    TupleMember
      { $$ = BisonArray<MemberItem>::New($1); }
  | TupleList ',' TupleMember
      { $$ = $1.Append($3); }
  ;

TupleMember:
    AttributesOpt Expr
      { $$ = { $2, nullptr, $1}; }
  | AttributesOpt TupleName ':' Expr
      { $$ = { $4, $2, $1}; }
  | AttributesOpt op_ellipses
      { $$ = { nullptr, nullptr, $1 }; $$.m_ellipses = $2; }
  ;

TupleName:
     Identifier
       { $$ = $1; }
   | kw_this
       { $$ = $1; }
   ;

ExprList:
    Expr
      { $$ = BisonArray<Node *>::New($1); }
  | ExprList ',' Expr
      { $$ = $1.Append($3); }
  ;

ClassDecl:
    kw_class Identifier InheritanceSpecifier DeclarationList
      { $$ = new Class($2, $3, $4); }
  | TemplateClause kw_class Identifier SpecializationOpt InheritanceSpecifier DeclarationList
      { Entity *e = new Class($3, $5, $6);
        $$ = new Specialization(e, $1, $4); }
  ;

EnumList:
    EnumMember
      { $$ = BisonArray<MemberItem>::New($1); }
  | EnumList ',' EnumMember
      { $$ = $1.Append($3); }
  ;

EnumMember:
    Identifier
      { $$ = { nullptr, $1, nullptr }; }
  | Identifier '=' Expr
      { $$ = { $3, $1, nullptr }; }
  ;

NamespaceDecl:
    kw_namespace NamespaceSpecifier DeclarationList
      { $$ = { $2, $3 }; }
  ;

NamespaceSpecifier:
    Identifier
      { $$ = BisonArray<Token *>::New($1); }
  | NamespaceSpecifier '.' Identifier
      { $$ = $1.Append($3); }
  ;

ImportDecl:
    kw_namespace NamespaceSpecifier '@' NamespaceSpecifier
      { $$ = { $2, $4 }; }
  ;

TemplateClause:
    Template
      { $$ = $1; }
    /* FIXME: add requires clause */
  ;

Template:
    kw_template t_langle t_rangle
      { $$ = { $1, BisonArray<TemplateInfo>::New() }; }
  | kw_template t_langle TemplateParams t_rangle
      { $$ = { $1, $3 }; ProcessTemplateParams($$); }
  ;

TemplateParams:
    TemplateParam
      { $$ = BisonArray<TemplateInfo>::New($1); }
  | TemplateParams ',' TemplateParam
      { $$ = $1.Append($3); }
  ;

TemplateParam:
    TemplateParamName
      { $$ = $1; }
  | TemplateParamName ':' Expr
      { $$ = $1; $$.m_type = $3; }
  | TemplateParamName '=' Expr
      { $$ = $1; $$.m_init = $3; }
  | TemplateParamName ':' Expr '=' Expr
      { $$ = $1; $$.m_type = $3; $$.m_init = $5; }
  ;

TemplateParamName:
    Identifier
      { $$ = { nullptr, $1, nullptr, false }; }
  | Identifier op_ellipses
      { $$ = { nullptr, $1, nullptr, true }; }
  ;

SpecializationOpt:
    /* empty */
      { $$ = BisonArray<Node *>::Empty(); }
  | t_langle ExprList t_rangle
      { $$ = $2; }
  ;

InheritanceSpecifier:
    /* empty */
      { $$ = BisonArray<BaseInfo>::Empty(); }
  | ':' BaseClassList
      { $$ = $2; }
  ;

BaseClassList:
    BaseSpecifier
      { $$ = BisonArray<BaseInfo>::New($1); }
  | BaseClassList ',' BaseSpecifier
      { $$ = $1.Append($3); }
  ;

BaseSpecifier:
    AttributesOpt Expr
      { $$ = { $2,  $1 }; }
  ;

DeclarationList:
    '{' DeclarationList2 '}'
      { $$ = $2; }
  ;

DeclarationList2:
    AttributesOpt Declaration
      { $2.m_attrs = $1; $$ = BisonArray<DeclInfo>::New($2); }
  | DeclarationList2 ';' AttributesOpt Declaration
      { $4.m_attrs = $3; $$ = $1.Append($4); }
  ;

Declaration:
    /* empty */
      { $$ = DeclInfo::NewEmpty(); }
  | ClassDecl
      { $$ = DeclInfo::NewNormal($1); }
  | NamespaceDecl
      { $$ = DeclInfo::NewNamespace($1); }
  | ImportDecl
      { $$ = DeclInfo::NewImport($1); }
  | UsingStmt
      { verify(false); }
  | MacroDecl
      { $$ = DeclInfo::NewNormal($1); }
  | FuncDecl
      { $$ = DeclInfo::NewNormal($1); }
  | ConsDestructDecl
      { $$ = DeclInfo::NewNormal($1); }
  | ConstStmt
      { $$ = DeclInfo::NewNormal($1); }
  | TypeStmt
      { $$ = DeclInfo::NewNormal($1); }
  | kw_var VarDeclList
      { $$ = DeclInfo::NewFields(ProcessFieldDecls($2)); }
  | ':'
      { $$ = DeclInfo::NewAttrGroup(); }
  | error
      { $$ = DeclInfo::NewEmpty(); }
  ;

AttributesOpt:
    /* empty */
      { $$ = nullptr; }
  | Attributes
      { $$ = new AttributeList($1[0].m_start->GetLocation(), $1); }
  ;

Attributes:
    Attribute
      { $$ = BisonArray<AST::AttrInfo>::New($1); }
  | Attributes Attribute
      { $$ = $1.Append($2); }
  ;

Attribute:
    '@' Identifier
      { $$ = AST::AttrInfo::NewNormal($1, new Ident($2)); }
  | '@' Identifier t_attrargs TupleValue ')'
      { Node *e = new Ident($2);
        e = new Apply($3->GetLocation(), binop::apply, e, $4);
        $$ = AST::AttrInfo::NewNormal($1, e); }
  | '@' t_attrargs Expr ')'
      { $$ = AST::AttrInfo::NewIndirect($1, $3); }
  | '@' '[' Expr '?' Attributes ']'
      { $$ = AST::AttrInfo::NewConditional($1, $3, $5, { }); }
  | '@' '[' Expr ':' Attributes ']'
      { $$ = AST::AttrInfo::NewConditional($1, $3, { }, $5); }
  | '@' '[' Expr '?' Attributes ':' Attributes ']'
      { $$ = AST::AttrInfo::NewConditional($1, $3, $5, $7); }
  ;

MacroDecl:
    kw_macro MethodDecl
      { $$ = $2; }
  ;

FuncDecl:
    kw_func MethodDecl
      { $$ = $2; }
  ;

ConsDestructDecl:
    kw_this MethodDecl
      { $$ = $2; }
  | kw_t_this MethodDecl
      { $$ = $2; }
  ;

// Because constructors and destructors cannot appear inside a method, and would
// conflict with construct/destruct statements, this hack is needed so that the
// allowed method types can be explicitly enumerated in the grammar rules.
MethodDecl:
    IdentifierOpt '(' TupleValue ')' FuncInfos ReturnTypeOpt ProcBody
      {
        MethodKind mk = $<m_token>0->m_lexeme == kw_macro     ? mk_macro
                      : $<m_token>0->m_lexeme == kw_func      ? mk_method
                      : $<m_token>0->m_lexeme == kw_this      ? mk_construct
                      :                                         mk_destruct;

        if (!$1) {
          if (mk == mk_construct)
            $1 = $<m_token>0, $1->m_value = String::Get(wks_op_apply);
          else if (mk == mk_destruct)
            $1 = $<m_token>0, $1->m_value = String::Get(wkhs_destructor);
        }

        $$ = new Method($<m_token>0->GetLocation(), mk, $1, $3, $5, $6, $7);
      }
  ;

FuncInfos:
    /* empty */
      { $$ = AST::FuncInfo::New(); }
  | FuncInfos FuncInfo
      { }
  ;

FuncInfo:
    kw_const
      { $<m_funcinfo>0.m_const = true; }
  | '&'
      { $<m_funcinfo>0.m_ref = AST::FuncInfo::ref_lvalue; }
  | op_and
      { $<m_funcinfo>0.m_ref = AST::FuncInfo::ref_rvalue; }
  | kw_throws
      { $<m_funcinfo>0.m_throws = AST::FuncInfo::fi_bool;
        $<m_funcinfo>0.m_throwsExpr = nullptr; }
  | kw_throws2 Expr
      { $<m_funcinfo>0.m_throws = AST::FuncInfo::fi_bool;
        $<m_funcinfo>0.m_throwsExpr = $2; }
  | kw_throwsq '(' Expr ')'
      { $<m_funcinfo>0.m_throws = AST::FuncInfo::fi_expr;
        $<m_funcinfo>0.m_throwsExpr = $3; }
  ;

ProcBody:
    /* empty */
      { $$ = { ProcBody::pb_none, nullptr }; }
  | '{' Block '}'
      { $$ = { ProcBody::pb_none, $2 }; }
  | '=' Expr
      { $$ = { ProcBody::pb_expr, $2 }; }
  | '=' kw_default
      { $$ = { ProcBody::pb_default, nullptr }; }
  | '=' kw_deferred
      { $$ = { ProcBody::pb_deferred, nullptr }; }
  | '=' kw_delete
      { $$ = { ProcBody::pb_delete, nullptr }; }
  ;

CaptureList:
    Capture
      { verify(false); }
  | CaptureList ',' Capture
      { verify(false); }
  ;

Capture:
    kw_this
      { verify(false); }
  | kw_mutable
      { verify(false); }
  | '&'
      { verify(false); }
  | '='
      { verify(false); }
  | Identifier
      { verify(false); }
  | Identifier '=' Expr
      { verify(false); }
  | '&' Identifier
      { verify(false); }
  | kw_throws
      { verify(false); }
  | kw_throws2 Expr
      { verify(false); }
  | kw_throwsq '(' Expr ')'
      { verify(false); }
  /* don't forget "Identifier kw_const" and "& Identifier kw_mutable" and
     similar variants!. */
  ;

ReturnType:
    AttributesOpt Expr %prec op_lowest_precedence
      { $$ = MemberItem::New(); $$.m_value = $2; $$.m_attrs = $1; }
  ;

ReturnTypeOpt:
    /* empty */
      { $$ = MemberItem::New(); }
  | op_deref ReturnType
      { $$ = $2; }
  ;

Identifier:
    t_ident
      { $$ = $1; }
  | t_ident pf_ident
      { $$ = $1;
        auto s = safe_cast<String *>($$->m_value);
        $$->m_value = s->AsNameMacro(); }
  | t_string pf_ident
      { $$ = $1; $$->m_lexeme = t_ident; }
  ;

IdentifierOpt:
    /* empty */
      { $$ = nullptr; }
  | Identifier
      { $$ = $1; }
  ;

%%

static Location g_currentLocation;

int yylex() {
  Token *t = Token::GetNextToken();

  yylval.m_token = t;
  g_currentLocation = t->GetLocation();
  return t->m_lexeme;
}

void yyerror(const char *s) {
  EmitError(g_currentLocation) << s;
}

static Node *ProcessVarDecls(SafeArray<VarInfo> infos) {
  // Only a single Expr can be appended to the StatementList as a result of
  // these declarations, even if multiple names are being declared.  This is
  // rather inconvenient.  Handle this by producing a chain of VarDecls, as
  // will eventually exist anyway.  GenerateBlock() below will have to
  // traverse this list to tack on the statements that follow.

#if 0
  // FIXME: When there's a list of variables, we only want the type expr to be
  // evaluated once.  Right now, each var in the list will do its own
  // evaluation.
#endif

  // Create a VarDecl for each variable in the list.  We must process
  // the list backwards to properly scope each variable in the final
  // tree.
  Node *chain = nullptr;
  for (uint32_t i = infos.size(); i > 0; ) {
    VarInfo &vi = infos[--i];
    verify(vi.m_name.size() == 1);
    Token *n = vi.m_name[0].m_name;

    // FIXME: what about per-decl attributes?

    Expr *e = vi.m_type ? new Expr(vi.m_type) : nullptr;
    Var *ve = new Var(n->GetLocation(), n, e);
    VarDecl *vd = new VarDecl(ve, chain, vi.m_init, dm_leaves_scope);
    chain = vd;
  }

  return chain;
}

static void SetVarDeclAttributes(VarDecl *vd, AttributeList *al) {
  // Propagate the attribute list to all the Var entities hanging off the
  // VarDecls.
  while (vd) {
    vd->m_entity->SetAttributes(al);
    vd = safe_cast<VarDecl *>(vd->m_expr);
  }
}

static BisonArray<Field *> ProcessFieldDecls(SafeArray<VarInfo> infos) {
#if 0
  // FIXME: When there's a list of variables, we only want the type expr to be
  // evaluated once.  Right now, each var in the list will do its own
  // evaluation.
#endif

  auto decls = BisonArray<Field *>::New();
  for (VarInfo &vi : infos) {
    Field *fe = new Field(vi);
    decls = decls.Append(fe);
  }

  return decls;
}

static Node *NormalizeChain(Node *chain) {
  // Normalize Sequence node chain.
  if (!chain) {
    // Empty statement list; replace with void literal.
    return new Literal({ }, Value::New(Type::Void()));
  } else {
    // Otherwise keep statement(s).
    return chain;
  }
}

static Node *GenerateBlock(SafeArray<Object *> stmts) {
  // Only parenthesized expressions or other statement lists take this form.
  // Sequences of executable statements will be turned into a chain of
  // Sequence nodes.  Declarative statements will populate a new Scope, and
  // a Block node will bind this Scope to the Sequence node chain.
  //
  // Variable declarations are a complication, as they both populate the
  // Scope *and* must be inserted into the Sequence node chain as a VarDecl,
  // so that the scope of the variables is properly tracked (essential for
  // correctly destructing variables that go out of scope).  A VarDecl will
  // always be the last node in a Sequence node chain; the statements that
  // follow the variable declaration will form a new Sequence node chain
  // that is under the immediately preceding VarDecl.
  //
  // The statements are processed in reverse order (last statement first),
  // as that simplifies the code.
  //
  // FIXME: should we have a StatementBlockEntity that wraps Block nodes, so
  // that the declarative entities have a proper parent?  Or track down the
  // nearest enclosing entity and use that?
  Scope *s = new Scope();
  Node *chain = nullptr;
  for (uint32_t i = stmts.size(); i > 0; ) {
    Object *obj = stmts[--i];

    // Get entity declarations out of the way; they get added to the scope.
    if (auto e = dyn_cast<Entity *>(obj)) {
      s->AddUndetermined(nullptr, e);
      continue;
    }

    // Handle local variables declarations and construct/destruct statements.
    auto e = safe_cast<Node *>(obj);
    if (e->Kind() == nk_VarDecl) {
      // First normalize the Sequence node chain so far.
      auto vd = safe_cast<VarDecl *>(obj);
      chain = NormalizeChain(chain);

      // Walk down the chain of VarDecls, looking for the tail end.  Stick the
      // chain so far unto the tail.
      VarDecl *last_vd = vd;
      while (last_vd->m_expr)
        last_vd = safe_cast<VarDecl *>(last_vd->m_expr);
      last_vd->m_expr = chain;
      chain = vd;

      // If locals are being declared, add them to the scope.
      if (vd->m_destructMode == dm_leaves_scope) {
        while (vd != last_vd) {
          s->AddUndetermined(nullptr, vd->m_entity);
          vd = safe_cast<VarDecl *>(vd->m_expr);
        }
        s->AddUndetermined(nullptr, vd->m_entity);
      }

      continue;
    }

    // Handle everything else.
    chain = chain ? new Sequence(e, chain, false) : e;
  }

  // Normalize the final Sequence node chain.
  chain = NormalizeChain(chain);

  // Finally, wrap the chain in a Block node if anything was declared.
  if (s->IsEmpty())
    return chain;
  else
    return new Block(s, chain);
}

namespace {
  class DerivationTypeOp: public BC::NativeFunction {
  public:
    DerivationTypeOp() : BC::NativeFunction("DerivationTypeOp") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra);
      vector<Type> ts;
      for (size_t i = 0; i < argcnt; i++)
        ts.push_back(args[i].AsType());
      Type rt = Type::Generic(Type::Tuple(ts));

      BC::Union rv;
      rv.SetType(rt);
      return rv;
    }
  };

  static DerivationTypeOp g_DerivationTypeOp;

  class DerivationType: public NativeOperator {
  public:
    DerivationType() : NativeOperator("DerivationType") { }

    virtual Node *Run(Apply *an, Node::Context &ctx) {
      CallBuiltin *cn = new CallBuiltin(an->m_location, Type::JType(),
                                        &g_DerivationTypeOp);
      for (auto arg : an->m_arguments)
        cn->AppendArg(arg);
      return cn;
    }

    Type ResolveTypes(Apply *an, Node::Context &ctx) {
      Type t = Type::JType();
      for (size_t i = 0; i < an->m_formalTypes.size(); i++) {
        if (an->m_arguments[i]->m_type.DropQualifiers().IsSubtypeOf(t) == NO)
          return Type::Suppress();
        an->m_formalTypes[i] = t;
      }
      return t;
    }

    static DerivationType s_macro;
  };

  DerivationType DerivationType::s_macro;
}

static void ProcessTemplateParams(TemplateClause &tc) {
  for (TemplateInfo& ti : tc.m_parms) {
    if (!ti.m_type) {
      verify(!ti.m_init);  // FIXME: should deduce type somewhere

      // Supply type of "type".
      ti.m_type = new Ident({ }, String::Get(wks_type));
    }
  }
}

static Node *ProcessTemplateType(Location sl, SafeArray<TemplateInfo> infos) {
  vector<Node *> args;
  for (TemplateInfo& ti : infos) {
    args.push_back(ti.m_type);
    verify(!ti.m_isVariadic); // FIXME
    verify(!ti.m_init); // FIXME: proper compilation error

    // FIXME: some kind of sanity checks for names, which are otherwise
    // ignored?
  }

  return new Apply(sl, &DerivationType::s_macro, nullptr, &args);
}
