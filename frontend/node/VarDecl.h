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

// A VarDecl node represents storage for a variable on the stack frame,
// allocated for the duration of VarDecl node's operands.  It knows the
// variable's type, and hence knows how big it is and the destructor to call
// when it goes out of scope.  The first operand is an expression tree that
// constructs the variable, and the second is the code that has the constructed
// variable in its scope.  An exception thrown from the first operand does not
// cause destruction of the variable, as it was never properly constructed in
// the first place.

#pragma once

#include "Node.h"
#include "entity/Entity.h"
#include "parser/ParserDecls.h"

class Ident;
class Initializer;
class Token;
class Var;

enum DestructMode: char {
  dm_invalid,
  dm_leaves_scope,      // When var goes out of scope for any reason
  dm_leaves_full_expr,  // Like above, but delayed until end of full expression
  dm_not_on_return,     // For any reason except ordinary method return
  dm_is_destructor,     // "initializer" is actually a destructor call
  dm_never              // Never destruct
};

// FIXME: VarDecls are pointed by other than their parents!  It can't replace
// itself with something else.  Make sure of that.

class VarDecl: public Node {
  DECLARE_NODE(VarDecl)

protected:
  void DeflateFields(Deflator &DF);
  VarDecl(Inflator &IF);

  virtual Node *ResolveType_(Context &ctx);
  virtual Node *ResolveFully_(Context &ctx);

  virtual bool Dump(BufferWriter &bw, int level, int maxlevel,
                    const char *desc, int descwidth);

public:
  // Passed as init argument to constructor to inhibit any kind of
  // initialization.
  static Node *DONT_INIT;

  // Passed as init argument to an implicit constructor to use the field
  // initializer.
  static Node *USE_FIELD_INIT;

  // Variant for constructor/destructors.
  VarDecl(Node *field, Node *e, Node *init, DestructMode dm);
  VarDecl(Node *field, Node *e, Ident *cons,
          AST::SafeArray<AST::MemberItem> args, DestructMode dm);

  // Variant for temps and variable declarations.
  VarDecl(Var *ve, Node *e, Node *init, DestructMode dm);

  void SetInitExpr(Node *e);

  virtual void VisitChildren(Visitor *v);

  // Note: "constructor" means both constructor and destructor.
  bool IsConstructor() { return m_field != nullptr; }
  void SetLegalConstructor();
  void SetGenerated();
  Node *ResolveField(Context &ctx);

  bool NeedsDestructing(bool inException) {
    return m_destructor &&
      (m_destructMode == dm_leaves_scope ||
       m_destructMode == dm_leaves_full_expr ||
       (m_destructMode == dm_not_on_return && inException));
  }

  Type VariableType();

  Node               *m_initexpr    = nullptr; // Constructor
  Node               *m_expr        = nullptr; // Node in variable's scope
  Node               *m_field       = nullptr; // Constructed field
  Entity             *m_entity      = nullptr; // Corresponding Entity
  VarDecl            *m_rvalueTemp  = nullptr; // For initing lvalue w/rvalue
  Node               *m_destructor  = nullptr; // Destructor (if type has one)
  DestructMode        m_destructMode  = dm_invalid;

private:
  Initializer        *m_initializer                 = nullptr;
  bool                m_constructorMode             = false;
  bool                m_generated                   = false;
  bool                m_dontInitialize              = false;
  bool                m_autoType                    = false;
};
