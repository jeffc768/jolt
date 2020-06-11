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

// An Initializer node performs construction of raw storage for local variables
// coming into scope and objects allocated on the heap.  Handles everything from
// simple expressions to uniform initializer syntax and comprehensions.

#pragma once

#include "Node.h"
#include "parser/ParserDecls.h"

class Ident;
class VarDecl;

class Initializer: public Node {
  DECLARE_NODE(Initializer)

protected:
  void DeflateFields(Deflator &DF);
  Initializer(Inflator &IF);

  virtual Node *ResolveType_(Context &ctx);
  virtual Node *ResolveFully_(Context &ctx);

  virtual bool Dump(BufferWriter &bw, int level, int maxlevel,
                    const char *desc, int descwidth);

public:
  Initializer(Location sl, Node *type, Node *value = nullptr);
  Initializer(Location sl, Type type, Node *value, bool block,
              bool dontLower = false);
  Initializer(Location sl, Node *value, bool dummy);
  Initializer(Location sl, Ident *name, AST::SafeArray<AST::MemberItem> args);

  // Ask whether the initializer came with an explicit type, and the ability
  // to set it if it did not.
  bool NeedsType() { return !m_typeExpr && !m_type.IsKnown() && !m_autoType; }

  // If no type is set at construction, one of these must be called prior to
  // ResolveFully().
  void SetType(Type t);
  void SetAutoType();

  // Have the node resolve instead of replacing itself, so the owner can pick up
  // some stuff.
  void BlockReplacement() { m_blockReplacement = true; }

  void BindNames(Visitor *v);

  virtual void VisitChildren(Visitor *v);

  // These are for the owner to pick up if replacement on resolution is blocked.
  Node               *m_replacement   = nullptr;

  // When a reference type is being initialized with an rvalue, a temp needs to
  // be created for the rvalue.
  VarDecl            *m_rvalueTemp    = nullptr;

  // This might not be the same as the type of value produced by our
  // replacement.
  Type                m_deducedType;

private:
  // (Optional) expression yielding type being initialized.
  Node               *m_typeExpr      = nullptr;

  // (Optional) The constructor name.
  Ident              *m_name          = nullptr;

  // (Optional) Value to be copy-constructed.
  vector<Node *>      m_values;

  // Is the type determined solely by our value?
  bool                m_autoType      = false;

  // Resolve instead of replacing if this is true.
  bool                m_blockReplacement  = false;

  // When initializing a reference, don't strip off the Deref node.
  bool                m_dontLower     = false;
};
