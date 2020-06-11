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

// An If node presents a single if statement or ? operator.  A boolean operand
// selects one of two other operands for execution, which becomes the value of
// this node.

#pragma once

#include "Node.h"
#include "parser/ParserDecls.h"

class Label;

class If: public Node {
  DECLARE_NODE(If)

protected:
  void DeflateFields(Deflator &DF);
  If(Inflator &IF);

  If(AST::IfClause &ic, If *parent);

  virtual Node *ResolveType_(Context &ctx);
  virtual Node *ResolveFully_(Context &ctx);

  virtual bool Dump(BufferWriter &bw, int level, int maxlevel,
                    const char *desc, int descwidth);

public:
  If(AST::IfClause &main, AST::SafeArray<AST::IfClause> elseifs,
     AST::IfClause &els);
  If(Location sl, Node *cond, Node *t, Node *f);

  void ComputeType();

  virtual void VisitChildren(Visitor *v);

  Node               *m_condition   = nullptr;
  Node               *m_ifTrue      = nullptr;
  Node               *m_ifFalse     = nullptr;
  Label              *m_label       = nullptr;

  // Is this node from an "else if"?
  bool                m_elseif      = false;
};
