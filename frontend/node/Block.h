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

// A Block node represents a lexical scope introduced by a compound statement,
// a parenthesized list of statements, or by some method calls.  It keeps track
// of local variables and entities declared within it.

#pragma once

#include "Node.h"

class Label;
class Scope;

class Block: public Node {
  DECLARE_NODE(Block)

protected:
  void DeflateFields(Deflator &DF);
  Block(Inflator &IF);

  virtual Node *ResolveType_(Context &ctx);
  virtual Node *ResolveFully_(Context &ctx);

  virtual bool Dump(BufferWriter &bw, int level, int maxlevel,
                    const char *desc, int descwidth);

public:
  // Wrap expression e in a new scope s.
  Block(Scope *s, Node *e);

  void ComputeType();

  virtual void VisitChildren(Visitor *v);

  Scope              *m_scope       = nullptr;
  Label              *m_label       = nullptr;
  Node               *m_expr        = nullptr;
};
