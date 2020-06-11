// Copyright (c) 2018, Jeff Cohen
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

// An Enum node defines an enumeration type.  Once it fully resolves, it
// replaces itself with a type literal whose value is the enumeration type.
// Note that multiple Enum nodes could resolve to the same type.

#pragma once

#include "Node.h"
#include "parser/ParserDecls.h"
#include <string>

class Value;

class Enum: public Node {
  DECLARE_NODE(Enum)

protected:
  void DeflateFields(Deflator &DF);
  Enum(Inflator &IF);

  virtual Node *ResolveType_(Context &ctx);
  virtual Node *ResolveFully_(Context &ctx);

  virtual bool Dump(BufferWriter &bw, int level, int maxlevel,
                    const char *desc, int descwidth);

public:
  Enum(Location sl, AST::SafeArray<AST::MemberItem> args, Node *avType);

  virtual void VisitChildren(Visitor *v);

  // The type of member associated values.
  Type                      m_avType;

private:
  // The enumeration members.
  vector<String *>          m_names;
  vector<AttributeList *>   m_attrlists;
  vector<Value *>           m_values;

  // Computed name expressions for union tags.
  vector<String *>          m_computedNames;

  // The type expr of member associated values.
  Node                     *m_avTypeExpr        = nullptr;

  // Expressions yielding associated values.
  vector<Node *>            m_valueExprs;
};
