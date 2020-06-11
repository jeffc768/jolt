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

// A Label node presents a block statement label, it's sole operand being the
// expression tree of the block so labeled.  The label may be refereced only
// from within that operand, including any nested methods.  Every method itself
// gets a Label node, allowing it to be targeted by Transfer nodes representing
// return statements.

#pragma once

#include "Node.h"

class Transfer;

class Label: public Node {
  DECLARE_NODE(Label)

  // Nearest enclosing template root.
  Parameters         *m_templateRoot  = nullptr;

protected:
  void DeflateFields(Deflator &DF);
  Label(Inflator &IF);

  virtual Node *ResolveType_(Context &ctx);
  virtual Node *ResolveFully_(Context &ctx);

  virtual bool Dump(BufferWriter &bw, int level, int maxlevel,
                    const char *desc, int descwidth);

public:
  enum LabelKind { lk_method, lk_block, lk_loop };

  Label(Location sl, String *name, Node *e);

  void AddTransfer(Transfer *tn);

  virtual void VisitChildren(Visitor *v);

  virtual Parameters **GetTemplateRootHolder();

  bool IsNextLegal() { return m_kind == lk_loop; }
  bool IsExitLegal() { return m_kind != lk_method; }
  bool IsReturnLegal() { return m_kind == lk_method; }

  bool DefinesLabel(String *label) {
    return m_kind != lk_method && m_label == label;
  }

  bool IsMethod(String *name) {
    return m_kind == lk_method && m_label == name;
  }

  LabelKind           m_kind          = lk_method;
  String             *m_label         = nullptr;   // null for unlabeled blocks
  Node               *m_expr          = nullptr;
  vector<Transfer *> m_transfers;
};
