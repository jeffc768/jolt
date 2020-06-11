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

// A Member node represents the Jolt "." operator.  It eventually replaces
// itself with whatever the member represents.

#pragma once

#include "Node.h"

class Entity;
class Ident;
class NativeOperator;
class String;

enum WellKnownString: int;

class Member: public Node {
  DECLARE_NODE(Member)

  // These are implemented in various files under type/*.cpp.
  Node *HandleBools(Context &ctx);
  Node *HandleChars(Context &ctx);
  Node *HandleEnums(Context &ctx);
  Node *HandleTypes(Context &ctx);
  Node *HandlePointers(Context &ctx);
  Node *HandleArrays(Context &ctx);
  Node *HandleSets(Context &ctx);
  Node *HandleFloats(Context &ctx);
  Node *HandleIntegers(Context &ctx);
  Node *HandleGenerics(Context &ctx);
  Node *HandleTuples(Context &ctx);
  Node *HandleVoids(Context &ctx);
  Node *HandleCodeFragments(Context &ctx);

protected:
  void DeflateFields(Deflator &DF);
  Member(Inflator &IF);

  virtual Node *ResolveType_(Context &ctx);
  virtual Node *ResolveFully_(Context &ctx);

  virtual bool Dump(BufferWriter &bw, int level, int maxlevel,
                    const char *desc, int descwidth);

public:
  Member(Location sl, Node *object, Node *expr, bool isDeref);
  Member(Location sl, Node *object, String *member, bool op);
  Member(Location sl, Node *object, WellKnownString member, bool op);
  Member(Location sl, Node *object, bool op, Ident *member);
  Member(Location sl, Node *object, Ident *member, NativeOperator *op);

  void ReplaceObjectWithEntityRef(Entity *e);

  virtual void VisitChildren(Visitor *v);

  Node               *m_object        = nullptr;
  Node               *m_expr          = nullptr;
  Ident              *m_member        = nullptr;
  Entity             *m_entity        = nullptr;

  // Must the object be dereferenced?  (x->y rather than x.y)
  bool                m_deref         = false;

  // The member is an operator, which may or may not actually be a member.
  // Do not report an error if it isn't; a global definitions will be
  // considered.
  bool                m_operator      = false;

  // For operators, duplicate the member so that it can be bound as a global,
  // in case it doesnt exist as a class member.
  Ident              *m_global        = nullptr;

  // If this node encounters a reason why it cannot replace itself, the reason
  // goes here.  Only relevant upon full resolution.
  enum Reason {
    r_not_found,        // No member by that name
    r_try_operator,     // Same as above, but error suppressed
    r_builtin           // Run supplied native operator macro
  };
  Reason              m_reason        = r_not_found;

  // When we encounter a built-in operation, like adding integer subranges,
  // we resolve to a native macro for Apply node to execute.
  NativeOperator     *m_macro         = nullptr;
};
