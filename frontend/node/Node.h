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

// Node is the abstract base class for classes representing nodes in an
// expression tree.

#pragma once

#include "type/Type.h"
#include "parser/Location.h"
#include "util/Object.h"
#include <string>

class BufferWriter;
class Label;
class Literal;
class Location;
class Node;
class SymbolTable;
class Value;
class VarDecl;

enum NodeKind {
  nk_AddrOf,
  nk_Apply,
  nk_Attributes,
  nk_Binary,
  nk_Block,
  nk_BuildPointer,
  nk_Call,
  nk_CallBuiltin,
  nk_Cast,
  nk_Catch,
  nk_Construct,
  nk_Delete,
  nk_Deref,
  nk_Enum,
  nk_ExtractAddress,
  nk_ExtractDescriptor,
  nk_FieldAddr,
  nk_For,
  nk_GlobalAddr,
  nk_Ident,
  nk_If,
  nk_Index,
  nk_Initializer,
  nk_Label,
  nk_List,
  nk_Literal,
  nk_LiteralAddr,
  nk_Load,
  nk_Member,
  nk_MethodBody,
  nk_New,
  nk_OverloadSetRef,
  nk_PointerCast,
  nk_Quote,
  nk_Regardless,
  nk_Sequence,
  nk_Shared,
  nk_Store,
  nk_Switch,
  nk_TempDef,
  nk_TempUse,
  nk_Transfer,
  nk_TypeHolder,
  nk_TypeOf,
  nk_Unary,
  nk_Union,
  nk_VarDecl,
  nk_VarAddr,
  nk_Vtable,
  nk_VtableCast,
  nk_VtableSlot,
  nk_While
};

enum DestructMode: char;

#define DECLARE_NODE_COMMON(N) \
public: \
  virtual NodeKind Kind(); \
private:

#define DECLARE_NODE(N) \
  DECLARE_OBJECT2(N, Node) \
  DECLARE_NODE_COMMON(N)

#define IMPLEMENT_NODE_COMMON(N) \
  NodeKind N::Kind() { return nk_##N; }

#define IMPLEMENT_NODE(N) \
  IMPLEMENT_OBJECT(N) \
  IMPLEMENT_NODE_COMMON(N)

// Helper utility to keep track of temporary variables required for a block
// of code.
class TempScope {
  VarDecl *m_head;
  VarDecl *m_tail;

public:
  TempScope() : m_head(nullptr), m_tail(nullptr) { }

  void AddTemp(VarDecl *v);

  // Insert body of temporary scope into VarDecl chain and return.
  Node *Get(Node *body);
};

// Determine address of argument value.  If the expression yielding the
// address is simple, use it directly; otherwise, create a temporary to hold
// it.
class ArgAddr {
  Node       *m_addr;
  bool        m_addLoad;

public:
  ArgAddr(TempScope &ts, Location sl, Node *addr, Type t, DestructMode dm);

  Node *GetLValue();
  Node *GetAddr();
};

class Node: public Object {
protected:
  Node(Location sl, Type t = Type())
      : m_type(t),
        m_location(sl) { }

  void DeflateFields(Deflator &DF);
  Node(Inflator &IF);

public:
  using base_t = Object;
  static Metadata s_metadata;

  struct Context {
    // Whether the value of this subtree is discarded.
    bool              m_void          = false;

    /* ================== Input above, Output below ======================== */

    // Has an error occurred during processing of the subtree.
    bool              m_error         = false;

    /* ============ Pushes generally not used by ResolveType =============== */

    struct PushVoid {
      PushVoid(Context &ctx, bool value) : m_context(ctx), m_old(ctx.m_void) {
        ctx.m_void = value;
      }
      ~PushVoid() { m_context.m_void = m_old; }

      Context        &m_context;
      bool            m_old;
    };
  };

  // Resolve the type of the expression rooted at this node--but no more.
  // Normally, it returns itself, but a node has the option of replacing itself
  // with something else.
  //
  // Note that type resolving only uses the Context to report an error
  // occurred.
  Node *ResolveType(Context &ctx);

  // Fully resolve the expression rooted at this node.  Normally, it returns
  // itself, but a node has the option of replacing itself with something else.
  Node *ResolveFully(Context &ctx);

  // Resolve expression rooted at this node to the same extent as node "to".
  Node *ResolveTo(Context &ctx, Node *to);

  bool IsTypeResolved() { return m_state != st_initial; }
  bool IsFullyResolved() { return m_state == st_fully_resolved; }

  // Evaluate an expression as a constant expression, returning a Value.
  Value *Evaluate(Type t = Type());
  Type EvaluateType();

  // Walk the subtree rooted at this node, looking for and gathering up the
  // pseudo-typed Literal nodes responsible for this node's pseudo type.
  void GatherPseudoLiterals(vector<Node *> &nodes);

  // Walk the subtree rooted at this node, looking for pseudo-typed Literal
  // nodes and lowering them to the desired type (these would've been returned
  // by GatherPseudoLiterals() above).  Returns new root of subtree.
  Node *LowerPseudos(Type toType, bool extern_C = false);

  // If this node is a special enum, return the corresponding string; else,
  // return null.
  String *GetSpecialEnum();

  // Check if this node is one of the special enum literal values, or one of
  // the equivalent enum types.
  bool IsNanOrInfinity();
  bool IsMinOrMax();
  bool IsNull();

  Node *LowerSpecialEnums(Type t);

  // Likewise for other special enums.
  Node *LowerNanOrInfinity(Type t);
  Node *LowerNull(Type t);

  // Helper function to convert a pointer into an l-value of the pointed-at.
  virtual Node *Deref();

  // Convert an l-value into a pointer r-value.  It will descend a subtree until
  // it finds a Deref node, then splices it out.  If the dereferenced pointer is
  // fat, and makeThin is true, it is converted to a thin pointer.  The
  // expression must be fully resolved, again with a few fat pointer exceptions.
  Node *AddrOf(bool makeThin);

  // Subclasses of Visitor can walk the expression tree, doing whatever it is
  // they need to do.  Each Node subclass must redefine VisitChildren()
  // if it has any.  The ChildKind enumeration allows the Node subclass
  // to inform the visitor what sort of child it is visiting.
  enum ChildKind {
    ck_noParent,
    ck_operand1,
    ck_operand2,
    ck_allocsize,
    ck_initializer,
    ck_type,
    ck_member,
    ck_condition,
    ck_forValue,
    ck_forExtra,
    ck_ifTrue,
    ck_ifFalse,
    ck_argument = 1000000
  };

  class Visitor {
  protected:
    void VisitChildren(Node *e) {
      e->VisitChildren(this);
    }

  public:
    virtual ~Visitor() { }
    virtual void Visit(Node *e, ChildKind ck) = 0;
  };

  virtual void VisitChildren(Visitor *) { }

  virtual NodeKind Kind() = 0;

  // Bind names in subtree rooted at this node.  Note: though redeclared in
  // various subclasses, this is *not* virtual.
  void BindNames(SymbolTable &st);

  // Use a condition expression.  Ensure it evaluates to a boolean r-value.
  Node *UseCondition(Context &ctx);

  // Determine the type of a compound statement, given its parts that yield a
  // value, an optional label node with associated transfers, and a type with
  // any valueless types from other parts of the statement.  It both returns
  // the joined type, and casts all other parts to that type as needed.
  static Type DetermineStatementType(Type t, const vector<Node **> parts,
                                     Label *label, bool exitsOnly = false);

  // Format a readable form of this node and its children.
  virtual bool Dump(BufferWriter &bw, int level, int maxlevel,
                    const char *desc, int descwidth) = 0;

  // Variant called directly from within debuggers.
  const char *Dump(int maxlevel);

protected:
  virtual Node *ResolveType_(Context &ctx) = 0;
  virtual Node *ResolveFully_(Context &ctx) = 0;

  bool DumpCommon(BufferWriter &bw, int level, const char *kind,
                  const char *desc, int descwidth);

  enum state_t: uint8_t { st_initial, st_type_resolved, st_fully_resolved };

  state_t             m_state         = st_initial;

public:
  // Static type of this expression.  If the expression is an l-value, it will
  // be a reference type.
  Type                m_type;

  // Source location that originated this node.
  Location            m_location;
};

// Used by gdb and lldb.
void FormatNode(Node *n, int maxlevel);

namespace messageimpl {
  inline Location GetLocation(Node *e) {
    return e->m_location;
  }
}
