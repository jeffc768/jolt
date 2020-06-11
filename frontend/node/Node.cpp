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

// Note: only nodes that are created during AST translation put their children
// on the ready queue in the "created" state.  Other nodes, such as Binary node,
// are presume to be constructed with subexpressions that have already begun
// resolving.  Nodes are not immediately put on the ready queue so that any
// build attributes can be resolved first; build(false) prevents the affected
// subtree from ever getting onto the ready queue.

#include "Node.h"
#include "AddrOf.h"
#include "Binary.h"
#include "Block.h"
#include "BuildPointer.h"
#include "Cast.h"
#include "Construct.h"
#include "Deref.h"
#include "Expr.h"
#include "ExtractAddress.h"
#include "FieldAddr.h"
#include "For.h"
#include "GlobalAddr.h"
#include "Ident.h"
#include "If.h"
#include "Index.h"
#include "Initializer.h"
#include "Label.h"
#include "List.h"
#include "Literal.h"
#include "LiteralAddr.h"
#include "Load.h"
#include "MethodBody.h"
#include "Shared.h"
#include "Sequence.h"
#include "Transfer.h"
#include "Unary.h"
#include "VarAddr.h"
#include "VarDecl.h"
#include "While.h"
#include "entity/Class.h"
#include "entity/Const.h"
#include "entity/Scope.h"
#include "entity/Var.h"
#include "parser/SymbolTable.h"
#include "target/Target.h"
#include "target/TranslateClosure.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"
#include <iostream>
#include <set>

void TempScope::AddTemp(VarDecl *v) {
  if (!m_head)
    m_head = v;
  else
    m_tail->m_expr = v;
  m_tail = v;
}

Node *TempScope::Get(Node *body) {
  if (m_tail) {
    m_tail->m_expr = body;
    return m_head;
  } else {
    return body;
  }
  verify(false); // FIXME: must resolve returned tree!
}

/******************************************************************************/

ArgAddr::ArgAddr(TempScope &ts, Location sl, Node *addr, Type t,
                 DestructMode dm)
    : m_addr(addr), m_addLoad(false) {
  switch (addr->Kind()) {
    case nk_VarAddr:
    case nk_GlobalAddr:
    case nk_Vtable:
    case nk_VtableCast:
    case nk_Literal:
      return;
    case nk_FieldAddr: {
      // Field reference simple only if the object it's based on has a
      // simple address (including nested fields).
      Node *a = addr;
      do {
        auto fan = safe_cast<FieldAddr *>(a);
        switch (fan->m_address->Kind()) {
          case nk_VarAddr:
          case nk_GlobalAddr:
            return;
          case nk_FieldAddr:
            a = fan->m_address;
            break;
          default:
            a = nullptr;
            break;
        }
      } while(a);
      [[fallthrough]];
    }
    default:
      // Not simple.
      Var *ve = new Var(sl, t);
      VarDecl *vd = new VarDecl(ve, nullptr, addr, dm);
      ts.AddTemp(vd);
      ve->TrackResolution();
      m_addr = new VarAddr(sl, ve);
      m_addLoad = true;
      return;
  }
}

Node *ArgAddr::GetLValue() {
  return GetAddr()->Deref();
}

Node *ArgAddr::GetAddr() {
  if (m_addLoad)
    return new Load(m_addr->m_location, m_addr->Deref());
  else
    return m_addr;
}

/******************************************************************************/

Metadata Node::s_metadata { &Object::s_metadata };

void Node::DeflateFields(Deflator &DF) {
  DF << m_state;
  DF << m_type;
  DF << m_location;
}

Node::Node(Inflator &IF)
    : Object(IF),
      m_type(),
      m_location() {
  IF >> m_state;
  IF >> m_type;
  IF >> m_location;
}

Node *Node::ResolveType(Context &ctx) {
  if (m_state == st_initial) {
    m_state = st_type_resolved;
    return ResolveType_(ctx);
  }

  return this;
}

Node *Node::ResolveFully(Context &ctx) {
  if (m_state != st_fully_resolved) {
    Node *e = ResolveType(ctx);
    if (e->m_state != st_fully_resolved) {
      m_state = st_fully_resolved;
      e = e->ResolveFully_(ctx);
    }
    return e;
  }

  return this;
}

Value *Node::Evaluate(Type t) {
  verify(IsFullyResolved());
  verify(m_type != tk_valueless);

  // Special case metaclasses: they are singletons, so leave it an lvalue.
  if (m_type == tk_class && m_type.Class()->IsMetaclass()) {
    verify(m_type != rt_rvalue);

    // Short-circuit conversion to a type.  Expression must be trivial.
    if (t == tk_type) {
      if (Kind() == nk_Deref) {
        Node *e = safe_cast<::Deref *>(this)->m_expr;
        if (e->Kind() == nk_GlobalAddr) {
          Entity *cn = safe_cast<GlobalAddr *>(e)->m_entity;
          if (cn->Kind() == ek_Class) {
            Class *ce = static_cast<Class *>(cn);
            if (ce->IsMetaclass()) {
              verify(m_type.Class() == ce);
              Type t = ce->MetaInstance()->AsType();
              return Value::NewType(t);
            }
          }
        }
      } else if (Kind() == nk_Ident) {
        Class *ce = m_type.Class();
        Type t = ce->MetaInstance()->AsType();
        return Value::NewType(t);
      }
    }

    if (!t.IsKnown())
      t = m_type;
  }

  if (!t.IsKnown())
    t = m_type.RValue();

  // If the expression is simply a literal, short-circuit the evaluation.
  if (t.Const() == m_type.Const()) {
    if (Kind() == nk_Literal) {
      auto ln = safe_cast<Literal *>(this);
      return ln->m_value;
    } else if (Kind() == nk_LiteralAddr) {
      Value *v = safe_cast<LiteralAddr *>(this)->m_data;
      (void)v;
      verify(false); // FIXME
    } else if (Kind() == nk_Deref && m_type.IsSimpleType()) {
      Node *e = safe_cast<::Deref *>(this)->m_expr;
      if (e->Kind() == nk_GlobalAddr) {
        Entity *cn = safe_cast<GlobalAddr *>(e)->m_entity;
        if (cn->Kind() == ek_Const) {
          Const *ce = static_cast<Const *>(cn);
          if (ce->m_object)
            return ce->m_object;
        }
      }
    }
  }

  // Allocate Value to hold result of the expression.
  Value *result = Value::New(t);

  // Wrap expression inside Construct/Initializer nodes, to evaluate it and
  // put the value into the memory we allocated for it.
  Node *e = new Initializer(m_location, t, this, false);

  Value *v = Value::NewPtr(Type::PointerTo(t.Lower()), result->Address());
  Literal *ln = new Literal(m_location, v);
  e = new Construct(ln, e);

  // FIXME: if a class constructor was executed, somehow arrange for the
  // destructor to eventually get executed!

  // Make sure everything used by this expression is also translated.
  Expr *cte = new Expr(e);
  TranslateClosure tc { ep_compile, cte };
  tc.Finish();

  result = Target::Get(ep_compile)->Evaluate(tc, result, cte);
  return result;
}

Type Node::EvaluateType() {
  Value *v = Evaluate(Type::JType());
  return v->AsType();
}

namespace {
  // Walk down an expression tree that yields a pseudo type, looking for
  // Literal nodes.  When we find one, remember it.
  class FindPseudoVisitor {
    vector<Node *> &m_nodes;

  public:
    FindPseudoVisitor(vector<Node *> &n) : m_nodes(n) { }

    void Visit(Node *e) {
      // Don't descend into subtrees that are not yielding a pseudo value.
      if (e->m_type != tk_pseudo)
        return;

      switch (e->Kind()) {
        case nk_List: {
          auto ln = safe_cast<List *>(e);
          m_nodes.push_back(ln);
          return;
        }

        case nk_Literal: {
          auto ln = safe_cast<Literal *>(e);
          m_nodes.push_back(ln);
          return;
        }

        case nk_Binary: {
          auto bn = safe_cast<Binary *>(e);
          Visit(bn->m_operand1);
          Visit(bn->m_operand2);
          return;
        }

        case nk_Block: {
          auto bn = safe_cast<Block *>(e);
          Visit(bn->m_expr);
          return;
        }

        case nk_For: {
          auto fn = safe_cast<For *>(e);
          if (fn->m_else)
            Visit(fn->m_else);
          return;
        }

        case nk_If: {
          auto in = safe_cast<If *>(e);
          Visit(in->m_ifTrue);
          Visit(in->m_ifFalse);
          return;
        }

        case nk_Label: {
          auto ln = safe_cast<Label *>(e);
          for (auto tn : ln->m_transfers)
            if (tn->m_kind == tk_exit || tn->m_kind == tk_return)
              Visit(tn->m_expr);
          Visit(ln->m_expr);
          return;
        }

        case nk_MethodBody: {
          auto mbn = safe_cast<MethodBody *>(e);
          Visit(mbn->m_body);
          return;
        }

        case nk_Sequence: {
          auto sn = safe_cast<Sequence *>(e);
          Visit(sn->m_valueIs == Sequence::vi_first ? sn->m_first
                                                    : sn->m_second);
          return;
        }

        case nk_Unary: {
          auto un = safe_cast<Unary *>(e);
          Visit(un->m_operand);
          return;
        }

        case nk_VarDecl: {
          auto vdn = safe_cast<VarDecl *>(e);
          Visit(vdn->m_expr);
          return;
        }

        case nk_While: {
          auto wn = safe_cast<While *>(e);
          if (wn->m_else)
            Visit(wn->m_else);
          return;
        }

        default:
          verify(false);
          return;
      }
    }
  };
}

void Node::GatherPseudoLiterals(vector<Node *> &nodes) {
  FindPseudoVisitor lpv { nodes };
  lpv.Visit(this);
}

namespace {
  // Walk down an expression tree that yields a pseudo type, looking for
  // Literal nodes.  When we find one, lower it to the desired type.
  class LowerPseudoVisitor {
    // Type that pseudo value must be converted to.
    Type              m_type;

    // Are we lowering an argument for an extern_C function?
    bool              m_extern_C;

  public:
    LowerPseudoVisitor(Type t, bool extern_C)
        : m_type(t), m_extern_C(extern_C) { }

    Node *Visit(Node *e) {
      switch (e->Kind()) {
        case nk_Literal: {
          if (e->m_type != tk_pseudo)
            return e;
          auto ln = safe_cast<Literal *>(e);

          if ((m_extern_C || m_type == tk_pointer) &&
              ln->m_type == Type::PseudoString()) {
            // Handle string literal being lowered to a char const*.  It is
            // assumed a zero byte terminator is present.
            Type t = m_type;
            if (m_extern_C && !t.IsKnown())
              t = Type::PointerTo(Type::Char8().Const());
            ln->m_value = ln->m_value->Lower(t);
            ln->m_type = ln->m_value->ObjectType();
            t = Type::PointerTo(ln->m_type);
            e = new LiteralAddr(ln->m_location, t, ln->m_value);
            Node *idx = new Literal(e->m_location, Value::New(Type::Int()));
            e = new Index(e->m_location, e->Deref(), idx);
            return e;
          }

          ln->m_value = ln->m_value->Lower(m_type);
          ln->m_type = ln->m_value->ObjectType();
          if (m_type == rt_rvalue)
            return ln;

          Type t = Type::PointerTo(ln->m_type);
          e = new LiteralAddr(ln->m_location, t, ln->m_value);
          return e->Deref();
        }

        case nk_Binary: {
          auto bn = safe_cast<Binary *>(e);
          if (bn->m_type == tk_pseudo) {
            // If the operator is a relation, the result type is bool and
            // any pseudos in the operands have no bearing on us.
            bn->m_operand1 = Visit(bn->m_operand1);
            bn->m_operand2 = Visit(bn->m_operand2);
            bn->m_type = Type::Join(bn->m_operand1->m_type,
                                    bn->m_operand2->m_type);
          }
          return e;
        }

        case nk_Block: {
          auto bn = safe_cast<Block *>(e);
          bn->m_expr = Visit(bn->m_expr);
          bn->ComputeType();
          return e;
        }

        case nk_For: {
          auto fn = safe_cast<For *>(e);
          if (fn->m_else)
            fn->m_else = Visit(fn->m_else);
          fn->ComputeType();
          return e;
        }

        case nk_If: {
          auto in = safe_cast<If *>(e);
          in->m_ifTrue = Visit(in->m_ifTrue);
          in->m_ifFalse = Visit(in->m_ifFalse);
          if (!in->m_elseif)
            in->ComputeType();
          return e;
        }

        case nk_Label: {
          auto ln = safe_cast<Label *>(e);
          for (auto tn : ln->m_transfers) {
            if (tn->m_kind == tk_exit || tn->m_kind == tk_return) {
              tn->m_expr = Visit(tn->m_expr);
              tn->ComputeType();
            }
          }
          ln->m_expr = Visit(ln->m_expr);
          ln->m_type = ln->m_expr->m_type;
          return e;
        }

        case nk_MethodBody: {
          auto mbn = safe_cast<MethodBody *>(e);
          mbn->m_body = Visit(mbn->m_body);
          return e;
        }

        case nk_Sequence: {
          auto sn = safe_cast<Sequence *>(e);
          if (sn->m_valueIs == Sequence::vi_first)
            sn->m_first = Visit(sn->m_first);
          else
            sn->m_second = Visit(sn->m_second);
          sn->ComputeType();
          return e;
        }

        case nk_Unary: {
          auto un = safe_cast<Unary *>(e);
          un->m_operand = Visit(un->m_operand);
          un->m_type = Type::Join(un->m_operand->m_type, un->m_operand->m_type);
          return e;
        }

        case nk_VarDecl: {
          auto vdn = safe_cast<VarDecl *>(e);
          vdn->m_expr = Visit(vdn->m_expr);
          vdn->m_type = vdn->m_expr->m_type;
          return e;
        }

        case nk_While: {
          auto wn = safe_cast<While *>(e);
          if (wn->m_else)
            wn->m_else = Visit(wn->m_else);
          wn->ComputeType();
          return e;
        }

        default:
          return e;
      }
    }
  };
}

Node *Node::LowerPseudos(Type t, bool extern_C) {
  LowerPseudoVisitor lpv { t, extern_C };
  return lpv.Visit(this);
}

Node *Node::Deref() {
  if (m_type == Type::Unbound())
    return this;
  return new ::Deref(m_location, this);
}

String *Node::GetSpecialEnum() {
  // First, check for an enum literal.
  if (auto ln = dyn_cast<Literal *>(this)) {
    if (ln->m_type == Type::PseudoEnum())
      return safe_cast<String *>(ln->m_value->As<Object *>());
  }

  // Next, check for an enum-valued subexpr, where the enum has but a single
  // member.
  if (m_type == tk_enum) {
    if (m_type.IdentCount() != 1)
      return nullptr;

    // FIXME: rule out enums with explicit values?

    return m_type.IdentAt(0);
  }

  return nullptr;
}

bool Node::IsNanOrInfinity() {
  String *ident = GetSpecialEnum();
  return ident == String::Get(wks_nan) || ident == String::Get(wks_infinity);
}

bool Node::IsMinOrMax() {
  String *ident = GetSpecialEnum();
  return ident == String::Get(wks_min) || ident == String::Get(wks_max);
}

bool Node::IsNull() {
  String *ident = GetSpecialEnum();
  return ident == String::Get(wks_null);
}

Node *Node::LowerSpecialEnums(Type t) {
  Node *n = this;

  if (n->m_type == tk_enum) {
    // We have a value of type enum(xxx); convert to a pseudo enum.  We must
    // still evaluate this subtree, as it may have side-effects.  Wrap it in a
    // Sequence node.
    Value *v = Value::NewPseudo(Type::PseudoEnum(), n->GetSpecialEnum());
    auto ln = new Literal(n->m_location, v);
    n = new Sequence(n, ln, false);

    Node::Context ctx;
    if (m_state == Node::st_fully_resolved)
      n = n->ResolveFully(ctx);
    else
      n = n->ResolveType(ctx);
  }

  return n->LowerPseudos(t);
}

namespace {
  // Walk down an expression tree that yields a dereferenced address, deleteing
  // any Deref node that potentially yields the value of the expression.
  class AddrOfVisitor {
    // Type that pseudo value must be converted to.
    bool              m_makeThin;

  public:
    AddrOfVisitor(bool mt) : m_makeThin(mt) { }

    Node *Visit(Node *e) {
      if (e->m_type == tk_valueless)
        return e;

      switch (e->Kind()) {
        case nk_Deref: {
          auto dn = safe_cast<Deref *>(e);
          if (m_makeThin && !dn->m_expr->m_type.IsThin()) {
            if (dn->m_expr->Kind() == nk_AddrOf)
              return safe_cast<AddrOf *>(dn->m_expr)->m_expr;
            else
              return new ExtractAddress(dn->m_expr);
          }
          return dn->m_expr;
        }

        case nk_Block: {
          auto bn = safe_cast<Block *>(e);
          bn->m_expr = Visit(bn->m_expr);
          bn->ComputeType();
          return e;
        }

        case nk_For: {
          auto fn = safe_cast<For *>(e);
          if (fn->m_else)
            fn->m_else = Visit(fn->m_else);
          fn->ComputeType();
          return e;
        }

        case nk_If: {
          auto in = safe_cast<If *>(e);
          in->m_ifTrue = Visit(in->m_ifTrue);
          in->m_ifFalse = Visit(in->m_ifFalse);
          if (!in->m_elseif)
            in->ComputeType();
          return e;
        }

        case nk_Label: {
          auto ln = safe_cast<Label *>(e);
          for (auto tn : ln->m_transfers) {
            if (tn->m_kind == tk_exit || tn->m_kind == tk_return) {
              tn->m_expr = Visit(tn->m_expr);
              tn->ComputeType();
            }
          }
          ln->m_expr = Visit(ln->m_expr);
          ln->m_type = ln->m_expr->m_type;
          return e;
        }

        case nk_MethodBody: {
          auto mbn = safe_cast<MethodBody *>(e);
          mbn->m_body = Visit(mbn->m_body);
          return e;
        }

        case nk_Sequence: {
          auto sn = safe_cast<Sequence *>(e);
          if (sn->m_valueIs == Sequence::vi_first)
            sn->m_first = Visit(sn->m_first);
          else
            sn->m_second = Visit(sn->m_second);
          sn->ComputeType();
          return e;
        }

        case nk_VarDecl: {
          auto vdn = safe_cast<VarDecl *>(e);
          vdn->m_expr = Visit(vdn->m_expr);
          vdn->m_type = vdn->m_expr->m_type;
          return e;
        }

        case nk_While: {
          auto wn = safe_cast<While *>(e);
          if (wn->m_else)
            wn->m_else = Visit(wn->m_else);
          wn->ComputeType();
          return e;
        }

        default:
          verify(e->m_type == Type::Suppress());
          return e;
      }
    }
  };
}

Node *Node::AddrOf(bool makeThin) {
  verify(IsFullyResolved());
  AddrOfVisitor aov { makeThin };
  return aov.Visit(this);
}

namespace {
  // Walk the expression tree, binding all names that can be bound right now.
  class BindNamesVisitor: public Node::Visitor {
    SymbolTable                &m_symtab;

  public:
    BindNamesVisitor(SymbolTable &st) : m_symtab(st) { }

    virtual void Visit(Node *e, Node::ChildKind ck) {
      switch (e->Kind()) {
        case nk_Block: {
          auto bn = safe_cast<Block *>(e);
          SymbolTable::PushScope scope(m_symtab);
          m_symtab.AddSymbols(bn->m_scope);
          bn->m_scope->BindNamesInAttributes(m_symtab);
          bn->m_scope->BindNames(m_symtab, [](Entity *e) {
            // Arguments were already bound; doing it again, besides redundant,
            // will break the use of "this" in argument types.
            return e->Kind() != ek_Argument;
          });
          e->VisitChildren(this);
          break;
        }

        case nk_For: {
          auto fn = safe_cast<For *>(e);
          fn->BindNames(this, m_symtab);
          break;
        }

        case nk_Ident: {
          auto in = safe_cast<Ident *>(e);
          in->BindNames(m_symtab, ck);
          e->VisitChildren(this);
          break;
        }

        case nk_Initializer: {
          auto in = safe_cast<Initializer *>(e);
          in->BindNames(this);
          break;
        }

        case nk_Label: {
          auto ln = safe_cast<Label *>(e);
          SymbolTable::PushLabel label(m_symtab, ln);
          if (!m_symtab.IsRedo())
            *ln->GetTemplateRootHolder() = m_symtab.CurrentTemplate();
          e->VisitChildren(this);
          break;
        }

        case nk_Shared:
          // Do not descend into shared subtrees.
          if (!m_symtab.IsRedo()) {
            auto sn = safe_cast<Shared *>(e);
            *sn->GetTemplateRootHolder() = m_symtab.CurrentTemplate();
          }
          break;

        case nk_Transfer: {
          auto tn = safe_cast<Transfer *>(e);
          tn->BindNames(m_symtab, this);
          break;
        }

        default:
          e->VisitChildren(this);
          break;
      }
    }
  };

  // Helper utility to merge type information for block nodes.
  class TypeMerger {
    Type     m_type;
    bool     m_makeRValue;

  public:
    TypeMerger() : m_type(), m_makeRValue(false) { }

    void Merge(Type t) {
      // Ignore paths that never complete.
      if (t != Type::Transfers()) {
        if (m_type.IsKnown()) {
          // If this path has a different l-value type, then all paths will
          // need to be converted to r-values.
          if (m_type != rt_rvalue) {
            if (m_type.Mutable() != t.Mutable())
              m_makeRValue = true;
          } else if (t != rt_rvalue) {
            m_makeRValue = true;
          }

          // Merge with existing type.
          m_type = Type::Join(m_type, t);
        } else {
          // First non-transfer type seen.
          m_type = t;
        }
      }
    }

    bool MakeRValue() { return m_makeRValue; }

    void Complete(Type &t) {
      if (!m_type.IsKnown())
        m_type = Type::Transfers();
      if (m_makeRValue)
        m_type = m_type.RValue();
      t = m_type;
    }
  };
}

void Node::BindNames(SymbolTable &st) {
  BindNamesVisitor v(st);
  v.Visit(this, ck_noParent);
}

Node *Node::UseCondition(Context &ctx) {
  // FIXME: Allow conversions to bool.  Ensure that new code is idempotent, as
  // this can be re-entered even after work is done.

  Node *e = ResolveType(ctx);

  if (e->m_type.DropQualifiers() != Type::Bool()) {
    if (e->m_type != tk_valueless ||
        e->m_type == Type::NotTyped()) {
      e->m_type = Type::Suppress();
      ctx.m_error = true;
      EmitError(e) << "Condition is not a boolean.";
    }
  } else if (e->m_type.Mutable() != Type::Bool()) {
    e = new Initializer(e->m_location, Type::Bool(), e, false);
    e = e->ResolveType(ctx);
  }

  return e;
}

Type Node::DetermineStatementType(Type t, const vector<Node **> parts,
                                  Label *label, bool exitsOnly) {
  // First join all the types.
  for (Node **e : parts)
    t = Type::Join(t, (*e)->m_type);
  if (label) {
    for (Transfer *tn : label->m_transfers)
      if (!exitsOnly || tn->m_kind == tk_exit)
        t = Type::Join(t, tn->m_exprType);
  }

  if (t == tk_valueless)
    return t;

  // Wrap the subexpression so as to suitable cast to the desired type.  While
  // Initializer nodes can handle this for us, we can't use it on types that
  // literals can resolve to, because when that happens it's too late for new
  // Initializer nodes to resolve.
  auto wrap = [t](Node *e) -> Node * {
    if (t == e->m_type)
      return e;

    if (t == tk_integer || t == tk_float) {
      if (e->IsFullyResolved()) {
        if (e->m_type != rt_rvalue)
          e = new Load(e->m_location, e);
        return new Cast(e->m_location, t, e);
      }
    } else if (t == tk_tuple) {
      verify(false);
    } else if (t == tk_enum) {
      verify(false);
    } else if (t == tk_array && !t.IndexType().IsKnown()) {
      if (e->m_type.IndexType().IsKnown()) {
        auto *dn = safe_cast<::Deref *>(e);
        Value *sz = Value::NewInt(Type::IntPtrT(),
                                  e->m_type.IndexType().Cardinality());
        Literal *lv = new Literal(dn->m_location, sz);
        Type pt = Type::PointerTo(t.RValue());
        dn->m_expr = new BuildPointer(pt, dn->m_expr, lv);
        dn->m_type = dn->m_expr->m_type.BaseType().LValueRef();
        return dn;
      }
    } else if (t == tk_pseudo) {
      // Eventually, the pseudo will be lowered to a real type and we'll be
      // back here again.  That's when we'll deal with the potential cast.
      return e;
    }

    return new Initializer(e->m_location, t, e, false, true);
  };

  // Create Initializer nodes for everything, which will cast as needed.
  // Note: Initializer strips off a Deref when initializing a reference, so we
  // need to add it back.
  for (Node **e : parts) {
    if ((*e)->m_type != Type::Transfers())
      *e = wrap(*e);
  }

  if (label) {
    for (Transfer *tn : label->m_transfers) {
      if (!exitsOnly || tn->m_kind == tk_exit) {
        tn->m_exprType = t;
        tn->m_expr = wrap(tn->m_expr);
      }
    }
  }

  return t;
}

const char *Node::Dump(int maxlevel) {
  DebugBuffer db;
  Dump(db, 0, maxlevel, nullptr, 0);
  return db.Get();
}

bool Node::DumpCommon(BufferWriter &bw, int level, const char *kind,
                      const char *desc, int descwidth) {
  for (int i = 0; i < level; i++)
    if (bw.Append("   ", 3))
      return true;

  if (desc) {
    if (bw.Append('['))
      return true;

    int len = (int)strlen(desc);
    if (bw.Append(desc, len))
      return true;

    if (bw.Append("] ", 2))
      return true;

    for (int i = len; i < descwidth; i++)
      if (bw.Append(' '))
        return true;
  }

  if (bw.Append(kind, strlen(kind)))
    return true;

  if (m_state != st_fully_resolved)
    if (bw.Append(m_state == st_initial ? '*' : '-'))
      return true;

  if (bw.Append(' '))
    return true;

  if (m_type.Dump(bw))
    return true;

  if (m_location) {
    if (bw.Append(' '))
      return true;

    if (m_location.Dump(bw))
      return true;
  }

  return false;
}

void FormatNode(Node *n, int maxlevel) {
  std::cout << n->Dump(maxlevel);
}
