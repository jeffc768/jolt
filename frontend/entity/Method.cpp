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

#include "Method.h"
#include "Argument.h"
#include "AttributeList.h"
#include "BaseSpecifier.h"
#include "Class.h"
#include "Field.h"
#include "FormalArguments.h"
#include "Namespace.h"
#include "Scope.h"
#include "node/Apply.h"
#include "node/Block.h"
#include "node/Expr.h"
#include "node/FieldAddr.h"
#include "node/GlobalAddr.h"
#include "node/Ident.h"
#include "node/Label.h"
#include "node/Literal.h"
#include "node/Load.h"
#include "node/Member.h"
#include "node/MethodBody.h"
#include "node/Sequence.h"
#include "node/Transfer.h"
#include "node/VarAddr.h"
#include "node/VarDecl.h"
#include "node/Vtable.h"
#include "node/VtableCast.h"
#include "parser/SymbolTable.h"
#include "target/DependencyFinder.h"
#include "target/Target.h"
#include "target/TranslateClosure.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/SHA512.h"
#include "util/String.h"
#include "util/Value.h"

Method *Method::s_main = nullptr;

IMPLEMENT_ENTITY(Method)

Method::Method(Location sl, ::MethodKind mk, Token *name,
               AST::SafeArray<AST::MemberItem> formals, AST::FuncInfo fi,
               AST::MemberItem &rt, AST::ProcBody &body)
    : Entity(nullptr, sl, name),
      m_kind(mk),
      m_isConst(fi.m_const),
      m_formals(new FormalArguments(sl, formals, rt,
                                    body.m_keyword == AST::ProcBody::pb_expr)) {
  switch (body.m_keyword) {
    case AST::ProcBody::pb_deferred:
      m_isDeferred = true;
      break;
    case AST::ProcBody::pb_delete:
    case AST::ProcBody::pb_default:
      verify(false); // FIXME
      break;
    case AST::ProcBody::pb_expr:
    case AST::ProcBody::pb_none:
      m_body = body.m_body ? new Expr(body.m_body) : nullptr;
  }
}

void Method::SetParent(Entity *parent, AttributeList *attrGroup) {
  Entity::SetParent(parent, attrGroup);

  if (parent->Kind() == ek_Class) {
    m_formals->AddImplicitArguments(m_kind);
  } else {
    if (m_isConst) {
      EmitError(this) << "Only class methods can be declared const.";
      m_isConst = false;
    }
    if (m_isDeferred) {
      EmitError(this) << "Only class methods can be declared deferred.";
      m_isDeferred = false;
    }
  }

  if (m_body)
    BuildBody();
}

void Method::BuildBody() {
  Scope *s = new Scope();
  Node *body = m_body->Root();

  // Create the VarDecls, which wrap the method body.  Add arguments with names
  // to the scope.  Note that the arguments must be processed from right-to-
  // left so that the VarDecls are properly nested.
  if (Argument * ae = m_formals->m_returnValue) {
    DestructMode dm = dm_never;
    if (ae->m_mechanism == am_construct) {
      dm = dm_not_on_return;
      s->AddEntity(ae);  // make "obj" available
    }

    body = new VarDecl(ae, body, VarDecl::DONT_INIT, dm);
  }

  for (size_t i = m_formals->m_arguments.size(); i > 0; ) {
    Argument *ae = m_formals->m_arguments[--i];
    verify(ae->m_mechanism == am_in);

    body = new VarDecl(ae, body, VarDecl::DONT_INIT, dm_never);
    if (ae->Name())
      s->AddEntity(ae);
  }

  // Wrap with Block node containing all the above VarDecls.
  body = new Block(s, body);

  // Wrap with implicit return statement if there's a return type.
  if (m_formals->m_returnValue && m_kind != mk_construct)
    body = new Transfer(tk_return, body);

  // Wrap with MethodBody node.
  body = new MethodBody(m_formals, body);

  // Wrap with Label node providing a target for return Transfer nodes.
  body = new Label(body->m_location, Name(), body);
  m_body->Replace(body);
}

Method::Method(NewArguments &newargs)
    : Entity(newargs.m_parent, { }, newargs.m_name),
      m_kind(newargs.m_kind),
      m_nativeMethod(newargs.m_nativeMethod),
      m_body(newargs.m_body),
      m_formals(newargs.m_formals),
      m_isTranslated(newargs.m_nativeMethod != nm_none),
      m_isExecutable(newargs.m_nativeMethod != nm_none) {
  if (newargs.m_parent->Kind() == ek_Class)
    m_formals->AddImplicitArguments(m_kind);

  if (m_body)
    BuildBody();
}

Method::Method(Entity *parent, String *name, ::MethodKind mk)
    : Entity(parent, { }, name),
      m_state(st_resolved),
      m_kind(mk),
      m_inClosure(true),
      m_isTranslated(true) {
  // Created in a fully resolved state.
}

void Method::SetCoverageAndRedefines(InheritedMethods &coverage,
                                     InheritedMethods &redefines) {
  m_coverage.swap(coverage);
  m_redefines.swap(redefines);

  // If this is a placeholder or conflicted method, there isn't an argument
  // list defined.  Go and borrow one from the coverage set.  It is only used
  // for sorting method entities by signature, so this won't cause any
  // problems.
  if (m_formals) {
    verify(m_kind != mk_conflicted && m_kind != mk_placeholder);
  } else {
    verify(m_kind == mk_conflicted || m_kind == mk_placeholder);
    Method *me = m_coverage[0].m_method;
    m_formals = me->m_formals;
    m_isConst = me->m_isConst;
    m_isStatic = me->m_isStatic;
    m_isDeferred = m_redefines.size() == 0;
    for (auto &rd : m_redefines) {
      if (rd.m_method->m_isFinal) {
        m_isFinal = true;
        break;
      }
    }
  }

  // Go and add ourself to the m_redefinedBy array of the methods we redefined.
  for (auto &rd : m_redefines)
    rd.m_method->m_redefinedBy.push_back(this);
}

void Method::SetVtblSlots() {
  // Set all relevant slots for this method.  At this time, there might not
  // be a bytecode representation yet generated, in which case a null will
  // go into the slot(s).  The slot(s) will be updated if and when the method
  // is translated to bytecodes.  It ought to be impossible for the slot(s)
  // to be used before translation occurs, because the translation of any code
  // that uses this method will cause this method to be translated also (along
  // with any other methods that may use the slots in question).

  // Skip macros.
  if (m_kind == mk_macro)
    return;

  // Skip members of namespaces.
  if (Parent()->Kind() != ek_Class)
    return;

  Class *ce = static_cast<Class *>(Parent());

  // Skip non-virtual methods.
  if (!m_isVirtual)
    return;

  // FIXME: a bodyless method, especially for deferred methods, ought to put
  // something into the slot to cause an appropriate run time error.

  intptr_t func = reinterpret_cast<intptr_t>(m_compileTarget
      ? m_compileTarget->GlobalStorage(ep_compile) : nullptr);
  if (m_isIntroducing) {
    // Introducing methods have only one slot to worry about.
    if (m_vtblSlot >= 0)
      ce->SetVtblSlot(m_vtblSlot, func, this);
  } else {
    // Redefining methods potentially have many slots buried in the class
    // hierarchy to update.

    // If this is a placeholder, then at most one of inherited methods has a
    // body.  All covered slots are set to this method.
    Method *me = this;
    if (me->m_kind == mk_placeholder) {
      // If none have a body, there's nothing to fill the slots with.
      if (me->m_isDeferred)
        return;

      // Go find the one method with a body.
      for (auto &rd : m_redefines) {
        Method *r = rd.m_method;
        if (!r->m_isDeferred) {
          me = r;
          func = reinterpret_cast<intptr_t>(r->m_compileTarget
              ? r->m_compileTarget->GlobalStorage(ep_compile) : nullptr);
          break;
        }
      }
      verify(me != this);
      // FIXME: may have to create a thunk to adjust "this" and other args.
    }

    for (auto &im : m_coverage) {
      // Find the offset of the introducing method's class's vtable within
      // our own vtable.
      Class *base = ce;
      size_t offset = 0;
      for (size_t j = im.m_path.size(); j > 0; ) {
        BaseSpecifier *bs = base->GetBase(im.m_path[--j]);
        base = bs->m_baseClass;
        offset += bs->m_vtblOffset;
      }

      // Fill the slot.
      verify(im.m_method->m_vtblSlot >= 0);
      ce->SetVtblSlot(im.m_method->m_vtblSlot + offset, func, me);
    }
  }
}

void Method::DeflateFields(Deflator &DF) {
  Entity::DeflateFields(DF);
  DF << WX(m_state);
  DF << m_kind;
  DF << m_isConst;
  DF << m_formals;
  DF << m_generatedKind;

  if (m_state == st_resolved) {
    // FIXME:  For module deflations, we don't want to deflate the body for a
    // method unless it is either (1) generic, (2) usable in epoch compile,
    // (3) declared with the inline attribute.  Even if none of these apply,
    // we still might want to for sufficiently "small" methods, for some
    // definition of small, in the hopes LLVM will inline it anyway.
    //
    // Regardless, for now just deflate all method bodies.  It's a performance
    // issue, not a correctness issue--well, maybe a keeping code proprietary
    // issue too.  Anyway, unless it's used, it won't get inflated in importing
    // modules.
    DF << m_body;

    DF << m_nativeMethod;
    DF << m_isStatic;
    DF << m_isRedefining;
    DF << m_isHiding;
    DF << m_isIntroducing;
    DF << m_isDeferred;
    DF << m_isFinal;
    DF << m_isNoInherit;
    DF << m_isAutoRedefine;
    DF << m_isExternC;
    DF << m_coverage;
    DF << m_redefines;
    DF << m_redefinedBy;
    DF << m_vtblSlot;
    DF << m_isVirtual;
    DF << m_isRedefined;
    DF << m_dependencies;
  } else {
    verify(m_state == st_initial);
    DF << m_body;
  }
}

Method::Method(Inflator &IF)
    : Entity(IF) {
  IF >> WX(m_state);
  IF >> m_kind;
  IF >> m_isConst;
  IF >> m_formals;
  IF >> m_generatedKind;

  if (m_state == st_resolved) {
    IF >> m_body;
    IF >> m_nativeMethod;
    IF >> m_isStatic;
    IF >> m_isRedefining;
    IF >> m_isHiding;
    IF >> m_isIntroducing;
    IF >> m_isDeferred;
    IF >> m_isFinal;
    IF >> m_isNoInherit;
    IF >> m_isAutoRedefine;
    IF >> m_isExternC;
    IF >> m_coverage;
    IF >> m_redefines;
    IF >> m_redefinedBy;
    IF >> m_vtblSlot;
    IF >> m_isVirtual;
    IF >> m_isRedefined;
    IF >> m_dependencies;

    m_isTranslated = m_nativeMethod != nm_none;
    m_isExecutable = m_nativeMethod != nm_none;
  } else {
    IF >> m_body;
  }
}

void Method::BindNames(SymbolTable &st) {
  Entity::BindNames(st);

  SymbolTable::PushMethod method(st, this);
  m_formals->BindNames(st);

  if (m_body)
    m_body->BindNames(st);
}

void Method::Setup() {
  if (m_state != st_initial)
    return;

  // See if we're a Main method and if so remember it.
  if (strcmp(Name()->c_str(), "Main") == 0) {
    if (Parent() == Namespace::ModuleNamespace()) {
      if (s_main)
        EmitWarning1(this) << "Extra Main method ignored.";
      else
        s_main = this;
    }
  }

  // Datamine our attributes bag.
  if (auto al = Attributes()) {
    al->ResolveFully();

    Value *v;
    if ((v = al->GetAttribute(Class::wkc_InheritAttribute))) {
      switch (v->As<char>()) {
        case Class::it_final:
          m_isFinal = true;
          break;
        case Class::it_auto_redefine:
          m_isAutoRedefine = true;
          break;
        case Class::it_no_inherit:
          m_isNoInherit = true;
          break;
        case Class::it_deferred:
          m_isDeferred = true;
          m_isVirtual = true;
          break;
        default:
          verify(false);
      }
    }

    if ((v = al->GetAttribute(Class::wkc_RedefineAttribute))) {
      switch (v->As<char>()) {
        case Class::rt_introduce:
          m_isIntroducing = true;
          break;
        case Class::rt_redefine:
          m_isRedefining = true;
          break;
        case Class::rt_hide:
          m_isHiding = true;
          break;
        default:
          verify(false);
      }
    }

    if ((v = al->GetAttribute(Class::wkc_VirtualAttribute)))
      m_isVirtual = v->AsBool();

    if ((v = al->GetAttribute(Class::wkc_ExternCAttribute)))
      m_isExternC = v->AsBool();

    if (m_isDeferred && !m_isVirtual) {
      EmitWarning1(this) << "Deferred method cannot be marked non-virtual.";
      m_isVirtual = true;
    }

    if (m_isVirtual && m_kind == mk_macro) {
      m_isVirtual = false;
      EmitError(this) << "Macro cannot be virtual.";
    }

    // Note that a final method will still be virtual if it redefines a virtual
    // method; m_isVirtual will be set to true later.
    if (m_isVirtual && m_isFinal)
      m_isVirtual = false;
  }

  // Set type of "this" argument for methods.
  if (m_formals->m_hasThis) {
    auto cls = safe_cast<Class *>(Parent());
    Type t = cls->AsType().LValueRef();
    if (m_isConst)
      t = t.Const();
    m_formals->m_arguments[0]->SetType(t);
    if (m_kind == mk_construct)
      m_formals->m_returnValue->SetType(cls->MetaInstance()->AsType());
  }

  if (m_kind == mk_construct || m_kind == mk_destruct)
    m_isNoInherit = true;
  else if (Name() == wks_op_assign)
    m_isNoInherit = true;

  if (m_kind == mk_construct || m_kind == mk_destruct)
    if (!m_isHiding)
      m_isIntroducing = m_isRedefining = true;

  if (!m_isIntroducing && !m_isRedefining && !m_isHiding)
    m_isIntroducing = true;

  if (m_isExternC && m_formals->m_hasThis) {
    EmitError(this) << "Class methods may not be extern C.";
    m_isExternC = false;
  }

  if (m_isExternC) {
    if (m_body) {
      EmitError(this) << "Extern C functions may not have a body.";
      m_body = nullptr;
    }

    BypassNameMangling();
    // FIXME: diagnose multiple extern C functions with the same name,
    // regardless of which namespace they were declared in.
  } else if (m_formals->m_variadic == vt_c) {
    EmitError(this) << "C-style variadic arguments only allowed for @extern_C "
                       "functions.";
  }

  m_state = st_setup;
}

void Method::ResolveSignature() {
  if (m_state >= st_signature)
    return;

  Setup();

  m_formals->ResolveFully();

  if (m_formals->m_deducedReturnType)
    m_body->ResolveType();

  m_state = st_signature;
}

void Method::ResolveFully() {
  if (m_state == st_resolved)
    return;

  ResolveSignature();

  if (m_kind == mk_construct || m_kind == mk_destruct ||
      m_generatedKind == gm_assignment) {
    // Only constructors, destructors and assigners do anything here.  Insert
    // missing construct/destructs.

    // Gather all VarDecls representing construct/destruct statements.  As a
    // side-effect, such VarDecls are marked as legal (assuming they actually
    // are).  The point is, any other such statements, especially those
    // introduced through macros (and thus don't exist yet) will by definition
    // be illegal since they are not so marked.
    class Visitor: public Node::Visitor {
    public:
      vector<VarDecl *>   m_fields;

      Visitor() { }

      virtual void Visit(Node *e, Node::ChildKind ck) {
        // We are only interested in top-level construct/destruct statements.
        // Do not descend into conditional or looping nodes, nor into argument
        // lists, or expressions, in general.  Any that we miss as a result will
        // eventually be flagged as illegal.
        switch (e->Kind()) {
          case nk_Block:
          case nk_Label:
          case nk_MethodBody:
          case nk_Sequence:
            e->VisitChildren(this);
            break;

          case nk_VarDecl: {
            auto vd = safe_cast<VarDecl *>(e);
            if (vd->IsConstructor()) {
              m_fields.push_back(vd);
              vd->SetLegalConstructor();
            }
            Visit(vd->m_expr, Node::ck_operand1);
            break;
          }

          // FIXME: do esc_Attributes.  Handling build(false) on a construct
          // or destruct statement will be difficult, as Method will wait
          // on something that never gets put on the ready queue.

          default:
            break;
        }
      }
    };

    Visitor v;
    v.Visit(m_body->Root(), Node::ck_noParent);

    // Must wait for field VarDecls to identify their field entities.
    for (size_t i = 0; i < v.m_fields.size(); ) {
      Node::Context ctx;
      VarDecl *vd = v.m_fields[i];
      if (auto *rv = vd->ResolveField(ctx); rv == vd)
        i++;
      else
        v.m_fields.erase(v.m_fields.begin() + i);
    }

    // Also wait until our class knows its fields.
    auto ce = safe_cast<Class *>(Parent());
    ce->ResolveFields();

    if (m_generatedKind == gm_assignment) {
      InsertImplicitAssignments();
    } else {
      auto ce = safe_cast<Class *>(Parent())->MetaInstance();
      if (ce)
        ce->ResolveVtable();
      if (m_kind == mk_construct)
        InsertImplicitConstructs(v.m_fields);
      else if (m_kind == mk_destruct)
        InsertImplicitDestructs(v.m_fields);
      else
        verify(false);
    }
  }

  if (m_body)
    m_body->ResolveFully();

  m_state = st_resolved;
}

Node *Method::AsValue(Location sl) {
  // The value representation of a method entity is a reference to the
  // associated Jolt method object.  As a OverloadSet resolved an overload
  // to offer this method, our signature has been resolved and that's all we
  // need to go ahead.
  return (new GlobalAddr(sl, this, Type::Function()))->Deref();
}

void *Method::GlobalStorage(Epoch ep) {
  return GetTarget(ep)->GlobalStorage(ep);
}

const std::string &Method::GlobalStorageName() {
  return ExternalName();
}

Entity::ResolutionState Method::GetResolutionState() {
  verify(m_state == st_resolved);
  if (!m_body)
    return rs_ok;
  else if (m_body->ErrorOccurred())
    return rs_hasError;
  else if (m_body->GetType() == Type::Unbound())
    return rs_hasUnbound;
  else
    return rs_ok;
  // FIXME: need to check formal arguments also
}

void Method::AddToClosure(TranslateClosure *tc) {
  if (m_kind != mk_conflicted && m_kind != mk_placeholder) {
    // If it hasn't been already, translate the body and create the list
    // of dependencies.
    Epoch ep = tc->GetEpoch();
    GetTarget(ep);
    if (ep == ep_compile) {
      // If executable, then all dependencies, direct and indirect, are already
      // translated and executable.
      if (m_isExecutable)
        return;
      if (!IsNative() && !m_inClosure)
        SetVtblSlots();
    }

    // Register dependencies from the method body.
    if (!IsNative()) {
      if (!m_inClosure) {
        if (m_body) {
          DependencyFinder df(m_dependencies);
          df.Visit(m_body->Root(), Node::ck_operand1);
        }
        m_inClosure = true;
      }
      tc->AddToClosure(m_dependencies);
    }
  }

  // If this is a virtual method, then methods it redefines and is redefined
  // by must be added to the closure.
  if (!m_isVirtual)
    return;

  if (m_isRedefining) {
    for (auto &rd : m_redefines)
      tc->AddToClosure(rd.m_method);
  }

  // Any virtual method that redefines us must also go into the closure--if it
  // belongs to a class that's in the closure.
  for (Method *me : m_redefinedBy)
    if (tc->InClosure(safe_cast<Class *>(me->Parent())))
      tc->AddToClosure(me);

  // So what happens if a class is added to the closure after this, one that
  // redefines this method?  That class iterates all its methods looking for
  // those whose vtable slot(s) are marked used and adds them to the closure.
  //
  // What if that class is several levels removed in the hierarchy, and none
  // of the intermediate classes are in the closure?  Initially it will not
  // detect that any of its methods need to be in the closure.  But it will
  // put its base classes into the closure, and they their own base classes,
  // until a class is reached that does see the need to put some of its methods
  // into the closure.  These methods put their redefinitions into the closure,
  // and so on until the far descendant is reached.
}

void Method::FinalizeClosure(TranslateClosure *tc) {
  if (m_kind != mk_conflicted && m_kind != mk_placeholder) {
    // If it hasn't been already, translate the body and create the list
    // of dependencies.
    if (tc->GetEpoch() == ep_compile) {
      if (!IsNative() && !m_isTranslated) {
        m_compileTarget->Generate();
        m_isTranslated = true;
      }
    } else {
      m_runTarget->Generate();
    }
  }
}

void Method::SetExecutable() {
  verify(m_isTranslated);
  m_isExecutable = true;
}

int Method::SignatureCompare(Method *other) {
  if (m_isConst == other->m_isConst)
    return m_formals->Compare(other->m_formals);
  else if (m_isConst)
    return 1;
  else
    return -1;
}

VariadicType Method::Variadic() {
  return m_formals->m_variadic;
}

void Method::MakeInstance(Class *cls) {
  verify(m_formals->m_hasThis);
  verify(m_kind != mk_construct);
  verify(Parent() == cls);
}

void Method::MakeStatic(Class *metaclass) {
  verify(m_formals->m_hasThis);
  SetParent(metaclass);
  m_isStatic = true;
}

void Method::SetGenerated(GeneratedKind gk) {
  verify(m_state == st_initial);
  m_generatedKind = gk;
}

bool Method::HasNoArgs() {
  return m_formals->m_arguments.size() == size_t(m_formals->m_hasThis);
}

FormalArguments *Method::LowerFormals() {
  if (m_formals->m_nSlots >= 0)
    return m_formals;

  int slotOrd = 0;

  if (Argument *ae = m_formals->m_returnValue) {
    // If the return type is simple, it is returned directly; otherwise, a
    // pointer to the unconstructed storage reserved for the returned value is
    // passed in by value.
    if (ae->m_type.IsSimpleType())
      ae->m_isReturned = true;
    else
      ae->m_slot = slotOrd++;
  }

  for (Argument *ae : m_formals->m_arguments) {
    verify(ae->m_mechanism == am_in);
    // If the argument is a simple value, it is passed-by-value; otherwise,
    // a thin pointer to the value is passed.
    if (ae->m_type != tk_void)
      ae->m_slot = slotOrd++;
  }

  m_formals->m_nSlots = slotOrd;
  return m_formals;
}

unsigned Method::GetVtblSlot() {
  verify(m_isVirtual);
  int slot = m_isIntroducing ?  m_vtblSlot : m_coverage[0].m_method->m_vtblSlot;
  verify(slot >= 0);
  return slot;
}

TargetMethod *Method::GetTarget(Epoch ep) {
  if (ep == ep_compile) {
    if (!m_compileTarget) {
      if (m_nativeMethod != nm_none)
        m_compileTarget = Target::Get(ep_compile)->For(this, m_nativeMethod);
      else
        m_compileTarget = Target::Get(ep_compile)->For(this);
    }
    return m_compileTarget;
  } else {
    if (!m_runTarget) {
      if (m_nativeMethod != nm_none)
        m_runTarget = Target::Get(ep_run)->For(this, m_nativeMethod);
      else
        m_runTarget = Target::Get(ep_run)->For(this);
    }
    return m_runTarget;
  }
}

void Method::InsertImplicitConstructs(vector<VarDecl *> &fields) {
  // Implicitly constructed fields are constructed as soon as possible, so we
  // need to know where to insert constructors that go before the first
  // explicitly constructed base or field.
  Node **where = &safe_cast<Label *>(m_body->Root())->m_expr;
  where = &safe_cast<MethodBody *>(*where)->m_body;
  auto b = safe_cast<Block *>(*where);
  where = &b->m_expr;

  // Also skip past the VarDecls that correspond to arguments.
  Var *objVar = nullptr;
  Var *thatVar = nullptr;
  while ((*where)->Kind() == nk_VarDecl &&
         safe_cast<VarDecl *>(*where)->m_entity->Kind() == ek_Argument) {
    auto vd = safe_cast<VarDecl *>(*where);
    String *name = vd->m_entity->Name();
    if (name == wks_obj)
      objVar = safe_cast<Var *>(vd->m_entity);
    else if (name == wks_that)
      thatVar = safe_cast<Var *>(vd->m_entity);
    where = &vd->m_expr;
  }

  // If this class starts introduces a vtable, then set it to zero immediately.
  // No virtual calls until construction is complete.
  auto ce = safe_cast<Class *>(Parent())->MetaInstance();
  if (ce->GetVtableSize() > 0 && ce->GetVtableLocation() < 0) {
    Node *van = (new VarAddr({ }, objVar))->Deref();
    Field *fe = ce->Fields()[0];
    Member *mn = new Member({ }, van, fe->Name(), false);
    VarDecl *vd = new VarDecl(mn, *where, VarDecl::USE_FIELD_INIT,
                              dm_not_on_return);
    vd->SetGenerated();
    *where = vd;
    where = &vd->m_expr;
  }

  // FIXME: do implicit base construction.

  vector<Field *> &order = safe_cast<Class *>(Parent())->MetaInstance()->Fields();
  size_t i = 0, j = 0;
  while (i < fields.size() && j < order.size()) {
    VarDecl *vd = fields[i];
    Field *fe = order[j++];
    if (vd->m_entity == fe) {
      // Explicit construction of field right where it should be.
      where = &vd->m_expr;
      i++;
    } else {
      if (fe->Name() == wkhs_vtbl)
        continue;

      // Apparently a field was skipped.  Insert implicit construction.
      // FIXME: do field initializers and copy constructors
      Node *van = (new VarAddr({ }, objVar))->Deref();
      // FIXME: should really just use FieldNodes directly.
      Member *mn = new Member({ }, van, fe->Name(), false);
      vd = new VarDecl(mn, *where, VarDecl::USE_FIELD_INIT, dm_not_on_return);
      vd->SetGenerated();
      *where = vd;
      where = &vd->m_expr;
    }
  }

  // Insert remaining constructors.
  while (j < order.size()) {
    Field *fe = order[j++];
    if (fe->Name() == wkhs_vtbl)
      continue;

    Node *van = (new VarAddr({ }, objVar))->Deref();
    // FIXME: should really just use FieldNodes directly.
    Member *mn = new Member({ }, van, fe->Name(), false);
    Node *init = VarDecl::USE_FIELD_INIT;
    if (m_generatedKind == gm_copy_constructor) {
      // If this is a generated copy constructor, i.e. it isn't even declared
      // by the user, then all the inserted constructors must ignore any field
      // initializer and do a copy construction of the field.
      van = (new VarAddr({ }, thatVar))->Deref();
      van = (new Load({ }, van))->Deref();
      init = new Member({ }, van, fe->Name(), false);
    }
    VarDecl *vd = new VarDecl(mn, *where, init, dm_not_on_return);
    vd->SetGenerated();
    *where = vd;
    where = &vd->m_expr;
  }

  // Setup vtable if present.  FIXME: we only want to do this for the most
  // derived class's constructor, and just before it returns normally.  Missing
  // some necessary infrastructure to do this for now.
  if (ce->GetVtableSize() > 0) {
    Node *van = (new VarAddr({ }, objVar))->Deref();
    Node *vtbl = new Vtable({ }, ce);
    Node *init = InitVtableAddresses(van, ce, vtbl);
    init = new Sequence(*where, init, false);
    *where = init;
  }

  // If all explicit constructions haven't been accounted for, then some of them
  // are in the wrong order.  Complain.
  verify(i == fields.size()); // FIXME
}

Node *::Method::InitVtableAddresses(Node *obj, Class *ce, Node *vtbl) {
  int loc = ce->GetVtableLocation();
  Node *rv = nullptr;

  // Handle case where class introduces vtable.
  if (loc < 0) {
    FieldAddr *fan = new FieldAddr({ }, ce->Fields()[0], obj);
    Member *functor = new Member({ }, fan->Deref(), wks_op_assign, true);
    rv = new Apply(functor, nullptr, vtbl);
    return rv;
  }

  for (; loc < (int)ce->GetBaseCount(); loc++) {
    BaseSpecifier *bs = ce->GetBase(loc);
    Class *bce = bs->m_baseClass;
    if (bce->GetVtableSize() > 0) {
      FieldAddr *fan = new FieldAddr({ }, obj, loc);
      Node *bvtbl = new VtableCast(vtbl, ce, loc, true);
      Node *e = InitVtableAddresses(fan->Deref(), bce, bvtbl);

      if (rv == 0)
        rv = e;
      else
        rv = new Sequence(rv, e, false);
    }
  }

  return rv;
}

void Method::InsertImplicitDestructs(vector<VarDecl *> &fields) {
  // Implicitly destructed fields are destructed as late as possible, so we
  // need to know where to insert destructors that go after the last
  // explicitly destructed base or field.
  Node **where = &safe_cast<Label *>(m_body->Root())->m_expr;
  where = &safe_cast<MethodBody *>(*where)->m_body;
  auto b = safe_cast<Block *>(*where);
  where = &b->m_expr;

  // Also skip past the VarDecls that correspond to arguments.
  Var *thisVar = nullptr;
  while ((*where)->Kind() == nk_VarDecl &&
         safe_cast<VarDecl *>(*where)->m_entity->Kind() == ek_Argument) {
    auto vd = safe_cast<VarDecl *>(*where);
    String *name = vd->m_entity->Name();
    if (name == wks_this)
      thisVar = safe_cast<Var *>(vd->m_entity);
    where = &vd->m_expr;
  }

  // FIXME: do implicit base destruction.
  // FIXME: zap vtables ASAP -- but only if a non-trivial destructor
  // exists (point being, to prevent virtual calls).

  vector<Field *> &order = safe_cast<Class *>(Parent())->Fields();
  size_t i = 0, j = order.size();
  while (i < fields.size() && j > 0) {
    VarDecl *vd = fields[i];
    Field *fe = order[--j];
    if (vd->m_entity == fe) {
      // Explicit destruction of field right where it should be.
      where = &vd->m_expr;
      i++;
    } else {
      if (fe->Name() == wkhs_vtbl)
        continue;

      // Apparently a field was skipped; insert implicit destruction.
      // FIXME: should really just use FieldNodes directly.
      Node *van = (new VarAddr({ }, thisVar))->Deref();
      van = (new Load({ }, van))->Deref();
      Member *mn = new Member({ }, van, fe->Name(), false);
      vd = new VarDecl(mn, *where, (Node *)nullptr, dm_is_destructor);
      vd->SetGenerated();
      *where = vd;
      where = &vd->m_expr;
    }
  }

  // If all explicit destructions haven't been accounted for, then some of them
  // are in the wrong order.  Complain.
  verify(i == fields.size()); // FIXME

  // Insert remaining destructors.  These execute after the body of the
  // destructor.
  if (j > 0) {
    Node *chain = new Literal({ }, Value::New(Type::Void()));
    for (i = 0; i < j; i++) {
      Field *fe = order[i];
      if (fe->Name() == wkhs_vtbl)
        continue;

      Node *van = (new VarAddr({ }, thisVar))->Deref();
      van = (new Load({ }, van))->Deref();
      // FIXME: should really just use FieldNodes directly.
      Member *mn = new Member({ }, van, fe->Name(), false);
      VarDecl *vd = new VarDecl(mn, chain, nullptr, dm_is_destructor);
      vd->SetGenerated();
      chain = vd;
    }

    Sequence *sn = new Sequence(*where, chain, false);
    *where = sn;
  }
}

void Method::InsertImplicitAssignments() {
  // Find the spot where the generated assignments will be inserted into the
  // method body.
  Node *where = safe_cast<Label *>(m_body->Root())->m_expr;
  where = safe_cast<MethodBody *>(where)->m_body;
  auto b = safe_cast<Block *>(where);
  where = b->m_expr;
  Var *thisVar = nullptr;
  Var *thatVar = nullptr;
  VarDecl *vd = nullptr;
  while (where->Kind() == nk_VarDecl) {
    vd = safe_cast<VarDecl *>(where);
    String *name = vd->m_entity->Name();
    where = safe_cast<VarDecl *>(where)->m_expr;
    if (name == wks_this)
      thisVar = safe_cast<Var *>(vd->m_entity);
    else if (name == wks_that)
      thatVar = safe_cast<Var *>(vd->m_entity);
  }

  // FIXME: do implicit base assignment.

  vector<Field *> &order = safe_cast<Class *>(Parent())->Fields();
  Node *chain = nullptr;
  for (size_t i = order.size(); i > 0; ) {
    Field *fe = order[--i];
    if (fe->Name() == wkhs_vtbl)
      continue;

    // FIXME: should really just use FieldNodes directly.
    Node *van = (new VarAddr({ }, thisVar))->Deref();
    van = (new Load({ }, van))->Deref();
    Member *dst = new Member({ }, van, fe->Name(), false);

    van = (new VarAddr({ }, thatVar))->Deref();
    van = (new Load({ }, van))->Deref();
    Member *src = new Member({ }, van, fe->Name(), false);

    Member *functor = new Member({ }, dst, wks_op_assign, true);
    Apply *an = new Apply(functor, nullptr, src);
    if (chain)
      chain = new Sequence(an, chain, false);
    else
      chain = an;
  }

  vd->m_expr = chain ? chain : new Literal({ }, Value::New(Type::Void()));
}

Method::NewArguments::NewArguments(Entity *parent)
    : m_parent(parent),
      m_formals(new FormalArguments()) { }

void Method::NewArguments::SetName(const char *name) {
  m_name = String::Get(name);
}

void Method::NewArguments::SetName(WellKnownString name) {
  m_name = String::Get(name);
}

void Method::NewArguments::SetReturnType(Type t) {
  verify(!m_formals->m_returnValue);
  m_formals->m_returnValue = new Argument({ }, am_out, t, (char *)nullptr);
}

void Method::NewArguments::AppendIn(const char *name, Type t) {
  Argument *ae = new Argument({ }, am_in, t, name);
  m_formals->m_arguments.push_back(ae);
}

void Method::NewArguments::AppendOut(ArgumentMechanism m, const char *name,
                                     Type t) {
  verify(!m_formals->m_returnValue);
  m_formals->m_returnValue = new Argument({ }, m, t, name);
}

void Method::NewArguments::Append(ArgumentMechanism m, const char *name,
                                  Type t) {
  switch (m) {
    case am_out:
    case am_construct:
      AppendOut(m, name, t);
      break;
    default:
      AppendIn(name, t);
      break;
  }
}

void Method::AppendToHash(SHA512 &hash) {
  hash.Append('M');
  hash.Append('(');
  size_t implicitCnt = m_formals->ImplicitArgCount();
  bool first = true;
  for (size_t i = implicitCnt; i < m_formals->m_arguments.size(); i++) {
    if (!first)
      hash.Append(',');
    first = false;
    m_formals->m_arguments[i]->m_type.AppendToHash(hash);
  }
  hash.Append(')');

  // FIXME: generic methods
  // FIXME: may need to special case variadic arglist
}

void Method::InheritedMethod::Deflate(Deflator &DF) const {
  DF << m_method;
  DF << m_path;
}

void Method::InheritedMethod::Inflate(Inflator &IF) {
  IF >> m_method;
  IF >> m_path;
}
