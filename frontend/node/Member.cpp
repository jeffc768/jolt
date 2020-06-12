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

#include "Member.h"
#include "Apply.h"
#include "Deref.h"
#include "FieldAddr.h"
#include "Ident.h"
#include "Initializer.h"
#include "Literal.h"
#include "Load.h"
#include "NativeOperator.h"
#include "OverloadSetRef.h"
#include "VarAddr.h"
#include "VarDecl.h"
#include "entity/BaseSpecifier.h"
#include "entity/Class.h"
#include "entity/Field.h"
#include "entity/Namespace.h"
#include "entity/Nullified.h"
#include "entity/OverloadSet.h"
#include "entity/Var.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"

// Note that not all Member node states are in this file.  Those specific to
// a type are in that type's file; i.e., type/*Type.cpp.

IMPLEMENT_NODE(Member)

Member::Member(Location sl, Node *object, Node *expr, bool isDeref)
    : Node(sl),
      m_object(object),
      m_expr(expr),
      m_deref(isDeref) {
}

Member::Member(Location sl, Node *object, String *member, bool op)
    : Node(sl),
      m_object(object),
      m_member(new Ident(sl, member)),
      m_operator(op) { }

Member::Member(Location sl, Node *object, WellKnownString member, bool op)
    : Node(sl),
      m_object(object),
      m_member(new Ident(sl, member)),
      m_operator(op) {
  if (op)
    m_global = new Ident(sl, member);
}

Member::Member(Location sl, Node *object, bool op, Ident *member)
    : Node(sl),
      m_object(object),
      m_member(member),
      m_operator(op) {
  if (op)
    m_global = new Ident(sl, member->m_identifier);
}

Member::Member(Location sl, Node *object, Ident *member, NativeOperator *op)
    : Node(sl, Type::Void()),
      m_object(object),
      m_member(member),
      m_reason(r_builtin),
      m_macro(op) {
}

void Member::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_object;
  DF << m_deref;
  DF << m_expr;
  DF << m_member;
  DF << m_operator;
  DF << m_global;

  verify(!m_entity);
  verify(!m_macro);
}

Member::Member(Inflator &IF)
    : Node(IF) {
  IF >> m_object;
  IF >> m_deref;
  IF >> m_expr;
  IF >> m_member;
  IF >> m_operator;
  IF >> m_global;
}

Node *Member::ResolveType_(Context &ctx) {
  // Before doing anything else, first determine if this is really a function
  // type constructor.
  m_object = m_object->ResolveType(ctx);
  if (m_expr && m_deref) {
    if (m_object->m_type == tk_type || m_object->Kind() == nk_List) {
      m_expr = m_expr->ResolveType(ctx);
      return HandleTypes(ctx);
    }
  }

  // This is a member access after all.  Extract the identifier from the right-
  // hand operand (and it must be one), and deref the left-hand operand if
  // necessary.
  if (m_expr) {
    m_member = dyn_cast<Ident *>(m_expr);
    m_expr = nullptr;
    verify(m_member); // FIXME
  }

  if (m_deref) {
    m_object = new Member(m_location, m_object, wks_op_deref, false);
    m_object = new Apply(m_object, nullptr);
    m_object = m_object->ResolveType(ctx);
    m_deref = false;
  }

  // Member nodes don't really have a type; it just flags an error.  So we
  // do full resolution, replacing ourselves with something that does have a
  // type.
  Node *in = m_member->ResolveMember(ctx);
  verify(in == m_member);

  // Look for valueless types.  If member has a type at all, it's because
  // Ident node failed to evaluate a name macro.
  verify(!m_member->m_type || m_member->m_type == tk_valueless);
  if (m_object->m_type == tk_valueless) {
    if (m_member->m_type)
      m_type = Type::Join(m_object->m_type, m_member->m_type);
    else
      m_type = m_object->m_type;
  } else if (m_member->m_type) {
    m_type = m_member->m_type;
  }

  if (m_type == tk_valueless)
    return this;

  // Note: we let the Apply node owning us fully resolve our object as a
  // general rule.

  // We now have what we need to begin the lookup.  There are three kinds
  // of values to deal with:
  //   1. Namespace values, which must be evaluated at compile time so that
  //      we have a namespace object to search.
  //   2. Class instances, where the type of the value provides the scope
  //      to search.
  //   3. Everything else, in other words built-in types, are handled specially
  //      and do not go through method lookups.

  Entity *chain = nullptr;
  m_reason = r_builtin;  // Assume for now
  m_type = Type::Void();
  switch (m_object->m_type.Kind()) {
    case tk_namespace: {
      // Handle namespaces.  Evaluate m_object as a compile time expression
      // that yields a namespace object.
      Context::PushVoid pv(ctx, false);
      m_object = m_object->ResolveFully(ctx);
      Value *v = m_object->Evaluate();
      chain = Namespace::GetFromObject(v);
      verify(chain);
      break;
    }
    case tk_class:
      // Handle classes.  The type directly provides the class object.
      // FIXME: should get it from class's storage.
      chain = m_object->m_type.Class();
      verify(chain);
      break;
    case tk_bool:
      return HandleBools(ctx);
    case tk_char:
      return HandleChars(ctx);
    case tk_enum:
      return HandleEnums(ctx);
    case tk_type:
      return HandleTypes(ctx);
    case tk_pointer:
      return HandlePointers(ctx);
    case tk_array:
      return HandleArrays(ctx);
    case tk_set:
      return HandleSets(ctx);
    case tk_float:
      return HandleFloats(ctx);
    case tk_integer:
      return HandleIntegers(ctx);
    case tk_tuple:
      return HandleTuples(ctx);
    case tk_generic:
      return HandleGenerics(ctx);
    case tk_codefragment:
      return HandleCodeFragments(ctx);
    case tk_pseudo:
      if (m_object->m_type == Type::PseudoInteger())
        return HandleIntegers(ctx);
      else if (m_object->m_type == Type::PseudoChar())
        return HandleChars(ctx);
      else if (m_object->m_type == Type::PseudoList())
        return HandleTuples(ctx);
      else if (m_object->m_type == Type::PseudoFloat())
        return HandleFloats(ctx);
      else if (m_object->m_type == Type::PseudoEnum())
        return HandleEnums(ctx);
      break;
    case tk_void:
      return HandleVoids(ctx);
    default:
      // Otherwise there is no member by that name.
      break;
  }

  m_reason = r_not_found; // Definitely not a built-in.
  if (chain) {
    // Depending on the type of member we are looking up, there are three
    // outcomes:
    //   1. The containing object is a namespace or is acting as a namespace,
    //      as for class members that reside in the common (not member) scope.
    //      Replace ourselves with the found entity, just as Ident node would
    //      do.
    //   2. The containing object actually contains the member, as for class
    //      members residing in the member scope.  We replace ourselves with
    //      either an Apply node or FieldAddr, as appropriate.
    //   3. Special cases, such operator-> on a metaclass.

    // Go and get the member name.
    String *name = m_member->m_identifier;

    // Check for * on a metaclass.  We need to convert the metaclass to a type
    // immediately in order to avoid a circular dependency when a class has a
    // field that has a pointer type to itself.  We can do this even though the
    // Ident node has not yet fully resolved the class name due to a similar
    // trick there.  Check for all operators checked by state HandleMetaClasses.
    if (name == wkhs_op_ptr ||
        name == wks_op_const ||
        name == wks_op_mutable ||
        name == wkhs_op_ref ||
        name == wkhs_op_r_ref ||
        name == wkhs_op_generic) {
      if (chain->Kind() == ek_Class) {
        auto ce = safe_cast<Class *>(chain);
        if (ce->IsMetaclass()) {
          // No, CastTo won't work here because that needs the Ident node to
          // have first fully resolved itself.
          Type t = ce->MetaInstance()->AsType();
          m_object = new Literal(m_location, Value::NewType(t));
          m_reason = r_builtin;  // except it really is a builtin
          return HandleTypes(ctx);
        }
      }
    }

    if (auto e = chain->LookupEntity(name)) {
      m_entity = e;
    } else {
      // Not found.  Check namespaces used by the namespace or class type.
      // FIXME: do it.  Need to share code with Ident node if possible.

      // The member wasn't found, but before we go and complain go look for a
      // few special cases.
      if (chain->Kind() == ek_Class) {
        auto ce = safe_cast<Class *>(chain);
        if (ce->IsMetaclass()) {
          String *name = m_member->m_identifier;

          // FIXME: handle concepts
          if (name == wkhs_op_ptr ||
              name == wks_op_index ||
              name == wkhs_op_ref ||
              name == wkhs_op_r_ref ||
              name == wkhs_op_generic) {
            m_object = new Initializer(m_object->m_location, Type::JType(),
                                       m_object, false);

            // Start over with new object.
            m_state = st_initial;
            return this->ResolveType(ctx);
          }
        }

        // And if that fails, report a "not declared" error.
      }
    }
  }

  // Get an expression node that represents the entity.
  if (!m_entity) {
    // Ugly hack.  We must special case the vtbl field of classes, as it's
    // impossible to put that into the member scope, as it must be frozen
    // before we know if there is a vtbl field.  Yuck.
    if (m_member->m_identifier == String::Get(wkhs_vtbl)) {
      auto ce = safe_cast<Class *>(m_object->m_type.Class());
      ce->ResolveFields();
      Context::PushVoid pv(ctx, false);
      m_object = m_object->ResolveFully(ctx);
      Field *fe = ce->Fields()[ce->GetBaseCount()];
      Node *e = (new FieldAddr(m_location, fe, m_object))->Deref();
      return e->ResolveFully(ctx);
    } else {
      if (!m_operator) {
        String *name = m_member->m_identifier;
        EmitError(this) << name << " not declared.";
        ctx.m_error = true;
      } else {
        m_reason = r_try_operator;
      }
      m_type = Type::Suppress();
      return this;
    }
  } else if (m_entity->Kind() == ek_Nullified) {
    if (!m_operator) {
      String *name = m_member->m_identifier;
      EmitError(this) << name << " has no built definitions.";
      ctx.m_error = true;
    } else {
      m_reason = r_try_operator;
    }
    m_type = Type::Suppress();
    return this;
  }

  Node *e = nullptr;
  if (m_entity->Kind() == ek_Field) {
    // Return a reference to the field.
    auto fe = safe_cast<Field *>(m_entity);
    fe->ResolveType();
    auto ce = safe_cast<Class *>(fe->Parent());
    ce->ResolveFields();
    Context::PushVoid pv(ctx, false);
    m_object = m_object->ResolveFully(ctx);
    e = m_object;
    VarDecl *vd = nullptr;
    if (e->Kind() == nk_Deref && 0 && // FIXME
        !safe_cast<::Deref *>(e)->m_expr->m_type.IsThin()) {
      auto dn = safe_cast<::Deref *>(e);
      Type vt = dn->m_expr->m_type;
      Var *ve = new Var(m_location, vt);
      ve->TrackResolution();
      vd = new VarDecl(ve, nullptr, dn->m_expr, dm_leaves_full_expr);
      e = (new VarAddr(m_location, ve))->Deref();
      e = (new FieldAddr(m_location, e, 0))->Deref();
      e = (new Load(m_location, e))->Deref();
    }
    ce = m_object->m_type.Class();
    vector<size_t> &fp = ce->PathForField(fe);
    for (size_t i = 0; i < fp.size(); i++) {
      BaseSpecifier *bs = ce->GetBase(fp[i]);
      ce = bs->m_baseClass;
      e = (new FieldAddr(m_location, e, bs->m_ord))->Deref();
    }
    e = (new FieldAddr(m_location, fe, e))->Deref();
    if (vd) {
      vd->m_expr = e;
      e = vd;
    }
  } else if (m_entity->Kind() == ek_OverloadSet) {
    auto ose = safe_cast<OverloadSet *>(m_entity);
    // What we do depends on whether this overload set is a member of a class
    // or namespace.
    Entity *chain = ose->Parent();
    switch (chain->Kind()) {
      case ek_Class:
        ose->ResolveFully();
        // Return a reference to the associated OverloadSet object along
        // with it's "this" object.
        e = new OverloadSetRef(m_location, ose, m_object);
        break;
      case ek_Namespace:
        // Handle it like any other entity.
        e = m_entity->AsValue(m_location);

        // Remove implicitly generated application.
        if (e->Kind() == nk_Apply) {
          auto an = safe_cast<Apply *>(e);
          e = an->m_functor;
        }
        break;
      default:
        verify(false);
    }
  } else {
    // Handle non-methods.
    e = m_entity->AsValue(m_location);
  }

  return e->ResolveFully(ctx);
}

Node *Member::ResolveFully_(Context &ctx) {
  // If we get here, this node couldn't replace itself due to an error.
  return this;
}

void Member::ReplaceObjectWithEntityRef(Entity *e) {
  m_object = e->AsValue(m_object->m_location);
}

void Member::VisitChildren(Visitor *v) {
  v->Visit(m_object, ck_operand1);

  if (m_expr)
    v->Visit(m_expr, ck_member);

  if (m_member)
    v->Visit(m_member, ck_member);

  if (m_global)
    v->Visit(m_global, ck_operand1);
}

bool Member::Dump(BufferWriter &bw, int level, int maxlevel,
                  const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Member", desc, descwidth))
    return true;

  if (m_macro) {
    if (bw.Append(" nm:", 4))
      return true;
    const char *name = m_macro->Name();
    if (bw.Append(name, strlen(name)))
      return true;
  }

  if (m_operator && bw.Append(" [operator]", 11))
    return true;

  if (m_reason == r_try_operator) {
    if (bw.Append(" [tryop]", 8))
      return true;
  } else if (m_reason == r_builtin) {
    if (bw.Append(" [builtin]", 10))
      return true;
  }

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_object->Dump(bw, level, maxlevel, m_deref ? "obj->" : "obj", 5))
      return true;
    if (m_expr && m_expr->Dump(bw, level, maxlevel, "expr", 5))
      return true;
    if (m_member && ((Node *)m_member)->Dump(bw, level, maxlevel, "ident", 5))
      return true;
  }

  return false;
}
