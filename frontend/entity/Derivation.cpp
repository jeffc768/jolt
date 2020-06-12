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

#include "Class.h"
#include "Const.h"
#include "Derivation.h"
#include "Specialization.h"
#include "util/InDeflator.h"
#include "util/Hash.h"
#include "util/Value.h"

IMPLEMENT_OBJECT(Derivation)

Derivation::Derivation(Generic *ge, Type t)
    : m_entity(ge),
      m_type(t),
      m_params(t.FieldCount()),
      m_bindings(t.FieldCount()) {
  for (size_t i = 0; i < t.FieldCount(); i++)
    m_params[i] = int(i);
}

Derivation::Derivation(Derivation *that)
    : m_entity(that->m_entity) {
  m_bindings.reserve(that->m_bindings.size());
  m_bindings.insert(m_bindings.end(), that->m_bindings.begin(),
                                      that->m_bindings.end());
}

void Derivation::DeflateFields(Deflator &DF) {
  DF << m_entity;
  DF << m_type;
  DF << m_params;
  DF << m_bindings;
}

Derivation::Derivation(Inflator &IF) : Object(IF) {
  IF >> m_entity;
  IF >> m_type;
  IF >> m_params;
  IF >> m_bindings;
}

uint32_t Derivation::Hash() {
  HashState hs;
  hs.Hash(m_type);

  for (size_t i = 0; i < m_params.size(); i++)
    hs.Hash(m_params[i]);

  for (size_t i = 0; i < m_bindings.size(); i++) {
    Value *v1 = m_bindings[i];
    if (!v1)
      break;

    Type t = v1->ObjectType();
    void *a1 = v1->Address();

    // We now need to compare the values.
    switch (t.Kind()) {
      case tk_void:
        // Only one value of type void, so nothing to hash.
        break;

      case tk_bool:
      case tk_float:
      case tk_integer:
      case tk_enum:
      case tk_char:
      case tk_set:
      case tk_generic: {
        // While this comparison may not be correct from a type standpoint, it
        // doesn't matter as it only needs to be consistent, not correct.  The
        // alternative requires handling each type separately.
        char *p = reinterpret_cast<char *>(a1);
        hs.Hash(p, t.StorageSize());
        break;
      }

      case tk_pseudo:
        verify(false); // FIXME
        break;

      case tk_type: {
        Type t1 = *reinterpret_cast<Type *>(a1);
        hs.Hash(t1);
        break;
      }

      case tk_namespace: {
        char *p = reinterpret_cast<char *>(a1);
        hs.Hash(p, t.StorageSize());
        break;
      }

      case tk_class: {
        Class *ce = t.Class();

        // If it's a metaclass, there can be only one so it isn't worth
        // hashing.
        if (ce->IsMetaclass())
          break;

        // FIXME: for now, just compare the raw bytes.  Need to invoke
        // operator <=>.
        char *p = reinterpret_cast<char *>(a1);
        hs.Hash(p, t.StorageSize());
        break;
      }

      case tk_array: {
        // FIXME: This won't work for the general case, but for now it only
        // needs to handle string literals.
        verify(t.ElementType() == tk_char);
        char *p = reinterpret_cast<char *>(a1);
        hs.Hash(p, t.StorageSize());
        break;
      }

      case tk_tuple:
        // FIXME.  This should be used for generic too; bulk hashing isn't
        // correct, as it hashes padding between values.
        verify(false);
        break;

      case tk_codefragment:
        // FIXME.  This should be used for generic too.  Must hash the entire
        // node tree, which is non-trivial to implement.
        verify(false);
        break;

      default:
        verify(false); // internal error
        break;
    }
  }

  return hs.fmix32();
}

int Derivation::Compare(Derivation *that) {
  verify(m_entity == that->m_entity);

  if (m_type != that->m_type)
    return m_type < that->m_type ? -1 : 1;

  // We now know both values have the same number of unbound parameters.
  for (size_t i = 0; i < m_params.size(); i++)
    if (m_params[i] != that->m_params[i])
      return m_params[i] < that->m_params[i] ? -1 : 1;

  // We now know they have the same unbound parameters, in the same order.  All
  // that's left is to compare the bound parameters.
  for (size_t i = 0; i < m_bindings.size(); i++) {
    Value *v1 = m_bindings[i];
    Value *v2 = that->m_bindings[i];
    if (v1 == v2)
      continue;

    Type t1 = v1->ObjectType();
    Type t2 = v2->ObjectType();
    void *a1 = v1->Address();
    void *a2 = v2->Address();

    // Consistency is what matters, not meaninfulness.
    if (t1 != t2)
      return t1 < t2 ? 1 : -1;

    // We now need to compare the values.
    switch (t1.Kind()) {
      case tk_void:
        // Only one value of type void, so they must be equal.
        break;

      case tk_bool:
      case tk_float:
      case tk_integer:
      case tk_enum:
      case tk_char:
      case tk_set:
      case tk_generic: {
        // While this comparison may not be correct from a type standpoint, it
        // doesn't matter as it only needs to be consistent, not correct.  The
        // alternative requires handling each type separately.
        int n = memcmp(a1, a2, t1.StorageSize());
        if (n != 0)
          return n;
        break;
      }

      case tk_pseudo:
        verify(false); // FIXME
        break;

      case tk_type: {
        Type u1 = *reinterpret_cast<Type *>(a1);
        Type u2 = *reinterpret_cast<Type *>(a2);
        if (u1 != u2)
          return u1 < u2 ? 1 : -1;
        break;
      }

      case tk_namespace: {
        int n = memcmp(a1, a2, t1.StorageSize());
        if (n != 0)
          return n;
        break;
      }

      case tk_class: {
        Class *ce = t1.Class();

        // If it's a metaclass, they must be equal as there is only one possible
        // value: the meta-instance.
        if (ce->IsMetaclass())
          break;

        // FIXME: for now, just compare the raw bytes.  Need to invoke
        // operator <=>.
        int n = memcmp(a1, a2, t1.StorageSize());
        if (n != 0)
          return n;
        break;
      }

      case tk_array: {
        // FIXME: This won't work for the general case, but for now it only
        // needs to handle string literals.
        verify(t1.ElementType() == tk_char);
        int n = memcmp(a1, a2, t1.StorageSize());
        if (n != 0)
          return n;
        break;
      }

      case tk_tuple:
        // FIXME.  This should be used for generic too; bulk comparison isn't
        // correct, as it hashes padding between values.
        verify(false);
        break;

      case tk_codefragment:
        // FIXME.  This should be used for generic too.  Must be able to do
        // a deep comparison of the entire node tree, which is non-trivial.
        verify(false);
        break;

      default:
        verify(false); // internal error
        break;
    }
  }

  // They are equivalent.
  return 0;
}

#ifndef NDEBUG
bool ValidateBindingType(Value *arg, Type t) {
    Type u = arg->ObjectType();
    Type v = t.Lower();
    if (u != v) {
      verify(u == tk_array && v == tk_array);
      verify(u.ElementType() == v.ElementType());

      Type u2 = u.IndexType();
      Type v2 = v.IndexType();
      verify(!u2 || !v2 || u2 == v2);
    }
    return true;
}
#endif

Derivation *Derivation::BindParameters(const vector<Value *> &args) {
  if (args.size() == 0)
    return this;

  Derivation *d = new Derivation(this);

  // Compute the type of the new generic value.
  vector<Type> ts;
  for (size_t i = args.size(); i < m_type.FieldCount(); i++)
    ts.push_back(m_type.TypeOf(i));
  d->m_type = Type::Generic(Type::Tuple(ts));

  // Compute ordering of remaining unbound parameters.
  // FIXME: permit re-ordering.
  d->m_params.insert(d->m_params.begin(), m_params.begin() + args.size(),
                                          m_params.end());

  // Bind the parameters.
  for (size_t i = 0; i < args.size(); i++) {
    verify(ValidateBindingType(args[i], m_type.TypeOf(i)));
    int ord = m_params[i];
    verify(!d->m_bindings[ord]);
    d->m_bindings[ord] = args[i];
  }

  // Derivations are unique; see if this new one already exists in the
  // parent Generic's map.  Return the old one if it does.
  return m_entity->m_derivations.
      insert(std::make_pair(d, nullptr)).first->first;
}

GenericKind Derivation::Kind() {
  return m_entity->m_nonSpecialized->m_kind;
}

Class *Derivation::CastToClassObject() {
  verify(m_entity->m_nonSpecialized->m_kind == gk_class);
  Entity *e = CastToEntity();
  return safe_cast<Class *>(e);
}

Const *Derivation::CastToTypeValue() {
  verify(m_entity->m_nonSpecialized->m_kind == gk_type);
  Entity *e = CastToEntity();
  return safe_cast<Const *>(e);
}

Entity *Derivation::CastToEntity() {
  // First see if a derivation has already happened.
  auto I = m_entity->m_derivations.find(this);
  verify(I != m_entity->m_derivations.end());
  if (I->second)
    return I->second;

  // If there are any unbound parameters, try and bind them to their defaults.
  Derivation *d = this;
  if (m_type.FieldCount() > 0) {
    vector<Type> ts;
    d = new Derivation(this);
    d->m_type = Type::Generic(Type::Tuple(ts));
    for (size_t i = 0; i < m_type.FieldCount(); i++) {
      int ord = m_params[i];
      auto dv = dyn_cast<Value *>(m_entity->m_defaults[ord]);
      verify(dv); // FIXME: default expressions
      verify(ValidateBindingType(dv, m_type.TypeOf(i)));
      verify(!d->m_bindings[ord]);
      d->m_bindings[ord] = dv;
    }
    d = m_entity->m_derivations.insert(std::make_pair(d, nullptr)).first->first;
  }

  // Identify the correct specialization to use and ask it for a derivation.
  // FIXME: no specializations for now.
  Entity *e = m_entity->m_nonSpecialized->Derive(d);

  // Memoize and return derivation.
  I->second = e;
  if (d != this)
    m_entity->m_derivations[d] = e;
  return e;
}
