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

#include "OverloadSet.h"
#include "Argument.h"
#include "AttributeList.h"
#include "Class.h"
#include "FormalArguments.h"
#include "Method.h"
#include "Scope.h"
#include "node/Apply.h"
#include "node/Expr.h"
#include "node/List.h"
#include "node/Literal.h"
#include "node/OverloadSetRef.h"
#include "target/TranslateClosure.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"
#include <map>

IMPLEMENT_ENTITY(OverloadSet)

OverloadSet::OverloadSet(Entity *parent, StringHelper name,
                         vector<Method *> &methods)
    : Entity(parent,
             methods.size() > 0 ? methods[0]->GetLocation() : Location() /* FIXME? */,
             name) {
  m_methods.swap(methods);
}

void OverloadSet::DeflateFields(Deflator &DF) {
  verify(m_resolved);

  Entity::DeflateFields(DF);
  DF << m_methods;
}

OverloadSet::OverloadSet(Inflator &IF) : Entity(IF) {
  IF >> m_methods;
  m_resolved = true;
}

void OverloadSet::BindNames(SymbolTable &st) {
  Entity::BindNames(st);

  for (auto me : m_methods)
    me->BindNames(st);
}

void OverloadSet::BindNames(SymbolTable &st, bool (*filter)(Entity *e)) {
  for (auto me : m_methods) {
    if (filter(me))
      me->BindNames(st);
  }
}

void OverloadSet::BindNamesInAttributes(SymbolTable &st) {
  for (auto me : m_methods) {
    if (AttributeList *al = me->Attributes())
      al->BindNames(st);
  }
}

namespace {
  // Predicate used to sort by method signatures.
  struct SignatureCmp {
    bool operator()(Method *left, Method *right) const {
      return left->SignatureCompare(right) < 0;
    }
  };
}

void OverloadSet::ResolveFully() {
  if (m_resolved)
    return;

  for (auto me : m_methods) {
    me->TrackResolution();
    me->ResolveSignature();
  }

  // If a generator has been associated with this overload set, now is the time
  // to let it generate methods (as the signatures of user-supplied methods
  // have been resolved).  Afterwards, wait on resolution of any newly added
  // signatures.
  if (m_generator) {
    auto ce = safe_cast<Class *>(Parent());
    (ce->*m_generator)(this);
    m_generator = nullptr;

    // Redo due to potentially added methods.
    for (auto me : m_methods)
      me->ResolveSignature();
  }

  m_resolved = true;

  auto ce = dyn_cast<Class *>(Parent());
  if (!ce)
    return;
  ce->ResolveBases();

  // We are nearly fully resolved.  All that's left is to deal with inherited
  // methods.  As base classes are fully resolved before the overload sets for
  // this class are even created, blocking on dependents is not possible.
  // Start by grouping all inherited methods by their signature.
  using Map = std::map<Method *, InheritedEntities, SignatureCmp>;
  Map sigs;
  for (auto &ie : m_inherited) {
    auto ose = safe_cast<OverloadSet *>(ie.m_entity);

    for (auto me : ose->m_methods) {
      auto I = sigs.insert(Map::value_type(me, InheritedEntities())).first;
      I->second.emplace_back(me, ie.m_baseOrd);
    }
  }

  // Throw our own methods into the grouping.
  for (auto me : m_methods) {
    auto I = sigs.insert(Map::value_type(me, InheritedEntities())).first;
    I->second.emplace_back(me, size_t(-1));
  }

  // Within each signature group, resolve inheritance.
  for (auto &I : sigs) {
    InheritedEntities &ies = I.second;

    // Let's look at all the inherited (and possibly one non-inherited)
    // definitions for this signature and see what we got.
    bool isFinal = false;
    bool isVirtual = false;
    Method::InheritedMethods coverage;
    Method::InheritedMethods inherited;
    Method *ours = nullptr;
    for (auto &ie : ies) {
      auto me = safe_cast<Method *>(ie.m_entity);

      // If this our definition, not an inherited one, put it aside for now.
      if (ie.m_baseOrd == size_t(-1)) {
        ours = me;
        continue;
      }

      switch (me->MethodKind()) {
        case mk_conflicted:
        case mk_placeholder:
          // Add m_redefines of the conflicted/placeholder method to our own.
          for (auto &im2 : me->m_redefines)
            inherited.emplace_back(im2, ie.m_baseOrd);
          break;

        default:
          // A directly inherited method.
          inherited.emplace_back(me, ie.m_baseOrd);
          break;
      }

      // Note if any are marked final.  That would make redefinition illegal,
      // though it could still be hidden and a new definition introduced.
      if (me->m_isFinal)
        isFinal = true;

      // Also note if any are virtual.  That makes the redefinition virtual.
      if (me->m_isVirtual)
        isVirtual = true;

      // Accumulate coverage set.
      for (auto &im2 : me->m_coverage)
        coverage.emplace_back(im2, ie.m_baseOrd);
    }

    // Count the number of methods with implementations and the number that
    // are deferred.
    int nDeferred = 0;
    int nImpl = 0;
    for (auto &I : inherited) {
      Method *me2 = I.m_method;
      if (me2->m_body || me2->IsNative())
        nImpl++;
      if (me2->m_isDeferred)
        nDeferred++;
    }

    // One of these two conditions must be true regarding this signature,
    // otherwise we wouldn't be here.  Note that when there is an "ours", it
    // is already in our m_methods array and it's going to stay there regard-
    // less of what may or may not be inherited.
    verify(ours || coverage.size() > 0);

    // Handle case where no definitions are inherited but we have one of our
    // own.  Nothing to do but look for errors and warnings.
    if (ours && coverage.size() == 0) {
      if (ours->m_isIntroducing && ours->m_isRedefining)
        ours->m_isRedefining = false;
      if (ours->m_isRedefining) {
        EmitError(ours) << "Method is marked redefine but there are no "
                           "inherited definitions to redefine.";
      } else if (ours->m_isHiding) {
        EmitWarning2(ours) << "Method is marked hiding but there are no "
                              "inherited definitions to hide.";
      } else {
        verify(ours->m_isIntroducing);
      }
    }

    // Handle case where definitions are inherited and we have one of our own.
    if (ours && coverage.size() > 0) {
      if (ours->m_isIntroducing && ours->m_isRedefining)
        ours->m_isIntroducing = false;
      if (ours->m_isIntroducing) {
        EmitError(ours) << "Method does not redefine or hide inherited "
                           "definitions.";
      } else if (ours->m_isHiding) {
        // For all intents and purposes, pretend nothing was inherited.
        ours->m_isIntroducing = true;
      } else if (isFinal) {
        verify(ours->m_isRedefining);
        EmitError(ours) << "Cannot redefine final method.";
        ours->m_isIntroducing = true; // for error recovery, pretend hiding
      } else {
        verify(ours->m_isRedefining);
        ours->SetCoverageAndRedefines(coverage, inherited);
      }

      if (isVirtual) {
        ours->m_isVirtual = true;
      } else if (ours->m_isVirtual) {
        ours->m_isVirtual = false;
        EmitError(ours) << "Redefinition of non-virtual method cannot be "
                           "virtual.";
        // FIXME: this can cause a false vtable to be added to the class,
        // though how much it matters is a good question as this is a
        // compilation error.
      }
    }

    // Handle case where we do not have one of our own, but some are inherited.
    if (!ours) {
      // We are conflicted if there is more than one inherited implementation,
      // or there are zero inherited implementations but more than one inherited
      // deferred method (lacking implementations).
      bool isConflicted = nImpl > 1 || (nImpl == 0 && nDeferred > 1);

      // FIXME: even when there's one implementation inherited, it can still
      // be conflicted.  The types of the out arguments must be subtypes of the
      // corresponding out arguments of the deferred signatures!

      Method *me = nullptr;
      if (isConflicted)
        me = Method::NewConflicted(Parent(), Name());
      else
        me = Method::NewPlaceholder(Parent(), Name());
      me->SetCoverageAndRedefines(coverage, inherited);
      m_methods.push_back(me);

      // Need to propagate for future redefines.
      if (isVirtual)
        me->m_isVirtual = true;
    }

    // FIXME: we need to deal with attempts to use methods at compile time that
    // flunk one of the above attribute tests.

    // For introducing methods, add itself as the initial m_coverage contents.
    if (ours && ours->m_isIntroducing) {
      verify(ours->m_coverage.size() == 0);
      ours->m_coverage.emplace_back(ours);
    }
  }
}

Node *OverloadSet::AsValue(Location sl) {
  // Return a reference to the associated OverloadSet object (ourselves).
  Node *e = new OverloadSetRef(sl, this, nullptr);
  return new Apply(e, nullptr);
}

void *OverloadSet::GlobalStorage(Epoch ep) {
  // FIXME: return an object literal that is an instance of a class that
  // inherits from proc{signature} for each overload.
  verify(false);
  return nullptr;
}

const std::string &OverloadSet::GlobalStorageName() {
  // FIXME: return C++ name of above object literal.
  verify(false);
  static std::string name;
  return name;
}

Entity::ResolutionState OverloadSet::GetResolutionState() {
  verify(m_resolved);
  return rs_ok;
  // FIXME: do we need more?
}

void OverloadSet::AddToClosure(TranslateClosure *tc) {
  // Alas, it is necessary to propagate the linkage from the parent at this
  // time due to how OverloadSets are added to their parent.
  auto lk = Parent()->Linkage();
  SetLinkage(lk);
  for (Method *me : m_methods) {
    me->SetLinkage(lk);
    tc->AddToClosure(me);
  }
}

void OverloadSet::FinalizeClosure(TranslateClosure *) {
}

Method *OverloadSet::LookupFinalize(Method *me, vector<Argument *> &argEntities,
                                    Argument *&returnValue) {
  FormalArguments *formals = me->m_formals;

  for (auto ae : formals->m_arguments)
    argEntities.push_back(ae);

  returnValue = formals->m_returnValue;

  // If we have a placeholder, go get the actual method and the path to it
  // through the class hierarchy.
  if (me->m_kind != mk_placeholder)
    return me;

  // First look for an inherited implementation, ignoring the deferred
  // methods lacking an implementation.
  for (auto &rd : me->m_redefines) {
    Method *me2 = rd.m_method;
    if (me2->m_body || me2->IsNative())
      return me2;
  }

  // Failing that, there can only be a single deferred method lacking an
  // implementation (anything else would be conflicted).
  verify(me->m_redefines.size() == 1);
  return me->m_redefines[0].m_method;
}

Method *OverloadSet::Lookup(Apply *an, vector<Argument *> &argEntities,
                            Argument *&returnValue, bool constReceiver) {
  verify(m_resolved);
  auto &args = an->m_arguments;

  // Keep track of almost matches, in case we don't find an exact match.
  vector<Method *> candidates;

  // FIXME: for now, we're going to have an extremely simple overload resolution
  // in place.  The types of the actual arguments must exactly match the formal
  // arguments.  Nor are we going to handle variable argument lists or "do"
  // arguments.
  for (Method *me : m_methods) {
    bool exactMatch = true;
    FormalArguments *formals = me->m_formals;
    returnValue = nullptr;

    // Account for the implicit arguments that do not participate in the
    // signature, such as "this" arguments.
    size_t skip = formals->ImplicitArgCount();
    auto variadic = formals->m_variadic;

    // Try to match the in arguments.
    if (args.size() < formals->m_arguments.size() - skip) {
      goto nextmethod;
    } else if (args.size() > formals->m_arguments.size() - skip) {
      // FIXME: handle vt_jolt
      if (variadic != vt_c)
        goto nextmethod;
    }

    // Only a const method can accept a const receiver.
    if (!me->m_isConst && constReceiver)
      goto nextmethod;
    else if (me->m_isConst && !constReceiver)
      exactMatch = false;

    // Compare actual and formal argument types.
    for (size_t j = skip; j < formals->m_arguments.size(); j++) {
      Argument *ae = formals->m_arguments[j];
      Type actual = args[j - skip]->m_type;
      Type formal = ae->m_type;

      if (actual == tk_pseudo) {
        auto &lits = an->m_pseudoLiterals;
        auto &bases = an->m_pseudoBases;
        for (size_t i = bases[j - skip]; i < bases[j - skip + 1]; i++) {
          if (auto lit = dyn_cast<Literal *>(lits[i])) {
            if (!lit->m_value->IsSubtypeOf(formal))
              goto nextmethod;
            if (formal == Type::Float())
              exactMatch = false; // prefer double parameters
          } else if (auto list = safe_cast<List *>(lits[i])) {
            if (!list->IsSubtypeOf(formal)) {
              goto nextmethod;
            }
          }
        }
        continue;
      }

      if (formal == rt_rvalue) {
        actual = actual.DropQualifiers();
        formal = formal.DropQualifiers();
      } else if (formal == rt_lvalueref) {
        if (actual != rt_lvalueref) {
          if (formal == ct_const)
            exactMatch = false;
          else
            goto nextmethod;
        }
        actual = actual.LValueRef().Lower();
        formal = formal.Lower();
      } else {
        if (actual == rt_lvalueref)
          goto nextmethod;
        actual = actual.LValueRef().Lower();
        formal = formal.Lower();
      }

      if (actual.IsSubtypeOf(formal) != YES) // FIXME: handle MAYBE
        goto nextmethod;
      // FIXME: handle r-value references
      if (actual != formal)
        exactMatch = false;
    }

    // We have a match; if exact, we're done.
    if (exactMatch)
      return LookupFinalize(me, argEntities, returnValue);

    // If not an exact match, remember it for later in case we don't find an
    // exact match.
    candidates.push_back(me);

  nextmethod:
    ;
  }

  // If there were no inexact matches either, we're done.
  if (candidates.empty())
    return nullptr;

  // Select the best match of the remaining candidates.
  Method *best = candidates[0];
  FormalArguments *bestFormals = best->m_formals;
  for (size_t i = 1; i < candidates.size(); i++) {
    Method *me = candidates[i];
    FormalArguments *meFormals = me->m_formals;

    // Account for the implicit arguments that do not participate in the
    // signature, such as "this" arguments.  Note that the number that do
    // participate must be the same for both signatures, otherwise they
    // wouldn't be candidates.
    size_t bestSkip = bestFormals->ImplicitArgCount();
    size_t meSkip = meFormals->ImplicitArgCount();

    // Keep track of which appears to be better.  We keep score for each
    // argument.
    int bestWins = 0, meWins = 0;

    // Try to match the in arguments.
    for (size_t j = meSkip; j < meFormals->m_arguments.size(); j++) {
      Type meType = meFormals->m_arguments[j]->m_type;
      Type bestType = bestFormals->m_arguments[j - meSkip + bestSkip]->m_type;

      Type meLowered = meType.Lower();
      Type bestLowered = bestType.Lower();

      // If the corresponding formal argument types are identical, then they
      // cannot be used to differentiate the two signatures.
      if (meType == bestType)
        continue;

      if (meLowered != bestLowered) {
        // If best's is a subtype of me's, then it is better, as it is closer
        // to the actual arg's type.
        if (bestLowered.IsSubtypeOf(meLowered) == YES)
          bestWins++;

        // Likewise for the other order.  It is possible both to be subtypes of
        // each other, even though they are different types.  This eventually
        // causes an ambiguous overload error.
        if (meLowered.IsSubtypeOf(bestLowered) == YES)
          meWins++;
      }

      // If one is an lvalue reference and the other an rvalue reference, the
      // rvalue reference wins.
      if (meType == rt_lvalueref && bestType == rt_rvalueref)
        bestWins++;
      else if (meType == rt_rvalueref && bestType == rt_lvalueref)
        meWins++;

      // If one is const and the other isn't, then the other wins; the only way
      // both could be present is if the receiver isn't const.
      if (!constReceiver) {
        if (me->m_isConst && !best->m_isConst)
          bestWins++;
        else if (!me->m_isConst && best->m_isConst)
          meWins++;
      }
    }

    // FIXME: need to handle better the case where neither is best because
    // neither is a subtype of the other, such as int vs uint.
    if (bestWins == 0 && meWins > 0)
      best = me;
    verify(bestWins * meWins == 0); // FIXME: handle ambiguity
  }

  return LookupFinalize(best, argEntities, returnValue);
}

Method *OverloadSet::Find() {
  for (Method *me : m_methods) {
    if (me->m_isConst || !me->m_formals->m_hasThis)
      continue;
    FormalArguments *formals = me->m_formals;
    if (formals->m_arguments.size() == formals->ImplicitArgCount())
      return me;
  }
  return nullptr;
}

Method *OverloadSet::Find(Type t) {
  for (Method *me : m_methods) {
    if (me->m_isConst || !me->m_formals->m_hasThis)
      continue;
    FormalArguments *formals = me->m_formals;
    if (formals->m_arguments.size() != formals->ImplicitArgCount() + 1)
      continue;
    Argument *ae = formals->m_arguments[formals->ImplicitArgCount()];
    if (ae->m_mechanism != am_in)
      continue;
    if (ae->m_type == t)
      return me;
  }
  return nullptr;
}

void OverloadSet::AddGeneratedMethod(Method *me) {
  verify(!m_resolved);
  me->m_isIntroducing = true;
  me->m_isRedefining = true;
  m_methods.push_back(me);
  me->TrackResolution();
}

bool OverloadSet::HasVirtualMethods() {
  for (Method *me : m_methods) {
    me->Setup();
    if (me->IsVirtual())
      return true;
  }

  return false;
}

void OverloadSet::AssignVtblSlots(size_t &vtblSize) {
  for (Method *me : m_methods) {
    if (!me->IsVirtual())
      continue;

    // Only introducing methods get assigned a slot.
    if (!me->m_isIntroducing)
      continue;

    // Inherited methods are represented by a placeholder, which does not have
    // m_isIntroducing set, so if we got this far then assign a slot.
    me->m_vtblSlot = int(vtblSize++);
  }
}

void OverloadSet::SetVtblSlots() {
  for (Method *me : m_methods) {
    // Skip conflicted methods.
    if (me->m_kind == mk_conflicted)
      continue;

    me->SetVtblSlots();
  }
}

void OverloadSet::SetGenerator(void (Class::*gen)(OverloadSet *)) {
  verify(!m_resolved && !m_generator);
  m_generator = gen;
}

void OverloadSet::SetInherited(InheritedEntities &ies) {
  m_inherited.swap(ies);
}

bool OverloadSet::HasExplicitDestructor() {
  if (m_methods.size() > 0)
    return !m_methods[0]->IsGenerated() && Name() == wkhs_destructor;
  return false;
}

bool OverloadSet::HasEmptyArgListOverload() {
  for (auto me : m_methods) {
    if (me->HasNoArgs())
      return true;
  }

  return false;
}
