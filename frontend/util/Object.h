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

// Object is the base class for just about everything.  It provides
// serialization and a custom memory allocator.

#pragma once

#include "Metadata.h"
#include "Verify.h"
#include <cstdint>
#include <new>
#include <utility>

class Deflator;
class Inflator;
class Parameters;

class Object {
  class ReleaseAllMemory;
  friend class ReleaseAllMemory;

  static ReleaseAllMemory s_ram;

protected:
  virtual ~Object() { }

  // Destructors are protected, to prevent objects from being created
  // as local variables.  As the DECLARE_ macros also declare a default,
  // protected destructor, this method is needed to do custom destruction.  It
  // is called immediately before the destructor.
  virtual void PreDestroy() { }

  Object() = default;

  Object(const Object &) = delete;
  Object(Object &&) = delete;
  Object &operator=(const Object &) = delete;
  Object &operator=(Object &&) = delete;

  Object(Inflator &IF);

  // Every object must be able to report its size.  It is typically
  // implemented by IMPLEMENT_OBJECT.
  virtual size_t GetObjectSize() = 0;

public:
  static Metadata s_metadata;
  virtual Metadata &GetMetadata() = 0;

  // Return the root of the nearest enclosing template.  Only overridden by
  // subclasses whose instances outside a template can be directly referenced
  // after name binding by something in a template.
  virtual Parameters **GetTemplateRootHolder();

  void *operator new(size_t, void *p) { return p; }

  // Memory for an object must come from our memory manager.
  void *operator new(size_t s);
};

/******************************************************************************/

// Metaprogramming aids -- a big motivator for Jolt.

template<class P> struct PointingAt;
template<class P> struct PointingAt<P *> {
  using type = P;
};


// Handle upcasting; statically known to succeed.
template<class To, class From>
inline std::enable_if_t<
    std::is_base_of<std::remove_pointer_t<To>,
                    std::remove_pointer_t<From>>::value, To>
dyn_cast(From obj) {
  return obj;
}

// Handle downcasting; may or may not succeed.
template<class To, class From>
inline std::enable_if_t<!std::is_same<To, From>::value &&
    std::is_base_of<std::remove_pointer_t<From>,
                    std::remove_pointer_t<To>>::value, To>
dyn_cast(From obj) {
  // Note: remove_pointer_t succeeds even if the type isn't a pointer;
  // PointingAt will fail.
  using T = typename PointingAt<To>::type;

  if (obj) {
    Metadata &mmdFrom = obj->GetMetadata();
    Metadata &mmdTo = T::s_metadata;

    if (mmdFrom.m_ord >= mmdTo.m_ord &&
        mmdFrom.m_ord <= mmdTo.m_ordOfLastSubclass) {
      return static_cast<To>(obj);
    }
  }

  return nullptr;
}

template<class To, class From> inline To safe_cast(From obj) {
#ifndef NDEBUG
  verify(!obj || dyn_cast<To>(obj));
#endif
  return static_cast<To>(obj);
}

/******************************************************************************/

// All objects must use this macro in their class declaration.
#define DECLARE_OBJECT2(T, BASE) \
  protected: \
    friend Metadata; \
    virtual size_t GetObjectSize(); \
    virtual ~T() = default; \
    Object *GetObjectBase() { return this; } \
\
  public: \
    using base_t = BASE; \
    static Metadata s_metadata; \
    virtual Metadata &GetMetadata();

#define DECLARE_OBJECT(T) DECLARE_OBJECT2(T, Object)

// Macros ending with _ are helpers not to be used outside of this file.
#define IMPLEMENT_OBJECT_NOSIZE_(T, PREFIX) \
  Metadata PREFIX T::s_metadata { &base_t::s_metadata }; \
\
  Metadata &PREFIX T::GetMetadata() { return s_metadata; }

#define IMPLEMENT_OBJECT_(T, PREFIX) \
  IMPLEMENT_OBJECT_NOSIZE_(T, PREFIX) \
  size_t PREFIX T::GetObjectSize() { return sizeof(T); }

// All fixed-sized objects must use this macro at the global level in its cpp
// file.  The "2" variants are for objects declared within other classes.
#define IMPLEMENT_OBJECT(T) IMPLEMENT_OBJECT_(T,)

#define IMPLEMENT_OBJECT2(T, PREFIX) IMPLEMENT_OBJECT_(T, PREFIX::)

// All variable-sized objects must use this macro instead.
#define IMPLEMENT_OBJECT_NOSIZE(T) \
  IMPLEMENT_OBJECT_NOSIZE_(T,)

#define IMPLEMENT_OBJECT_NOSIZE2(T, PREFIX) \
  IMPLEMENT_OBJECT_NOSIZE_(T, PREFIX::)
