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

// Mix-in interfaces must inherit from Interface, so that managed pointers
// to interfaces know how to process them during garbage collection.

#pragma once

#include "Object.h"

class Deflator;
class Inflator;

template<class T>
class Interface {
  friend Deflator;
  friend Inflator;

  unsigned offsetToObject;

protected:
  Interface() : offsetToObject(0) { }
  virtual ~Interface() { }

public:
  // Implemented by IMPLEMENT_OBJECT.  Note that it is not declared virtual
  // there, so if an object doesn't implement an interface, that method is a
  // trivial, inline, non-virtual method that will get discarded.
  virtual Object *GetObjectBase() = 0;

  using interface_t = T;

  // And since an object may be moved via a direct pointer before its interface
  // is accessed, use this opportunity to cache the offset while it's being
  // moved.
  Interface(const Interface &that) {
    Interface *ptr = const_cast<Interface *>(&that);
    if (ptr->offsetToObject == 0) {
      Object *obj = ptr->GetObjectBase();
      ptr->offsetToObject = (unsigned)(intptr_t(ptr) - intptr_t(obj));
    }

    offsetToObject = ptr->offsetToObject;
  }

  virtual Metadata &GetMetadata() = 0;
};
