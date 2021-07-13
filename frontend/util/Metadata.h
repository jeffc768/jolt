// Copyright (c) 2015, Jeff Cohen
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

// Metadata keeps track of the data needed for serialization.  Since that
// does a lot of the heavy lifting needed for dynamic casting, it handles that
// too, eliminating the need for RTTI.  Since LLVM is not friendly to clients
// that need RTTI, because it is doing the same thing to implement bitcode, this
// is a good thing.

#pragma once

#include <cstdint>

class Deflator;
class Inflator;
class Object;

using external_id_t = uint8_t;
using class_id_t    = uint16_t;

// Every Object subclass has a static member of this type.  The static
// constructor registers the class.
class Metadata {
public:
  // External id assigned to the class when its instances of serialized.  Zero
  // if the class is not serializable.
  external_id_t        m_externalId            = 0;

  // Internal ordinal, which is larger than its base class and smaller than any
  // assigned to child classes.
  class_id_t           m_ord                   = 0;

  // Largest ordinal assigned to a subclass.  A class is a child, directly or
  // indirectly, if and only if its ord is between m_ord and m_ordOfLastSubclass
  // inclusve.
  class_id_t           m_ordOfLastSubclass     = 0;

  // Function to call to deflate a class instance into a serialized stream.
  // Non-null only if the class is serializable.
  using deflator_func_t = void (*)(Deflator &, Object *self);
  deflator_func_t      m_deflator              = nullptr;

  // Functions to call to create a new instance of the class, indexed by
  // external id.
  using creator_func_t = Object *(*)(Inflator &);
  static creator_func_t GetInflator(external_id_t ord);

  // Registers class with metadata of base class.
  Metadata(Metadata *base);

  Metadata(const Metadata &) = delete;
  Metadata &operator=(const Metadata &) = delete;

  // Called from main() before anything else happens.
  static void SetupMetadata();

  template<class T> static Object *Inflate(Inflator &IF);
  template<class T> static void Deflate(Deflator &DF, Object *self);
  template<class T> static void Reference(Deflator &DF, Object *self);
};
