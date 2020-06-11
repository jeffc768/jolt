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

// Target is an abstract interface for implementing a back end to the Jolt
// compiler.  A different back end can be used for each epoch, compile time and
// run time, though it's possible for a single back end to handle both epochs
// (in theory... the design is currently too dependent on the bytecode target
// for epoch compile for any other target to support it).
//
// In keeping with the spirit of the Mozilla Public License, it is possible
// to add a new back end without modifying any files covered by the MPL.  If
// a company wants to hook up a proprietary code generator, it won't be forced
// to "share" any code that exposes any interfaces of this code generator,
// much less the code generator itself.

#pragma once

#include "util/Object.h"
#include <string>
#include <vector>

class Class;
class Const;
class Expr;
class Field;
class Value;
class Method;
class String;
class TranslateClosure;
class TargetClass;
class TargetConst;
class TargetEntity;
class TargetGlobal;
class TargetMethod;
class Type;

namespace BC {
  class BytecodeFunction;
}

using std::vector;

enum Epoch: uint8_t {
  ep_compile,
  ep_run
};

class Target {
  // Eventually, one can select by name which back end to use for each epoch
  // via command line arguments.  Static constructs shall register the back
  // ends linked into the compiler executable.  But for now, just hard code it.
  static Target *GetBytecodeInterpreter();

  // Selected target for epoch run.
  static Target      *s_runTarget;

  // Target name.
  const char         *m_name;

  // Next target in chain of available targets.
  Target             *m_link;

protected:
  Target(const char *name);
  virtual ~Target() { }

  virtual bool IsSimpleType_(Type t) = 0;
  virtual vector<size_t> GetStructLayout_(Type t) = 0;
  virtual unsigned GetAlignment_(Type t) = 0;

public:
  // Set target to use for epoch run.
  static bool SetTarget(const char *name);
  const char *TargetName() { return m_name; }
  virtual void Setup() = 0;

  // Get the target for the indicated epoch.
  static Target *Get(Epoch ep) {
    if (ep == ep_compile)
      return GetBytecodeInterpreter();
    else
      return s_runTarget;
  }

  // Target-specific versions of various entities are created as they are
  // added to a translation closure.
  virtual TargetClass *For(Class *) = 0;
  virtual TargetConst *For(Const *) = 0;
  virtual TargetGlobal *For(Field *) = 0;
  virtual TargetMethod *For(Method *) = 0;
  virtual TargetMethod *For(Method *, int) = 0; // FIXME

  // Evaluate a compile time expression whose closure is fully resolved (this
  // will not block on a dependency).  Not supported by all targets.
  virtual Value *Evaluate(TranslateClosure &tc, Value *result, Expr *e) = 0;

  // Write compiled code to a file.
  virtual void WriteToFile(const char *fname) = 0;

  // Is the type considered simple for the run target?  In other words, a
  // first-class value.
  static bool IsSimpleType(Type t);

  // Get layout information for a struct type for the run target.  One offset
  // is returned for each struct member; the final offset is the size of the
  // struct.
  static vector<size_t> GetStructLayout(Type t);

  // Get a type's alignment for the run target.
  static unsigned GetAlignment(Type t);
};

// The non-abstract methods of the Target* classes are implemented in
// ToBytecode.cpp.

class TargetClass: public Object {
protected:
  TargetClass(Class *ce) : m_class(ce) { }

public:
  // Generate the representation of the entity for the target.  This will be
  // called at most once for each entity per epoch.
  virtual void Generate() = 0;

  // Has global storage been allocated yet?  Only valid for epoch compile.
  virtual bool HasGlobalStorage(Epoch ep) = 0;

  // Return the global storage assigned to the entity.  The storage is allocated
  // on the first call.  Only valid for epoch compile.
  virtual void *GlobalStorage(Epoch ep) = 0;

  Class              *m_class;
};

class TargetConst: public Object {
protected:
  TargetConst(Const *ce) : m_const(ce) { }

public:
  // Generate the representation of the entity for the target.  This will be
  // called at most once for each entity per epoch.
  virtual void Generate() = 0;

  // Return the global storage assigned to the entity.  The storage is allocated
  // on the first call.  Only valid for epoch compile.
  virtual void *GlobalStorage(Epoch ep) = 0;

  Const              *m_const;
};

class TargetGlobal: public Object {
protected:
  TargetGlobal(Field *fe) : m_field(fe) { }

public:
  // Generate the representation of the entity for the target.  This will be
  // called at most once for each entity per epoch.
  virtual void Generate() = 0;

  // Return the global storage assigned to the entity.  The storage is allocated
  // on the first call.  Only valid for epoch compile.
  virtual void *GlobalStorage(Epoch ep) = 0;

  Field              *m_field;
};

class TargetMethod: public Object {
protected:
  TargetMethod(Method *me) : m_method(me) { }

public:
  // Generate the representation of the entity for the target.  This will be
  // called at most once for each entity per epoch.
  virtual void Generate() = 0;

  // Return the global storage assigned to the entity.  The storage is allocated
  // on the first call.  Only valid for epoch compile.
  virtual void *GlobalStorage(Epoch ep) = 0;

  Method             *m_method;
};
