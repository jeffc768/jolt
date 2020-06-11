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

#include "DeflatedObject.h"
#include "InDeflator.h"
#include "entity/Parameters.h"

IMPLEMENT_OBJECT(DeflatedObject)

void DeflatedObject::PreDestroy() {
  delete[] m_blob;
  m_blob = nullptr;
}

void DeflatedObject::DeflateFields(Deflator &DF) {
  // We can't just dump the blob in an external module.  It has Types in it
  // that would be meaningless to an importing module.  We need to temporarily
  // inflate it, then re-deflate it in module mode.
  TemplateInflator IF;
  Object *obj = IF(this);
  DF << obj;
}

DeflatedObject *DeflatedObject::Inflate(Inflator &IF) {
  // And reverse the process on inflation.
  Object *obj;
  IF >> obj;

  TemplateDeflator DF;
  DeflatedObject *dobj;
  if (auto p = dyn_cast<Parameters *>(obj))
    dobj = DF(p);
  else
    dobj = DF(obj);
  IF.RegisterObject(dobj);
  return dobj;
}
