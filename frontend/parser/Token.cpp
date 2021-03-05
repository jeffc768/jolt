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

#include "Token.h"
#include "Location.h"
#include "util/InDeflator.h"
#include "util/Integer.h"
#include "util/String.h"
#include "util/Verify.h"

static const int BLOCKSIZE = 16383;

namespace {
  struct block_t {
    block_t          *m_next;
    Token             m_tokens[BLOCKSIZE];
  };
}

static block_t *g_block = nullptr;
static int g_next = BLOCKSIZE;

void *Token::operator new(size_t size) {
  verify(size == sizeof(Token));

  if (g_next == BLOCKSIZE) {
    block_t *old = g_block;
    g_block = reinterpret_cast<block_t *>(malloc(sizeof(block_t)));
    g_block->m_next = old;
    g_next = 0;
  }

  return g_block->m_tokens + g_next++;
}

void Token::Clear() {
  while (g_block) {
    block_t *next = g_block->m_next;
    free(g_block);
    g_block = next;
  }

  g_next = BLOCKSIZE;
}

String *Token::StringValue() {
  return safe_cast<String *>(m_value.m_object);
}

void Token::SetStringValue(String *v) {
  m_value.m_object = v;
}

Location Token::GetLocation() {
  return Location(m_line_no, m_col);
}

/******************************************************************************/

IMPLEMENT_OBJECT(Float)
