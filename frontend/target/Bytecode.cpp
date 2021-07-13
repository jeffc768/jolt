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

#include "Config.h"
#include "Bytecode.h"
#include "type/Type.h"
#include <cstdio>

using namespace BC;

//#define BC_TRACE

/******************************************************************************/

namespace {
  struct OpcodeInfo {
    uint8_t         m_length;       // Length of instruction in bytes
    int8_t          m_delta;        // Change in stack depth (see below)
    bool            m_isTerminator; // Must terminate a block
    uint8_t         m_type;         // Type of following value
    char            m_name[16];     // Printable name of instruction
  };

  // A m_type of ot_delta indicates that the number following the opcode (when
  // added to m_delta) has the amount the stack is popped.
}

enum OpcodeType {
  ot_none,
  ot_branchOffset,
  ot_delta,
  ot_address,
  ot_positive,
  ot_negative,
  ot_signed
};

static const OpcodeInfo g_opcodes[] = {
  /* op_add32        */ { 1,  -1,  false,  ot_none,         "add32"           },
  /* op_add64        */ { 1,  -1,  false,  ot_none,         "add64"           },
  /* op_addf32       */ { 1,  -1,  false,  ot_none,         "addf32"          },
  /* op_addf64       */ { 1,  -1,  false,  ot_none,         "addf64"          },
  /* op_and32        */ { 1,  -1,  false,  ot_none,         "and32"           },
  /* op_and64        */ { 1,  -1,  false,  ot_none,         "and64"           },
  /* op_call         */ { 2,  -1,  false,  ot_delta,        "call"            },
  /* op_callfat      */ { 2,   1,  false,  ot_delta,        "callfat"         },
  /* op_callval      */ { 2,   0,  false,  ot_delta,        "callval"         },
  /* op_convf32tof64 */ { 1,   0,  false,  ot_none,         "op_convf32tof64" },
  /* op_convf32toi32 */ { 1,   0,  false,  ot_none,         "op_convf32toi32" },
  /* op_convf32toi64 */ { 1,   0,  false,  ot_none,         "op_convf32toi64" },
  /* op_convf64tof32 */ { 1,   0,  false,  ot_none,         "op_convf64tof32" },
  /* op_convf64toi32 */ { 1,   0,  false,  ot_none,         "op_convf64toi32" },
  /* op_convf64toi64 */ { 1,   0,  false,  ot_none,         "op_convf64toi64" },
  /* op_convi32tof32 */ { 1,   0,  false,  ot_none,         "op_convi32toi32" },
  /* op_convi32tof64 */ { 1,   0,  false,  ot_none,         "op_convi32toi64" },
  /* op_convi64tof32 */ { 1,   0,  false,  ot_none,         "op_convi64toi32" },
  /* op_convi64tof64 */ { 1,   0,  false,  ot_none,         "op_convi64toi64" },
  /* op_convs32to64  */ { 1,   0,  false,  ot_none,         "convs32to64"     },
  /* op_convu32to64  */ { 1,   0,  false,  ot_none,         "convu32to64"     },
  /* op_conv64to32   */ { 1,   0,  false,  ot_none,         "conv64to32"      },
  /* op_divf32       */ { 1,  -1,  false,  ot_none,         "divf32"          },
  /* op_divf64       */ { 1,  -1,  false,  ot_none,         "divf64"          },
  /* op_divs32       */ { 1,  -1,  false,  ot_none,         "divs32"          },
  /* op_divs64       */ { 1,  -1,  false,  ot_none,         "divs64"          },
  /* op_divu32       */ { 1,  -1,  false,  ot_none,         "divu32"          },
  /* op_divu64       */ { 1,  -1,  false,  ot_none,         "divu64"          },
  /* op_dup          */ { 1,   1,  false,  ot_none,         "dup"             },
  /* op_exts32       */ { 2,   0,  false,  ot_positive,     "exts32"          },
  /* op_exts64       */ { 2,   0,  false,  ot_positive,     "exts64"          },
  /* op_extz32       */ { 2,   0,  false,  ot_positive,     "extz32"          },
  /* op_extz64       */ { 2,   0,  false,  ot_positive,     "extz64"          },
  /* op_brfalse8     */ { 2,  -1,  true,   ot_branchOffset, "brfalse8"        },
  /* op_brfalse16    */ { 3,  -1,  true,   ot_branchOffset, "brfalse16"       },
  /* op_brfalse32    */ { 5,  -1,  true,   ot_branchOffset, "brfalse32"       },
  /* op_brtrue8      */ { 2,  -1,  true,   ot_branchOffset, "brtrue8"         },
  /* op_brtrue16     */ { 3,  -1,  true,   ot_branchOffset, "brtrue16"        },
  /* op_brtrue32     */ { 5,  -1,  true,   ot_branchOffset, "brtrue32"        },
  /* op_jump8        */ { 2,   0,  true,   ot_branchOffset, "jump8"           },
  /* op_jump16       */ { 3,   0,  true,   ot_branchOffset, "jump16"          },
  /* op_jump32       */ { 5,   0,  true,   ot_branchOffset, "jump32"          },
  /* op_leaargs8     */ { 2,   1,  false,  ot_positive,     "leaargs8"        },
  /* op_leaargs16    */ { 3,   1,  false,  ot_positive,     "leaargs16"       },
  /* op_leaargs32    */ { 5,   1,  false,  ot_positive,     "leaargs32"       },
  /* op_leaframe8    */ { 2,   1,  false,  ot_positive,     "leaframe8"       },
  /* op_leaframe16   */ { 3,   1,  false,  ot_positive,     "leaframe16"      },
  /* op_leaframe32   */ { 5,   1,  false,  ot_positive,     "leaframe32"      },
  /* op_leaptr8      */ { 2,   0,  false,  ot_positive,     "leaptr8"         },
  /* op_leaptr16     */ { 3,   0,  false,  ot_positive,     "leaptr16"        },
  /* op_leaptr32     */ { 5,   0,  false,  ot_positive,     "leaptr32"        },
  /* op_loads8       */ { 1,   0,  false,  ot_none,         "loads8"          },
  /* op_loadu8       */ { 1,   0,  false,  ot_none,         "loadu8"          },
  /* op_loads16      */ { 1,   0,  false,  ot_none,         "loads16"         },
  /* op_loadu16      */ { 1,   0,  false,  ot_none,         "loadu16"         },
  /* op_load32       */ { 1,   0,  false,  ot_none,         "load32"          },
  /* op_load64       */ { 1,   0,  false,  ot_none,         "load64"          },
  /* op_loadfat      */ { 1,   1,  false,  ot_none,         "loadfat"         },
  /* op_mulf32       */ { 1,  -1,  false,  ot_none,         "mulf32"          },
  /* op_mulf64       */ { 1,  -1,  false,  ot_none,         "mulf64"          },
  /* op_muls32       */ { 1,  -1,  false,  ot_none,         "muls32"          },
  /* op_muls64       */ { 1,  -1,  false,  ot_none,         "muls64"          },
  /* op_mulu32       */ { 1,  -1,  false,  ot_none,         "mulu32"          },
  /* op_mulu64       */ { 1,  -1,  false,  ot_none,         "mulu64"          },
  /* op_neg32        */ { 1,   0,  false,  ot_none,         "neg32"           },
  /* op_neg64        */ { 1,   0,  false,  ot_none,         "neg64"           },
  /* op_negf32       */ { 1,   0,  false,  ot_none,         "negf32"          },
  /* op_negf64       */ { 1,   0,  false,  ot_none,         "negf64"          },
  /* op_not32        */ { 1,   0,  false,  ot_none,         "not32"           },
  /* op_not64        */ { 1,   0,  false,  ot_none,         "not64"           },
  /* op_or32         */ { 1,  -1,  false,  ot_none,         "or32"            },
  /* op_or64         */ { 1,  -1,  false,  ot_none,         "or64"            },
  /* op_pop          */ { 1,  -1,  false,  ot_none,         "pop"             },
  /* op_pop8         */ { 2,   0,  false,  ot_delta,        "pop8"            },
  /* op_pop16        */ { 3,   0,  false,  ot_delta,        "pop16"           },
  /* op_pushp8       */ { 2,   1,  false,  ot_positive,     "pushp8"          },
  /* op_pushn8       */ { 2,   1,  false,  ot_negative,     "pushn8"          },
  /* op_pushp16      */ { 3,   1,  false,  ot_positive,     "pushp16"         },
  /* op_pushn16      */ { 3,   1,  false,  ot_negative,     "pushn16"         },
  /* op_push32       */ { 5,   1,  false,  ot_signed,       "push32"          },
  /* op_push64       */ { 9,   1,  false,  ot_signed,       "push64"          },
  /* op_rems32       */ { 1,  -1,  false,  ot_none,         "rems32"          },
  /* op_rems64       */ { 1,  -1,  false,  ot_none,         "rems64"          },
  /* op_remu32       */ { 1,  -1,  false,  ot_none,         "remu32"          },
  /* op_remu64       */ { 1,  -1,  false,  ot_none,         "remu64"          },
  /* op_ret          */ { 1,   0,  true,   ot_none,         "ret"             },
  /* op_retfat       */ { 1,  -2,  true,   ot_none,         "retfat"          },
  /* op_retval       */ { 1,  -1,  true,   ot_none,         "retval"          },
  /* op_seteq32      */ { 1,  -1,  false,  ot_none,         "seteq32"         },
  /* op_seteq64      */ { 1,  -1,  false,  ot_none,         "seteq64"         },
  /* op_seteqf32     */ { 1,  -1,  false,  ot_none,         "seteqf32"        },
  /* op_seteqf64     */ { 1,  -1,  false,  ot_none,         "seteqf64"        },
  /* op_setgef32     */ { 1,  -1,  false,  ot_none,         "setgef32"        },
  /* op_setgef64     */ { 1,  -1,  false,  ot_none,         "setgef64"        },
  /* op_setges32     */ { 1,  -1,  false,  ot_none,         "setges32"        },
  /* op_setges64     */ { 1,  -1,  false,  ot_none,         "setges64"        },
  /* op_setgeu32     */ { 1,  -1,  false,  ot_none,         "setgeu32"        },
  /* op_setgeu64     */ { 1,  -1,  false,  ot_none,         "setgeu64"        },
  /* op_setgtf32     */ { 1,  -1,  false,  ot_none,         "setgtf32"        },
  /* op_setgtf64     */ { 1,  -1,  false,  ot_none,         "setgtf64"        },
  /* op_setgts32     */ { 1,  -1,  false,  ot_none,         "setgts32"        },
  /* op_setgts64     */ { 1,  -1,  false,  ot_none,         "setgts64"        },
  /* op_setgtu32     */ { 1,  -1,  false,  ot_none,         "setgtu32"        },
  /* op_setgtu64     */ { 1,  -1,  false,  ot_none,         "setgtu64"        },
  /* op_setlef32     */ { 1,  -1,  false,  ot_none,         "setlef32"        },
  /* op_setlef64     */ { 1,  -1,  false,  ot_none,         "setlef64"        },
  /* op_setles32     */ { 1,  -1,  false,  ot_none,         "setles32"        },
  /* op_setles64     */ { 1,  -1,  false,  ot_none,         "setles64"        },
  /* op_setleu32     */ { 1,  -1,  false,  ot_none,         "setleu32"        },
  /* op_setleu64     */ { 1,  -1,  false,  ot_none,         "setleu64"        },
  /* op_setltf32     */ { 1,  -1,  false,  ot_none,         "setltf32"        },
  /* op_setltf64     */ { 1,  -1,  false,  ot_none,         "setltf64"        },
  /* op_setlts32     */ { 1,  -1,  false,  ot_none,         "setlts32"        },
  /* op_setlts64     */ { 1,  -1,  false,  ot_none,         "setlts64"        },
  /* op_setltu32     */ { 1,  -1,  false,  ot_none,         "setltu32"        },
  /* op_setltu64     */ { 1,  -1,  false,  ot_none,         "setltu64"        },
  /* op_setne32      */ { 1,  -1,  false,  ot_none,         "setne32"         },
  /* op_setne64      */ { 1,  -1,  false,  ot_none,         "setne64"         },
  /* op_setnef32     */ { 1,  -1,  false,  ot_none,         "setnef32"        },
  /* op_setnef64     */ { 1,  -1,  false,  ot_none,         "setnef64"        },
  /* op_shl32        */ { 1,  -1,  false,  ot_none,         "shl32"           },
  /* op_shl64        */ { 1,  -1,  false,  ot_none,         "shl64"           },
  /* op_shrs32       */ { 1,  -1,  false,  ot_none,         "shrs32"          },
  /* op_shrs64       */ { 1,  -1,  false,  ot_none,         "shrs64"          },
  /* op_shru32       */ { 1,  -1,  false,  ot_none,         "shru32"          },
  /* op_shru64       */ { 1,  -1,  false,  ot_none,         "shru64"          },
  /* op_store8       */ { 1,  -2,  false,  ot_none,         "store8"          },
  /* op_store16      */ { 1,  -2,  false,  ot_none,         "store16"         },
  /* op_store32      */ { 1,  -2,  false,  ot_none,         "store32"         },
  /* op_store64      */ { 1,  -2,  false,  ot_none,         "store64"         },
  /* op_storefat     */ { 1,  -3,  false,  ot_none,         "storefat"        },
  /* op_subf32       */ { 1,  -1,  false,  ot_none,         "subf32"          },
  /* op_subf64       */ { 1,  -1,  false,  ot_none,         "subf64"          },
  /* op_sub32        */ { 1,  -1,  false,  ot_none,         "sub32"           },
  /* op_sub64        */ { 1,  -1,  false,  ot_none,         "sub64"           },
  /* op_swap         */ { 1,   0,  false,  ot_none,         "swap"            },
  /* op_swap8        */ { 2,   0,  false,  ot_positive,     "swap8"           },
  /* op_swap16       */ { 3,   0,  false,  ot_positive,     "swap16"          },
  /* op_xor32        */ { 1,  -1,  false,  ot_none,         "xor32"           },
  /* op_xor64        */ { 1,  -1,  false,  ot_none,         "xor64"           },

  /* op_exit         */ { 1,   0,  true,   ot_branchOffset, "exit"            },
  /* op_fallthrough  */ { 1,   0,  true,   ot_none,         "fallthrough"     },
  /* op_if           */ { 1,  -1,  true,   ot_branchOffset, "if"              }
};

static_assert(op_end_of_opcodes * sizeof(OpcodeInfo) == sizeof(g_opcodes));

/******************************************************************************/

static uint16_t g_token = 0;  // Token to identify blocks already visited.

BasicBlock::BasicBlock(BasicBlock *chain)
    : m_chain(chain),
      m_bytes(),
      m_offsetLastOpcode(-1),
      m_successors(),
      m_stackOnEntry(0),
      m_stackOnExit(0),
      m_baseBlock(nullptr),
      m_adjustment(0),
      m_keepValue(false),
      m_token(g_token),
      m_branchSize(4),
      m_offset(0),
      m_fallThrough(false) {
}

void BasicBlock::Append(Opcode op) {
  verify(m_offsetLastOpcode < 0 ||
         !g_opcodes[m_bytes[m_offsetLastOpcode]].m_isTerminator);
  m_offsetLastOpcode = int(m_bytes.size());
  m_bytes.push_back(static_cast<uint8_t>(op));
}

void BasicBlock::AppendBlock(BasicBlock *target) {
  m_successors.push_back(target);
}

void BasicBlock::AppendByte(unsigned val) {
  verify(val < 256);
  m_bytes.push_back(static_cast<uint8_t>(val));
}

void BasicBlock::AppendWord(unsigned val) {
  verify(val < 65536);
  m_bytes.push_back(static_cast<uint8_t>(val >> 8));
  m_bytes.push_back(static_cast<uint8_t>(val));
}

void BasicBlock::AppendDWord(uint32_t val) {
  m_bytes.push_back(static_cast<uint8_t>(val >> 24));
  m_bytes.push_back(static_cast<uint8_t>(val >> 16));
  m_bytes.push_back(static_cast<uint8_t>(val >> 8));
  m_bytes.push_back(static_cast<uint8_t>(val));
}

void BasicBlock::AppendQWord(uint64_t val) {
  AppendDWord(static_cast<uint32_t>(val >> 32));
  AppendDWord(static_cast<uint32_t>(val));
}

void BasicBlock::AppendLea(Opcode op, unsigned val) {
  if (op == op_leaptr8 && val == 0)
    return;
  uint8_t opcode = static_cast<uint8_t>(op);
  if (val < 0x100) {
    m_bytes.push_back(opcode);
    AppendByte(val);
  } else if (val < 0x10000) {
    m_bytes.push_back(opcode + 1);
    AppendWord(val);
  } else {
    m_bytes.push_back(opcode + 2);
    AppendDWord(val);
  }
}

void BasicBlock::AppendLoad(Type t) {
  if (t == tk_pointer)
    if (t.IsThin())
      Append(sizeof(void *) == 4 ? op_load32 : op_load64);
    else
      Append(op_loadfat);
  else if (t.IsString())
    Append(sizeof(void *) == 4 ? op_load32 : op_load64);
  else if (t == tk_void)
    Append(op_pop);
  else {
    verify(t.IsSimpleType());
    switch (t.StorageSize()) {
      case 0: /* let address stand in for value */ break;
      case 1:  Append(t.IsSigned() ? op_loads8  : op_loadu8);   break;
      case 2:  Append(t.IsSigned() ? op_loads16 : op_loadu16);  break;
      case 4:  Append(op_load32);   break;
      case 8:  Append(op_load64);   break;
      default: verify(false);
    }
  }
}

void BasicBlock::AppendPush(int val) {
  if (val >= 0) {
    AppendPush(unsigned(val));
  } else if (val >= -256) {
    Append(op_pushn8);
    AppendByte(val & 255);
  } else if (val >= -65536) {
    Append(op_pushn16);
    AppendWord(val & 65535);
  } else {
    Append(op_push32);
    AppendDWord(val);
  }
}

void BasicBlock::AppendPush(unsigned val) {
  if (val < 256) {
    Append(op_pushp8);
    AppendByte(val);
  } else if (val < 65536) {
    Append(op_pushp16);
    AppendWord(val);
  } else {
    Append(op_push32);
    AppendDWord(val);
  }
}

void BasicBlock::AppendPush(int64_t val) {
  if (val >= 0) {
    AppendPush(static_cast<uint64_t>(val));
  } else if (val >= -(32768LL * 65536LL)) {
    AppendPush(static_cast<int>(val));
    Append(op_convs32to64);
  } else {
    Append(op_push64);
    AppendQWord(val);
  }
}

void BasicBlock::AppendPush(uint64_t val) {
  if (val < (65536LL * 65536LL)) {
    AppendPush(static_cast<unsigned>(val));
    Append(op_convu32to64);
  } else {
    Append(op_push64);
    AppendQWord(val);
  }
}

void BasicBlock::AppendPush(const void *val) {
  uintptr_t v = reinterpret_cast<uintptr_t>(val);
  if constexpr (sizeof(void *) == 4)
    AppendPush(static_cast<unsigned>(v));
  else
    AppendPush(static_cast<uint64_t>(v));
}

void BasicBlock::AppendPush(float val) {
  static_assert(sizeof(float) == sizeof(uint32_t));
  Append(op_push32);
  AppendDWord(*(uint32_t*)&val);
}

void BasicBlock::AppendPush(double val) {
  static_assert(sizeof(double) == sizeof(uint64_t));
  Append(op_push64);
  AppendQWord(*(uint64_t*)&val);
}

void BasicBlock::AppendStore(Type t) {
  if (t == tk_pointer)
    if (t.IsThin())
      Append(sizeof(void *) == 4 ? op_store32 : op_store64);
    else
      Append(op_storefat);
  else if (t.IsString())
    Append(sizeof(void *) == 4 ? op_store32 : op_store64);
  else if (t == tk_void)
    Append(op_pop);
  else {
    verify(t.IsSimpleType());
    switch (t.StorageSize()) {
      case 0:  Append(op_pop); Append(op_pop); break;
      case 1:  Append(op_store8);   break;
      case 2:  Append(op_store16);  break;
      case 4:  Append(op_store32);  break;
      case 8:  Append(op_store64);  break;
      default: verify(false);
    }
  }
}

void BasicBlock::SetMerge(BasicBlock *base, int adjustment, bool keepValue) {
  m_baseBlock = base;
  m_adjustment = adjustment;
  m_keepValue = keepValue;
}

uint8_t *BasicBlock::AssembleBlocks(BasicBlock *entry, size_t &maxStack) {
  // Pass 1:  Determine stack depths for all reachable blocks.
  DetermineStackDepths(entry, maxStack);

  // Pass 2:  Convert op_exit to op_pop followed by op_jump32.
  ConvertExitsToJumps(entry);

  // Pass 3:  Eliminate empty blocks; i.e., blocks that only contain a
  //          terminating instruction.
  RemoveEmptyBlocks(entry);

  // Pass 4:  Assign a physical ordering to the blocks, eliminate jumps to
  //          immediately following blocks, and convert op_if to op_brtrue
  //          or op_brfalse.
  vector<BasicBlock *> ordering;
  OrderBlocks(entry, ordering);

  // Pass 5:  Assign offsets to all blocks and determine minimum offsets for
  //          all branching instructions.
  size_t imageSize = AssignOffsets(ordering);

  // Pass 6:  Linearize.
  return Linearize(ordering, imageSize);
}

void BasicBlock::DetermineStackDepths(BasicBlock *entry, size_t &maxStack) {
  BasicBlockVector todo;
  todo.push_back(entry);
  maxStack = 0;
  entry->m_token = ++g_token;

  while (todo.size() > 0) {
    BasicBlock *b = todo.back();
    todo.pop_back();

    // The stack on entry was set by our predecessor when we were pushed onto
    // the todo stack (or defaulted to zero for the entry block), or if this is
    // a merge block, is the same as the base block for the control flow
    // subgraph this block closes out, plus adjustments.
    size_t depth = b->GetStackOnEntry();

    // Walk the instructions one by one, computing the stack depth along
    // the way.
    verify(b->m_offsetLastOpcode >= 0);
    int i = 0;
    do {
      const OpcodeInfo &oi = g_opcodes[b->m_bytes[i]];
      verify(oi.m_isTerminator ^ (i < b->m_offsetLastOpcode));

      // Compute delta to stack depth.
      int delta = oi.m_delta;
      if (oi.m_type == ot_delta) {
        if (oi.m_length == 2)
          delta -= b->m_bytes[i + 1];
        else if (oi.m_length == 3)
          delta -= (b->m_bytes[i + 1] << 8) + b->m_bytes[i + 2];
        else
          verify(false);
      }

      // Adjust stack depths.
      verify(delta >= 0 || size_t(-delta) <= depth);
      depth += delta;
      if (depth > maxStack)
        maxStack = depth;

      // Advance to next instruction.
      i += oi.m_length;
    } while (i <= b->m_offsetLastOpcode);
    b->m_stackOnExit = depth;

    // Handle successors.
    for (i = 0; size_t(i) < b->m_successors.size(); i++) {
      BasicBlock *s = b->m_successors[i];

      // Propagate depth to successor blocks.
      if (s->m_baseBlock) {
        // Never mind.  Merge blocks do not get their starting stack depth from
        // their predecessors.  However, assuming we're not exiting to this
        // block, we can verify our depth matches the merge block's.
        if (b->m_bytes[b->m_offsetLastOpcode] != op_exit) {
          verify(depth == s->GetStackOnEntry());
        }
      } else if (s->m_token == g_token) {
        // The successor has already been visited.  Excluding merge blocks, this
        // can happen only for back edges in loops.  Verify that the depth
        // remains unchanged on each iteration.
        verify(b->m_bytes[b->m_offsetLastOpcode] != op_exit);
        verify(depth == s->GetStackOnEntry());
      } else {
        // Now we can propagate depth.
        verify(b->m_bytes[b->m_offsetLastOpcode] != op_exit);
        s->m_stackOnEntry = depth;
      }

      // Queue for visiting if it hasn't been already.
      if (s->m_token != g_token) {
        s->m_token = g_token;
        todo.push_back(s);
      }
    }
  }

  // Convert stack size to bytes.
  maxStack *= sizeof(Union);
}

void BasicBlock::ConvertExitsToJumps(BasicBlock *entry) {
  BasicBlockVector todo;
  todo.push_back(entry);
  entry->m_token = ++g_token;

  while (todo.size() > 0) {
    BasicBlock *b = todo.back();
    todo.pop_back();

    // Look for blocks terminating with an op_exit.
    if (b->m_bytes[b->m_offsetLastOpcode] == op_exit) {
      BasicBlock *s = b->m_successors[0];

      verify(b->m_stackOnExit >= s->m_stackOnEntry);
      verify(b->m_stackOnExit - s->m_stackOnEntry < 65536);
      unsigned delta = unsigned(b->m_stackOnExit - s->m_stackOnEntry);

      // Remove the op_exit from the end of the block.
      b->m_bytes.pop_back();
      b->m_offsetLastOpcode = -1;

      // If the merge block expects a value on the top of the stack, and the
      // stack must also shrink, that value must remain on the top after the
      // stack is popped.
      if (delta == 0) {
        // No stack adjustment needed.
      } else if (delta == 1) {
        if (s->m_keepValue)
          b->Append(op_swap);
        b->Append(op_pop);
      } else if (delta < 256) {
        if (s->m_keepValue) {
          b->Append(op_swap8);
          b->AppendByte(delta);
        }
        b->Append(op_pop8);
        b->AppendByte(delta);
      } else {
        if (s->m_keepValue) {
          b->Append(op_swap16);
          b->AppendWord(delta);
        }
        b->Append(op_pop16);
        b->AppendWord(delta);
      }
      b->Append(op_jump32);
    }

    // Queue successors.
    for (size_t i = 0; i < b->m_successors.size(); i++) {
      BasicBlock *s = b->m_successors[i];

      if (s->m_token != g_token) {
        s->m_token = g_token;
        todo.push_back(s);
      }
    }
  }
}

void BasicBlock::RemoveEmptyBlocks(BasicBlock *entry) {
  BasicBlockVector todo;
  todo.push_back(entry);
  entry->m_token = ++g_token;

  while (todo.size() > 0) {
    BasicBlock *b = todo.back();
    todo.pop_back();

    bool done;
    do {
      done = true;
      if (b->m_successors.size() == 1) {
        // When there's only one successor, a qualifying empty block can have
        // any terminating instruction.  Simply replace our terminating jump
        // with whatever is in the successor.
        BasicBlock *s = b->m_successors[0];
        if (s->m_offsetLastOpcode > 0)
          break;
        b->m_bytes[b->m_offsetLastOpcode] = s->m_bytes[0];
        b->m_successors.assign(s->m_successors.begin(), s->m_successors.end());
        done = false;
      } else if (b->m_successors.size() > 1) {
        // When there are multiple successors, a qualifying empty block can only
        // terminate with a jump instruction.
        for (size_t i = 0; i < b->m_successors.size(); i++) {
          BasicBlock *s = b->m_successors[i];
          if (s->m_offsetLastOpcode > 0 || s->m_bytes[0] != op_jump32)
            break;
          b->m_successors[i] = s->m_successors[0];
          done = false;
        }
      }
    } while (!done);

    // Queue updated successors.
    for (size_t i = 0; i < b->m_successors.size(); i++) {
      BasicBlock *s = b->m_successors[i];

      if (s->m_token != g_token) {
        s->m_token = g_token;
        todo.push_back(s);
      }
    }
  }
}

void BasicBlock::OrderBlocks(BasicBlock *entry, BasicBlockVector &ordering) {
  BasicBlockVector todo;
  todo.push_back(entry);
  entry->m_token = ++g_token;

  while (todo.size() > 0) {
    BasicBlock *b = todo.back();
    todo.pop_back();
    ordering.push_back(b);

    if (b->m_successors.size() == 0) {
      // If there are no successors, the block must end with a return or raise.
      b->m_branchSize = 0;
    } else if (b->m_successors.size() == 1) {
      // When there's only one successor, the block must end with a jump.  If
      // the target has been visited already, this is a back edge and the jump
      // must be kept; otherwise, the successor will immediately follow this
      // block and the jump can be eliminated.
      BasicBlock *s = b->m_successors[0];
      if (s->m_token == g_token) {
        b->m_branchSize = 4;
      } else {
        b->m_branchSize = 0;
        b->m_bytes.pop_back();
        b->m_offsetLastOpcode = -1;
        b->Append(op_fallthrough);
      }
    } else if (b->m_successors.size() > 1) {
      // The last instruction must be an op_if.  Pick one of the successors to
      // immediately follow this block, and convert the op_if to either an
      // op_brtrue or op_brfalse.
      verify(b->m_bytes[b->m_offsetLastOpcode] == op_if);
      b->m_bytes[b->m_offsetLastOpcode] = op_brtrue32;
      b->m_branchSize = 4;

      // The branch instructions go to the first successor when the condition
      // is met; fall through is to the second successor when it is not met.
      if (b->m_successors[1]->m_token == g_token) {
        // If the second successor turns out to be a back edge, they must be
        // switched.
        b->m_bytes[b->m_offsetLastOpcode] = op_brfalse32;
        BasicBlock *s = b->m_successors[0];
        b->m_successors[0] = b->m_successors[1];
        b->m_successors[1] = s;
      } else if (b->m_successors[0]->m_token != g_token) {
        // Even if neither successor is a back edge, it is still desirable to
        // swap them if it will make the fall through block have no successors.
        if (b->m_successors[0]->m_successors.size() == 0) {
          b->m_bytes[b->m_offsetLastOpcode] = op_brfalse32;
          BasicBlock *s = b->m_successors[0];
          b->m_successors[0] = b->m_successors[1];
          b->m_successors[1] = s;
        }
      }
    }

    // Queue updated successors.
    for (size_t i = 0; i < b->m_successors.size(); i++) {
      BasicBlock *s = b->m_successors[i];

      if (s->m_token != g_token) {
        s->m_token = g_token;
        todo.push_back(s);
      }
    }
  }
}

size_t BasicBlock::AssignOffsets(BasicBlockVector &ordering) {
  size_t blockOffset;
  bool changed;

  // We start out assuming every branch offset is four bytes in length.  After
  // computing the offsets for all blocks, we go back and see if we can shrink
  // some branch offsets.  If we succeed, we recompute the block offsets.
  // Repeat until no more shrinkage occurs.

  do {
    changed = false;

    // Compute block offsets taking current branch offset lengths into account.
    blockOffset = 0;
    for (size_t i = 0; i < ordering.size(); i++) {
      BasicBlock *b = ordering[i];
      b->m_offset = blockOffset;
      blockOffset += b->m_bytes.size() + b->m_branchSize;
      if (b->m_bytes[b->m_offsetLastOpcode] == op_fallthrough)
        blockOffset--;
    }

    // Check branch offsets for possible shrinkage.  Note that offsets can only
    // shrink or stay the same; they can never grow.
    for (size_t i = 0; i < ordering.size(); i++) {
      BasicBlock *b = ordering[i];
      if (b->m_branchSize > 1) {
        BasicBlock *s = b->m_successors[0];
        int delta =int(s->m_offset - b->m_offset - b->m_offsetLastOpcode);
        if (delta == static_cast<int8_t>(delta)) {
          b->m_branchSize = 1;
          changed = true;
        } else if (delta == static_cast<int16_t>(delta)) {
          if (b->m_branchSize > 2) {
            b->m_branchSize = 2;
            changed = true;
          }
        }
      }
    }
  } while (changed);

  // Return the size of the linearized bytecode image.
  return blockOffset;
}

uint8_t *BasicBlock::Linearize(BasicBlockVector &ordering, size_t imageSize) {
  uint8_t *image = new uint8_t[imageSize];

  for (size_t i = 0; i < ordering.size(); i++) {
    BasicBlock *b = ordering[i];
    uint8_t *p = image + b->m_offset;
    for (auto byte : b->m_bytes)
      *p++ = byte;

    if (b->m_branchSize > 0) {
      BasicBlock *s = b->m_successors[0];
      int delta =int(s->m_offset - b->m_offset - b->m_offsetLastOpcode);
      if (b->m_branchSize == 1) {
        p[-1] -= 2;  // change opcode from op_xxx32 to op_xxx8
        *p++ = static_cast<uint8_t>(delta);
      } else if (b->m_branchSize == 2) {
        p[-1] -= 1;  // change opcode from op_xxx32 to op_xxx16
        *p++ = static_cast<uint8_t>(delta >> 8);
        *p++ = static_cast<uint8_t>(delta);
      } else {
        *p++ = static_cast<uint8_t>(delta >> 24);
        *p++ = static_cast<uint8_t>(delta >> 16);
        *p++ = static_cast<uint8_t>(delta >> 8);
        *p++ = static_cast<uint8_t>(delta);
      }
    }

    verify(p <= image + imageSize);
  }

  // Return the linearized bytecode image.
  return image;
}

void BasicBlock::Dump(BasicBlock *entry, const char *title) {
  BasicBlockVector todo;
  todo.push_back(entry);
  entry->m_token = ++g_token;
  printf("%s:\n\n", title);

  while (todo.size() > 0) {
    BasicBlock *b = todo.back();
    todo.pop_back();
    printf("Basic block %p:\n", (void *)b);
    printf("    Stack on entry %d    on exit %d\n",
        int(b->m_stackOnEntry), int(b->m_stackOnExit));
    printf("    Base block %p   adjustment %d   keep value %d\n",
        (void *)b->m_baseBlock, b->m_adjustment, b->m_keepValue);
    printf("    Successors");
    for (size_t j = 0; j < b->m_successors.size(); j++)
      printf(" %p", (void *)b->m_successors[j]);
    printf("\n\n");

    verify(b->m_offsetLastOpcode >= 0);
    int i = 0;
    int delta = 0;
    do {
      const OpcodeInfo &oi = g_opcodes[b->m_bytes[i]];
      verify(oi.m_isTerminator ^ (i < b->m_offsetLastOpcode));

      // Compute delta to stack depth.
      delta += oi.m_delta;
      if (oi.m_type == ot_delta) {
        if (oi.m_length == 2)
          delta -= b->m_bytes[i + 1];
        else if (oi.m_length == 3)
          delta -= (b->m_bytes[i + 1] << 8) + b->m_bytes[i + 2];
        else
          verify(false);
      }

      DumpInstr(&b->m_bytes[i], &b->m_bytes[0], delta);
      putchar('\n');

      // Advance to next instruction.
      i += oi.m_length;
    } while (i <= b->m_offsetLastOpcode);

    printf("\n\n");

    // Queue updated successors.
    for (size_t i = 0; i < b->m_successors.size(); i++) {
      BasicBlock *s = b->m_successors[i];

      if (s->m_token != g_token) {
        s->m_token = g_token;
        todo.push_back(s);
      }
    }
  }

  fflush(stdout);
}

void BasicBlock::DumpInstr(uint8_t *pc, uint8_t *base, int depth) {
  const OpcodeInfo &oi = g_opcodes[*pc];

  printf("%8d %4d: %s", depth, int(pc - base), oi.m_name);
  if (oi.m_type != ot_none && oi.m_type != ot_branchOffset) {
    uint64_t u = 0;
    int64_t s = 0;
    if (oi.m_length == 2) {
      u = pc[1];
      s = static_cast<int8_t>(u);
    } else if (oi.m_length == 3) {
      u = (pc[1] << 8) + pc[2];
      s = static_cast<int16_t>(u);
    } else if (oi.m_length == 5) {
      u = (pc[1] << 24) + (pc[2] << 16) + (pc[3] << 8) + pc[4];
      s = static_cast<int>(u);
    } else if (oi.m_length == 9) {
      u = (pc[1] << 24) + (pc[2] << 16) + (pc[3] << 8) + pc[4];
      u = (u << 32) + ((pc[5] << 24) + (pc[6] << 16) + (pc[7] << 8) + pc[8]);
      s = u;
    } else  {
      verify(false);
    }

    switch (oi.m_type) {
      case ot_branchOffset:
        printf(" %d", static_cast<int>(pc - base + s));
        break;
      case ot_delta:
        printf(" %d", static_cast<int>(s));
        break;
      case ot_address:
        printf(" %p", reinterpret_cast<void *>(intptr_t(u)));
        break;
      case ot_positive:
        printf(" %s", std::to_string(u).c_str());
        break;
      case ot_negative:
        printf(" -%s", std::to_string(u).c_str());
        break;
      case ot_signed:
        printf(" %s", std::to_string(s).c_str());
        break;
    }
  }
}

size_t BasicBlock::GetStackOnEntry() {
  if (m_baseBlock) {
    verify(m_baseBlock->m_token == g_token);
    m_stackOnEntry = m_baseBlock->m_stackOnExit + m_adjustment;
    m_stackOnEntry += size_t(m_keepValue);
  }
  return m_stackOnEntry;
}

/******************************************************************************/

const Union Function::s_null = Union();

BytecodeFunction::BytecodeFunction()
    : m_bytecode(nullptr),
      m_returnsValue(true) {
}

BytecodeFunction::~BytecodeFunction() {
  delete[] m_bytecode;
}

void BytecodeFunction::Execute(void *rv, size_t rvsize) {
  Interpreter I;

  if (m_returnsValue) {
    Union u = Call(I, 0, nullptr);
    if (rvsize == 1)
      *reinterpret_cast<uint8_t *>(rv) = u.m_u32;
    else if (rvsize == 2)
      *reinterpret_cast<int16_t *>(rv) = u.m_i32;
    else if (rvsize == 4)
      *reinterpret_cast<int *>(rv) = u.m_i32;
    else if (rvsize == 8)
      *reinterpret_cast<int64_t *>(rv) = u.m_i64;
    else
      verify(false);
  } else {
    Union args;
    args.SetAddr(rv);
    Call(I, 0, &args);
  }
}

void BytecodeFunction::SetBytecode(BasicBlock *entry, size_t maxfs) {
  verify(!m_bytecode);
  m_maxFrameSize = (maxfs + 15) & ~size_t(7);
  m_bytecode = BasicBlock::AssembleBlocks(entry, m_maxStackSize);
}

#ifdef BC_TRACE
static std::string s_indent;
#endif

Union BytecodeFunction::Call(Interpreter &I, size_t argcnt, Union *args,
                             Union *extra) {
  char *frame = reinterpret_cast<char *>(alloca(m_maxFrameSize));
  Union *stack = reinterpret_cast<Union *>(alloca(m_maxStackSize));
  uint8_t *pc = m_bytecode;

#ifdef BC_TRACE
  Union *stackBase = stack;
  printf("\n\n%sEntering %p:  argcnt=%d, maxfs=%d, maxss=%d",
         s_indent.c_str(), pc, (int)argcnt, (int)m_maxFrameSize,
         (int)m_maxStackSize);
  s_indent += "  ";

  struct ReturnHelper {
    ~ReturnHelper() {
      s_indent.resize(s_indent.size() - 2);
      printf("\n%sExiting\n", s_indent.c_str());
    }
  } rh;
#endif

  // Interpreter main loop.
  // FIXME: use computed goto where compiler supports it (i.e., g++) to
  // significantly improve branch prediction.
  while (true) {
#ifdef BC_TRACE
    printf("\n%s", s_indent.c_str());
    BasicBlock::DumpInstr(pc, m_bytecode, int(stack - stackBase));
#endif
    switch (*pc++) {
      case op_add32:        // Add 32-bit integers.
        stack[-2].m_u32 += stack[-1].m_u32;
        stack--;
        break;
      case op_add64:        // Add 64-bit integers.
        stack[-2].m_u64 += stack[-1].m_u64;
        stack--;
        break;
      case op_addf32:        // Add 32-bit f.p.
        stack[-2].m_f32 += stack[-1].m_f32;
        stack--;
        break;
      case op_addf64:        // Add 64-bit f.p.
        stack[-2].m_f64 += stack[-1].m_f64;
        stack--;
        break;
      case op_and32:        // Bit intersection of 32-bit integers.
        stack[-2].m_u32 &= stack[-1].m_u32;
        stack--;
        break;
      case op_and64:        // Bit intersection of 64-bit integers.
        stack[-2].m_u64 &= stack[-1].m_u64;
        stack--;
        break;
      case op_call:         // Call function with no return value.
        stack -= (*pc + 1);
        reinterpret_cast<Function *>(stack[0].AsAddr())->Call(I, *pc++,
                                                              stack + 1);
        break;
      case op_callfat: {    // Call function with fat pointer return value.
        Union extra;
        stack -= *pc;
        stack[-1] = reinterpret_cast<Function *>(stack[-1].AsAddr())->
                        Call(I, *pc++, stack, &extra);
        stack++;
        stack[-1] = extra;
        break;
      }
      case op_callval:      // Call function with return value.
        stack -= *pc;
        stack[-1] = reinterpret_cast<Function *>(stack[-1].AsAddr())->
                        Call(I, *pc++, stack);
        break;
      case op_convf32tof64: // Convert 32-bit f.p. to 64-bit f.p.
        stack[-1].m_f64 = (double)stack[-1].m_f32;
        break;
      case op_convf32toi32: // Convert 32-bit f.p. to 32-bit integer.
        stack[-1].m_i32 = (int32_t)stack[-1].m_f32;
        break;
      case op_convf32toi64: // Convert 32-bit f.p. to 64-bit integer.
        stack[-1].m_i32 = (int32_t)stack[-1].m_f32;
        break;
      case op_convf64tof32: // Convert 64-bit f.p. to 32-bit f.p.
        stack[-1].m_f32 = (float)stack[-1].m_f64;
        break;
      case op_convf64toi32: // Convert 64-bit f.p. to 32-bit integer.
        stack[-1].m_i64 = (int64_t)stack[-1].m_f64;
        break;
      case op_convf64toi64: // Convert 64-bit f.p. to 64-bit integer.
        stack[-1].m_i64 = (int64_t)stack[-1].m_f64;
        break;
      case op_convi32tof32: // Convert 32-bit integer to 32-bit f.p.
        stack[-1].m_f32 = (float)stack[-1].m_i32;
        break;
      case op_convi32tof64: // Convert 32-bit integer to 64-bit f.p.
        stack[-1].m_f64 = (double)stack[-1].m_i32;
        break;
      case op_convi64tof32: // Convert 64-bit integer to 32-bit f.p.
        stack[-1].m_f32 = (float)stack[-1].m_i64;
        break;
      case op_convi64tof64: // Convert 64-bit integer to 64-bit f.p.
        stack[-1].m_f64 = (double)stack[-1].m_i64;
        break;
      case op_convs32to64:  // Convert signed 32- to 64-bit integer.
        stack[-1].m_i64 = stack[-1].m_i32;
        break;
      case op_convu32to64:  // Convert unsigned 32- to 64-bit integer.
        stack[-1].m_u64 = stack[-1].m_u32;
        break;
      case op_conv64to32:   // Convert 64- to 32-bit integer.
        stack[-1].m_u32 = uint32_t(stack[-1].m_u64);
        break;
      case op_divf32:       // 32-bit f.p. division.
        stack[-2].m_f32 /= stack[-1].m_f32;
        stack--;
        break;
      case op_divf64:       // 64-bit f.p. division.
        stack[-2].m_f64 /= stack[-1].m_f64;
        stack--;
        break;
      case op_divs32:       // Signed 32-bit integer division.
        stack[-2].m_i32 /= stack[-1].m_i32;
        stack--;
        break;
      case op_divs64:       // Signed 64-bit integer division.
        stack[-2].m_i64 /= stack[-1].m_i64;
        stack--;
        break;
      case op_divu32:       // Unsigned 32-bit integer division.
        stack[-2].m_u32 /= stack[-1].m_u32;
        stack--;
        break;
      case op_divu64:       // Unsigned 64-bit integer division.
        stack[-2].m_u64 /= stack[-1].m_u64;
        stack--;
        break;
      case op_dup:          // Duplicate top of stack.
        stack[0] = stack[-1];
        stack++;
        break;
      case op_exts32:       // Sign extend to 32-bits.
        stack[-1].m_i32 = ((stack[-1].m_i32 << (32 - *pc)) >> (32 - *pc));
        pc++;
        break;
      case op_exts64:       // Sign extend to 64-bits.
        stack[-1].m_i64 = ((stack[-1].m_i64 << (64 - *pc)) >> (64 - *pc));
        pc++;
        break;
      case op_extz32:       // Zero extend to 32-bits.
        stack[-1].m_u32 = ((stack[-1].m_u32 << (32 - *pc)) >> (32 - *pc));
        pc++;
        break;
      case op_extz64:       // Zero extend to 64-bits.
        stack[-1].m_u64 = ((stack[-1].m_u64 << (64 - *pc)) >> (64 - *pc));
        pc++;
        break;
      case op_brfalse8:     // Branch if false -- 8 bit offset.
        stack--;
        if (stack[0].m_u32 != 0) {
          pc += 1;
        } else {
          pc += static_cast<int8_t>(*pc) - 1;
        }
        break;
      case op_brfalse16:    // Branch if false -- 16 bit offset.
        stack--;
        if (stack[0].m_u32 != 0) {
          pc += 2;
        } else {
          pc += static_cast<int16_t>((pc[0] << 8) + pc[1]) - 1;
        }
        break;
      case op_brfalse32:    // Branch if false -- 32 bit offset.
        stack--;
        if (stack[0].m_u32 != 0) {
          pc += 4;
        } else {
          pc += static_cast<int>((pc[0] << 24) + (pc[1] << 16) +
                                 (pc[2] << 8) + pc[3]) - 1;
        }
        break;
      case op_brtrue8:      // Branch if true -- 8 bit offset.
        stack--;
        if (stack[0].m_u32 == 0) {
          pc += 1;
        } else {
          pc += static_cast<int8_t>(*pc) - 1;
        }
        break;
      case op_brtrue16:     // Branch if true -- 16 bit offset.
        stack--;
        if (stack[0].m_u32 == 0) {
          pc += 2;
        } else {
          pc += static_cast<int16_t>((pc[0] << 8) + pc[1]) - 1;
        }
        break;
      case op_brtrue32:     // Branch if true -- 32 bit offset.
        stack--;
        if (stack[0].m_u32 == 0) {
          pc += 4;
        } else {
          pc += static_cast<int>((pc[0] << 24) + (pc[1] << 16) +
                                 (pc[2] << 8) + pc[3]) - 1;
        }
        break;
      case op_jump8:        // Unconditional branch -- 8 bit offset.
        pc += static_cast<int8_t>(*pc) - 1;
        break;
      case op_jump16:       // Unconditional branch -- 16 bit offset.
        pc += static_cast<int16_t>((pc[0] << 8) + pc[1]) - 1;
        break;
      case op_jump32:       // Unconditional branch -- 32 bit offset.
        pc += static_cast<int>((pc[0] << 24) + (pc[1] << 16) +
                               (pc[2] << 8) + pc[3]) - 1;
        break;
      case op_leaargs8:     // Load effective address -- 8-bit args offset.
        stack[0].SetAddr(reinterpret_cast<char *>(args) + *pc++);
        stack++;
        break;
      case op_leaargs16:    // Load effective address -- 16-bit args offset.
        stack[0].SetAddr(reinterpret_cast<char *>(args) + (pc[0] << 8) + pc[1]);
        pc += 2;
        stack++;
        break;
      case op_leaargs32:    // Load effective address -- 32-bit args offset.
        stack[0].SetAddr(reinterpret_cast<char *>(args) + (pc[0] << 24) +
                      (pc[1] << 16) + (pc[2] << 8) + pc[3]);
        pc += 4;
        stack++;
        break;
      case op_leaframe8:    // Load effective address -- 8-bit frame offset.
        stack[0].SetAddr(reinterpret_cast<char *>(frame) + *pc++);
        verify(stack[0].AsAddr() < frame + m_maxFrameSize);
        stack++;
        break;
      case op_leaframe16:   // Load effective address -- 16-bit frame offset.
        stack[0].SetAddr(reinterpret_cast<char *>(frame) + (pc[0] << 8) +
                      pc[1]);
        verify(stack[0].AsAddr() < frame + m_maxFrameSize);
        pc += 2;
        stack++;
        break;
      case op_leaframe32:   // Load effective address -- 32-bit frame offset.
        stack[0].SetAddr(reinterpret_cast<char *>(frame) + (pc[0] << 24) +
                      (pc[1] << 16) + (pc[2] << 8) + pc[3]);
        verify(stack[0].AsAddr() < frame + m_maxFrameSize);
        pc += 4;
        stack++;
        break;
      case op_leaptr8:      // Load effective address -- 8-bit pointer offset.
        stack[-1].SetAddr(reinterpret_cast<char *>(stack[-1].AsAddr()) + *pc++);
        break;
      case op_leaptr16:     // Load effective address -- 16-bit pointer offset.
        stack[-1].SetAddr(reinterpret_cast<char *>(stack[-1].AsAddr()) +
                       (pc[0]<<8) + pc[1]);
        pc += 2;
        break;
      case op_leaptr32:     // Load effective address -- 32-bit pointer offset.
        stack[-1].SetAddr(reinterpret_cast<char *>(stack[-1].AsAddr()) +
                       (pc[0] << 24) + (pc[1] << 16) + (pc[2] << 8) + pc[3]);
        pc += 4;
        break;
      case op_loads8:       // Load signed 8-bit value.
        stack[-1].m_i32 = *reinterpret_cast<int8_t *>(stack[-1].AsAddr());
        break;
      case op_loadu8:       // Load unsigned 8-bit value.
        stack[-1].m_u32 = *reinterpret_cast<uint8_t *> (stack[-1].AsAddr());
        break;
      case op_loads16:      // Load signed 16-bit value.
        stack[-1].m_i32 = *reinterpret_cast<int16_t *>(stack[-1].AsAddr());
        break;
      case op_loadu16:      // Load unsigned 16-bit value.
        stack[-1].m_u32 = *reinterpret_cast<uint16_t *> (stack[-1].AsAddr());
        break;
      case op_load32:       // Load 32-bit value.
        stack[-1].m_i32 = *reinterpret_cast<int *>(stack[-1].AsAddr());
        break;
      case op_load64:       // Load 64-bit value.
        stack[-1].m_i64 = *reinterpret_cast<int64_t *>(stack[-1].AsAddr());
        break;
      case op_loadfat: {    // Load fat pointer.
        uintptr_t *p = reinterpret_cast<uintptr_t *>(stack[-1].AsAddr());
        stack++;
        if constexpr (sizeof(void *) == 4) {
          stack[-2].m_u32 = (uint32_t)p[0];
          stack[-1].m_u32 = (uint32_t)p[1];
        } else {
          stack[-2].m_u64 = p[0];
          stack[-1].m_u64 = p[1];
        }
        break;
      }
      case op_mulf32:       // Multiply 32-bit f.p.
        stack[-2].m_f32 *= stack[-1].m_f32;
        stack--;
        break;
      case op_mulf64:       // Multiply 64-bit f.p.
        stack[-2].m_f64 *= stack[-1].m_f64;
        stack--;
        break;
      case op_muls32:       // Multiply signed 32-bit integers.
        stack[-2].m_i32 *= stack[-1].m_i32;
        stack--;
        break;
      case op_muls64:       // Multiply signed 64-bit integers.
        stack[-2].m_i64 *= stack[-1].m_i64;
        stack--;
        break;
      case op_mulu32:       // Multiply unsigned 32-bit integers.
        stack[-2].m_u32 *= stack[-1].m_u32;
        stack--;
        break;
      case op_mulu64:       // Multiply unsigned 64-bit integers.
        stack[-2].m_u64 *= stack[-1].m_u64;
        stack--;
        break;
      case op_neg32:        // Negate 32-bit integer.
        stack[-1].m_i32 = -stack[-1].m_i32;
        break;
      case op_neg64:        // Negate 64-bit integer.
        stack[-1].m_i64 = -stack[-1].m_i64;
        break;
      case op_negf32:        // Negate 32-bit f.p.
        stack[-1].m_f32 = -stack[-1].m_f32;
        break;
      case op_negf64:        // Negate 64-bit f.p.
        stack[-1].m_f64 = -stack[-1].m_f64;
        break;
      case op_not32:        // Bit inversion of 32-bit integer.
        stack[-1].m_u32 = ~stack[-1].m_u32;
        break;
      case op_not64:        // Bit inversion of 64-bit integer.
        stack[-1].m_u64 = ~stack[-1].m_u64;
        break;
      case op_or32:         // Bit union of 32-bit integers.
        stack[-2].m_u32 |= stack[-1].m_u32;
        stack--;
        break;
      case op_or64:         // Bit union of 64-bit integers.
        stack[-2].m_u64 |= stack[-1].m_u64;
        stack--;
        break;
      case op_pop:          // Pop top of stack.
        stack--;
        break;
      case op_pop8:         // Pop n values off stack, with 8-bit n.
        stack -= *pc++;
        break;
      case op_pop16:        // Pop n values off stack, with 16-bit n.
        stack -= ((pc[0] << 8) + pc[1]);
        pc += 2;
        break;
      case op_pushp8:       // Push 8-bit positive integer.
        stack[0].m_u32 = *pc++;
        stack++;
        break;
      case op_pushn8:       // Push 8-bit negative integer.
        stack[0].m_u32 = *pc++ | 0xffffff00;
        stack++;
        break;
      case op_pushp16:      // Push 16-bit positive integer.
        stack[0].m_u32 = (pc[0] << 8) + pc[1];
        pc += 2;
        stack++;
        break;
      case op_pushn16:      // Push 16-bit negative integer.
        stack[0].m_u32 = ((pc[0] << 8) + pc[1]) | 0xffff0000;
        pc += 2;
        stack++;
        break;
      case op_push32:      // Push 32-bit integer.
        stack[0].m_u32 = (pc[0] << 24) + (pc[1] << 16) + (pc[2] << 8) + pc[3];
        pc += 4;
        stack++;
        break;
      case op_push64: {     // Push 64-bit integer.
        uint32_t u1 = (pc[0] << 24) | (pc[1] << 16) | (pc[2] << 8) | pc[3];
        uint32_t u2 = (pc[4] << 24) | (pc[5] << 16) | (pc[6] << 8) | pc[7];
        stack[0].m_u64 = (static_cast<uint64_t>(u1) << 32) | u2;
        pc += 8;
        stack++;
        break;
      }
      case op_rems32:       // Signed 32-bit remainder.
        stack[-2].m_i32 %= stack[-1].m_i32;
        stack--;
        break;
      case op_rems64:       // Signed 64-bit remainder.
        stack[-2].m_i64 %= stack[-1].m_i64;
        stack--;
        break;
      case op_remu32:       // Unsigned 32-bit remainder.
        stack[-2].m_u32 %= stack[-1].m_u32;
        stack--;
        break;
      case op_remu64:       // Unsigned 64-bit remainder.
        stack[-2].m_u64 %= stack[-1].m_u64;
        stack--;
        break;
      case op_ret:          // Return without value.
        verify(!extra);
        return s_null;
      case op_retfat:       // Return with fat pointer value.
        verify(extra);
        *extra = stack[-1];
        return stack[-2];
      case op_retval:       // Return with value.
        verify(!extra);
        return stack[-1];
      case op_seteq32:      // 32-bit equality.
        stack[-2].m_u32 = stack[-2].m_u32 == stack[-1].m_u32;
        stack--;
        break;
      case op_seteq64:      // 64-bit equality.
        stack[-2].m_u32 = stack[-2].m_u64 == stack[-1].m_u64;
        stack--;
        break;
      case op_seteqf32:      // 32-bit f.p. equality.
        stack[-2].m_f32 = stack[-2].m_f32 == stack[-1].m_f32;
        stack--;
        break;
      case op_seteqf64:      // 64-bit f.p. equality.
        stack[-2].m_f32 = stack[-2].m_f64 == stack[-1].m_f64;
        stack--;
        break;
      case op_setgef32:     // 32-bit f.p. greater or equal.
        stack[-2].m_u32 = stack[-2].m_f32 >= stack[-1].m_f32;
        stack--;
        break;
      case op_setgef64:     // 64-bit f.p. greater or equal.
        stack[-2].m_u32 = stack[-2].m_f64 >= stack[-1].m_f64;
        stack--;
        break;
      case op_setges32:     // 32-bit signed greater or equal.
        stack[-2].m_u32 = stack[-2].m_i32 >= stack[-1].m_i32;
        stack--;
        break;
      case op_setges64:     // 64-bit signed greater or equal.
        stack[-2].m_u32 = stack[-2].m_i64 >= stack[-1].m_i64;
        stack--;
        break;
      case op_setgeu32:     // 32-bit unsigned greater or equal.
        stack[-2].m_u32 = stack[-2].m_u32 >= stack[-1].m_u32;
        stack--;
        break;
      case op_setgeu64:     // 64-bit unsigned greater or equal.
        stack[-2].m_u32 = stack[-2].m_u64 >= stack[-1].m_u64;
        stack--;
        break;
      case op_setgtf32:     // 32-bit f.p. greater.
        stack[-2].m_u32 = stack[-2].m_f32 > stack[-1].m_f32;
        stack--;
        break;
      case op_setgtf64:     // 64-bit f.p. greater.
        stack[-2].m_u32 = stack[-2].m_f64 > stack[-1].m_f64;
        stack--;
        break;
      case op_setgts32:     // 32-bit signed greater.
        stack[-2].m_u32 = stack[-2].m_i32 > stack[-1].m_i32;
        stack--;
        break;
      case op_setgts64:     // 64-bit signed greater.
        stack[-2].m_u32 = stack[-2].m_i64 > stack[-1].m_i64;
        stack--;
        break;
      case op_setgtu32:     // 32-bit unsigned greater.
        stack[-2].m_u32 = stack[-2].m_u32 > stack[-1].m_u32;
        stack--;
        break;
      case op_setgtu64:     // 64-bit unsigned greater.
        stack[-2].m_u32 = stack[-2].m_u64 > stack[-1].m_u64;
        stack--;
        break;
      case op_setlef32:     // 32-bit f.p. lesser or equal.
        stack[-2].m_u32 = stack[-2].m_f32 <= stack[-1].m_f32;
        stack--;
        break;
      case op_setlef64:     // 64-bit f.p. lesser or equal.
        stack[-2].m_u32 = stack[-2].m_f64 <= stack[-1].m_f64;
        stack--;
        break;
      case op_setles32:     // 32-bit signed lesser or equal.
        stack[-2].m_u32 = stack[-2].m_i32 <= stack[-1].m_i32;
        stack--;
        break;
      case op_setles64:     // 64-bit signed lesser or equal.
        stack[-2].m_u32 = stack[-2].m_i64 <= stack[-1].m_i64;
        stack--;
        break;
      case op_setleu32:     // 32-bit unsigned lesser or equal.
        stack[-2].m_u32 = stack[-2].m_u32 <= stack[-1].m_u32;
        stack--;
        break;
      case op_setleu64:     // 64-bit unsigned lesser or equal.
        stack[-2].m_u32 = stack[-2].m_u64 <= stack[-1].m_u64;
        stack--;
        break;
      case op_setltf32:     // 32-bit f.p. lesser.
        stack[-2].m_u32 = stack[-2].m_f32 < stack[-1].m_f32;
        stack--;
        break;
      case op_setltf64:     // 64-bit f.p. lesser.
        stack[-2].m_u32 = stack[-2].m_f64 < stack[-1].m_f64;
        stack--;
        break;
      case op_setlts32:     // 32-bit signed lesser.
        stack[-2].m_u32 = stack[-2].m_i32 < stack[-1].m_i32;
        stack--;
        break;
      case op_setlts64:     // 64-bit signed lesser.
        stack[-2].m_u32 = stack[-2].m_i64 < stack[-1].m_i64;
        stack--;
        break;
      case op_setltu32:     // 32-bit unsigned lesser.
        stack[-2].m_u32 = stack[-2].m_u32 < stack[-1].m_u32;
        stack--;
        break;
      case op_setltu64:     // 64-bit unsigned lesser.
        stack[-2].m_u32 = stack[-2].m_u64 < stack[-1].m_u64;
        stack--;
        break;
      case op_setne32:      // 32-bit inequality.
        stack[-2].m_u32 = stack[-2].m_u32 != stack[-1].m_u32;
        stack--;
        break;
      case op_setne64:      // 64-bit inequality.
        stack[-2].m_u32 = stack[-2].m_u64 != stack[-1].m_u64;
        stack--;
        break;
      case op_setnef32:      // 32-bit f.p. inequality.
        stack[-2].m_u32 = stack[-2].m_f32 != stack[-1].m_f32;
        stack--;
        break;
      case op_setnef64:      // 64-bit f.p. inequality.
        stack[-2].m_u32 = stack[-2].m_f64 != stack[-1].m_f64;
        stack--;
        break;
      case op_shl32:        // 32-bit logical shift left.
        stack[-2].m_u32 <<= stack[-1].m_u32;
        stack--;
        break;
      case op_shl64:        // 64-bit logical shift left.
        stack[-2].m_u64 <<= stack[-1].m_u32;
        stack--;
        break;
      case op_shrs32:       // 32-bit arithmetic shift right.
        stack[-2].m_i32 >>= stack[-1].m_u32;
        stack--;
        break;
      case op_shrs64:       // 64-bit arithmetic shift right.
        stack[-2].m_i64 >>= stack[-1].m_u32;
        stack--;
        break;
      case op_shru32:       // 32-bit logical shift right.
        stack[-2].m_u32 >>= stack[-1].m_u32;
        stack--;
        break;
      case op_shru64:       // 64-bit logical shift right.
        stack[-2].m_u64 >>= stack[-1].m_u32;
        stack--;
        break;
      case op_store8:       // Store 8-bit value.
        *reinterpret_cast<int8_t *>(stack[-2].AsAddr()) = stack[-1].m_i32;
        stack -= 2;
        break;
      case op_store16:      // Store 16-bit value.
        *reinterpret_cast<int16_t *>(stack[-2].AsAddr()) = stack[-1].m_i32;
        stack -= 2;
        break;
      case op_store32:      // Store 32-bit value.
        *reinterpret_cast<int *>(stack[-2].AsAddr()) = stack[-1].m_i32;
        stack -= 2;
        break;
      case op_store64:      // Store 64-bit value.
        *reinterpret_cast<int64_t *>(stack[-2].AsAddr()) = stack[-1].m_i64;
        stack -= 2;
        break;
      case op_storefat: {   // Store fat pointer.
        uintptr_t *p = reinterpret_cast<uintptr_t *>(stack[-3].AsAddr());
        if constexpr (sizeof(void *) == 4) {
          p[0] = stack[-2].m_u32;
          p[1] = stack[-1].m_u32;
        } else {
          p[0] = (uintptr_t)stack[-2].m_u64;
          p[1] = (uintptr_t)stack[-1].m_u64;
        }
        stack -= 3;
        break;
      }
      case op_subf32:        // Subtract 32-bit f.p.
        stack[-2].m_f32 -= stack[-1].m_f32;
        stack--;
        break;
      case op_subf64:        // Subtract 64-bit f.p.
        stack[-2].m_f64 -= stack[-1].m_f64;
        stack--;
        break;
      case op_sub32:        // Subtract 32-bit integers.
        stack[-2].m_u32 -= stack[-1].m_u32;
        stack--;
        break;
      case op_sub64:        // Subtract 64-bit integers.
        stack[-2].m_u64 -= stack[-1].m_u64;
        stack--;
        break;
      case op_swap: {       // Swap top two values on stack.
        Union u = stack[-2];
        stack[-2] = stack[-1];
        stack[-1] = u;
        break;
      }
      case op_swap8: {      // Swap top of stack with n values down, 8-bit n.
        Union *p = stack - 1 - *pc++;
        Union u = *p;
        *p = stack[-1];
        stack[-1] = u;
        break;
      }
      case op_swap16: {     // Swap top of stack with n values down, 16-bit n.
        Union *p = stack - 1 - (pc[0] << 8) - pc[1];
        pc += 2;
        Union u = *p;
        *p = stack[-1];
        stack[-1] = u;
        break;
      }
      case op_xor32:        // Bit difference of 32-bit integers.
        stack[-2].m_u32 ^= stack[-1].m_u32;
        stack--;
        break;
      case op_xor64:        // Bit difference of 64-bit integers.
        stack[-2].m_u64 ^= stack[-1].m_u64;
        stack--;
        break;
      default:
        verify(false);
    }
  }

  /* not reached */
}
