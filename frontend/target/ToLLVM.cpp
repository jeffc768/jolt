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

#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Bitcode/BitcodeWriterPass.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/Scalar.h>

#include "Target.h"
#include "TranslateClosure.h"
#include "entity/Argument.h"
#include "entity/BaseSpecifier.h"
#include "entity/Class.h"
#include "entity/Const.h"
#include "entity/Field.h"
#include "entity/FormalArguments.h"
#include "entity/Method.h"
#include "entity/Scope.h"
#include "node/Attributes.h"
#include "node/Binary.h"
#include "node/Block.h"
#include "node/BuildPointer.h"
#include "node/Call.h"
#include "node/CallBuiltin.h"
#include "node/Cast.h"
#include "node/Construct.h"
#include "node/Deref.h"
#include "node/Expr.h"
#include "node/ExtractAddress.h"
#include "node/ExtractDescriptor.h"
#include "node/FieldAddr.h"
#include "node/GlobalAddr.h"
#include "node/If.h"
#include "node/Index.h"
#include "node/Label.h"
#include "node/Literal.h"
#include "node/LiteralAddr.h"
#include "node/Load.h"
#include "node/MethodBody.h"
#include "node/Sequence.h"
#include "node/Shared.h"
#include "node/Store.h"
#include "node/Transfer.h"
#include "node/Unary.h"
#include "node/VarAddr.h"
#include "node/VarDecl.h"
#include "node/Vtable.h"
#include "node/VtableCast.h"
#include "node/VtableSlot.h"
#include "node/While.h"
#include "type/Type.h"
#include "util/String.h"
#include "util/Value.h"
#include <fstream>
#include <map>
#include <stdio.h>

#ifndef LLVM_VERSION_MAJOR
  // This macro is defined starting in 3.1.  Assume 3.0.
  #define LLVM_VERSION 300
#else
  #define LLVM_VERSION (LLVM_VERSION_MAJOR * 100 + LLVM_VERSION_MINOR)
#endif

#if LLVM_VERSION < 700
  #error LLVM 7.0 or newer required.
#endif

#if LLVM_VERSION < 1000
namespace llvm {
  using CodeGenFileType = LLVMTargetMachine::CodeGenFileType;
  constexpr static const CodeGenFileType CGFT_AssemblyFile =
      LLVMTargetMachine::CGFT_AssemblyFile;
  constexpr static const CodeGenFileType CGFT_ObjectFile =
      LLVMTargetMachine::CGFT_ObjectFile;
}
#endif

static const llvm::Twine g_noName;

// Cached LLVM simple types.  GetModule() must be called at least once before
// these can be used.
static llvm::Type *g_voidTy;
static llvm::Type *g_int1Ty;
static llvm::Type *g_int8Ty;
static llvm::Type *g_int16Ty;
static llvm::Type *g_int32Ty;
static llvm::Type *g_int64Ty;
static llvm::Type *g_floatTy;
static llvm::Type *g_doubleTy;
static llvm::PointerType *g_voidptrTy;

static llvm::LLVMContext g_context;

// Helper function to create LLVM attribute lists.
inline void CreateAttributeListHelper(llvm::AttributeList &set) { }

template<typename... ARGS>
inline void CreateAttributeListHelper(llvm::AttributeList &set,
                                      unsigned idx,
                                      llvm::Attribute::AttrKind attr,
                                      ARGS... remainder) {
  set = set.addAttribute(g_context, idx, attr);
  CreateAttributeListHelper(set, remainder...);
}

template<typename... ARGS>
inline void CreateAttributeList(llvm::AttributeList &set, ARGS... args) {
  if (!set.isEmpty())
    return;

  set = llvm::AttributeList();
  CreateAttributeListHelper(set, args...);
}

class LLVMClass;
class LLVMMethod;

class LLVMTarget: public Target {
public:
  LLVMTarget(const char *name) : Target(name) { }

  virtual void Setup();

  virtual TargetClass *For(Class *);
  virtual TargetConst *For(Const *);
  virtual TargetGlobal *For(Field *);
  virtual TargetMethod *For(Method *);
  virtual TargetMethod *For(Method *, int);

  virtual Value *Evaluate(TranslateClosure &tc, Value *result, Expr *e) {
    verify(0 && "LLVM target cannot currently do compile time evaluation.");
    return nullptr;
  }

  // Get LLVM module, creating it first if necessary.
  llvm::Module *GetModule() { return m_module; }

  // Map Jolt type to an LLVM type.
  llvm::Type *LLVMType(Type t);

  virtual void WriteToFile(const char *fname) = 0;

  virtual bool IsSimpleType_(Type t) {
    // No struct or array type is first-class in LLVM.
    return false;
  }

  virtual vector<size_t> GetStructLayout_(Type t);
  virtual unsigned GetAlignment_(Type t);


  Epoch               m_epoch     = ep_run;

  // Linked list of all classes and methods to be emitted to the LLVM module.
  LLVMClass          *m_classes   = nullptr;
  LLVMMethod         *m_methods   = nullptr;

protected:
  // Get native TargetMachine.
  llvm::TargetMachine *GetTargetMachine();

  // Finalize the LLVM module for code generation, including the addition of
  // a main() function and setting up the passes.
  llvm::TargetMachine *Finalize(llvm::Pass *pass);

  // Helper for writing object code in some format.
  void WriteCodeToFile(const char *fname, llvm::CodeGenFileType ft);

private:
  llvm::TargetMachine *m_targetMachine  = nullptr;
  llvm::Module        *m_module         = nullptr;
};

class LLVMObjTarget: public LLVMTarget {
public:
  LLVMObjTarget() : LLVMTarget("llvm-obj") { }
  ~LLVMObjTarget() { }

  virtual void WriteToFile(const char *fname);
};

static LLVMObjTarget LLVMObjGen;

class LLVMBCTarget: public LLVMTarget {
public:
  LLVMBCTarget() : LLVMTarget("llvm-bc") { }
  ~LLVMBCTarget() { }

  virtual void WriteToFile(const char *fname);
};

static LLVMBCTarget LLVMBCGen;

class LLVMLLTarget: public LLVMTarget {
public:
  LLVMLLTarget() : LLVMTarget("llvm-ll") { }
  ~LLVMLLTarget() { }

  virtual void WriteToFile(const char *fname);
};

static LLVMLLTarget LLVMLLGen;

class LLVMAsmTarget: public LLVMTarget {
public:
  LLVMAsmTarget() : LLVMTarget("llvm-asm") { }
  ~LLVMAsmTarget() { }

  virtual void WriteToFile(const char *fname);
};

static LLVMAsmTarget LLVMAsmGen;

class LLVMClass: public TargetClass {
  DECLARE_OBJECT(LLVMClass)

public:
  LLVMClass(Class *ce) : TargetClass(ce) { }

  virtual void Generate();
  virtual bool HasGlobalStorage(Epoch ep);
  virtual void *GlobalStorage(Epoch ep);

  void EmitVtable();
  llvm::Constant *EmitVtableInit(Class *derivedClass, size_t offset);

  llvm::GlobalVariable *m_storage     = nullptr;
  llvm::GlobalVariable *m_vtable      = nullptr;
  llvm::Type           *m_vtableType  = nullptr;
  size_t                m_mapBase     = 0;
  vector<int>           m_vtblMap;
  vector<int>           m_fieldMap;
  LLVMClass            *m_nextClass   = nullptr;
};

class LLVMConst: public TargetConst {
  DECLARE_OBJECT(LLVMConst)

public:
  LLVMConst(Const *ce) : TargetConst(ce) { }

  virtual void Generate();
  virtual void *GlobalStorage(Epoch ep);

  llvm::GlobalVariable *m_storage     = nullptr;
};

class LLVMGlobal: public TargetGlobal {
  DECLARE_OBJECT(LLVMGlobal)

public:
  LLVMGlobal(Field *fe) : TargetGlobal(fe) { }

  virtual void Generate();
  virtual void *GlobalStorage(Epoch ep);

  llvm::GlobalVariable *m_storage     = nullptr;
};

class LLVMMethod: public TargetMethod {
  DECLARE_OBJECT(LLVMMethod)

public:
  LLVMMethod(Method *me) : TargetMethod(me) { }
  LLVMMethod(Method *me, int nm)
    : TargetMethod(me), m_function(GetNativeFunction(ep_run, nm)) { }
  // FIXME: don't hard code epoch.

  virtual void Generate();
  virtual void *GlobalStorage(Epoch ep);

  llvm::FunctionType *GetType();
  void Emit();
  llvm::Function *GetNativeFunction(Epoch ep, int nm);

  LLVMMethod         *m_nextMethod      = nullptr;

  // The LLVM representation of this method.  A non-null value does not mean
  // the function has been translated to LLVM yet.
  llvm::FunctionType *m_type            = nullptr;
  llvm::Function     *m_function        = nullptr;
};

/******************************************************************************/

void LLVMTarget::Setup() {
  llvm::InitializeNativeTarget();
  llvm::InitializeAllAsmPrinters();
  m_module = new llvm::Module("program", g_context);
  g_voidTy = llvm::Type::getVoidTy(g_context);
  g_int1Ty = llvm::Type::getInt1Ty(g_context);
  g_int8Ty = llvm::Type::getInt8Ty(g_context);
  g_int16Ty = llvm::Type::getInt16Ty(g_context);
  g_int32Ty = llvm::Type::getInt32Ty(g_context);
  g_int64Ty = llvm::Type::getInt64Ty(g_context);
  g_floatTy = llvm::Type::getFloatTy(g_context);
  g_doubleTy = llvm::Type::getDoubleTy(g_context);
  g_voidptrTy = llvm::PointerType::get(g_int8Ty, 0);
}

llvm::Type *LLVMTarget::LLVMType(Type t) {
  void *&td = t.TargetData();
  llvm::Type *&lt = (llvm::Type *&)td;
  if (lt)
    return lt;

  switch (t.RawKind()) {
    case tk_void:
      lt = g_voidTy;
      break;
    case tk_bool:
      lt = g_int1Ty;
      break;
    case tk_integer:
    case tk_char:
      switch (t.StorageSize()) {
      case 1: lt = g_int8Ty;  break;
      case 2: lt = g_int16Ty; break;
      case 4: lt = g_int32Ty; break;
      case 8: lt = g_int64Ty; break;
      default:
        // Don't current support integers that aren't a power of two.
        verify(false);
      }
      break;
    case tk_float:
      if (t == Type::Float())
        lt = g_floatTy;
      else if (t == Type::Double())
        lt = g_doubleTy;
      else
        verify(false);
      break;
    case tk_pseudo:
      lt = g_int1Ty;
      break;
    case tk_class: {
      Class *ce = t.Class();
      auto st = llvm::StructType::create(g_context, ce->ExternalName().c_str());
      lt = st;

      auto c = safe_cast<LLVMClass *>(ce->GetTarget(ep_run));
      c->m_fieldMap.resize(t.FieldCount(), -1);
      vector<llvm::Type *> tl;
      int ord = 0;
      for (size_t i = 0; i < t.FieldCount(); i++)
        if (Type u = t.TypeOf(i); u != tk_void) {
          tl.push_back(LLVMType(u));
          c->m_fieldMap[i] = ord++;
        }

      st->setBody(tl);
      break;
    }
    case tk_tuple: {
      vector<llvm::Type *> tl;
      for (size_t i = 0; i < t.FieldCount(); i++)
        tl.push_back(LLVMType(t.TypeOf(i)));
      lt = llvm::StructType::get(g_context, tl);
      break;
    }
    case tk_union: {
      Type tag = t.TagType();
      unsigned align = t.Alignment();
      verify((t.StorageSize() % align) == 0);

      // Note that the tag will always occupy multiple of "align" bytes, because
      // 1) a variant has a greater alignment, in which there'll be padding to
      //    make that happen, or
      // 2) all variants have a lesser alignment, in which "align" is the
      //    alignment of the tag and variants are forced to have that alignment
      //    too.
      // The LLVM type created matches what clang creates for C++ union types.
      size_t size = (t.StorageSize() - tag.StorageSize()) / align;
      llvm::Type *vt = align == 1 ? g_int8Ty
                     : align == 2 ? g_int16Ty
                     : align == 4 ? g_int32Ty
                     : g_int64Ty;
      verify(align == 8 || vt != g_int64Ty);
      llvm::Type *ut = llvm::ArrayType::get(vt, size);
      vector<llvm::Type *> tl { LLVMType(tag), ut };
      lt = llvm::StructType::get(g_context, tl);
      break;
    }
    case tk_pointer:
      if (t.BaseType() == tk_void)
        lt = g_voidptrTy;
      else
        lt = llvm::PointerType::get(LLVMType(t.BaseType()), 0);
      break;
    case tk_fatpointer: {
      vector<llvm::Type *> tl;
      tl.push_back(LLVMType(t.TypeOf(0)));
      tl.push_back(LLVMType(t.TypeOf(1)));
      lt = llvm::StructType::get(g_context, tl);
      break;
    }
    case tk_enum:
      return LLVMType(t.AsSubrange());
    case tk_array:
      if (t.IndexType())
        return llvm::ArrayType::get(LLVMType(t.ElementType()),
                                    t.IndexType().Cardinality());
      else
        return llvm::ArrayType::get(LLVMType(t.ElementType()), 0);
    case tk_set:
      return LLVMType(t.BaseType());
    default:
      verify(false);
      lt = nullptr;
  }

  return lt;
}

llvm::TargetMachine *LLVMTarget::GetTargetMachine() {
  if (m_targetMachine)
    return m_targetMachine;

  std::string errmsg;
  const llvm::Target *target =
    llvm::TargetRegistry::lookupTarget(TARGET_TRIPLE, errmsg);
  if (!target) {
    printf("Unable to select machine target: %s\n", errmsg.c_str());
    return nullptr;
  }
  llvm::TargetOptions options;
  m_targetMachine = target->createTargetMachine(TARGET_TRIPLE, "", "", options,
                                                llvm::Reloc::PIC_);
  return m_targetMachine;
}

llvm::TargetMachine *LLVMTarget::Finalize(llvm::Pass *pass) {
  // Define and initialize all vtables.
  for (LLVMClass *lc = m_classes; lc != nullptr; lc = lc->m_nextClass) {
    lc->EmitVtable();
    if (lc->m_class->Linkage() != lk_import)
      lc->m_vtable->setInitializer(lc->EmitVtableInit(lc->m_class, 0));
  }

  // Emit all methods.
  LLVMMethod *lm;
  for (lm = m_methods; lm != nullptr; lm = lm->m_nextMethod)
    lm->Emit();

  // Need to link with C run time.
  // (note: addLibrary is gone in llvm 3.4; doesn't seem to be needed)
  //m_module->addLibrary("c");
  //m_module->addLibrary("crtend");

  // Create main() function to call the Jolt Main method.
  // FIXME: no check for return type other than int.
  if (Method *me = Method::Main()) {
    lm = safe_cast<LLVMMethod *>(me->GetTarget(ep_run));

    vector<llvm::Type *> tl;
    tl.push_back(g_int32Ty);
    tl.push_back(llvm::PointerType::get(g_voidptrTy, 0));
    llvm::FunctionType *ft = llvm::FunctionType::get(g_int32Ty, tl, false);
    llvm::Function *f = llvm::Function::Create(ft,
      llvm::GlobalValue::ExternalLinkage, "main", m_module);

    llvm::BasicBlock *b = llvm::BasicBlock::Create(g_context, "", f);
    llvm::Value *rv = llvm::CallInst::Create(lm->m_function, "", b);
    if (!me->LowerFormals()->m_returnValue)
      rv = llvm::ConstantInt::get(g_int32Ty, 0, true);
    llvm::ReturnInst::Create(g_context, rv, b);
  }

  m_module->setDataLayout(GetTargetMachine()->createDataLayout());
  m_module->setTargetTriple(TARGET_TRIPLE);

  // Do some (very) elemental optimizations, optionally with a file writing
  // pass.
  llvm::legacy::PassManager pm;
  pm.add(llvm::createVerifierPass());
  pm.add(llvm::createCFGSimplificationPass());
  if (pass)
    pm.add(pass);
  pm.run(*m_module);
  return GetTargetMachine();
}

void LLVMTarget::WriteCodeToFile(const char *fname, llvm::CodeGenFileType ft) {
  bool binary = (ft != llvm::CGFT_AssemblyFile);
  if (!fname) {
    if (binary) {
#ifdef _WIN32
      fname = "jolt.obj";
#else
      fname = "jolt.o";
#endif
    } else {
      fname = "jolt.s";
    }
  }

  std::error_code err;
  llvm::raw_fd_ostream raw_out(fname, err,
               binary ? llvm::sys::fs::F_None : llvm::sys::fs::F_Text);
  if (err) {
    printf("%s\n", err.message().c_str());
    return;
  }

  llvm::TargetMachine *tm = Finalize(nullptr);
  llvm::legacy::PassManager pm;

#if LLVM_VERSION < 700
  tm->addPassesToEmitFile(pm, raw_out, ft);
#else
  tm->addPassesToEmitFile(pm, raw_out, nullptr, ft);
#endif
  pm.run(*m_module);
}

void LLVMTarget::WriteToFile(const char *fname) {
}

void LLVMObjTarget::WriteToFile(const char *fname) {
  WriteCodeToFile(fname, llvm::CGFT_ObjectFile);
}

void LLVMBCTarget::WriteToFile(const char *fname) {
  std::error_code err;
  llvm::raw_fd_ostream out(fname, err, llvm::sys::fs::F_None);
  if (err) {
    printf("%s\n", err.message().c_str());
    return;
  }

  Finalize(llvm::createBitcodeWriterPass(out));
}

void LLVMLLTarget::WriteToFile(const char *fname) {
  std::error_code err;
  llvm::raw_fd_ostream out(fname, err, llvm::sys::fs::F_Text);
  if (err) {
    printf("%s\n", err.message().c_str());
    return;
  }

  Finalize(llvm::createPrintModulePass(out, ""));
}

void LLVMAsmTarget::WriteToFile(const char *fname) {
  WriteCodeToFile(fname, llvm::CGFT_AssemblyFile);
}

vector<size_t> LLVMTarget::GetStructLayout_(Type t) {
  size_t count = t.FieldCount();
  vector<size_t> offsets(count + 1);
  if (count == 0)
    return offsets;

  llvm::Type *lt = LLVMType(t);
  auto *st = llvm::dyn_cast<llvm::StructType>(lt);
  auto *sl = m_module->getDataLayout().getStructLayout(st);
  offsets[count] = size_t(sl->getSizeInBytes());

  if (t == tk_class) {
    auto c = safe_cast<LLVMClass *>(t.Class()->GetTarget(ep_run));
    for (size_t i = 0; i < count; i++)
      if (int ord = c->m_fieldMap[i]; ord >= 0)
        offsets[i] = size_t(sl->getElementOffset(unsigned(ord)));
      else
        offsets[i] = i > 0 ? offsets[i - 1] : 0;
  } else {
    verify(t == tk_tuple);
    // FIXME: need class hack for void members!
    for (size_t i = 0; i < count; i++)
      offsets[i] = size_t(sl->getElementOffset(unsigned(i)));
  }
  return offsets;
}

unsigned LLVMTarget::GetAlignment_(Type t) {
  llvm::Type *lt = LLVMType(t);
  auto dl = m_module->getDataLayout();
  size_t align = dl.getABITypeAlignment(lt);
  return align;
}

static llvm::Module *GetModule(Epoch ep) {
  verify(ep == ep_run);
  return static_cast<LLVMTarget *>(Target::Get(ep))->GetModule();
}

class ToLLVM {
public:
  ToLLVM(LLVMTarget &target, llvm::Function *func, FormalArguments *formals)
      : m_llvm(target),
        m_function(func),
        m_formals(formals) { }

  llvm::BasicBlock *NewBlock() {
    return llvm::BasicBlock::Create(g_context, g_noName, m_function);
  }

  // Translate an expression subtree into LLVMs.  Any instructions created
  // are appended to the basic block provided.  The value of an instruction, if
  // any, is left on the interpreter stack.
  //
  // A node that alters flow of control may also substitute a new basic block
  // for the one provided.  For example, an if node would create a new block
  // representing the merged flow of control from the two alternatives.  This
  // block would substitute for the original.  When code following the node is
  // unreachable, because the node causes a non-local transfer of control, it
  // substitutes null for the basic block pointer.  All callers MUST check for
  // this and handle it appropriately.
  //
  // A full expression marks the scope of temporary variables.  Regardless of
  // where they are initialized, they are not destructed until the end of the
  // nearest enclosing full expression.
  llvm::Value *TranslateFullExpr(llvm::BasicBlock *&b, Node *e);
  llvm::Value *Translate(llvm::BasicBlock *&b, Node *e);
  llvm::Value *Translate_(llvm::BasicBlock *&b, Binary *bn);
  llvm::Value *Translate_(llvm::BasicBlock *&b, Block *bn);
  llvm::Value *Translate_(llvm::BasicBlock *&b, BuildPointer *bpn);
  llvm::Value *Translate_(llvm::BasicBlock *&b, Call *cn);
  llvm::Value *Translate_(llvm::BasicBlock *&b, CallBuiltin *cbn);
  llvm::Value *Translate_(llvm::BasicBlock *&b, Cast *cn);
  llvm::Value *Translate_(llvm::BasicBlock *&b, Construct *cn);
  llvm::Value *Translate_(llvm::BasicBlock *&b, If *in);
  llvm::Value *Translate_(llvm::BasicBlock *&b, Literal *ln);
  llvm::Value *Translate_(llvm::BasicBlock *&b, LiteralAddr *lan);
  llvm::Value *Translate_(llvm::BasicBlock *&b, MethodBody *mbn);
  llvm::Value *Translate_(llvm::BasicBlock *&b, Transfer *tn);
  llvm::Value *Translate_(llvm::BasicBlock *&b, Unary *un);
  llvm::Value *Translate_(llvm::BasicBlock *&b, VarDecl *vd, bool fullexpr);
  llvm::Value *Translate_(llvm::BasicBlock *&b, VtableSlot *vsn);
  llvm::Value *Translate_(llvm::BasicBlock *&b, While *wn);

  // Translate list of statements (each of which is a full expression and may
  // have variable declarations).
  llvm::Value *TranslateBlock(llvm::BasicBlock *&b, Node *stmts);

  // Translate a method body to IR.
  static void TranslateFunction(llvm::Function *bf,
                                FormalArguments *formals,
                                Node *e);

  // Emit code to push the address of a variable onto the stack.
  llvm::Value *EmitVarAddr(llvm::BasicBlock *&b, Var *ve);

  // Helper for translating MethodBody nodes.
  void GenerateArgumentPrologs(llvm::BasicBlock *&b,
                               FormalArguments *formals,
                               Argument *&rv);

  // Get the LLVM Module.
  llvm::Module *GetModule() { return m_llvm.GetModule(); }

  // Unwind the stack, destructing locals and executing regardless clauses,
  // until the specified mark is reached.
  void UnwindStack(llvm::BasicBlock *&b, int mark, bool inException);

  // Map Jolt type to an LLVM type.
  llvm::Type *LLVMType(Type t) { return m_llvm.LLVMType(t); }

  // Generate legal, unique LLVM name for variable.
  const std::string NameFor(Var *ve) {
    String *name = ve->Name();
    return name ? std::string(name->c_str()) : std::string();
  }

  // LLVM module into which code is being generated.
  LLVMTarget         &m_llvm;

  // LLVM function into which code is being generated.
  llvm::Function     *m_function;

  // Target location to hold value of object-valued expression.  Yields an
  // l-value.
  struct TargetTracker {
    ToLLVM           *m_parent;
    Node             *m_target;
    TargetTracker    *m_prev;

    TargetTracker(ToLLVM *tollvm, Node *newTarget)
        : m_parent(tollvm), m_target(newTarget),
          m_prev(tollvm->m_targetTracker) {
      tollvm->m_targetTracker = this;
    }
    ~TargetTracker() { m_parent->m_targetTracker = m_prev; }

    // If addr is null, translate current target instead.
    llvm::Value *Translate(llvm::BasicBlock *&b, Node *addr,
                           bool keepfat = false) {
      if (addr)
        return m_parent->Translate(b, addr);

      // We have to temporarily pop the current target, in case the address
      // refers to a target as well, which will be the previous one.  This
      // happens when constructing an array, for example.
      m_parent->m_targetTracker = m_prev;
      llvm::Value *v = m_parent->Translate(b, m_target);
      Type t = m_target->Kind() == nk_Deref
                   ? safe_cast<Deref *>(m_target)->m_expr->m_type
                   : m_target->m_type;
      if (!keepfat && !t.IsThin()) {
        if (b)
          v = llvm::ExtractValueInst::Create(v, 0, g_noName, b);
      }
      m_parent->m_targetTracker = this;
      return v;
    }

    llvm::Type *BaseType(Node *addr) {
      if (!addr)
        addr = m_target;
      return m_parent->LLVMType(addr->m_type.BaseType());
    }
  };

  TargetTracker      *m_targetTracker = nullptr;

  // Formal argument list description.
  FormalArguments    *m_formals;

  // Insertion point for allocas used to reserve space for local variables
  // and temporaries within a function's entry block.
  llvm::Instruction  *m_allocaInsertionPoint  = nullptr;

  // Map of VarDecls to their corresponding LLVM address value.
  enum VarBase { vb_unknown, vb_direct };

  struct VarInfo {
    llvm::Value      *m_addr    = nullptr;
    VarBase           m_base    = vb_unknown;
  };

  std::map<Var *, VarInfo> m_varMap;

  // Stack of nodes requiring action when they go out of scope, including
  // VarDecls and nodes related to exception handling.
  vector<VarDecl *> m_unwindStack;

  // A simple array that allows retrieving an LLVM argument value by index.
  vector<llvm::Argument *> m_llvmArgs;

  // Map of Label node to their reachable Transfer nodes.  Also marks the level
  // of the destruct stack on entry to the corresponding scope.  These may
  // differ for exit and next transfers, hence inner and outer marks.
  struct TransferInfo {
    TransferInfo(llvm::Value *v, llvm::BasicBlock *b, TransferKind tk)
        : m_value(v), m_fromBlock(b), m_kind(tk) { }

    llvm::Value      *m_value;
    llvm::BasicBlock *m_fromBlock;
    TransferKind      m_kind;
  };

  struct LabelInfo {
    vector<TransferInfo> m_transfers;
    int               m_innerMark   = -1;
    int               m_outerMark   = -1;
  };

  std::map<Label *, LabelInfo> m_labelToTransfersMap;

  // Map of node to its associated LabelInfo.
  Node               *m_nodeWithLabel     = nullptr;
  LabelInfo          *m_labelInfoForNode  = nullptr;

  // Some common constants.
  llvm::Value        *m_zero = llvm::ConstantInt::get(g_int32Ty, 0);

  // The LexicalScope class handles common bookkeeping with regards to entering
  // and exiting lexical scopes, namely tracking transfers to this scope and
  // what needs destructing as a result.
  class LexicalScope {
  public:
    LexicalScope(ToLLVM *self, Node *node) : m_self(self) {
      m_labelInfo = self->m_nodeWithLabel == node ? self->m_labelInfoForNode
                                                  : &m_dummyLabelInfo;
      verify(m_labelInfo->m_innerMark == -1 && m_labelInfo->m_outerMark == -1);
      m_labelInfo->m_innerMark = int(m_self->m_unwindStack.size());
      m_labelInfo->m_outerMark = m_labelInfo->m_innerMark;
    }

    ~LexicalScope() {
      verify(int(m_self->m_unwindStack.size()) == m_labelInfo->m_outerMark);
      m_labelInfo->m_innerMark = -1;
      m_labelInfo->m_outerMark = -1;
    }

    void push_back(llvm::Value *v, llvm::BasicBlock *b, TransferKind tk) {
      m_labelInfo->m_transfers.push_back(TransferInfo(v, b, tk));
    }

    unsigned size() { return (unsigned)m_labelInfo->m_transfers.size(); }
    TransferInfo &operator[](size_t i) { return m_labelInfo->m_transfers[i]; }

  private:
    ToLLVM           *m_self;
    LabelInfo         m_dummyLabelInfo;
    LabelInfo        *m_labelInfo         = nullptr;
  };
};

llvm::Value *ToLLVM::TranslateFullExpr(llvm::BasicBlock *&b, Node *e) {
  int mark = int(m_unwindStack.size());

  llvm::Value *v = Translate(b, e);

  if (b)
    UnwindStack(b, mark, false);
  m_unwindStack.resize(mark);

  return v;
}

llvm::Value *ToLLVM::Translate(llvm::BasicBlock *&b, Node *e) {
  switch (e->Kind()) {
    case nk_Attributes:
      return Translate(b, safe_cast<Attributes *>(e)->m_expr);

    case nk_Binary:
      return Translate_(b, safe_cast<Binary *>(e));

    case nk_Block:
      return Translate_(b, safe_cast<Block *>(e));

    case nk_BuildPointer:
      return Translate_(b, safe_cast<BuildPointer *>(e));

    case nk_Call:
      return Translate_(b, safe_cast<Call *>(e));

    case nk_CallBuiltin:
      return Translate_(b, safe_cast<CallBuiltin *>(e));

    case nk_Cast:
      return Translate_(b, safe_cast<Cast *>(e));

    case nk_Construct:
      return Translate_(b, safe_cast<Construct *>(e));

    case nk_Deref:
      return Translate(b, safe_cast<Deref *>(e)->m_expr);

    case nk_ExtractAddress: {
      auto ean = safe_cast<ExtractAddress *>(e);
      llvm::Value *fp = Translate(b, ean->m_pointer);
      if (b)
        return llvm::ExtractValueInst::Create(fp, 0, g_noName, b);
      return nullptr;
    }

    case nk_ExtractDescriptor: {
      auto edn = safe_cast<ExtractDescriptor *>(e);
      llvm::Value *fp = m_targetTracker->Translate(b, edn->m_pointer, true);
      if (b)
        return llvm::ExtractValueInst::Create(fp, 1, g_noName, b);
      return nullptr;
    }

    case nk_FieldAddr: {
      auto fan = safe_cast<FieldAddr *>(e);
      llvm::Value *target = m_targetTracker->Translate(b, fan->m_address);
      if (b) {
        Node *addr = fan->m_address ? static_cast<Node *>(fan->m_address)
                                    : m_targetTracker->m_target;
        int ord;
        Type bt = addr->m_type.BaseType();
        if (bt == tk_class) {
          Class *ce = bt.Class();
          auto c = safe_cast<LLVMClass *>(ce->GetTarget(ep_run));
          ord = c->m_fieldMap[fan->m_ord];
        } else {
          verify(bt == tk_tuple);
          ord = (int)fan->m_ord; // FIXME: handle void tuple members also!
        }
        if (ord >= 0) {
          llvm::Value *args[2] {
            m_zero,
            llvm::ConstantInt::get(g_int32Ty, ord)
          };
          llvm::Type *t = m_targetTracker->BaseType(fan->m_address);
          return llvm::GetElementPtrInst::Create(t, target, args, g_noName, b);
        }
      }
      return nullptr;
    }

    case nk_GlobalAddr: {
      auto gan = safe_cast<GlobalAddr *>(e);
      void *gs = gan->m_entity->GlobalStorage(m_llvm.m_epoch);
      return reinterpret_cast<llvm::GlobalVariable *>(gs);
    }

    case nk_If:
      return Translate_(b, safe_cast<If *>(e));

    case nk_Index: {
      auto in = safe_cast<Index *>(e);
      llvm::Value *op1 = m_targetTracker->Translate(b, in->m_address);
      if (b) {
        llvm::Value *op2 = Translate(b, in->m_index);
        if (b) {
          llvm::Value *args[2] = { m_zero, op2 };
          llvm::Type *t = m_targetTracker->BaseType(in->m_address);
          return llvm::GetElementPtrInst::Create(t, op1, args, g_noName, b);
        }
      }
      return nullptr;
    }

    case nk_Label: {
      auto ln = safe_cast<Label *>(e);

      // Let the statement immediately under us find the label info.
      m_nodeWithLabel = ln->m_expr;
      m_labelInfoForNode = &m_labelToTransfersMap[ln];

      // Compound statements handle the label themselves.
      llvm::Value *v = Translate(b, ln->m_expr);
      m_nodeWithLabel = nullptr;
      m_labelInfoForNode = nullptr;

      // FIXME: handle transfers from nested methods somehow.
      return v;
    }

    case nk_Literal:
      return Translate_(b, safe_cast<Literal *>(e));

    case nk_LiteralAddr:
      return Translate_(b, safe_cast<LiteralAddr *>(e));

    case nk_Load: {
      auto ln = safe_cast<Load *>(e);
      llvm::Value *addr = Translate(b, ln->m_address);
      if (b && ln->m_type != tk_void)
        return new llvm::LoadInst(addr, g_noName, b);
      return nullptr;
    }

    case nk_MethodBody:
      return Translate_(b, safe_cast<MethodBody *>(e));

    case nk_Sequence: {
      auto sn = safe_cast<Sequence *>(e);
      llvm::Value *first = Translate(b, sn->m_first);
      if (b) {
        llvm::Value *second = Translate(b, sn->m_second);
        if (b) {
          if (sn->m_valueIs == Sequence::vi_first)
            return first;
          else if (sn->m_valueIs == Sequence::vi_second)
            return second;
        }
      }
      return nullptr;
    }

    case nk_Shared:
      return Translate(b, safe_cast<Shared *>(e)->m_expr->Root());

    case nk_Store: {
      auto sn = safe_cast<Store *>(e);
      llvm::Value *addr = Translate(b, sn->m_address);
      if (b) {
        llvm::Value *op = Translate(b, sn->m_operand);
        if (b && sn->m_operand->m_type != tk_void)
          new llvm::StoreInst(op, addr, false, b);
      }
      return nullptr;
    }

    case nk_Transfer:
      return Translate_(b, safe_cast<Transfer *>(e));

    case nk_Unary:
      return Translate_(b, safe_cast<Unary *>(e));

    case nk_VarAddr:
      return EmitVarAddr(b, safe_cast<VarAddr *>(e)->m_var);

    case nk_VarDecl:
      return Translate_(b, safe_cast<VarDecl *>(e), false);

    case nk_Vtable: {
      auto vn = safe_cast<Vtable *>(e);
      Epoch ep = m_llvm.m_epoch;
      auto c = safe_cast<LLVMClass *>(vn->m_class->GetTarget(ep));
      return new llvm::BitCastInst(c->m_vtable, g_voidptrTy, g_noName, b);
    }

    case nk_VtableCast: {
      auto vcn = safe_cast<VtableCast *>(e);
      auto cls = safe_cast<LLVMClass *>(vcn->m_class->GetTarget(ep_run));
      BaseSpecifier *bs = vcn->m_class->GetBase(vcn->m_ord);
      llvm::Value *v = Translate(b, vcn->m_vtbl);
      if (vcn->m_upcast) {
        llvm::Value *args[2] = {
          m_zero,
          llvm::ConstantInt::get(g_int32Ty, bs->m_ord)
        };
        v = new llvm::BitCastInst(v, cls->m_vtable->getType(), g_noName, b);
        v = llvm::GetElementPtrInst::Create(cls->m_vtableType, v, args,
                                            g_noName, b);
        return new llvm::BitCastInst(v, g_voidptrTy, g_noName, b);
      } else {
        // This is going to require some very messy pointer arithmetic.
        verify(0 && "FIXME: implement downcasts");
        return nullptr;
      }
    }

    case nk_VtableSlot:
      return Translate_(b, safe_cast<VtableSlot *>(e));

    case nk_While:
      return Translate_(b, safe_cast<While *>(e));

    default:
      verify(0 && "Expr node cannot be translated to IR.");
      return nullptr;
  }
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, Literal *ln) {
  // FIXME: provide for a larger variety of types.
  Value *v = ln->m_value;
  Type t = v->ObjectType();
  if (t == Type::Bool()) {
    return v->AsBool() ? llvm::ConstantInt::getTrue(g_context)
                       : llvm::ConstantInt::getFalse(g_context);
  } else if (t == Type::SByte()) {
    return llvm::ConstantInt::get(g_int8Ty, v->As<int8_t>(), true);
  } else if (t == Type::Short()) {
    return llvm::ConstantInt::get(g_int16Ty, v->As<int16_t>(), true);
  } else if (t == Type::Int()) {
    return llvm::ConstantInt::get(g_int32Ty, v->As<int32_t>(), true);
  } else if (t == Type::Long()) {
    return llvm::ConstantInt::get(g_int64Ty, v->As<int64_t>(), true);
  } else if (t == Type::Address()) {
    // FIXME: doesn't have static initialization of a pointer to a statically
    // alloocated object.
    uintptr_t uip = v->As<intptr_t>();
    verify(uip == 0);
    return llvm::ConstantPointerNull::get(g_voidptrTy);
  } else if (t == tk_pseudo) {
    // If any pseudo values get here, it's becaus they were in a void context.
    // Since the value isn't being used, just replace it with zero.
    return llvm::ConstantInt::get(g_int1Ty, 0, false);
  } else if (t == Type::Float()) {
    return llvm::ConstantFP::get(g_floatTy, v->As<float>());
  } else if (t == Type::Double()) {
    return llvm::ConstantFP::get(g_doubleTy, v->As<double>());
  } else {
    verify(t.IsSimpleType());
    llvm::Type *lt = LLVMType(t);
    if (lt->isPointerTy())
      return llvm::ConstantPointerNull::get(
          llvm::dyn_cast<llvm::PointerType>(lt));
    bool isSigned = t.IsSigned();
    switch (t.StorageSize()) {
      case 0: return nullptr;
      case 1: return llvm::ConstantInt::get(lt, v->As<uint8_t>(), isSigned);
      case 2: return llvm::ConstantInt::get(lt, v->As<uint16_t>(), isSigned);
      case 4: return llvm::ConstantInt::get(lt, v->As<uint32_t>(), isSigned);
      case 8: return llvm::ConstantInt::get(lt, v->As<uint64_t>(), isSigned);
      default: verify(false); return nullptr;
    }
  }
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, LiteralAddr *lan) {
  int8_t *s = reinterpret_cast<int8_t *>(lan->m_data->Address());
  Type t = lan->m_type.BaseType().ElementType();
  size_t len = lan->m_type.BaseType().IndexType().Cardinality();
  auto lt = llvm::ArrayType::get(LLVMType(t), len);
  vector<llvm::Constant *> consts;

  // Yes, this is ugly as hell.  Surely there's a better way to do it!
  // Note: ConstantDataArray doesn't work, as it throws away everything after
  // a zero byte, regardless of the data type.
  // FIXME: this doesn't even handle classes and tuples.
  switch (t.StorageSize()) {
    case 1:
      for (size_t i = 0; i < len; i++)
        consts.push_back(llvm::ConstantInt::get(g_int8Ty, s[i]));
      break;
    case 2:
      for (size_t i = 0; i < len; i++)
        consts.push_back(llvm::ConstantInt::get(g_int16Ty, ((int16_t*)s)[i]));
      break;
    case 4:
      for (size_t i = 0; i < len; i++)
        consts.push_back(llvm::ConstantInt::get(g_int32Ty, ((int32_t*)s)[i]));
      break;
    case 8:
      for (size_t i = 0; i < len; i++)
        consts.push_back(llvm::ConstantInt::get(g_int64Ty, ((int64_t*)s)[i]));
      break;
    default:
      verify(false);
  }

  llvm::Constant *c = llvm::ConstantArray::get(lt, consts);
  llvm::GlobalVariable *g;
  if (Const *ce = lan->m_const) {
    g = new llvm::GlobalVariable(*GetModule(), c->getType(), true,
        llvm::GlobalValue::LinkOnceODRLinkage, c, ce->ExternalName());
  } else {
    g = new llvm::GlobalVariable(*GetModule(), c->getType(), true,
        llvm::GlobalValue::PrivateLinkage, c, g_noName);
    g->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  }

  return g;
}

static llvm::Value *WidenOperand(llvm::BasicBlock *b, llvm::Value *v, Type t) {
  if (t == tk_bool || t.StorageSize() >= 4)
    return v;
  if (t.IsSigned())
    return new llvm::SExtInst(v, g_int32Ty, g_noName, b);
  else
    return new llvm::ZExtInst(v, g_int32Ty, g_noName, b);
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, Unary *un) {
  // Translate our operand to IR.  Propagate unreachability immediately.
  llvm::Value *op = Translate(b, un->m_operand);
  if (!b)
    return nullptr;

  if (un->m_type == tk_float) {
    assert(un->m_opcode == Unary::op_neg);
    return llvm::BinaryOperator::CreateFNeg(op, g_noName, b);
  }

  // Widen an operand smaller than 32 bits.
  op = WidenOperand(b, op, un->m_operand->m_type);

  // Now that the operands are out of the way, go translate the unary
  // operation.
  switch (un->m_opcode) {
    case Unary::op_not:
      return llvm::BinaryOperator::CreateNot(op, g_noName, b);
    case Unary::op_neg:
      return llvm::BinaryOperator::CreateNeg(op, g_noName, b);
    default:
      verify(false);
      return nullptr;
  }
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, Binary *bn) {
  // Translate our operands to IR.  Propagate unreachability immediately.
  llvm::Value *op1 = Translate(b, bn->m_operand1);
  if (!b)
    return nullptr;

  llvm::Value *op2 = Translate(b, bn->m_operand2);
  if (!b)
    return nullptr;

  if (bn->m_operand1->m_type == tk_float) {
    switch (bn->m_opcode) {
      case Binary::op_add:
        return llvm::BinaryOperator::Create(llvm::Instruction::FAdd,
                                            op1, op2, g_noName, b);
      case Binary::op_sub:
        return llvm::BinaryOperator::Create(llvm::Instruction::FSub,
                                            op1, op2, g_noName, b);
      case Binary::op_mul:
        return llvm::BinaryOperator::Create(llvm::Instruction::FMul,
                                            op1, op2, g_noName, b);
      case Binary::op_div:
        return llvm::BinaryOperator::Create(llvm::Instruction::FDiv,
                                            op1, op2, g_noName, b);
      case Binary::op_seteq:
        return llvm::CmpInst::Create(llvm::Instruction::FCmp,
                                     llvm::ICmpInst::FCMP_OEQ,
                                     op1, op2, g_noName, b);
      case Binary::op_setne:
        return llvm::CmpInst::Create(llvm::Instruction::FCmp,
                                     llvm::ICmpInst::FCMP_ONE,
                                     op1, op2, g_noName, b);
      case Binary::op_setlt:
        return llvm::CmpInst::Create(llvm::Instruction::FCmp,
                                     llvm::ICmpInst::FCMP_OLT,
                                     op1, op2, g_noName, b);
      case Binary::op_setle:
        return llvm::CmpInst::Create(llvm::Instruction::FCmp,
                                     llvm::ICmpInst::FCMP_OLE,
                                     op1, op2, g_noName, b);
      case Binary::op_setgt:
        return llvm::CmpInst::Create(llvm::Instruction::FCmp,
                                     llvm::ICmpInst::FCMP_OGT,
                                     op1, op2, g_noName, b);
      case Binary::op_setge:
        return llvm::CmpInst::Create(llvm::Instruction::FCmp,
                                     llvm::ICmpInst::FCMP_OGE,
                                     op1, op2, g_noName, b);
      default:
        verify(false);
        return nullptr;
    }
  }

  // Widen operands smaller than 32 bits.
  op1 = WidenOperand(b, op1, bn->m_operand1->m_type);
  op2 = WidenOperand(b, op2, bn->m_operand2->m_type);

  // Now that the operands are out of the way, go translate the binary
  // operation.
  bool isSigned = bn->m_operand1->m_type.IsSigned();
  switch (bn->m_opcode) {
    case Binary::op_add:
      return llvm::BinaryOperator::CreateAdd(op1, op2, g_noName, b);
    case Binary::op_sub:
      return llvm::BinaryOperator::CreateSub(op1, op2, g_noName, b);
    case Binary::op_mul:
      return llvm::BinaryOperator::CreateMul(op1, op2, g_noName, b);
    case Binary::op_div:
      if (isSigned)
        return llvm::BinaryOperator::CreateSDiv(op1, op2, g_noName, b);
      else
        return llvm::BinaryOperator::CreateUDiv(op1, op2, g_noName, b);
    case Binary::op_rem:
      if (isSigned)
        return llvm::BinaryOperator::CreateSRem(op1, op2, g_noName, b);
      else
        return llvm::BinaryOperator::CreateURem(op1, op2, g_noName, b);
    case Binary::op_and:
      return llvm::BinaryOperator::CreateAnd(op1, op2, g_noName, b);
    case Binary::op_or:
      return llvm::BinaryOperator::CreateOr(op1, op2, g_noName, b);
    case Binary::op_xor:
      return llvm::BinaryOperator::CreateXor(op1, op2, g_noName, b);
    case Binary::op_seteq:
      return llvm::CmpInst::Create(llvm::Instruction::ICmp,
                                   llvm::ICmpInst::ICMP_EQ,
                                   op1, op2, g_noName, b);
    case Binary::op_setne:
      return llvm::CmpInst::Create(llvm::Instruction::ICmp,
                                   llvm::ICmpInst::ICMP_NE,
                                   op1, op2, g_noName, b);
    case Binary::op_setlt:
      return llvm::CmpInst::Create(llvm::Instruction::ICmp,
                                   isSigned ? llvm::ICmpInst::ICMP_SLT
                                            : llvm::ICmpInst::ICMP_ULT,
                                   op1, op2, g_noName, b);
    case Binary::op_setle:
      return llvm::CmpInst::Create(llvm::Instruction::ICmp,
                                   isSigned ? llvm::ICmpInst::ICMP_SLE
                                            : llvm::ICmpInst::ICMP_ULE,
                                   op1, op2, g_noName, b);
    case Binary::op_setgt:
      return llvm::CmpInst::Create(llvm::Instruction::ICmp,
                                   isSigned ? llvm::ICmpInst::ICMP_SGT
                                            : llvm::ICmpInst::ICMP_UGT,
                                   op1, op2, g_noName, b);
    case Binary::op_setge:
      return llvm::CmpInst::Create(llvm::Instruction::ICmp,
                                   isSigned ? llvm::ICmpInst::ICMP_SGE
                                            : llvm::ICmpInst::ICMP_UGE,
                                   op1, op2, g_noName, b);
    case Binary::op_shl:
      return llvm::BinaryOperator::CreateShl(op1, op2, g_noName, b);
    case Binary::op_shr:
      if (isSigned)
        return llvm::BinaryOperator::CreateAShr(op1, op2, g_noName, b);
      else
        return llvm::BinaryOperator::CreateLShr(op1, op2, g_noName, b);
    default:
      verify(false);
      return nullptr;
  }
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, Block *bn) {
  // FIXME: handle nested methods and classes.

  // Simple case of no label, and thus no exits.
  if (!bn->m_label)
    return TranslateBlock(b, bn->m_expr);

  LexicalScope ls(this, bn);
  bool hasValue = bn->m_type.IsSimpleNonUnit();

  // Now translate the body of the block.
  llvm::Value *v = TranslateBlock(b, bn->m_expr);
  if (b)
    ls.push_back(v, b, tk_exit);

  // Now we have to merge the flow of control.  We need yet another block to
  // which all the exits can branch and return as the new value of b.  But
  // only if there are any exits!
  if (ls.size() == 0) {
    // Flow of control never reaches the end of the block.
    b = nullptr;
    return nullptr;
  }

  b = NewBlock();
  for (size_t i = 0; i < ls.size(); i++)
    llvm::BranchInst::Create(b, ls[i].m_fromBlock);

  // If our value isn't a simple type, temporaries must be used.  This was
  // handled bn the lowering process.  So from an LLVM perspective, we do not
  // have a value.  We also don't have a value if its type is void.
  if (!hasValue)
    return nullptr;

  // Finally, determine the value of this block.  If only one branch reached the
  // merge block, its value is our value; otherwise, a phi node is needed.
  if (ls.size() == 1)
    return ls[0].m_value;

  llvm::Type *t = LLVMType(bn->m_type);
  llvm::PHINode *pn = llvm::PHINode::Create(t, ls.size(), g_noName, b);
  for (size_t i = 0; i < ls.size(); i++)
    pn->addIncoming(ls[i].m_value, ls[i].m_fromBlock);
  return pn;
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, BuildPointer *bpn) {
  llvm::Value *src = Translate(b, bpn->m_pointer);
  if (!b)
    return nullptr;

  llvm::Value *dsc = Translate(b, bpn->m_descriptor);
  if (!b)
    return nullptr;

  llvm::Type *fptype = LLVMType(bpn->m_type);
  llvm::Value *rv = llvm::UndefValue::get(fptype);

  // Cast source pointer if needed.
  Type ct = bpn->m_type.BaseType();
  verify(ct == tk_array);
  llvm::Type *dsttype =
    llvm::PointerType::get(llvm::ArrayType::get(LLVMType(ct.ElementType()), 0),
                           0);
  src = new llvm::BitCastInst(src, dsttype, g_noName, b);

  rv = llvm::InsertValueInst::Create(rv, src, 0, g_noName, b);

  if (bpn->m_descriptor->m_type == tk_pointer)
    dsc = new llvm::BitCastInst(dsc, g_voidptrTy, g_noName, b);

  rv = llvm::InsertValueInst::Create(rv, dsc, 1, g_noName, b);

  return rv;
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, Call *cn) {
  // Translate our operands to IR.  Propagate unreachability immediately.
  vector<Call::Slot> operands;
  Call::Slot rv;
  cn->GetSlotOrdering(operands, rv);
  llvm::Value *dest = nullptr;
  if (rv.m_arg) {
    dest = Translate(b, rv.m_arg);
    if (!b)
      return nullptr;
  }

  llvm::Value *func = Translate(b, cn->m_function);
  if (!b)
    return nullptr;

  vector<llvm::Value *> args;
  for (size_t i = 0; i < operands.size(); i++) {
    Node *e = operands[i].m_arg;
    llvm::Value *arg = m_targetTracker->Translate(b, e);
    if (!b)
      return nullptr;
    if (!e || e->m_type != tk_void)
      args.push_back(arg);
  }

  // FIXME: if our context needs to catch exceptions, use an invoke instruction
  // instead and generate the necessary exception handling.

  llvm::Value *v = llvm::CallInst::Create(func, args, g_noName, b);
  if (rv.m_arg) {
    new llvm::StoreInst(v, dest, false, b);
    v = nullptr;
  }
  return v;
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, CallBuiltin *cbn) {
  verify(false); // FIXME
  return nullptr;
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, Cast *cn) {
  // Translate our operand to IR.  Propagate unreachability immediately.
  llvm::Value *op = Translate(b, cn->m_operand);
  if (!b)
    return nullptr;

  // FIXME: handle floats.

  Type tt = cn->m_type;
  Type st = cn->m_operand->m_type;

  llvm::Type *ltt = LLVMType(tt);
  llvm::Type *lst = LLVMType(st);

  if (ltt == lst)
    return op;

  if (tt == tk_float || st == tk_float) {
    auto opcode = llvm::CastInst::getCastOpcode(op, st.IsSigned(),
                                                ltt, tt.IsSigned());
    return llvm::CastInst::Create(opcode, op, ltt, g_noName, b);
  }

  size_t ts = tt == tk_bool ? 1 : tt.StorageSize() * 8;
  size_t ss = st == tk_bool ? 1 : st.StorageSize() * 8;

  // FIXME: isn't this handled by getCastOpcode above?
  if (ts > ss) {
    if (st.IsSigned())
      return new llvm::SExtInst(op, LLVMType(tt), g_noName, b);
    else
      return new llvm::ZExtInst(op, LLVMType(tt), g_noName, b);
  } else if (ts < ss) {
    return new llvm::TruncInst(op, LLVMType(tt), g_noName, b);
  } else if ((ltt->isPointerTy() ^ lst->isPointerTy()) == 0) {
    return new llvm::BitCastInst(op, LLVMType(tt), g_noName, b);
  } else if (lst->isPointerTy()) {
    return new llvm::PtrToIntInst(op, LLVMType(tt), g_noName, b);
  } else {
    return new llvm::IntToPtrInst(op, LLVMType(tt), g_noName, b);
  }
}

void ToLLVM::GenerateArgumentPrologs(llvm::BasicBlock *&b,
                                     FormalArguments *formals,
                                     Argument *&rv) {
  for (auto ae : formals->m_arguments) {
    int slot = ae->m_slot;

    // Add a VarDecl mapping to a location right now.  When the argument is
    // passed-by-value, this necessitates the creation of a variable for two
    // reasons.  First, even am_in arguments are mutable, just as in C/C++.
    // Second, even if they weren't, when used as the receiver of a method
    // call, a reference to the receiver is passed to the method.  It's not
    // possible to have a reference to an LLVM argument.  In most cases the
    // optimizer will kill these variables and convert them to SSA form.
    VarInfo &vi = m_varMap[ae];
    verify(vi.m_base == vb_unknown);
    std::string n = NameFor(ae);
    verify(ae->m_mechanism == am_in);

    // If the argument is a simple value, create a local variable and
    // initialize it to the argument, and use the variable from this point
    // on; otherwise, we already have a pointer to storage we're free to
    // use as we please.
    Type t = ae->m_type.Lower();
    if (t.IsSimpleType()) {
      vi.m_base = vb_direct;
      if (t != tk_void) {
        vi.m_addr = new llvm::AllocaInst(LLVMType(t), 0, n,
            m_allocaInsertionPoint);
        new llvm::StoreInst(m_llvmArgs[slot], vi.m_addr, b);
      }
    } else {
      vi.m_base = vb_direct;
      vi.m_addr = m_llvmArgs[slot];
      vi.m_addr->setName(n);
    }
  }

  if (Argument *ae = formals->m_returnValue) {
    int slot = ae->m_slot;
    verify(ae->m_mechanism != am_in);
    if (ae->m_isReturned) {
      rv = ae;
    } else {
      // Storage for the argument has been passed to us.  Use it as the
      // address of the variable.
      VarInfo &vi = m_varMap[ae];
      verify(vi.m_base == vb_unknown);
      std::string n = NameFor(ae);
      vi.m_base = vb_direct;
      vi.m_addr = m_llvmArgs[slot];
      vi.m_addr->setName(n);
    }
  }
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, MethodBody *mbn) {
  // First step, build an array of the LLVM values that correspond to the
  // arguments.
  llvm::Function *f = m_function;
  for (auto I = f->arg_begin(); I != f->arg_end(); ++I)
    m_llvmArgs.push_back(&*I);

  // Walk the formal argument list, priming the VarDecl map for the arguments,
  // and emitting whatever setup code is needed given the circumstances for
  // each argument.
  Argument *rv = nullptr; // Argument that's returned as function value
  GenerateArgumentPrologs(b, m_formals, rv);

  LexicalScope ls(this, mbn);
  TargetTracker tt(this, nullptr);

  // For object-valued method bodies, setup the target.
  if (m_formals->m_returnValue && mbn->m_type == rt_rvalue) {
    Argument *ae = m_formals->m_returnValue;
    if (!ae->m_isReturned)
      tt.m_target = (new VarAddr({ }, ae))->Deref();
  }

  // Translate the body to IR.
  Translate(b, mbn->m_body);

  if (rv) {
    if (rv->m_mechanism == am_out) {
      // The selected out argument is also the declared return type of the
      // method, so go create a phi node to merge all the return statements
      // together.
      // Note: the only way the declared return type cannot be selected would
      // be if it wasn't a simple type, in which case as part of the lowering
      // process the returned value is copy constructed into the supplied
      // storage right before returning.

      // If nothing reaches the exit, say so to our parent.
      if (ls.size() == 0) {
        b = nullptr;
        return nullptr;
      }

      // Handle simple case of one predecessor.
      b = NewBlock();
      if (ls.size() == 1) {
        llvm::BranchInst::Create(b, ls[0].m_fromBlock);
        return ls[0].m_value;
      }

      llvm::Type *t = LLVMType(rv->m_type.Lower());
      llvm::PHINode *pn = llvm::PHINode::Create(t, ls.size(), g_noName, b);
      for (size_t i = 0; i < ls.size(); i++) {
        llvm::BranchInst::Create(b, ls[i].m_fromBlock);
        pn->addIncoming(ls[i].m_value, ls[i].m_fromBlock);
      }
      return pn;
    } else {
      // An explicit out argument has been selected as the return value
      // (such an argument can have implicit semantics only if it is NOT
      // a simple type).  A variable has been created to hold its value
      // during method execution, so go load and return it.

      // If nothing reaches the exit, say so to our parent.
      if (!b && ls.size() == 0)
        return nullptr;

      llvm::BasicBlock *bb = NewBlock();

      // No return was inserted to return the value of the method body, because
      // the method has a return type of void.  We need to special case this
      // falling off the end of the method.
      if (b)
        llvm::BranchInst::Create(bb, b);

      for (size_t i = 0; i < ls.size(); i++)
        llvm::BranchInst::Create(bb, ls[i].m_fromBlock);

      llvm::Value *v = m_varMap[rv].m_addr;
      b = bb;
      return new llvm::LoadInst(v, g_noName, bb);
    }
  } else {
    // If nothing reaches the exit, say so to our parent.
    if (!b && ls.size() == 0)
      return nullptr;

    llvm::BasicBlock *bb = NewBlock();

    if (b)
      llvm::BranchInst::Create(bb, b);

    for (size_t i = 0; i < ls.size(); i++)
      llvm::BranchInst::Create(bb, ls[i].m_fromBlock);

    b = bb;
    return nullptr;
  }
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, While *wn) {
  LexicalScope ls(this, wn);

  // The layout of blocks generated for while loops is as follows:
  //
  //   test:   <condition>
  //           br body,else,condition
  //
  //   body:   <body>
  //           br next
  //
  //   next:   <next>
  //           br test
  //
  //   else:   <else>
  //           br exit
  //
  //   exit:   <big phi node>
  //
  // The <next> block only exists for lowered for nodes.  For normal while
  // loops, <body> branches directly to <test>.
  //
  // Note: unlike other nodes, we don't do a very good job of detecting
  // unreachable code here.  There are just too many permutations to look for.
  // Anyway, it isn't really needed as unreachable basic blocks can be detected
  // and removed later; it's just nice to save the effort where possible
  // (and save ourselves the trouble of translating it).

  // Miscellaneous.
  bool hasValue = wn->m_type.IsSimpleNonUnit();

  // Translate the condition.
  llvm::BasicBlock *test = NewBlock();
  llvm::BasicBlock *test2 = test;
  llvm::Value *tv = TranslateFullExpr(test2, wn->m_condition);

  // Translate the body.
  llvm::BasicBlock *body = NewBlock();
  llvm::BasicBlock *body2 = body;
  Translate(body2, wn->m_body);

  // Translate the iterator successor for lowered for loops.
  llvm::BasicBlock *next = test;
  if (wn->m_next) {
    next = NewBlock();
    if (body2)
      llvm::BranchInst::Create(next, body2);

    body2 = next;
    Translate(body2, wn->m_next);
  }

  if (body2)
    llvm::BranchInst::Create(test, body2);

  // Create exit block.
  llvm::BasicBlock *exit = NewBlock();

  // Finish up.
  llvm::BasicBlock *els = NewBlock();
  llvm::BasicBlock *els2 = els;
  llvm::Value *ev = Translate(els2, wn->m_else);
  if (els2)
    ls.push_back(ev, els2, tk_exit);
  if (test2)
    llvm::BranchInst::Create(body, els, tv, test2);
  llvm::BranchInst::Create(test, b);

  // Figure out if anything ever reaches the exit block.  While we're doing
  // that, append branches to the exit and next transfers.
  bool exitReached = false;
  for (size_t i = 0; i < ls.size(); i++) {
    llvm::BasicBlock *from = ls[i].m_fromBlock;
    if (ls[i].m_kind == tk_exit) {
      // Block test2 already had a branch appended.
      if (from != test2)
        llvm::BranchInst::Create(exit, from);
      exitReached = true;
    } else {
      llvm::BranchInst::Create(next, from);
    }
  }

  // If nothing reaches the exit, say so to our parent.
  if (!exitReached) {
    b = nullptr;
    return nullptr;
  }
  b = exit;

  // If our value isn't a simple type, temporaries must be used.  This was
  // handled in the lowering process.  So from an LLVM perspective, we do not
  // have a value.  We also don't have a value if its type is void.
  if (!hasValue)
    return nullptr;

  // Finally, determine the value of this While node.  If only one branch
  // reached the merge block, its value is our value; otherwise, a phi node
  // is needed.
  if (ls.size() == 1)
    return ls[0].m_value;

  llvm::Type *t = LLVMType(wn->m_type);
  llvm::PHINode *pn = llvm::PHINode::Create(t, ls.size(), g_noName, b);
  for (size_t i = 0; i < ls.size(); i++)
    if (ls[i].m_kind == tk_exit)
      pn->addIncoming(ls[i].m_value, ls[i].m_fromBlock);
  return pn;
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, If *in) {
  LexicalScope ls(this, in);
  bool hasValue = in->m_type.IsSimpleNonUnit();

  // Translate the condition.
  llvm::Value *v = TranslateFullExpr(b, in->m_condition);

  if (b) {
    // Create a new basic block for each of the two possible paths, and then
    // insert a conditional branch from the previous block to these new blocks.
    llvm::BasicBlock *tb = NewBlock();
    llvm::BasicBlock *fb = NewBlock();
    llvm::BranchInst::Create(tb, fb, v, b);

    // Now translate the true and false branches.
    llvm::Value *tv = Translate(tb, in->m_ifTrue);
    if (tb)
      ls.push_back(tv, tb, tk_exit);

    llvm::Value *fv = Translate(fb, in->m_ifFalse);
    if (fb)
      ls.push_back(fv, fb, tk_exit);
  }

  // Now we have to merge the flow of control.  We need yet another block to
  // which all the exits can branch and return as the new value of b.  But
  // only if there are any exits!
  if (ls.size() == 0) {
    // Flow of control never reaches the end of the if statement.
    b = nullptr;
    return nullptr;
  }

  b = NewBlock();
  for (size_t i = 0; i < ls.size(); i++)
    llvm::BranchInst::Create(b, ls[i].m_fromBlock);

  // If our value isn't a simple type, temporaries must be used.  This was
  // handled in the lowering process.  So from an LLVM perspective, we do not
  // have a value.  We also don't have a value if its type is void.
  if (!hasValue)
    return nullptr;

  // Finally, determine the value of this If.  If only one branch reached the
  // merge block, its value is our value; otherwise, a phi node is needed.
  if (ls.size() == 1)
    return ls[0].m_value;

  llvm::Type *t = LLVMType(in->m_type);
  llvm::PHINode *pn = llvm::PHINode::Create(t, ls.size(), g_noName, b);
  for (size_t i = 0; i < ls.size(); i++)
    pn->addIncoming(ls[i].m_value, ls[i].m_fromBlock);
  return pn;
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, Transfer *tn) {
  // Do guard if present; if not present, any following code is unreachable.
  llvm::BasicBlock *final = nullptr;
  if (tn->m_guard) {
    // Translate guard expression.
    llvm::Value *guard = Translate(b, tn->m_guard);
    if (!b)
      return nullptr;

    // Branch based on outcome.
    final = NewBlock();
    llvm::BasicBlock *b2 = NewBlock();
    llvm::BranchInst::Create(b2, final, guard, b);
    b = b2;
  }

  // Translate the expression.  We do not return the resulting value to our
  // parent, who always sees a void result, but save it away for the relevant
  // statement node to use.
  llvm::Value *v = Translate(b, tn->m_expr);
  if (!b)
    return nullptr;

  // Unwind until the target lexical scope is reached.
  LabelInfo &li = m_labelToTransfersMap[tn->m_label];
  if (tn->m_kind == tk_next)
    UnwindStack(b, li.m_innerMark, false);
  else
    UnwindStack(b, li.m_outerMark, false);

  // Register us with our Label node.  Note that if any of the expressions we
  // just translated don't reach here, the registration doesn't take place and
  // it will be as if this node didn't exist.
  li.m_transfers.push_back(TransferInfo(v, b, tn->m_kind));


  // Set what the parent node sees.
  b = final;
  return nullptr;
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, VarDecl *vd,
                                bool fullexpr) {
  if (vd->m_entity->Kind() == ek_Argument) {
      // Do nothing.
  } else if (vd->m_entity->Kind() == ek_Var) {
    auto ve = safe_cast<Var *>(vd->m_entity);
    if (ve->VarKind() == vk_local) {
      // Allocate storage for local variables.
      VarInfo &vi = m_varMap[ve];
      Type t = vd->VariableType().Lower();
      llvm::Type *lt = LLVMType(t);
      vi.m_base = vb_direct;
      if (t != tk_void)
        vi.m_addr = new llvm::AllocaInst(lt, 0, NameFor(ve),
                                         m_allocaInsertionPoint);
    } else {
      verify(false);
    }
  }

  // Translate the initializer.  Propagate unreachability immediately.
  if (fullexpr)
    TranslateFullExpr(b, vd->m_initexpr);
  else
    Translate(b, vd->m_initexpr);
  if (!b)
    return nullptr;

  // Translate the expression in which this variable is in scope, which is also
  // the value of this node.
  using func_t = llvm::Value *(ToLLVM::*)(llvm::BasicBlock *&, Node *);
  func_t recurse = fullexpr ? &ToLLVM::TranslateBlock : &ToLLVM::Translate;
  if (vd->m_destructMode == dm_leaves_scope ||
      vd->m_destructMode == dm_not_on_return) {
    m_unwindStack.push_back(vd);
    llvm::Value *v = (this->*recurse)(b, vd->m_expr);
    if (b)
      UnwindStack(b, int(m_unwindStack.size()) - 1, false);
    m_unwindStack.pop_back();
    return v;
  } else if (vd->m_destructMode == dm_leaves_full_expr) {
    m_unwindStack.push_back(vd);
    return (this->*recurse)(b, vd->m_expr);
  } else {
    return (this->*recurse)(b, vd->m_expr);
  }
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, Construct *cn) {
  // The object has a non-simple type, otherwise this Construct wouldn't be
  // here.  Set it up as the place to put the object created by the constructor.
  TargetTracker tt(this, cn->m_addr);

  // Translate the constructor call.
  Translate(b, cn->m_init);
  return nullptr;
}

llvm::Value *ToLLVM::Translate_(llvm::BasicBlock *&b, VtableSlot *vsn) {
  llvm::Value *e = Translate(b, vsn->m_vtable);
  if (!b)
    return nullptr;

  Class *ce = vsn->m_class;
  auto c = safe_cast<LLVMClass *>(ce->GetTarget(ep_run));
  size_t slot = vsn->m_slot;

  // Jolt pointers carry around vtable addresses as void *, so cast
  // to the proper type.
  e = new llvm::BitCastInst(e, c->m_vtable->getType(), g_noName, b);
  llvm::Value *args[2] = { m_zero, nullptr };

  // Walk up the class hierarchy until we come to the class that owns the slot.
  while (ce->InheritsVtblSlot(slot)) {
    BaseSpecifier *bs = ce->OwnsVtblSlot(slot);
    args[1] = llvm::ConstantInt::get(g_int32Ty, bs->m_ord);
    e = llvm::GetElementPtrInst::Create(c->m_vtableType, e, args, g_noName, b);
    ce = bs->m_baseClass;
    c = safe_cast<LLVMClass *>(ce->GetTarget(ep_run));
  }

  // Fetch the method pointer.
  int ord = c->m_vtblMap[slot - c->m_mapBase];
  verify(ord >= 0);
  args[1] = llvm::ConstantInt::get(g_int32Ty, ord);
  e = llvm::GetElementPtrInst::Create(c->m_vtableType, e, args, g_noName, b);
  return new llvm::LoadInst(e, g_noName, false, b);
}

llvm::Value *ToLLVM::TranslateBlock(llvm::BasicBlock *&b, Node *stmts) {
  while (b && stmts->Kind() == nk_Sequence) {
    auto sn = safe_cast<Sequence *>(stmts);
    stmts = sn->m_second;
    TranslateFullExpr(b, sn->m_first);
  }

  if (b) {
    if (stmts->Kind() == nk_VarDecl)
      return Translate_(b, safe_cast<VarDecl *>(stmts), true);
    else
      return TranslateFullExpr(b, stmts);
  }

  return nullptr;
}

void ToLLVM::TranslateFunction(llvm::Function *func,
                               FormalArguments *formals,
                               Node *e) {
  LLVMTarget *target = static_cast<LLVMTarget *>(Target::Get(ep_run));
  ToLLVM tollvm(*target, func, formals);

  // Create the entry block, used mainly to hold allocas for allocating stack
  // space for variables and temporaries.
  llvm::BasicBlock *entry = tollvm.NewBlock();

  // Create the successor to the entry block.  This second block is where
  // translation to LLVM begins.  The entry block branches to its successor,
  // with the branch instruction saved as the insertion point for allocas.
  llvm::BasicBlock *b = tollvm.NewBlock();
  tollvm.m_allocaInsertionPoint = llvm::BranchInst::Create(b, entry);

  // Visit the expression nodes, translating them to IR.
  llvm::Value *v = tollvm.Translate(b, e);

  // Finally, add a return instruction.
  if (b)
    llvm::ReturnInst::Create(g_context, v, b);
}

llvm::Value *ToLLVM::EmitVarAddr(llvm::BasicBlock *&b, Var *ve) {
  // FIXME: this applies only for local variables in the function currently
  // being translated.  It does not apply to variables in outer functions in
  // which this function is nested.
  VarInfo &vi = m_varMap[ve];
  verify(vi.m_base == vb_direct);
  return vi.m_addr;
}

void ToLLVM::UnwindStack(llvm::BasicBlock *&b, int mark, bool inException) {
  verify(mark >= 0 && mark <= int(m_unwindStack.size()));
  for (int i = int(m_unwindStack.size()) - 1; i >= mark; i--) {
    VarDecl *vd = m_unwindStack[i];
    if (vd->NeedsDestructing(inException)) {
      Translate(b, vd->m_destructor);
      verify(b);
    }
  }
}

/******************************************************************************/

TargetClass *LLVMTarget::For(Class *ce) {
  return new LLVMClass(ce);
}

IMPLEMENT_OBJECT(LLVMClass)

void LLVMClass::Generate() {
  LLVMTarget *target = static_cast<LLVMTarget *>(Target::Get(ep_run));
  verify(!m_nextClass);

  // FIXME: need to initialize all static fields.  Also need to create and
  // initialize internal static fields like the vtable.
  if (Class *ce = m_class->MetaInstance()) {
    // FIXME: for imported generics, we still need to do this, unless we
    // know the class was instantiated in the imported module.
    if (ce->Linkage() != lk_import) {
      auto gv = (llvm::GlobalVariable *)ce->GlobalStorage(ep_run); // FIXME
      llvm::Type *lt = target->LLVMType(m_class->AsType());
      gv->setInitializer(llvm::ConstantAggregateZero::get(lt));
      // FIXME: initialize storage...
    }
  }

  // Schedule vtable creation.  This must wait until we know which methods are
  // actually going to be generated, i.e., when we're asked to write the file.
  m_nextClass = target->m_classes;
  target->m_classes = this;
}

bool LLVMClass::HasGlobalStorage(Epoch ep) {
  return m_storage != nullptr;
}

void *LLVMClass::GlobalStorage(Epoch ep) {
  if (!m_storage) {
    // FIXME: why does this differ so much from ToCPP?
    if (m_class->MetaInstance()) {
      // FIXME: this isn't correct, need a proper solution.
      m_storage = (llvm::GlobalVariable *)
          safe_cast<LLVMClass *>(m_class->MetaInstance()->GetTarget(ep))
          ->GlobalStorage(ep);
      return m_storage;
    }
    LLVMTarget *target = static_cast<LLVMTarget *>(Target::Get(ep));
    Class *meta = m_class->Metaclass();
    llvm::Type *lt = target->LLVMType(meta->AsType());
    // FIXME: Use internal/private linkage if it can be proven the class is
    // inaccessible, directly or indirectly, from importing modules.  Use comdat
    // and linkonce_odr for generic classes.
    llvm::GlobalValue::LinkageTypes lk = llvm::GlobalValue::ExternalLinkage;
    if (m_class->Linkage() == lk_once)
      lk = llvm::GlobalValue::LinkOnceODRLinkage;
    m_storage = new llvm::GlobalVariable(*GetModule(ep), lt, false, lk,
        0, m_class->GlobalStorageName());
  }

  return m_storage;
}

void LLVMClass::EmitVtable() {
  if (m_vtable)
    return;

  LLVMTarget *target = static_cast<LLVMTarget *>(Target::Get(ep_run));
  size_t slot = 0;

  // Define the LLVM struct type for our vtable.  The first fields will be the
  // vtables of our base classes.
  vector<llvm::Type *> tl;
  for (size_t i = 0; i < m_class->GetBaseCount(); i++) {
    BaseSpecifier *bs = m_class->GetBase(i);
    Class *ce = bs->m_baseClass;
    auto base = safe_cast<LLVMClass *>(ce->GetTarget(ep_run));
    if (!base->m_vtable)
      base->EmitVtable();
    tl.push_back(base->m_vtable->getType()->getElementType());
    slot = bs->m_vtblOffset + ce->GetVtableSize();
  }

  // For root classes, which inherit from no one, the first field is the
  // address of the class object.
  if (m_class->HasClassSlot()) {
    Type t = m_class->Metaclass()->AsType();
    tl.push_back(llvm::PointerType::get(target->LLVMType(t), 0));
    slot++;
  }

  // Follow with a member for each method in this class assigned a slot.
  m_mapBase = slot;
  for (size_t i = slot; i < m_class->GetVtableSize(); i++) {
    int mappedOrd = -1;
    Method *me = m_class->GetMethodForVtblSlot(i);
    if (me && me->IsVirtual()) {
      auto cm = safe_cast<LLVMMethod *>(me->GetTarget(ep_run));
      mappedOrd = int(tl.size());
      tl.push_back(llvm::PointerType::get(cm->GetType(), 0));
    }
    m_vtblMap.push_back(mappedOrd);
  }

  // Create global variable to hold our vtable.
  std::string name = m_class->ExternalName() + "_vtbl";
  llvm::StructType *lt = llvm::StructType::get(g_context, tl);
  // FIXME: same as for class object.
  m_vtable = new llvm::GlobalVariable(*GetModule(ep_run), lt, true,
      llvm::GlobalValue::ExternalLinkage, 0, name);
  m_vtableType = lt;
}

llvm::Constant *LLVMClass::EmitVtableInit(Class *derivedClass, size_t offset) {
  LLVMTarget *target = static_cast<LLVMTarget *>(Target::Get(ep_run));
  vector<llvm::Constant *> values;
  size_t slot = 0;

  // Do initializations for base classes.
  for (size_t i = 0; i < m_class->GetBaseCount(); i++) {
    BaseSpecifier *bs = m_class->GetBase(i);
    Class *ce = bs->m_baseClass;
    auto base = safe_cast<LLVMClass *>(ce->GetTarget(ep_run));
    auto c = base->EmitVtableInit(derivedClass, offset + bs->m_vtblOffset);
    values.push_back(c);
    slot = bs->m_vtblOffset + ce->GetVtableSize();
  }

  // For root classes, which inherit from no one, initialize pointer to class
  // object.
  if (m_class->HasClassSlot()) {
    auto outer = safe_cast<LLVMClass *>(derivedClass->GetTarget(ep_run));
    // FIXME: is "outer" the metaclass instance?
    Type t = m_class->Metaclass()->AsType();
    llvm::Type *lt = llvm::PointerType::get(target->LLVMType(t), 0);
    auto gv = (llvm::GlobalVariable *)outer->GlobalStorage(ep_run);
    // FIXME: need cast?
    values.push_back(llvm::ConstantExpr::getPointerCast(gv, lt));
    slot++;
  }

  // Initialize the remaining slots.
  for (size_t i = slot; i < m_class->GetVtableSize(); i++) {
    Method *me = derivedClass->GetMethodForVtblSlot(i + offset);
    if (me && me->IsVirtual()) {
      auto rm = safe_cast<LLVMMethod *>(me->GetTarget(ep_run));
      Method *ime = m_class->GetMethodForVtblSlot(i);
      auto im = safe_cast<LLVMMethod *>(ime->GetTarget(ep_run));
      llvm::PointerType *lt = llvm::PointerType::get(im->GetType(), 0);
      // FIXME: create thunks were needed.
      // FIXME: need something better than null pointers
      if (me->Body())
        values.push_back(llvm::ConstantExpr::getPointerCast(rm->m_function,
                                                            lt));
      else
        values.push_back(llvm::ConstantPointerNull::get(lt));
    }
}

  llvm::Type *lt = m_vtable->getType()->getElementType();
  return llvm::ConstantStruct::get(llvm::dyn_cast<llvm::StructType>(lt),values);
}

/******************************************************************************/

TargetConst *LLVMTarget::For(Const *fe) {
  return new LLVMConst(fe);
}

IMPLEMENT_OBJECT(LLVMConst)

void LLVMConst::Generate() {
}

void *LLVMConst::GlobalStorage(Epoch ep) {
  // FIXME: Const handles this itself... should it?
  verify(false);
  return nullptr;
}

/******************************************************************************/

TargetGlobal *LLVMTarget::For(Field *fe) {
  return new LLVMGlobal(fe);
}

IMPLEMENT_OBJECT(LLVMGlobal)

void LLVMGlobal::Generate() {
  GlobalStorage(ep_run); // FIXME
  LLVMTarget *target = static_cast<LLVMTarget *>(Target::Get(ep_run));
  llvm::Constant *c = nullptr;
  Type t = m_field->GetType();
  llvm::Type *lt = target->LLVMType(t);

  if (m_field->Linkage() != lk_import) {
    if (t.IsSimpleType())
      c = llvm::ConstantInt::get(lt, 0, t.IsSigned());
    else
      c = llvm::ConstantAggregateZero::get(lt);

    m_storage->setInitializer(c);
    // FIXME: initialize storage...
  }
}

void *LLVMGlobal::GlobalStorage(Epoch ep) {
  if (!m_storage) {
    LLVMTarget *target = static_cast<LLVMTarget *>(Target::Get(ep));
    llvm::Type *lt = target->LLVMType(m_field->GetType());
    // FIXME: Use internal/private linkage if it can be proven the global is
    // inaccessible, directly or indirectly, from importing modules.
    auto lk = m_field->Linkage() == lk_once
        ? llvm::GlobalValue::LinkOnceODRLinkage
        : llvm::GlobalValue::ExternalLinkage;
    m_storage = new llvm::GlobalVariable(*GetModule(ep), lt, false, lk,
        0, m_field->GlobalStorageName());
  }
  return m_storage;
}

/******************************************************************************/

TargetMethod *LLVMTarget::For(Method *me) {
  return new LLVMMethod(me);
}

TargetMethod *LLVMTarget::For(Method *me, int nm) {
  return new LLVMMethod(me, nm);
}

IMPLEMENT_OBJECT(LLVMMethod)

void LLVMMethod::Generate() {
  verify(!m_nextMethod);

  // FIXME: temp hack.  Cannot export builtin operator.. because it uses
  // types as parameters.
  if (m_method->IsNative() && !m_function)
    return;

  // The function object probably exists, as for example it would be created
  // when this method was used in another method.  But not every method is
  // used internally, so...
  GlobalStorage(ep_run); // FIXME

  if (!m_method->IsNative()) {
    if (m_method->Body()) {
      // Schedule this method for later emission.  We must wait until all
      // vtables have been defined, so that VtableSlot nodes can be translated.
      LLVMTarget *target = static_cast<LLVMTarget *>(Target::Get(ep_run));
      m_nextMethod = target->m_methods;
      target->m_methods = this;
    } else {
      // We have a deferred method with no body.
      // FIXME: arrange to have a meaningful error reported if this gets
      // executed.

      // Or we have a "large" non-inline, non-generic imported method.
      // FIXME: for the generic case, we need link once semantics, and for
      // inline we need available externally.
    }
  }
}

void LLVMMethod::Emit() {
  FormalArguments *formals = m_method->LowerFormals();
  ToLLVM::TranslateFunction(m_function, formals, m_method->Body()->Root());
}

static void GatherFormals(FormalArguments *formals,
                          LLVMTarget *target,
                          vector<llvm::Type *> &args,
                          llvm::Type *&rv) {
  if (Argument *ae = formals->m_returnValue) {
    int slot = ae->m_slot;
    verify(ae->m_mechanism != am_in);
    Type t = ae->m_type.Lower();
    if (ae->m_isReturned)
      rv = target->LLVMType(t);
    else
      args[slot] = llvm::PointerType::get(target->LLVMType(t), 0);
  }

  for (auto ae : formals->m_arguments) {
    verify(ae->m_mechanism == am_in);
    int slot = ae->m_slot;
    if (slot >= 0) {
      Type t = ae->m_type.Lower();
      if (t.IsSimpleType())
        args[slot] = target->LLVMType(t);
      else
        args[slot] = llvm::PointerType::get(target->LLVMType(t), 0);
    }
  }
}

void *LLVMMethod::GlobalStorage(Epoch ep) {
  if (m_function)
    return m_function;

  // FIXME: Use internal linkage if it can be proven the method is
  // inaccessible, directly or indirectly, from importing modules.  Use
  // comdat and linkonce_odr for generic methods.  Use available_externally
  // for imported inline methods.  In a main module, everything except main()
  // and generics ought to be internal.
  llvm::GlobalValue::LinkageTypes lk = llvm::GlobalValue::ExternalLinkage;
  if (auto mlk = m_method->Linkage(); mlk == lk_once)
    lk = llvm::GlobalValue::LinkOnceODRLinkage;
  else if (mlk == lk_import && m_method->Body())
    lk = llvm::GlobalValue::AvailableExternallyLinkage;
  llvm::FunctionType *ft = GetType();
  m_function = llvm::Function::Create(ft, lk, m_method->ExternalName(),
                                      GetModule(ep));
  return m_function;
}

llvm::FunctionType *LLVMMethod::GetType() {
  if (!m_type) {
    FormalArguments *formals = m_method->LowerFormals();
    vector<llvm::Type *> args(formals->m_nSlots);
    llvm::Type *rv = g_voidTy;
    LLVMTarget *target = static_cast<LLVMTarget *>(Target::Get(ep_run));
    GatherFormals(formals, target, args, rv);
    bool vararg = formals->m_variadic == vt_c;
    m_type = llvm::FunctionType::get(rv, args, vararg);
  }

  return m_type;
}

static llvm::AttributeList alp_noalias;
static llvm::AttributeList alp_noalias_nounwind;

static llvm::Function *emitOperatorNew(llvm::Module *m, Method *me) {
  // Generate the optimized LLVM code for:
  //    void *operator new(size_t dx) {
  //      if (void *p = malloc(sz))
  //        return p;
  //      else
  //        throw bad_alloc;
  //    }
  vector<llvm::Type *> tl;
  tl.push_back(sizeof(size_t) == 4 ? g_int32Ty : g_int64Ty);
  auto ft = llvm::FunctionType::get(g_voidptrTy, tl, false);
  auto f = llvm::Function::Create(ft, llvm::Function::InternalLinkage,
                                  me->ExternalName(), m);

  CreateAttributeList(alp_noalias,
      llvm::AttributeList::ReturnIndex,   llvm::Attribute::NoAlias);
  f->setAttributes(alp_noalias);

  auto malloc = llvm::Function::Create(ft, llvm::GlobalValue::ExternalLinkage,
                                       "malloc", m);
  malloc->setCallingConv(llvm::CallingConv::C);
  CreateAttributeList(alp_noalias_nounwind,
      llvm::AttributeList::ReturnIndex,   llvm::Attribute::NoAlias,
      llvm::AttributeList::FunctionIndex, llvm::Attribute::NoUnwind);
  malloc->setAttributes(alp_noalias_nounwind);

  auto b = llvm::BasicBlock::Create(g_context, g_noName, f);
  auto arg = &*f->arg_begin();
  auto call = llvm::CallInst::Create(malloc, arg, g_noName, b);
  call->setCallingConv(llvm::CallingConv::C);
  call->setAttributes(alp_noalias_nounwind);

  // FIXME: check for error return from malloc and throw exception once
  // exceptions are implemented.
  llvm::ReturnInst::Create(g_context, call, b);
  return f;
}

static llvm::AttributeList alp_nounwind_readnone_inlinehint;

static llvm::Function *emitOperatorPlacementNew(llvm::Module *m,
                                                Method *me) {
  // Generate the optimized LLVM code for:
  //    void *operator new(void *p, size_t) throws() { return p; }
  vector<llvm::Type *> tl;
  tl.push_back(g_voidptrTy);
  tl.push_back(sizeof(size_t) == 4 ? g_int32Ty : g_int64Ty);
  auto ft = llvm::FunctionType::get(g_voidptrTy, tl, false);
  auto f = llvm::Function::Create(ft, llvm::Function::InternalLinkage,
                                  me->ExternalName(), m);

  CreateAttributeList(alp_nounwind_readnone_inlinehint,
      llvm::AttributeList::FunctionIndex, llvm::Attribute::NoUnwind,
      llvm::AttributeList::FunctionIndex, llvm::Attribute::ReadNone,
      llvm::AttributeList::FunctionIndex, llvm::Attribute::InlineHint);
  f->setAttributes(alp_nounwind_readnone_inlinehint);

  auto b = llvm::BasicBlock::Create(g_context, g_noName, f);
  llvm::ReturnInst::Create(g_context, &*f->arg_begin(), b);
  return f;
}

static llvm::AttributeList alp_nounwind;
static llvm::AttributeList alp_nocapture_nounwind;
static llvm::AttributeList alp_nocapture_nounwind_inlinehint;

static llvm::Function *emitOperatorDelete(llvm::Module *m, Method *me) {
  // Generate the optimized LLVM code for:
  //    void operator delete(void *p) throws() { free(p); }
  vector<llvm::Type *> tl;
  tl.push_back(g_voidptrTy);
  auto ft = llvm::FunctionType::get(g_voidTy, tl, false);
  auto f = llvm::Function::Create(ft, llvm::Function::InternalLinkage,
                                  me->ExternalName(), m);

  CreateAttributeList(alp_nocapture_nounwind_inlinehint,
      1,                                  llvm::Attribute::NoCapture,
      llvm::AttributeList::FunctionIndex, llvm::Attribute::NoUnwind,
      llvm::AttributeList::FunctionIndex, llvm::Attribute::InlineHint);
  f->setAttributes(alp_nocapture_nounwind_inlinehint);

  auto free = llvm::Function::Create(ft, llvm::GlobalValue::ExternalLinkage,
                                     "free", m);
  free->setCallingConv(llvm::CallingConv::C);
  CreateAttributeList(alp_nocapture_nounwind,
      1,                                  llvm::Attribute::NoCapture,
      llvm::AttributeList::FunctionIndex, llvm::Attribute::NoUnwind);
  free->setAttributes(alp_nocapture_nounwind);

  auto b = llvm::BasicBlock::Create(g_context, g_noName, f);
  auto arg = &*f->arg_begin();
  auto call = llvm::CallInst::Create(free, arg, g_noName, b);
  call->setCallingConv(llvm::CallingConv::C);
  call->setTailCall(true);
  CreateAttributeList(alp_nounwind,
      llvm::AttributeList::FunctionIndex, llvm::Attribute::NoUnwind);
  call->setAttributes(alp_nounwind);

  llvm::ReturnInst::Create(g_context, b);
  return f;
}

static llvm::AttributeList alp_nocapture12_nounwind_readnone_inlinehint;

static llvm::Function *emitOperatorPlacementDelete(llvm::Module *m, Method *me) {
  // Generate the optimized LLVM code for:
  //    void operator delete(void *, void *) throws() { }
  vector<llvm::Type *> tl;
  tl.push_back(g_voidptrTy);
  tl.push_back(g_voidptrTy);
  auto ft = llvm::FunctionType::get(g_voidTy, tl, false);
  auto f = llvm::Function::Create(ft, llvm::Function::InternalLinkage,
                                  me->ExternalName(), m);

  CreateAttributeList(alp_nocapture12_nounwind_readnone_inlinehint,
      1,                                  llvm::Attribute::NoCapture,
      2,                                  llvm::Attribute::NoCapture,
      llvm::AttributeList::FunctionIndex, llvm::Attribute::NoUnwind,
      llvm::AttributeList::FunctionIndex, llvm::Attribute::ReadNone,
      llvm::AttributeList::FunctionIndex, llvm::Attribute::InlineHint);
  f->setAttributes(alp_nocapture12_nounwind_readnone_inlinehint);

  auto b = llvm::BasicBlock::Create(g_context, g_noName, f);
  llvm::ReturnInst::Create(g_context, b);
  return f;
}

static llvm::Function *g_printf = nullptr;

static void CallPrintf(llvm::Module *m, llvm::BasicBlock *b,
                       const char *fmt, llvm::Value *v1,
                       llvm::Value *v2 = nullptr) {
  if (!g_printf) {
    vector<llvm::Type *> tl;
    tl.push_back(g_voidptrTy);
    auto ft = llvm::FunctionType::get(g_int32Ty, tl, true);
    g_printf = llvm::Function::Create(ft, llvm::Function::ExternalLinkage,
        "printf", m);
  }

  // Setup arguments to printf.
  auto c = llvm::ConstantDataArray::getString(g_context, fmt);
  auto var = new llvm::GlobalVariable(*m, c->getType(), true,
      llvm::GlobalValue::PrivateLinkage, c, g_noName);
  var->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  auto zero = llvm::ConstantInt::get(g_int32Ty, 0);
  llvm::Value *args[2] = { zero, zero };
  args[0] = llvm::GetElementPtrInst::Create(c->getType(), var, args,
                                            g_noName, b);

  // Call printf.
  if (!v2) {
    args[1] = v1;
    llvm::CallInst::Create(g_printf, args, g_noName, b);
  } else {
    llvm::Value *args3[] = { args[0], v1, v2 };
    llvm::CallInst::Create(g_printf, args3, g_noName, b);
  }
}

static llvm::Function *emitPrint(llvm::Module *m, Method *me,
                                 llvm::Type *t, const char *fmt) {
  vector<llvm::Type *> tl;
  tl.push_back(t);
  auto ft = llvm::FunctionType::get(g_voidTy, tl, false);
  auto f = llvm::Function::Create(ft, llvm::Function::InternalLinkage,
                                  me->ExternalName(), m);
  auto b = llvm::BasicBlock::Create(g_context, g_noName, f);
  CallPrintf(m, b, fmt, &*f->arg_begin());
  llvm::ReturnInst::Create(g_context, b);
  return f;
}

static llvm::Function *emitPrintFloat(llvm::Module *m, Method *me) {
  vector<llvm::Type *> tl;
  tl.push_back(g_floatTy);
  auto ft = llvm::FunctionType::get(g_voidTy, tl, false);
  auto f = llvm::Function::Create(ft, llvm::Function::InternalLinkage,
                                  me->ExternalName(), m);
  auto b = llvm::BasicBlock::Create(g_context, g_noName, f);
  llvm::Value *arg = &*f->arg_begin();
  arg = new llvm::FPExtInst(arg, g_doubleTy, g_noName, b);
  CallPrintf(m, b, "%.4f", arg);
  llvm::ReturnInst::Create(g_context, b);
  return f;
}

static llvm::Function *emitPrintString(llvm::Module *m, Method *me) {
  vector<llvm::Type *> tl;
  tl.push_back(llvm::PointerType::get(llvm::ArrayType::get(g_int8Ty, 0), 0));
  tl.push_back(sizeof(size_t) == 4 ? g_int32Ty : g_int64Ty);
  auto lt = llvm::StructType::get(g_context, tl);

  tl.resize(1);
  tl[0] = lt;
  auto ft = llvm::FunctionType::get(g_voidTy, tl, false);
  auto f = llvm::Function::Create(ft, llvm::Function::InternalLinkage,
                                  me->ExternalName(), m);

  auto b = llvm::BasicBlock::Create(g_context, g_noName, f);
  llvm::Value *arg = f->arg_begin();
  auto addr = llvm::ExtractValueInst::Create(arg, 0, g_noName, b);
  auto len = llvm::ExtractValueInst::Create(arg, 1, g_noName, b);
  CallPrintf(m, b, "%.*s", len, addr);
  llvm::ReturnInst::Create(g_context, b);
  return f;
}

static llvm::Function *emitPrintBool(llvm::Module *m, Method *me) {
  vector<llvm::Type *> tl;
  tl.push_back(g_int1Ty);
  auto ft = llvm::FunctionType::get(g_voidTy, tl, false);
  auto f = llvm::Function::Create(ft, llvm::Function::InternalLinkage,
                                  me->ExternalName(), m);
  auto b = llvm::BasicBlock::Create(g_context, g_noName, f);

  // Turn bool value into "true" or "false" string literals.
  auto c = llvm::ConstantDataArray::getString(g_context, "true", true);
  auto v = new llvm::GlobalVariable(*GetModule(ep_run),
      c->getType(), true, llvm::GlobalValue::PrivateLinkage, c, g_noName);
  v->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  auto zero = llvm::ConstantInt::get(g_int32Ty, 0);
  llvm::Value *args[2] = { zero, zero };
  auto v1 = llvm::GetElementPtrInst::Create(c->getType(), v, args, g_noName, b);

  c = llvm::ConstantDataArray::getString(g_context, "false", true);
  v = new llvm::GlobalVariable(*GetModule(ep_run), c->getType(), true,
      llvm::GlobalValue::PrivateLinkage, c, g_noName);
  v->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  auto v2 = llvm::GetElementPtrInst::Create(c->getType(), v, args, g_noName, b);
  auto arg = llvm::SelectInst::Create(&*f->arg_begin(), v1, v2, g_noName, b);

  CallPrintf(m, b, "%s", arg);
  llvm::ReturnInst::Create(g_context, b);
  return f;
}

llvm::Function *LLVMMethod::GetNativeFunction(Epoch ep, int nm) {
  llvm::Module *m = GetModule(ep);

  switch (nm) {
    case Method::nm_operator_new:
      return emitOperatorNew(m, m_method);
    case Method::nm_operator_placement_new:
      return emitOperatorPlacementNew(m, m_method);
    case Method::nm_operator_delete:
      return emitOperatorDelete(m, m_method);
    case Method::nm_operator_placement_delete:
      return emitOperatorPlacementDelete(m, m_method);
    case Method::nm_print_int:
      return emitPrint(m, m_method, g_int32Ty, "%d");
    case Method::nm_print_long:
      return emitPrint(m, m_method, g_int64Ty, "%lld");
    case Method::nm_print_uint:
      return emitPrint(m, m_method, g_int32Ty, "%u");
    case Method::nm_print_ulong:
      return emitPrint(m, m_method, g_int64Ty, "%llu");
    case Method::nm_print_bool:
      return emitPrintBool(m, m_method);
    case Method::nm_print_string:
      return emitPrintString(m, m_method);
    case Method::nm_print_char:
      return emitPrint(m, m_method, g_int8Ty, "%c");
    case Method::nm_print_float:
      return emitPrintFloat(m, m_method);
    case Method::nm_print_double:
      return emitPrint(m, m_method, g_doubleTy, "%.4f");
    case Method::nm_build_apply:
    case Method::nm_signed_subrange:
    case Method::nm_unsigned_subrange:
      return nullptr;
    default:
      verify(false);
      return nullptr;
  }
}
