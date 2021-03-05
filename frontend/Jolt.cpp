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

// Jolt.cpp : Defines the entry point for the console application.

#include "entity/Method.h"
#include "entity/Namespace.h"
#include "entity/Scope.h"
#include "parser/Location.h"
#include "parser/ParserDecls.h"
#include "parser/SymbolTable.h"
#include "parser/Token.h"
#include "target/Target.h"
#include "target/TranslateClosure.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"
#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#  include <process.h>
#else
#  include <unistd.h>
#endif

// Externals defined in JoltParser.y.
extern int Joltparse(void);
extern AST::BisonArray<AST::DeclInfo> yyparsetree;
#ifndef NDEBUG
extern int Joltdebug;   // Defined in JoltParser.y only in debug build
#else
static int Joltdebug;   // Not defined in JoltParser.y, so stub out locally
#endif

// Global flag to trace FSM states.
bool g_TraceFSM = false;

// Global vector of module search paths.
vector<std::string> g_moduleSearchPaths;

// At the end due to all the OS-specific header files required; don't
// pollute the namespace here with that stuff.
static void AppendExeDirectory();
static const char *g_UnixLinker;

static void DriveQueue() {
  // Keep resolving work units until all are fully resolved or some circular
  // dependency prevents further progress.
  while (Entity *e = Entity::GetNextTracked()) {
    e->ResolveFully();
  };
}

static const char *DeduceTarget(const char *out) {
  if (const char *p = strrchr(out, '.')) {
    if (!strcmp(p, ".o") || !strcmp(p, ".obj") || !strcmp(p, ".exe"))
      return "llvm-obj";
    if (!strcmp(p, ".s") || !strcmp(p, ".asm"))
      return "llvm-asm";
    if (!strcmp(p, ".bc"))
      return "llvm-bc";
    if (!strcmp(p, ".ll"))
      return "llvm-ll";
  } else {
    return "llvm-obj";
  }

  return nullptr;
}

static bool ParseFiles(vector<const char *> &files) {
  bool success = true;

  for (auto fname : files) {
    FILE *f = fopen(fname, "r");

    if (f == nullptr) {
      perror(fname);
      continue;
    }

    printf("Parsing %s:\n", fname);

    fseek(f, 0, SEEK_END);
    long len = ftell(f);
    rewind(f);

    char *buf = new char[len + 3];
    char *p = buf;
    int c;

    // FIXME:  For now assume all files are in UTF-8.  Eventually, handle other
    // formats and convert them to UTF-8.  For that matter, also verify that
    // files which are supposedly UTF-8 really are.
    while ((c = fgetc(f)) > 0)
      *p++ = char(c);
    fclose(f);

    // Append newline if file didn't end in one.
    if (p[-1] != '\n')
      *p++ = '\n';

    // Flex buffers need two zero byte terminators.
    *p++ = 0;
    *p++ = 0;

    Location::SetFile(fname, buf, p - buf);

    if (Joltparse()) {
      success = false;
    } else {
      // Convert entire syntax tree into nodes and entities.
      Namespace::ModuleNamespace()->Populate(yyparsetree, nullptr);
    }

    delete[] buf;
    Token::Clear();
  }

  return success;
}

static void ComputeClosure(Method *me) {
  TranslateClosure tc { ep_run };
  tc.AddToClosure(me);
  tc.Finish();
}

static void ComputeClosure(Scope *s) {
  TranslateClosure tc { ep_run };
  auto func = [&tc](const String *, Entity *e) mutable {
    tc.AddToClosure(e);
  };
  s->EnumerateMembers(func);
  tc.Finish();
}

std::string ReplaceSuffix(const char *basefn, const char *suffix) {
  if (const char *p = strrchr(basefn, '.')) {
    std::string fn(basefn, p - basefn);
    return fn + suffix;
  }
  else {
    return std::string(basefn) + suffix;
  }
}

static std::string SetModuleName(const char *basefn) {
  std::string mn { ReplaceSuffix(basefn, "") };
  auto p = mn.find_last_of("/\\", std::string::npos, 2);
  if (p != std::string::npos)
    mn.erase(0, ++p);
  Entity::SetModuleName(mn);
  return mn;
}

static void ExportModule(const char *basefn) {
  std::string fn { ReplaceSuffix(basefn, ".jmod") };

  FILE *f = fopen(fn.c_str(), "wb");
  if (!f) {
    perror("cannot open.");
    return;
  }

  {
    ModuleDeflator DF(f);
    uint32_t ord = DF(Namespace::ExportNamespace());
    verify(ord == 0);
  }

  fclose(f);
}

int main(int argc, char *argv[]) {
  const char *outfile = nullptr;
  vector<const char *> files;
  bool genstd = false;

  // Append the directory in which our executable was located to the
  // module search path; it takes precedence over any user-specified.
  AppendExeDirectory();

  // Go parse files listed on command line.
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "+d") == 0) {
      Joltdebug = 1;
      continue;
    } else if (strcmp(argv[i], "-d") == 0) {
      Joltdebug = 0;
      continue;
    } else if (strcmp(argv[i], "-trace") == 0) {
      g_TraceFSM = true;
      continue;
    } else if (strcmp(argv[i], "-genstd") == 0) {
      genstd = true;
      continue;
    } else if (strcmp(argv[i], "-I") == 0) {
      if (++i < argc)
        g_moduleSearchPaths.push_back(argv[i]);
      else
        printf("No directory path after -I\n");
      continue;
    } else if (strcmp(argv[i], "-o") == 0) {
      if (++i < argc)
        outfile = argv[i];
      else
        printf("No file name after -o\n");
      continue;
    } else {
      files.push_back(argv[i]);
    }
  }

  if (files.size() == 0) {
    printf("No source provided.\n");
    exit(1);
  }

  if (outfile) {
    const char *target = DeduceTarget(outfile);
    if (!target) {
      printf("Unrecognized file extension: %s\n", outfile);
      exit(1);
    }

    if (!Target::SetTarget(target)) {
      printf("Unknown target: %s\n", target);
      exit(1);
    }
  } else {
    Target::SetTarget("llvm-obj");
  }

  Target *target = Target::Get(ep_run);
  target->Setup();

  Metadata::SetupMetadata();
  std::string modname = SetModuleName(outfile ? outfile : files[0]);
  Namespace::CreateModuleNamespace(genstd);
  Namespace::ExportNamespace()->SetModuleName(String::Get(modname));

  if (!ParseFiles(files)) {
    PrintErrorMessages();
    exit(1);
  }

  {
    SymbolTable st;
    Namespace::ModuleNamespace()->BindNames(st);
  }

  // Resolve all work units.
  DriveQueue();

  // Verify existence of Main method.
  Method *me = Method::Main();
  Scope *s = Namespace::ExportNamespace()->NamespaceScope();
  bool exportEmpty = s->DefinitionCount() == 0;
  bool is_module = !me;

  if (is_module && exportEmpty)
    EmitError() << "No main method and nothing to export.";
  else if (!is_module && !exportEmpty)
    EmitError() << "Main method present; no module generated from non-empty "
                   "export namespace.";

  // Print the accumulated error messages, and do not proceed it if any errors
  // occured.
  PrintErrorMessages();
  if (GetSeverestErrorLevelAdded() <= ErrorLevel::error)
     exit(1);

  if (is_module) {
    // Find everything reachable from the contents of the export namespace.
    ComputeClosure(s);
    ExportModule(outfile ? outfile : files[0]);
  } else {
    // Find everything reachable from the Main() method.
    ComputeClosure(me);
  }

  // Bypass link stage if we're not being asked to produce an object file.
  if (strcmp("llvm-obj", target->TargetName())) {
    target->WriteToFile(outfile);
    return 0;
  }

  // Prevent linker messages from mixing with our stuff.
  fflush(stdout);

  // Write compiled code to the file.
#ifdef _WIN32
#  define OBJ ".obj"
#  define EXE ".exe"
#else
#  define OBJ ".o"
#  define EXE ""
#endif
  std::string fn { outfile ? std::string(outfile)
                           : ReplaceSuffix(files[0], EXE) };
  unlink(fn.c_str());

  std::string obj { ReplaceSuffix(fn.c_str(), OBJ) };
  unlink(obj.c_str());
  target->WriteToFile(obj.c_str());

  if (!is_module) {
    vector<std::string> list;
    Namespace::EnumerateImportedModules([&list](Namespace *n) {
      std::string mfn{ n->GetModuleFileName() };
      list.push_back(ReplaceSuffix(mfn.c_str(), OBJ));
    });

#ifdef _WIN32
    std::string out = "/out:" + fn;
    std::string lib = g_moduleSearchPaths[0] + "/runtime.lib";
    vector<const char *> args;
    args.push_back("LINK");
    args.push_back(obj.c_str());
    for (auto &I : list)
      args.push_back(I.c_str());
    args.push_back("/nologo");
    args.push_back(lib.c_str());
    args.push_back(out.c_str());
    args.push_back(nullptr);
    _spawnvp(_P_WAIT, "LINK", args.data());
#else
    // It sucks using the C compiler to link out executables, but getting the
    // right ld options is tricky and very platform-specific.  Someday...
    std::string cmd { g_UnixLinker };
    cmd += ' ' + obj;
    for (auto &I : list)
      cmd += ' ' + I;
    cmd += " -o " + fn;
    system(cmd.c_str());
#endif

    unlink(obj.c_str());
  }

  return 0;
}

/******************************************************************************/

// Until the day arrives that Jolt has a notion of an installation directory,
// this will have to do.  Assume that predefined modules live in the same
// directory as the compiler executable.  Determining that directory requires
// this mess of platform-specific code.

#if defined(__unix__)
#  include <sys/param.h>
#endif

static void ExtractPath(char *path) {
  if (char *p = strrchr(path, '/'))
    *p = 0;

  g_moduleSearchPaths.push_back(path);
}

#if defined(_WIN32)

static void AppendExeDirectory() {
  char *path;

  if (!_get_pgmptr(&path)) {
    if (char *p = strrchr(path, '\\'))
      *p = '/';

    ExtractPath(path);
  }
}

#elif defined(__sun)
#  include <limits.h>

static void AppendExeDirectory() {
  char path[PATH_MAX];

  if (realpath(getexecname(), path))
    ExtractPath(path);

  g_UnixLinker = "cc";
}

#elif defined(__linux)
#  include <limits.h>

static void AppendExeDirectory() {
  char path[PATH_MAX];
  ssize_t len = readlink("/proc/self/exe", path, PATH_MAX);

  if (len > 0 && len < PATH_MAX) {
    path[len] = 0;
    ExtractPath(path);
  }

  g_UnixLinker = "gcc";
}

#elif defined(__APPLE__) && defined(__MACH__)
#  include <mach-o/dyld.h>

static void AppendExeDirectory() {
  char path[PATH_MAX];
  uint32_t len = PATH_MAX;

  if (!_NSGetExecutablePath(path, &len)) {
    if (char *rp = realpath(path, NULL)) {
      ExtractPath(rp);
      free(rp);
    } else {
      ExtractPath(path);
    }
  }

  g_UnixLinker = "cc";
}

#elif defined(BSD)
#  include <sys/types.h>
#  include <sys/sysctl.h>

static void AppendExeDirectory() {
  char path[2048];

  int mib[4] = { CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1 };

  size_t len = sizeof(path);
  if (!sysctl(mib, 4, path, &len, NULL, 0))
    ExtractPath(path);

  g_UnixLinker = "clang";
}

#else

#  error Unsupported OS.

#endif

