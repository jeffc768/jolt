.SUFFIXES:
.SUFFIXES: .cpp .h .o

.PHONY: all clean

CCFLGS := -Wall -W -Wwrite-strings -Wno-unused-parameter -Wno-self-assign-field -Wno-missing-braces -Wno-missing-field-initializers -D__STDC_LIMIT_MACROS -fno-strict-aliasing

ifdef ENABLE_OPTIMIZED
	OBJDIR := Release
	CCFLGS += -DNDEBUG -O3 -felide-constructors -Wno-unused-variable
	# Don't use -fomit-frame-pointer on FreeBSD
	ifneq ($(shell uname),FreeBSD)
		CCFLGS += -fomit-frame-pointer
	endif
	LDFLGS :=
	BIFLGS := -v -d
	FLFLGS :=
else
	OBJDIR := Debug
	CCFLGS += -g -D_DEBUG
	LDFLGS :=
	BIFLGS := -v -d -t
	FLFLGS := 
endif

CCFLGS += $(CXXFLAGS) -std=c++17 -fno-rtti -fno-exceptions
LDFLGS += $(LDFLAGS)
ifeq ($(shell uname),FreeBSD)
  ifndef CXX
	  CXX = clang++
  endif
endif

CCFLGS += -I .
SRCS = $(wildcard *.cpp *.l *.y */*.cpp */*.l */*.y */*/*.cpp */*/*.l */*/*.y)
CCFLGS += -I $(shell $(USE_LLVM) --includedir) -DTARGET_TRIPLE=\"$(shell $(USE_LLVM) --host-target)\"
CCFLGS += $(filter-out -DNDEBUG -D_DEBUG -O2 -O3 -std=c++0x -std=c++11 -std=c++14 -g -fomit-frame-pointer -Wmissing-field-initializers -Wno-class-memaccess -W -Wall -D__STDC_LIMIT_MACROS,$(shell $(USE_LLVM) --cxxflags))
LDFLGS += $(filter-out -lffi -ledit,$(shell $(USE_LLVM) --ldflags --system-libs))
LIBS = $(shell $(USE_LLVM) --libs)
OBJS = $(patsubst %,$(OBJDIR)/%.o,$(sort $(basename $(SRCS))))
OBJDIRS = $(sort $(dir $(OBJS)))
DEPENDS = $(patsubst %.o,%.d,$(OBJS))
YACCFILES = $(filter %.y,$(SRCS))
FLEXFILES = $(filter %.l,$(SRCS))

# Cancel builtin rules.
%.cpp: %.y
%.h: %.y
%.cpp: %.l
%.o: %.cpp

all: $(OBJDIR)/std.o

clean:
	rm -rf $(OBJDIR) $(patsubst %.y,%.h,$(YACCFILES)) $(patsubst %.y,%.cpp,$(YACCFILES)) $(patsubst %.y,%.output,$(YACCFILES)) $(patsubst %.l,%.cpp,$(FLEXFILES))

$(OBJDIRS):
	@mkdir -p $(OBJDIRS)

$(OBJDIR)/%.o: %.cpp
	@echo Compiling $*.cpp for $(OBJDIR) build
	@if $(CXX) -MD -MT $@ -MP -MF $(OBJDIR)/$*.CXXd -c $(CCFLGS) $< -o $@ ; \
	then mv -f $(OBJDIR)/$*.CXXd $(OBJDIR)/$*.d ; \
	else rm -f $(OBJDIR)/$*.CXXd; exit 1; fi

# FIXME: figure out a way to get the generated parser put into OBJDIR.
.PRECIOUS: $(patsubst %.y,%.output,$(YACCFILES))
$(YACCFILES:%.y=$(OBJDIR)/%.o): $(OBJDIR)/%.o: %.cpp
%.cpp %.h: %.y
	@echo Bisoning $*.y
	@bison $(BIFLGS) -p $(<F:%Parser.y=%) -o $*.tab.c $< ; \
	mv -f $*.tab.c $*.cpp ; \
	mv -f $*.tab.h $*.h

# FIXME: figure out a way to get the generated scanner put into OBJDIR.
$(FLEXFILES:%.l=$(OBJDIR)/%.o): $(OBJDIR)/%.o: %.cpp
%.cpp: %.l
	@echo Flexing $*.l
	@flex $(FLFLGS) -o $*.cpp.tmp $< ; \
	sed -E -e 's/	register /	/'  $*.cpp.tmp >$*.cpp ; \
	rm $*.cpp.tmp


$(OBJDIR)/jolt: $(OBJS)
	@echo Linking Jolt
	@$(CXX) $(OBJS) $(LIBS) $(LDFLGS) -o $(OBJDIR)/jolt

$(OBJDIR)/std.o: $(OBJS) $(OBJDIR)/jolt ../runtime/modules/std/std.jolt
	$(OBJDIR)/jolt -genstd ../runtime/modules/std/std.jolt -o $(OBJDIR)/std.o
	@rm -f ../build/macos/xcode.products.dir

$(OBJS): | $(OBJDIRS)

$(OBJDIR)/parser/Lexer.o: parser/JoltParser.h
$(OBJDIR)/parser/JoltScanner.o: parser/JoltParser.h
$(OBJDIR)/parser/TokenHelpers.o: parser/JoltParser.h

-include /dev/null $(DEPENDS)
