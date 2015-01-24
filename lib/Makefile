#
# Makefile for basic HDirect support library.
#
TOP = ..
include $(TOP)/mk/boilerplate.mk

all ::

PACKAGE=hdirect
PACKAGE_DEPS=lang

#
# Should you want to build the library as a DLL, set it to NO.
# (not supported with ghc-5.xx)
#
BUILD_STATICALLY=YES

HC_OPTS += -package-name hdirect
# Since we are using FFI exts we have to compile through C.
HC_OPTS	+= -fglasgow-exts -fvia-C -Wall -fno-warn-deprecations 
HC_OPTS	+= -package lang
ifeq "$(BUILD_STATICALLY)" "YES"
HC_OPTS += -static
endif

cpp_opts = -cpp -DBEGIN_GHC_ONLY='-}' -DEND_GHC_ONLY='{-' -DBEGIN_NOT_FOR_GHC='{-' -DEND_NOT_FOR_GHC='-}' -DELSE_FOR_GHC='-}-}' 

HDirect_HC_OPTS      += $(cpp_opts)
WideString_HC_OPTS   += $(cpp_opts)
Pointer_HC_OPTS      += $(cpp_opts)
SRC_MKDEPENDHS_OPTS  += $(cpp_opts)


WideString_HC_OPTS  += -monly-3-regs
SRC_IHC_OPTS        += -fno-qualified-names 
HUGS_IHC_OPTS        = $(IHC_OPTS) --hugs -odir $(HUGSDIR)

WideString_IHC_OPTS  += -fno-imports -fno-export-lists -fkeep-hresult -fout-pointers-are-not-refs
PointerPrim_IHC_OPTS += -fkeep-hresult -fout-pointers-are-not-refs

SRC_CC_OPTS += -I.

IDL_SRCS = PointerPrim.idl WideString.idl
HUGS_IDL_SRCS += $(IDL_SRCS)
IDL_HS_SRCS = $(patsubst %.idl, %.hs, $(IDL_SRCS))

# Files that are part of both COM and non-COM builds.
C_SRCS  = PointerSrc.c WideStringSrc.c
H_SRCS  = HDirect.h PointerSrc.h WideStringSrc.h
HS_SRCS = Pointer.lhs HDirect.lhs $(IDL_HS_SRCS)

# for adding to SRC_DIST_FILES
HUGS_EXTRA_SRCS =

#
# The Hugs98 version of the library is deposited in a separate directory, so
# that it can coexist with the GHC version.
#
HUGSDIR=hugs

SRCS = $(HS_SRCS) $(C_SRCS)

SRC_DIST_FILES  = $(SRCS) $(H_SRCS) $(HUGS_IDL_SRCS) $(HUGS_EXTRA_SRCS) Makefile hdirect.pkg
CLEAN_FILES += $(patsubst %.idl,%.hs,$(wildcard *.idl))

#
# 'make install' setup:
#
INSTALL_DATAS	= hdirect.pkg $(HS_IFACES)
INSTALL_LIBS    = libHShdirect$(_way).a $(HUGS_FILES)

ifeq "$(BUILD_STATICALLY)" "NO"
INSTALL_PROGS   += HShdirect.dll
INSTALL_LIBS    += libHShdirect_imp.a
endif

ifeq "$(STANDALONE_DIST)" "YES"
all :: libHShdirect$(_way).a HShdirect.$(way_)o
endif

# We put the generated output for Hugs98 in a separate directory, $(HUGSDIR), so that
# GHC and Hugs builds can coexist.
all :: 
	@if [ -d $(HUGSDIR) ]; then : ; else mkdir $(HUGSDIR); fi;

clean :: 	
	if [ -d $(HUGSDIR) ]; then $(RM) -rf $(HUGSDIR)/ ; else : ; fi;

HUGS_FILES = Pointer.lhs HDirect.lhs $(patsubst %.idl, $(HUGSDIR)/%.hs, $(HUGS_IDL_SRCS)) $(patsubst %.idl, $(HUGSDIR)/%.$(dllext), $(HUGS_IDL_SRCS))

all :: $(HUGS_FILES)

all :: dlls

C_STUBS = $(patsubst %.idl, $(HUGSDIR)/%.c, $(IDL_SRCS))

$(HUGSDIR)/%.hs: %.idl
	$(IHC) $(HUGS_IHC_OPTS) -c $< -o $@

$(HUGSDIR)/%.c: $(HUGSDIR)/%.hs
	@:

$(HUGSDIR)/%.$(STUB_OBJ_SUFFIX) : %.c
	@$(RM) $@
	$(CC) $(CC_OPTS) -c $< -o $@

$(HUGSDIR)/%.$(STUB_OBJ_SUFFIX) : $(HUGSDIR)/%.c
	@$(RM) $@
	$(CC) $(CC_OPTS) -c $< -o $@

ifeq "$(USE_MSVC_TOOLS)" "YES"
$(HUGSDIR)/%.obj : %.c
	@$(RM) $@
	$(CCDLL) $(CCDLL_OPTS) -c $< -Fo$@
$(HUGSDIR)/%.obj : $(HUGSDIR)/%.c
	@$(RM) $@
	$(CCDLL) $(CCDLL_OPTS) -c $< -Fo$@
endif

.PRECIOUS: $(HUGSDIR)/%.$(STUB_OBJ_SUFFIX) $(HUGSDIR)/%.c $(HUGSDIR)/%.hs

HugsMod.def :
	@echo EXPORTS     > $@
	@echo initModule >> $@

$(HUGSDIR)/%.$(dllext) : $(HUGSDIR)/%.$(STUB_OBJ_SUFFIX)
	$(CCDLL) $(CCDLL_OPTS) -o $@ $^ $(CCDLL_LIBS)

dlls :: $(patsubst %.c, %.$(dllext), $(C_STUBS))

all :: $(HUGSDIR)/HDirect.lhs $(HUGSDIR)/Pointer.lhs

$(HUGSDIR)/HDirect.lhs : HDirect.lhs
	$(CP) $^ $@
$(HUGSDIR)/Pointer.lhs : Pointer.lhs
	$(CP) $^ $@

# Configuration of hugslibdir is for local testing only; remove before releasing.
ifeq "$(hugslibdir)" ""
hugslibdir=c:/src/hugs98/lib
endif

install-hugs ::
	mkdir -p $(hugslibdir)/hdirect
	cp $(HUGSDIR)/*.$(dllext) $(hugslibdir)/hdirect
	cp $(HUGSDIR)/*.hs $(hugslibdir)/hdirect
	cp *.lhs $(hugslibdir)/hdirect

# End of Hugs specific bit

ifeq "$(STANDALONE_DIST)" "YES"
libHShdirect$(_way).a : $(OBJS)
	$(AR) $(AR_OPTS) $@ $^

HShdirect.$(way_)o: $(OBJS)
	$(LD) -r $(LD_X) -o $@ $(OBJS)
endif

ifeq "$(BUILD_STATICALLY)" "NO"
dll ::
	$(HC) --mk-dll -o HShdirect.dll libHShdirect.a -fglasgow-exts -package lang -lwinmm -luser32
endif

boot :: $(IDL_HS_SRCS)

include $(TOP)/mk/target.mk

ifneq "$(BUILD_STATICALLY)" "YES"
all :: dll
endif

ifeq "$(FOR_WIN32)" "YES"
HDIRECT_LIBDIR=$(shell cygpath -p -w `pwd` | sed -e 's%\\%/%g')
else
HDIRECT_LIBDIR=$(shell pwd)
endif

ifeq "$(GHC_PKG)" ""
GHC_PKG=ghc-pkg
endif

install-pkg: hdirect.pkg
	@hd_libdir="$(HDIRECT_LIBDIR)" $(GHC_PKG) -u < hdirect.pkg || (echo "Unable to install 'hdirect' package for GHC - perhaps GHC isn't installed."; echo "Try running ghc-pkg utility \"ghc-pkg -u < $(datadir)/hdirect.pkg\""; echo "when GHC is installed.")

# extra dependencies for Hugs bits.
$(HUGSDIR)/PointerPrim.c : $(HUGSDIR)/PointerPrim.hs
$(HUGSDIR)/PointerPrim.$(STUB_OBJ_SUFFIX) : $(HUGSDIR)/PointerPrim.c

$(HUGSDIR)/WideString.c : $(HUGSDIR)/WideString.hs
$(HUGSDIR)/WideString.$(STUB_OBJ_SUFFIX) : $(HUGSDIR)/WideString.c

$(HUGSDIR)/WideString.$(dllext) : $(HUGSDIR)/WideString.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/WideString.$(dllext) : $(HUGSDIR)/WideStringSrc.$(STUB_OBJ_SUFFIX)

$(HUGSDIR)/PointerPrim.$(dllext) : $(HUGSDIR)/PointerPrim.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/PointerPrim.$(dllext) : $(HUGSDIR)/PointerSrc.$(STUB_OBJ_SUFFIX)

ifeq "$(FOR_WIN32)" "YES"
$(HUGSDIR)/WideString.$(dllext) : HugsMod.def
$(HUGSDIR)/PointerPrim.$(dllext) : HugsMod.def
endif
