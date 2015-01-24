#
# Makefile for HDirect Com library.
#
TOP = ..
FOR_WIN32=YES
include $(TOP)/mk/boilerplate.mk

#
# Should you want to build the library as a DLL, set it to NO.
# (not supported with ghc-5.xx)
#
BUILD_STATICALLY=YES

#
# The Hugs98 version of the library is deposited in a separate directory, so
# that it can coexist with the GHC version.
#
HUGSDIR=hugs

#
# The com library is a superset of the HDirect base library, but compiled differently
# (i.e., it consistently uses the COM task allocator).
# 
HDLIBDIR =../lib

HC_OPTS += -package-name com
HC_OPTS	+= -package lang -I$(HDLIBDIR) -DCOM
# Since we are using FFI exts we have to compile through C.
HC_OPTS += -fglasgow-exts -fvia-C
ifeq "$(BUILD_STATICALLY)" "YES"
HC_OPTS += -static
endif

SRC_CC_OPTS += -I. -I$(HDLIBDIR) -DCOM

all ::

cpp_opts = -cpp -DBEGIN_GHC_ONLY='-}' -DEND_GHC_ONLY='{-' -DBEGIN_NOT_FOR_GHC='{-' -DEND_NOT_FOR_GHC='-}' -DELSE_FOR_GHC='-}-}' 

HDirect_HC_OPTS += $(cpp_opts)
Pointer_HC_OPTS += $(cpp_opts)
Com_HC_OPTS          += $(cpp_opts)
ComException_HC_OPTS += $(cpp_opts)
ComPrim_HC_OPTS      += $(cpp_opts)
Automation_HC_OPTS   += $(cpp_opts) -H12m -fno-warn-missing-methods -fno-warn-deprecations
AutoPrim_HC_OPTS     += $(cpp_opts) -monly-3-regs
WideString_HC_OPTS   += $(cpp_opts)
ComServ_HC_OPTS      += $(cpp_opts)
ComDll_HC_OPTS       += $(cpp_opts)
SRC_MKDEPENDHS_OPTS  += $(cpp_opts)

#
# 5.04.2 is barfing when these are compiled with -O
#
EnumInterface_HC_OPTS += -Onot
ClassFactory_HC_OPTS += -Onot

SRC_IHC_OPTS         += -fno-qualified-names 
HUGS_IHC_OPTS        = $(IHC_OPTS) --hugs -odir $(HUGSDIR)

AutoPrim_IHC_OPTS    += -fno-imports -fno-export-lists -fhs-to-c  -fno-overload-variant -fout-pointers-are-not-refs -fsubtyped-interface-pointers
ComPrim_IHC_OPTS     += -fno-imports -fno-export-lists -fout-pointers-are-not-refs
WideString_IHC_OPTS  += -fno-imports -fno-export-lists -fkeep-hresult -fout-pointers-are-not-refs
PointerPrim_IHC_OPTS += -fkeep-hresult -fout-pointers-are-not-refs
StdTypes_IHC_OPTS    += -fno-export-list -fno-gen-variant-instances -fout-pointers-are-not-refs --gen-headers
SafeArray_IHC_OPTS   += -fhs-to-c -fno-export-lists --gen-headers -fout-pointers-are-not-refs --gen-headers
TypeLib_IHC_OPTS     += -fno-export-list -fappend-interface-short-name -fno-overload-variant --gen-headers
Connection_IHC_OPTS  += -fappend-interface-short-name

SafeArray_HC_OPTS    += -fvia-C '-\#include "SafeArray.h"'
TypeLib_HC_OPTS      += -fvia-C
Connection_HC_OPTS   += -fvia-C
ComServ_HC_OPTS      += -fvia-C

# Files that are part of both COM and non-COM builds.
C_SRCS  = PointerSrc.c WideStringSrc.c
H_SRCS  = $(HDLIBDIR)/HDirect.h $(HDLIBDIR)/PointerSrc.h $(HDLIBDIR)/WideStringSrc.h
HS_SRCS = Pointer.lhs HDirect.lhs PointerPrim.hs WideString.hs

COMLIB_C_SRCS    = ComPrimSrc.c AutoPrimSrc.c Registry.c SafeArrayPrim.c
COMLIB_HS_SRCS   = Com.lhs Automation.lhs AutoPrim.hs ComPrim.hs StdTypes.hs
COMLIB_HS_SRCS  += SafeArray.hs ComException.lhs
COMLIB_HS_SRCS  += TypeLib.hs Connection.hs
H_SRCS		+= comPrim.h autoPrim.h Registry.h safeArrayPrim.h SafeArray.h StdTypes.h

C_SRCS   += $(COMLIB_C_SRCS)
HS_SRCS  += $(COMLIB_HS_SRCS)

C_OBJS  := $(subst PointerSrc.$(way_)o,PointerSrcCom.$(way_)o,$(C_OBJS))

# for adding to SRC_DIST_FILES
HUGS_EXTRA_SRCS =

IDL_SRCS = $(wildcard *.idl)
HUGS_IDL_SRCS = $(IDL_SRCS)


#
# The COM server specific modules are compilable under both
# mingw32 and cygwin32, but there's a couple of niggling interop
# issues with using cygwin32 DLLs from MSVC compiled code, so
# it is recommended that you use mingw32 when you want to create
# COM in-proc servers.
#
#
COMLIB_EXTRA_HS_SRCS  += ComDll.lhs ComServ.lhs ClassFactory.lhs
COMLIB_EXTRA_HS_SRCS  += StdDispatch.lhs ConnectionPoint.lhs
COMLIB_EXTRA_HS_SRCS  += EnumInterface.lhs ExeServer.lhs

HS_SRCS     += $(COMLIB_EXTRA_HS_SRCS)

ifneq "$(USE_MSVC_TOOLS)" "YES"
SRC_CC_OPTS += -mno-cygwin -I.
endif

SRCS = $(HS_SRCS) $(C_SRCS)

SRC_DIST_FILES  = $(SRCS) $(H_SRCS) $(IDL_SRCS) $(HUGS_EXTRA_SRCS) Makefile com.pkg
CLEAN_FILES += $(patsubst %.idl,%.hs,$(wildcard *.idl)) $(COPIED_OVER)

#
# 'make install' setup:
#
INSTALL_DATAS	= com.pkg $(HS_IFACES) $(SRCS)
INSTALL_LIBS   += libHScom$(_way).a
ifneq "$(BUILD_STATICALLY)" "YES"
INSTALL_PROGS  += HScom.dll
INSTALL_LIBS   += libHScom_imp.a
endif

#OBJS += $(patsubst %.idl, %.$(way_)o, $(IDL_SRCS))

LIBCOM_OBJS += ComServ_stub.$(way_)o ClassFactory_stub.$(way_)o ComDll_stub.$(way_)o EnumInterface_stub.$(way_)o ConnectionPoint_stub.$(way_)o
LIBCOM_OBJS += $(filter-out PointerSrc.$(way_)o, $(OBJS))

all :: libHScom$(_way).a HScom.$(way_)o

# We put the generated output for Hugs98 in a separate directory, $(HUGSDIR), so that
# GHC and Hugs builds can coexist.
all :: 
	@if [ -d $(HUGSDIR) ]; then : ; else mkdir $(HUGSDIR); fi;

clean :: 	
	if [ -d $(HUGSDIR) ]; then $(RM) -rf $(HUGSDIR)/ ; else : ; fi;

all :: $(patsubst %.idl, $(HUGSDIR)/%.hs, $(IDL_SRCS))

all :: dlls

$(HUGSDIR)/PointerPrim.c : $(HUGSDIR)/PointerPrim.hs
$(HUGSDIR)/PointerPrim.$(STUB_OBJ_SUFFIX) : $(HUGSDIR)/PointerPrim.c

$(HUGSDIR)/WideString.c : $(HUGSDIR)/WideString.hs
$(HUGSDIR)/WideString.$(STUB_OBJ_SUFFIX) : $(HUGSDIR)/WideString.c

$(HUGSDIR)/WideString.$(dllext) : $(HUGSDIR)/WideString.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/WideString.$(dllext) : $(HUGSDIR)/WideStringSrc.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/WideString.$(dllext) : HugsMod.def

$(HUGSDIR)/PointerPrim.$(dllext) : $(HUGSDIR)/PointerPrim.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/PointerPrim.$(dllext) : $(HUGSDIR)/PointerSrcCom.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/PointerPrim.$(dllext) : HugsMod.def

$(HUGSDIR)/AutoPrim.$(dllext) : $(HUGSDIR)/AutoPrimSrc.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/AutoPrim.$(dllext) : $(HUGSDIR)/AutoPrim.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/AutoPrim.$(dllext) : $(HUGSDIR)/PointerSrcCom.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/AutoPrim.$(dllext) : $(HUGSDIR)/ComPrimSrc.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/AutoPrim.$(dllext) : HugsMod.def

$(HUGSDIR)/ComPrim.$(dllext) : HugsMod.def
$(HUGSDIR)/ComPrim.$(dllext) : $(HUGSDIR)/ComPrim.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/ComPrim.$(dllext) : $(HUGSDIR)/ComPrimSrc.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/ComPrim.$(dllext) : $(HUGSDIR)/PointerSrcCom.$(STUB_OBJ_SUFFIX)

$(HUGSDIR)/SafeArray.$(dllext) : $(HUGSDIR)/SafeArray.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/SafeArray.$(dllext) : $(HUGSDIR)/SafeArrayPrim.$(STUB_OBJ_SUFFIX)
$(HUGSDIR)/SafeArray.$(dllext) : HugsMod.def

# StdTypes.idl doesn't generate a .c file, so filter it out.
C_STUBS = $(patsubst %.idl, $(HUGSDIR)/%.c, $(filter-out StdTypes.idl, $(IDL_SRCS)))

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
$(HUGSDIR)/PointerSrcCom.$(STUB_OBJ_SUFFIX) : PointerSrc.c
	@$(RM) $@
	$(CCDLL) $(CCDLL_OPTS) -c -DCOM $< -Fo$@
$(HUGSDIR)/%.obj : %.c
	@$(RM) $@
	$(CCDLL) $(CCDLL_OPTS) -c -DCOM $< -Fo$@
$(HUGSDIR)/%.obj : $(HUGSDIR)/%.c
	@$(RM) $@
	$(CCDLL) $(CCDLL_OPTS) -c -DCOM $< -Fo$@
else
$(HUGSDIR)/PointerSrcCom.$(STUB_OBJ_SUFFIX) : PointerSrc.c
	@$(RM) $@
	$(CC) $(CC_OPTS) -c -DCOM $< -o $@
endif

.PRECIOUS: $(HUGSDIR)/%.$(STUB_OBJ_SUFFIX) $(HUGSDIR)/%.c $(HUGSDIR)/%.hs

HugsMod.def :
	@echo EXPORTS     > $@
	@echo initModule >> $@

COPIED_OVER=HDirect.lhs Pointer.lhs  WideString.idl PointerPrim.idl PointerSrc.c WideStringSrc.c

HDirect.lhs : $(HDLIBDIR)/HDirect.lhs
	$(CP) $< $@ 
Pointer.lhs : $(HDLIBDIR)/Pointer.lhs
	$(CP) $< $@ 
WideString.idl : $(HDLIBDIR)/WideString.idl
	$(CP) $< $@ 
PointerPrim.idl : $(HDLIBDIR)/PointerPrim.idl
	$(CP) $< $@ 
PointerSrc.c : $(HDLIBDIR)/PointerSrc.c
	$(CP) $< $@ 
WideStringSrc.c : $(HDLIBDIR)/WideStringSrc.c
	$(CP) $< $@ 

$(HUGSDIR)/%.$(dllext) : $(HUGSDIR)/%.$(STUB_OBJ_SUFFIX)
	$(CCDLL) $(CCDLL_OPTS) -o $@ $^ $(CCDLL_LIBS)

dlls :: $(patsubst %.c, %.$(dllext), $(C_STUBS))

HUGS_COMLIB_LHSS = HDirect.lhs Pointer.lhs Automation.lhs Com.lhs ComException.lhs

all :: $(patsubst %.lhs,$(HUGSDIR)/%.lhs,$(HUGS_COMLIB_LHSS))

$(HUGSDIR)/HDirect.lhs : HDirect.lhs
	$(CP) $^ $(HUGSDIR)/

$(HUGSDIR)/Pointer.lhs : Pointer.lhs
	$(CP) $^ $(HUGSDIR)/

$(HUGSDIR)/Automation.lhs : Automation.lhs
	$(CP) $^ $(HUGSDIR)/

$(HUGSDIR)/Com.lhs : Com.lhs
	$(CP) $^ $(HUGSDIR)/

$(HUGSDIR)/ComException.lhs : ComException.lhs
	$(CP) $^ $(HUGSDIR)/


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

PointerSrcCom.$(way_)o : PointerSrc.c
	@$(RM) $@
	$(CC) $(CC_OPTS) -c -DCOM $< -o $@

WideStringSrc.$(way_)o : WideStringSrc.c
	@$(RM) $@
	$(CC) $(CC_OPTS) -c -DCOM $< -o $@

ComServ_stub.$(way_)o : ComServ.$(way_)o
	@-

ClassFactory_stub.$(way_)o : ClassFactory.$(way_)o
	@-

ComDll_stub.$(way_)o : ComDll.$(way_)o
	@-

EnumInterface_stub.$(way_)o : EnumInterface.$(way_)o
	@-

ConnectionPoint_stub.$(way_)o : ConnectionPoint.$(way_)o
	@-

libHScom$(_way).a : $(LIBCOM_OBJS)
	$(RM) $@
	$(AR) $(AR_OPTS) $@ $^

HScom.$(way_)o: $(LIBCOM_OBJS)
	$(LD) -r $(LD_X) -o $@ $(LIBCOM_OBJS)

dll ::
	$(HC) --mk-dll -o HScom.dll libHScom.a -fglasgow-exts -syslib lang -lwinmm -lole32 -loleaut32 -luser32 

BOOT_SRCS=$(patsubst %.idl, %.hs, $(IDL_SRCS))
boot :: $(COPIED_OVER) $(BOOT_SRCS)

depend :: $(COPIED_OVER)

include $(TOP)/mk/target.mk

ifneq "$(BUILD_STATICALLY)" "YES"
all :: dll
endif

.PHONY: install-pkg

# ToDo: configure this approp. if you've installed the HDirect
#       libraries first.
ifeq "$(FOR_WIN32)" "YES"
HDIRECT_LIBDIR=$(shell cygpath -p -w `pwd` | sed -e 's%\\%/%g')
else
HDIRECT_LIBDIR=$(shell pwd)
endif

ifeq "$(GHC_PKG)" ""
GHC_PKG=ghc-pkg
endif

install-pkg: com.pkg
	@hd_libdir="$(HDIRECT_LIBDIR)"  $(GHC_PKG) -u < com.pkg || echo "Unable to install 'com' package; try running ghc-pkg utility manually, i.e., \"ghc-pkg -u < com.pkg\""
