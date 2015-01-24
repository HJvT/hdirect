# -----------------------------------------------------------------------------
#
TOP = ..
include $(TOP)/mk/boilerplate.mk

# Note: might be overridden from cmd-line (see install rule below)
INSTALLING=0

#
# Win32-specific - set this variable to YES if you want to include COM
# type-library reading/writing code
# (that code depends on the 'com' package, so if you're bootstrapping,
# you can't enable SUPPORT_TYPELIBS first-time around).
#SUPPORT_TYPELIBS=YES

#
# Win32 specific - set this variable to YES if you want the IDL compiler
# to consult HKLM\Software\Haskell\HaskellDirect\\<version>\\Options for
# the default set of options to use. Rarely enabled.
#USE_REGISTRY=YES

all ::

SRC_HC_OPTS  += -fglasgow-exts -static -fvia-C -Rghc-timing -Wall -recomp
SRC_HC_OPTS  += -package lang


# The end-product.
HS_PROG   = ihc$(exeext)

BOOT_SRCS = Parser.hs Version.hs OmgParser.hs NativeInfo.hs
REAL_SRCS = $(wildcard *.lhs) CustomAttributes.hs
HS_SRCS   = $(REAL_SRCS) $(BOOT_SRCS)
HS_OBJS   = $(addsuffix .o,$(basename $(HS_SRCS))) ErrorHook.o

SRC_DIST_FILES += $(REAL_SRCS) $(BOOT_SRCS)
SRC_DIST_FILES += Makefile .depend mkNativeInfo.c Parser.y OmgParser.y ErrorHook.c


#
# Compiling Happy-generated parsers with -Wall is a little bit too noisy..
#
silent_opts+=-fno-warn-incomplete-patterns  -fno-warn-missing-signatures
silent_opts+=-fno-warn-unused-binds -fno-warn-name-shadowing
silent_opts+=-fno-warn-unused-matches

cond_opts+= -cpp -DBEGIN_GHC_ONLY='-}' -DEND_GHC_ONLY='{-'
cond_opts+= -DBEGIN_NOT_FOR_GHC='{-' -DEND_NOT_FOR_GHC='-}' -DELSE_FOR_GHC='-}-}' 


Parser_HC_OPTS      = -Onot $(cond_opts) $(silent_opts)
OmgParser_HC_OPTS   = -Onot $(cond_opts) $(silent_opts)

PreProc_HC_OPTS     += $(cond_opts)
Utils_HC_OPTS       += $(cond_opts)
BasicTypes_HC_OPTS  += $(cond_opts)
CoreUtils_HC_OPTS   += $(cond_opts)
CoreIDL_HC_OPTS     += $(cond_opts)

Parser_HAPPY_OPTS   += -g +RTS -H6m -RTS
SRC_HSTAGS_OPTS     += -fglasgow-exts


ifeq "$(SUPPORT_TYPELIBS)" "YES"
Typelib_opts+= -cpp -DBEGIN_SUPPORT_TYPELIBS='-}' -DEND_SUPPORT_TYPELIBS='{-'
Typelib_opts+= -DBEGIN_NOT_SUPPORT_TYPELIBS='{-' -DEND_NOT_SUPPORT_TYPELIBS='-}'

ImportLib_HC_OPTS    += $(Typelib_opts)
Main_HC_OPTS         += $(Typelib_opts)
TLBWriter_HC_OPTS    += $(Typelib_opts)
Desugar_HC_OPTS      += $(Typelib_opts)
TypeInfo_HC_OPTS     += $(Typelib_opts)
MarshallAuto_HC_OPTS += $(Typelib_opts)
Opts_HC_OPTS         += $(Typelib_opts)

SRC_MKDEPENDHS_OPTS  += $(Typelib_opts)

# If you don't have the COM support libraries installed as a GHC
# package [see the install-pkg rule in comlib/Makefile if you want
# to add this package, but don't know how to], uncomment the next
# line (and comment out the line following.)
#SRC_HC_OPTS += -i../lib -L../lib -lole32 -loleaut32 -ladvapi32
SRC_HC_OPTS          += -package com

else
ImportLib_HC_OPTS    += -cpp
endif

Main_HC_OPTS         += -cpp 
TLBWriter_HC_OPTS    += -cpp


ifeq "$(USE_REGISTRY)" "YES"
Registry_opts   += -cpp -DBEGIN_USE_REGISTRY='-}' -DEND_USE_REGISTRY='{-'
Registry_opts   += -DBEGIN_NOT_USE_REGISTRY='{-' -DEND_NOT_USE_REGISTRY='-}'
Registry_opts   += -package win32

Main_HC_OPTS    += $(Registry_opts)
Opts_HC_OPTS    += $(Registry_opts)
SRC_LD_OPTS	+= -package win32 -ladvapi32
endif

# Package setup
PKG_NAME    = HaskellDirect ($(HS_PROG))

ifeq "$(MajorRelease)" "YES"
PKG_VERSION = version $(PACKAGE_VERSION)
else
PKG_VERSION = snapshot $(shell date +%d%m%y)
endif

# Don't want mkNativeInfo.o included.
OBJS    := $(HS_OBJS)

# Make sure Version.hs and NativeInfo.hs doesn't get nuked.
.PRECIOUS: %.hs

boot :: mkVersion
boot :: $(BOOT_SRCS)

dist :: mkVersion

.PHONY: mkVersion

GHCMAKE_OPTS = $(HC_OPTS) -cpp

ghcmake :: $(BOOT_SRCS) Version.hs
	$(HC) --make $(GHCMAKE_OPTS) $(HS_SRCS)

mkVersion :
	$(RM) Version.hs
	echo "--Automatically generated"         > Version.hs
	echo "module Version where"             >> Version.hs
	echo ""				        >> Version.hs
	echo "pkg_name :: String"               >> Version.hs
	echo "pkg_name    = \"$(PKG_NAME)\""    >> Version.hs
	echo "pkg_version :: String"            >> Version.hs
	echo "pkg_version = \"$(PKG_VERSION)\"" >> Version.hs

NativeInfo.hs : mkNativeInfo
	$(RM) $@
	./mkNativeInfo > $@ || ( rm $@ && exit 1 )

C_PROG = mkNativeInfo

ErrorHook.o : ErrorHook.c
	$(HC) -c $< -o $@

clean-tlbs:
	$(RM) Opts.o Main.o ImportLib.o TLBWriter.o TypeInfo.o


#
# Install setup
#
INSTALL_PROGS=$(HS_PROG)

install :: install-dirs

include $(TOP)/mk/target.mk
Parser.hs : Parser.y
OmgParser.hs : OmgParser.y
