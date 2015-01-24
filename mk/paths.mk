#
# H/Direct specific paths and settings.
#
# Currently just controlling the invocation of
# of ihc/hdirect itself.
#

ifndef IHC
IHC = $(TOP)/src/ihc
endif

#
# HDirect documentation uses sgmltools, which
# is no longer used for fptools/, so provide defns
# of the misc tools we're using from that package.
# (see suffix.mk for .sgml -> ?? rules)
#
ifndef SGMLVERB
SGMLVERB = sgmlverb
endif

ifndef SGML2LATEX
SGML2LATEX = sgml2latex
endif

ifndef SGML2HTML
SGML2HTML = sgml2html
endif

ifndef SGML2INFO
SGML2INFO = sgml2info
endif

ifndef SGML2TXT
SGML2TXT = sgml2txt
endif

ifndef CP
CP=cp
endif

IDL_SRCS=$(filter %.idl,$(SRCS) $(BOOT_SRCS))
IDL_OBJS=$(addsuffix .hs,$(basename $(IDL_SRCS)))

ifeq "$(STANDALONE_DIST)" "YES"
SRCS=$(wildcard *.lhs *.hs *.c *.lc *.prl *.lprl *.lit *.verb)

HS_SRCS=$(filter %.lhs %.hs %.hc,$(SRCS) $(BOOT_SRCS))
HS_OBJS=$(addsuffix .$(way_)o,$(basename $(HS_SRCS)))
HS_IFACES=$(addsuffix .$(way_)hi,$(basename $(HS_SRCS)))

C_SRCS=$(filter %.lc %.c,$(SRCS))
C_OBJS=$(addsuffix .$(way_)o,$(basename $(C_SRCS)))

OBJS=$(HS_OBJS) $(C_OBJS) $(SCRIPT_OBJS)

MKDEPENDHS_SRCS=$(HS_SRCS)
MKDEPENDC_SRCS=$(C_SRCS)

MOSTLY_CLEAN_FILES     += $(HS_OBJS) $(C_OBJS)
CLEAN_FILES            += $(HS_PROG) $(C_PROG) $(SCRIPT_PROG) $(PROG) $(LIBRARY) \
			  $(HS_IFACES) \
			  a.out core

endif

#
# The default is to use dllwrap and gcc to compile up
# the Hugs stubs. Should you want to use MSVC++ tools instead,
# you need to set USE_MSVC_TOOLS to ... YES.
#
ifeq "$(USE_MSVC_TOOLS)" "YES"
CCDLL=cl
CCDLL_OPTS=/Zi /LD /MD /I.
#SRC_CC_OPTS += /LD /MD
CCDLL_LIBS=ole32.lib oleaut32.lib uuid.lib advapi32.lib user32.lib winmm.lib urlmon.lib msvcrt.lib
#CC:=cl
STUB_OBJ_SUFFIX=obj
else
ifeq "$(FOR_WIN32)" "YES"
CCDLL=dllwrap 
CCDLL_OPTS += --def HugsMod.def
CCDLL_OPTS += -mno-cygwin --target=i386-mingw32
CCDLL_LIBS += -loleaut32 -lole32 -luser32
CLEAN_FILES += $(wildcard *.dll_o *.dll)
else
#
# Producing a .so file.
#
CCDLL=gcc
CCDLL_OPTS=-fPIC -shared -nostdlib
CLEAN_FILES += $(wildcard *.dll_o *.so)
endif
STUB_OBJ_SUFFIX=dll_o
endif

#
# File extensions for executables and DLLs. The current assumption is that
# anything non-Win32, uses the .so extension. Tweak to fit if that's
# not the case on your plat.
#
ifeq "$(FOR_WIN32)" "YES"
exeext=.exe
dllext=dll
else
dllext=so
endif
