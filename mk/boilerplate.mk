STANDALONE_DIST=YES
#-----------------------------------------------------------------------------
#
#
#-----------------------------------------------------------------------------

# Begin by slurping in the boilerplate from one level up.
# Remember, TOP is the top level of the innermost level
# (FPTOOLS_TOP is the fptools top)

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
ifneq "$(STANDALONE_DIST)" "YES"
HDIRECT_TOP := $(TOP)
TOP:=$(TOP)/..

include $(TOP)/mk/boilerplate.mk

# Reset TOP
TOP:=$(HDIRECT_TOP)
endif

# Determine what platform we're on; currently, we only need
# to distinguish between compiling on a Win32 box or not.

# Iff the OS env variable isn't defined, check for windir.
IS_WIN9x=$(patsubst X%X,YES,$(subst XX,NO,X$(windir)X))
FOR_WIN32=$(patsubst X%X,YES,$(subst XX,$(IS_WIN9x),X$(OS)X))

ifeq "$(STANDALONE_DIST)" "YES"
# This rule makes sure that "all" is the default target, regardless of where it appears
#		THIS RULE MUST REMAIN FIRST!
default: all

include $(TOP)/config.mk
endif

WAYS=$(HDirectWays)

ifneq "$(STANDALONE_DIST)" "YES"
GHC_TOP=$(HDIRECT_TOP)/../ghc
include $(GHC_TOP)/mk/paths.mk
endif

include $(TOP)/mk/paths.mk
include $(TOP)/mk/opts.mk
include $(TOP)/mk/suffix.mk
include $(TOP)/mk/version.mk

ifeq "$(STANDALONE_DIST)" "YES"
-include .depend
endif
