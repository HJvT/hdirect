# Toplevel Makefile for HaskellDirect
TOP=.
include $(TOP)/mk/boilerplate.mk

all ::

SUBDIRS = src

lib ::
	$(MAKE) -C lib boot && $(MAKE) -C lib all
ifeq "$(FOR_WIN32)" "YES"
	$(MAKE) -C comlib boot && $(MAKE) -C comlib all
endif

include $(TOP)/mk/target.mk
