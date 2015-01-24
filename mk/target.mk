#-----------------------------------------------------------------------------
#
# target.mk project stub
#
#-----------------------------------------------------------------------------

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:

ifneq "$(STANDALONE_DIST)" "YES"
HDIRECT_TOP := $(TOP)
TOP:=$(TOP)/..

ifneq "$(HDirectHC)" ""
HC=$(HDirectHC)
MKDEPENDHS=$(HDirectHC)
endif

include $(TOP)/mk/target.mk

# Reset TOP
TOP:=$(HDIRECT_TOP)
endif

ifeq "$(STANDALONE_DIST)" "YES"
.PHONY: all

ifneq "$(HS_PROG)" ""
all :: $(HS_PROG)

$(HS_PROG) :: $(HS_OBJS)
	$(HC) -o $@ $(HC_OPTS) $(LD_OPTS) $(HS_OBJS) $(LIBS)
endif

ifneq "$(C_PROG)" ""
all :: $(C_PROG)

$(C_PROG) :: $(C_OBJS)
	$(CC) -o $@ $(CC_OPTS) $(LD_OPTS) $(C_OBJS) $(LIBS)
endif

ifneq "$(LIBRARY)" ""

all :: $(LIBRARY)

$(LIBRARY) :: $(LIBOBJS)
	$(RM) $@
	$(AR) $(AR_OPTS) $@ $(LIBOBJS)
endif

ifneq "$(MOSTLY_CLEAN_FILES)" ""
mostlyclean::
	rm -f $(MOSTLY_CLEAN_FILES)
endif

ifneq "$(CLEAN_FILES)" ""
clean:: mostlyclean
	rm -f $(CLEAN_FILES)
endif

.PHONY: depend

# Compiler produced files that are targets of the source's imports.
MKDEPENDHS_OBJ_SUFFICES=o

depend :: $(MKDEPENDHS_SRCS) $(MKDEPENDC_SRCS)
	@$(RM) .depend
	@touch .depend
ifneq "$(DOC_SRCS)" ""
	$(MKDEPENDLIT) -o .depend $(MKDEPENDLIT_OPTS) $(filter %.lit,$(DOC_SRCS))
endif
ifneq "$(MKDEPENDHS_SRCS)" ""
	$(MKDEPENDHS) -M -optdep-f -optdep.depend $(foreach way,$(WAYS),-optdep-s -optdep$(way)) $(foreach obj,$(MKDEPENDHS_OBJ_SUFFICES),-optdep-o -optdep$(obj)) $(MKDEPENDHS_OPTS) $(patsubst -odir,,$(HC_OPTS)) $(MKDEPENDHS_SRCS)
endif

# the above patsubst is a hack to remove the '-odir $*' from HC_OPTS
# which is present when we're splitting objects.  The $* maps to
# nothing, since this isn't a pattern rule, so we have to get rid of
# the -odir too to avoid problems.

.PHONY: boot
boot :: depend

ifneq "$(SUBDIRS)" ""

all docs runtests boot TAGS clean veryclean maintainer-clean install info ::
	@echo "------------------------------------------------------------------------"
	@echo "===fptools== Recursively making \`$@' in $(SUBDIRS) ..."
	@echo "PWD = $(shell pwd)"
	@echo "------------------------------------------------------------------------"
# Don't rely on -e working, instead we check exit return codes from sub-makes.
	@case '${MFLAGS}' in *-[ik]*) x_on_err=0;; *-r*[ik]*) x_on_err=0;; *) x_on_err=1;; esac; \
	for i in $(SUBDIRS); do \
	  echo "------------------------------------------------------------------------"; \
	  echo "==fptools== $(MAKE) $@ $(MFLAGS);"; \
	  echo " in $(shell pwd)/$$i"; \
	  echo "------------------------------------------------------------------------"; \
	  $(MAKE) --no-print-directory -C $$i $(MFLAGS) $@; \
	  if [ $$? -eq 0 -o $$x_on_err -eq 0 ] ;  then true; else exit 1; fi; \
	done
	@echo "------------------------------------------------------------------------"
	@echo "===fptools== Finished making \`$@' in $(SUBDIRS) ..."
	@echo "PWD = $(shell pwd)"
	@echo "------------------------------------------------------------------------"

endif

###########################################
#
#	Targets: install install-strip uninstall
#
###########################################

# For each of these variables that is defined, you
# get one install rule
#
#	INSTALL_PROGS 	     executable programs in $(bindir)
#	INSTALL_SCRIPTS	     executable scripts in $(bindir)
#	INSTALL_LIBS	     platform-dependent libraries in $(libdir) (ranlib'ed)
#	INSTALL_LIB_SCRIPTS  platform-dependent scripts   in $(libdir)
#	INSTALL_LIBEXECS     platform-dependent execs in $(libdir)
#	INSTALL_DATAS	     platform-independent files in $(datadir)
#
# If the installation directory variable is undefined, the install rule simply
# emits a suitable error message.
#
# Remember, too, that the installation directory variables ($(bindir) and
# friends can be overridden from their original settings in mk/config.mk.in
# || mk/build.mk
#
.PHONY: install installdirs install-strip install-dirs uninstall install-docs show-install

show-install :
	@echo "bindir = $(bindir)"
	@echo "libdir = $(libdir)"
	@echo "libexecdir = $(libexecdir)  # by default, same as libdir"
	@echo "datadir = $(datadir)  # unused for ghc project"

INSTALL_DIR=$(TOP)/mkdirhier

INSTALL=$(TOP)/install-sh
INSTALL_PROGRAM = $(INSTALL) -m 755
INSTALL_SCRIPT  = $(INSTALL) -m 755
INSTALL_DATA    = $(INSTALL) -m 644
RANLIB=:

#
# Sometimes useful to separate out the creation of install directories 
# from the installation itself.
#
install-dirs ::
	@$(INSTALL_DIR) $(bindir)
	@$(INSTALL_DIR) $(libdir)
	@$(INSTALL_DIR) $(libexecdir)
	@$(INSTALL_DIR) $(datadir)

# Better do this first...
# but we won't for the moment, do it on-demand from
# within the various install targets instead.
#install:: install-dirs

ifneq "$(INSTALL_PROGS)" ""

#
# Here's an interesting one - when using the win32 version
# of install (provided via the cygwin toolkit), we have to
# supply the .exe suffix, *if* there's no other suffix.
#
# The rule below does this by ferreting out the suffix of each
# entry in the INSTALL_PROGS list. If there's no suffix, use
# $(exeext).
# 
# This is bit of a pain to express since GNU make doesn't have
# something like $(if ...), but possible using $(subst ..)
# [Aside: I added support for $(if ..) to my local copy of GNU
# make at one stage, perhaps I should propagate the patch to
# the GNU make maintainers..] 
#
INSTALL_PROGS := $(foreach p, $(INSTALL_PROGS), $(addsuffix $(subst _,,$(subst __,$(exeext),_$(suffix $(p))_)), $(basename $(p))))

install:: $(INSTALL_PROGS)
	@$(INSTALL_DIR) $(bindir)
	@for i in $(INSTALL_PROGS); do \
		    echo $(INSTALL_PROGRAM) $(INSTALL_BIN_OPTS) $$i $(bindir); \
		    $(INSTALL_PROGRAM) $(INSTALL_BIN_OPTS) $$i $(bindir) ;  \
	done
endif

#
# Just like INSTALL_PROGS, but prefix with install sites bin/lib/data and
# install without stripping.
#
ifneq "$(INSTALL_SCRIPTS)" ""
install:: $(INSTALL_SCRIPTS)
	@$(INSTALL_DIR) $(bindir)
	for i in $(INSTALL_SCRIPTS); do \
		$(INSTALL_SCRIPT) $(INSTALL_OPTS) $$i $(bindir); \
	done
endif

ifneq "$(INSTALL_LIB_SCRIPTS)" ""
install:: $(INSTALL_LIB_SCRIPTS)
	@$(INSTALL_DIR) $(libdir)
	for i in $(INSTALL_LIB_SCRIPTS); do \
		$(INSTALL_SCRIPT) $(INSTALL_OPTS) $$i $(libdir); \
	done
endif

ifneq "$(INSTALL_LIBEXEC_SCRIPTS)" ""
install:: $(INSTALL_LIBEXEC_SCRIPTS)
	@$(INSTALL_DIR) $(libexecdir)
	for i in $(INSTALL_LIBEXEC_SCRIPTS); do \
		$(INSTALL_SCRIPT) $(INSTALL_OPTS) $$i $(libexecdir); \
	done
endif

ifneq "$(INSTALL_LIBS)" ""
install:: $(INSTALL_LIBS)
	@$(INSTALL_DIR) $(libdir)
	for i in $(INSTALL_LIBS); do \
		case $$i in \
		  *.a) \
		    $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(libdir); \
		    $(RANLIB) $(libdir)/`basename $$i` ;; \
		  *.dll) \
		    $(INSTALL_DATA) -s $(INSTALL_OPTS) $$i $(libdir) ;; \
		  *) \
		    $(INSTALL_DATA) $(INSTALL_OPTS) $$i $(libdir); \
		esac; \
	done
endif

ifneq "$(INSTALL_LIBEXECS)" ""
#
# See above comment next to defn of INSTALL_PROGS for what
# the purpose of this one-liner is.
# 
INSTALL_LIBEXECS := $(foreach p, $(INSTALL_LIBEXECS), $(addsuffix $(subst _,,$(subst __,$(exeext),_$(suffix $(p))_)), $(basename $(p))))

install:: $(INSTALL_LIBEXECS)
	@$(INSTALL_DIR) $(libexecdir)
	-for i in $(INSTALL_LIBEXECS); do \
		$(INSTALL_PROGRAM) $(INSTALL_BIN_OPTS) $$i $(libexecdir); \
	done
endif

ifneq "$(INSTALL_DATAS)" ""
install:: $(INSTALL_DATAS)
	@$(INSTALL_DIR) $(datadir)
	for i in $(INSTALL_DATAS); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(datadir); \
	done
endif

ifneq "$(INSTALL_INCLUDES)" ""
install:: $(INSTALL_INCLUDES)
	@$(INSTALL_DIR) $(includedir)
	for i in $(INSTALL_INCLUDES); do \
		$(INSTALL_DATA) $(INSTALL_OPTS) $$i $(includedir); \
	done
endif

#
# Use with care..
#
uninstall:: 
	@for i in $(INSTALL_PROGS) "" ; do			\
	  if test "$$i"; then 					\
		echo rm -f $(bindir)/`basename $$i`;		\
		rm -f $(bindir)/`basename $$i`;			\
	  fi; 							\
	done
	@for i in $(INSTALL_LIBS) ""; do			\
	  if test "$$i"; then 					\
		echo rm -f $(libdir)/`basename $$i`;		\
		rm -f $(libdir)/`basename $$i`;			\
	  fi;							\
	done
	@for i in $(INSTALL_LIBEXECS) ""; do			\
	  if test "$$i"; then 					\
		echo rm -f $(libexecdir)/`basename $$i`;	\
		rm -f $(libexecdir)/`basename $$i`;		\
	  fi;							\
	done
	@for i in $(INSTALL_DATAS) ""; do			\
	  if test "$$i"; then 					\
		echo rm -f $(datadir)/`basename $$i`;		\
		rm -f $(datadir)/`basename $$i`;		\
	  fi;							\
	done

#
# install-strip is from the GNU Makefile standard.
#
ifneq "$(way)" ""
install-strip::
	@$(MAKE) EXTRA_INSTALL_OPTS='-s' install                                	
endif

#
# install links to script drivers.
#
ifneq "$(SCRIPT_LINK)" ""
install ::
	@if ( $(PERL) -e '$$fn="$(bindir)/$(SCRIPT_LINK)"; exit ((! -f $$fn || -l $$fn) ? 0 : 1);' ); then \
	   echo "Creating a symbol link from $(SCRIPT_PROG) to $(SCRIPT_LINK) in $(bindir)"; \
	   $(RM) $(bindir)/$(SCRIPT_LINK); \
	   $(LN_S) $(SCRIPT_PROG) $(bindir)/$(SCRIPT_LINK); \
	 else \
	   echo "Creating a symbol link from $(SCRIPT_PROG) to $(SCRIPT_LINK) in $(bindir) failed: \`$(bindir)/$(SCRIPT_LINK)' already exists"; \
	   echo "Perhaps remove \`$(bindir)/$(SCRIPT_LINK)' manually?"; \
	   exit 1; \
	 fi;

endif


endif

# -----------------------------------------------------------------------------
# Building source distributions
#
# Do it like this: 
#
#	$ make
#	$ make dist Project=Ghc
#
# WARNING: `make dist' calls `make distclean' before tarring up the tree.
#

.PHONY: dist

SRC_DIST_DIR=$(shell pwd)/$(SRC_DIST_NAME)

ifneq "$(SUBDIRS)" ""
dist ::
# Don't rely on -e working, instead we check exit return codes from sub-makes.
	echo $(SUBDIRS)
	@case '${MFLAGS}' in *-[ik]*) x_on_err=0;; *-r*[ik]*) x_on_err=0;; *) x_on_err=1;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MKDIRHIER) $(SRC_DIST_DIR)/$$i; \
	  $(MAKE) -C $$i $(MFLAGS) $@ SRC_DIST_DIR=$(SRC_DIST_DIR)/$$i; \
	  if [ $$? -eq 0 ] ;  then true; else exit $$x_on_err; fi; \
	done
endif

# The default dist rule:
#
# copy/link the contents of $(SRC_DIST_FILES) into the
# shadow distribution tree. SRC_DIST_FILES contain the
# build-generated files that you want to include in
# a source distribution.
#
#
ifneq "$(SRC_DIST_FILES)" ""
dist::
	@for i in $(SRC_DIST_FILES); do 		 \
	  if ( echo "$$i" | grep "~" >/dev/null 2>&1 ); then	 \
	    echo $(LN_S) `pwd`/`echo $$i | sed -e "s/^\([^~]*\)~.*/\1/g"` $(SRC_DIST_DIR)/`echo $$i | sed -e "s/.*~\(.*\)/\1/g"` ; \
	    $(LN_S) `pwd`/`echo $$i | sed -e "s/^\([^~]*\)~.*/\1/g"` $(SRC_DIST_DIR)/`echo $$i | sed -e "s/.*~\(.*\)/\1/g"` ; \
	  else \
	    if (test -f "$$i"); then 			   \
	      echo $(LN_S) `pwd`/$$i $(SRC_DIST_DIR)/$$i ; \
	      $(LN_S) `pwd`/$$i $(SRC_DIST_DIR)/$$i ;	   \
	     fi;					   \
	  fi; \
	done;
endif

dist-package :: dist-package-tar-gz

dist-package-tar-gz ::
	$(TAR) chzf $(SRC_DIST_NAME)-src.tar.gz $(SRC_DIST_NAME)

dist-package-zip ::
	cd ..; $(ZIP) $(ZIP_OPTS) -r $(SRC_DIST_NAME)-src.zip $(SRC_DIST_DIR)
