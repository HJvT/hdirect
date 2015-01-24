#-----------------------------------------------------------------------------
# template for should_compile tests.

IDL_SRCS += $(wildcard *.idl)

SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 0

# Override default .idl -> .hs pattern rule.
%.hs : %.idl
	@echo ---- Testing for successful compilation of $<
	$(RUNTEST) $(IHC) $(RUNTEST_OPTS) -- $(IHC_OPTS) -c $< -o $@

all :: $(IDL_OBJS)

