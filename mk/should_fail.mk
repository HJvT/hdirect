#-----------------------------------------------------------------------------
# template for should_fail tests

IDL_SRCS += $(wildcard *.idl)

SRC_RUNTEST_OPTS += -o1 $*.stdout -o2 $*.stderr -x 1

%.hs : %.idl
	@echo ---- Testing for failure to compile $<
	@$(RUNTEST) $(IHC) $(RUNTEST_OPTS) -- $(IHC_OPTS) -c $< -o $@

all :: $(IDL_OBJS)
