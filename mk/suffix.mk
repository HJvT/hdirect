#
# .idl -> .hs suffix rule
# 

%.hs : %.idl
	$(IHC) $(IHC_OPTS) -c $< -o $@

%.hs : %.odl
	$(IHC) $(IHC_OPTS) -c $< -o $@

.PRECIOUS: %.hs

%.hs : %.y
	$(HAPPY) $(HAPPY_OPTS) $<

#-----------------------------------------------------------------------------
# SGML suffix rules -- override whatever tools that are used higher up.
#
#
%.sgml : %.vsgml
	@$(RM) $@
	expand $< | $(SGMLVERB) > $@

%.tex : %.sgml
	@$(RM) $@
	$(SGML2LATEX) $(SGML2LATEX_OPTS) -m --output=tex $<

%.dvi : %.sgml
	@$(RM) $@
	$(SGML2LATEX) $(SGML2LATEX_OPTS) -m --output=dvi $<

%.ps : %.sgml
	@$(RM) $@
	$(SGML2LATEX) $(SGML2LATEX_OPTS) -m --output=ps $<

%.html : %.sgml
	@$(RM) $@
	$(SGML2HTML) $(SGML2HTML_OPTS) $<

%.info : %.sgml
	@$(RM) $@
	$(SGML2INFO) $(SGML2INFO_OPTS) $<

%.txt : %.sgml
	@$(RM) $@
	$(SGML2TXT) $(SGML2TXT_OPTS) $<



ifeq "$(STANDALONE_DIST)" "YES"

%.$(way_)o : %.hs
	$(HC_PRE_OPTS)
	$(HC) $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@))
	$(HC_POST_OPTS)
			 
%.$(way_)o : %.lhs	 
	$(HC_PRE_OPTS)
	$(HC) $(HC_OPTS) -c $< -o $@ -osuf $(subst .,,$(suffix $@))
	$(HC_POST_OPTS)

%.$(way_)hi : %.$(way_)o
	@:

.PRECIOUS: %.hs

%.hs : %.ly
	$(HAPPY) $(HAPPY_OPTS) $<

%.hs : %.gc
	$(GREENCARD) $(GREENCARD_OPTS) $< -o $@

%.lhs : %.gc
	$(GREENCARD) $(GREENCARD_OPTS) $< -o $@

%.$(way_)o : %.c
	@$(RM) $@
	$(CC) $(CC_OPTS) -c $< -o $@

endif
