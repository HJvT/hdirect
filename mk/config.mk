#
# HaskellDirect project information
#

# what to include in a binary distribution
HaskellDirectMainDir 		= hdirect
HaskellDirectBinDistDirs 	= hdirect
HaskellDirectBinDistDocs 	= hdirect/doc
HaskellDirectBinDistShScripts 	= hdirect-$(ProjectVersion)
HaskellDirectBinDistLinks 	= hdirect

include $(HaskellDirectMainDir)/mk/version.mk
