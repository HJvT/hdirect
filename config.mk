# Set this to where you've got your Haskell compiler
#  (used to compile the contents of src/, lib/ and examples/ )
#
# The default setting assume that the Haskell compiler you want to
# use is GHC and is available along your path as 'ghc'.
HC=ghc

#
# The C compiler
#
CC=gcc

# ghc is capable of generating Makefile dependencies too,
# just feed it the -M flag.
MKDEPENDHS=$(HC)

#
# Happy (only needed if you want to change the grammars.)
#
HAPPY=../../happy/src/happy -g

#
# Installation setup:
#
#  bindir   = directory where the IDL compiler will be installed (and maybe also a library DLL.)
#  libdir   = location where the *.a will go
#  datadir  = where to install the GHC interface files (.hi)
#
#  hugslibdir  = location of the Hugs98 library directory
#
# If you're only interested in installing the Hugs specific bits, do 'make install-hugs' (after
# having configured `hugslibdir' below.)

#bindir=/usr/local/bin
#libdir=/usr/local/lib
#datadir=/usr/local/lib/ghc-4.08
#hugslibdir=/hugs98/lib
