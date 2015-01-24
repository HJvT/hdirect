% 
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Dec. 9th 2003  08:37  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

\begin{code}
module Opts where

import Data.IORef
import GetOpt
import System.Environment
import System.IO.Unsafe ( unsafePerformIO)
import System.IO     ( hPutStrLn, stderr )
import Control.Monad  ( when )
import Utils  ( split, trace, notNull )
import Version
{- BEGIN_USE_REGISTRY
import Utils ( bailIf, hdirect_root )
import 
       Win32Registry  ( hKEY_LOCAL_MACHINE, regQueryValue, regOpenKeyEx, kEY_READ, regEnumKeyVals )
import 
       StdDIS	      ( MbString )
   END_USE_REGISTRY -}

\end{code}

Grimy stuff - to make it sort of possible to use the compiler
from within Hugs. 

\begin{code}
ihc_opts :: [Option]
ihc_opts = unsafePerformIO $ do
  args <- readIORef the_ihc_opts
  return (snd (getOpts options [] args))

the_ihc_opts :: IORef [String]
the_ihc_opts = unsafePerformIO $ do
    args <- getArgs
    let args' = expandDerivedArgs args ++ defaultArgs
    when ("-v" `elem` args') (hPutStrLn stderr ("Effective command line: " ++ unwords args'))
    newIORef args'

theOpts :: [String]
theOpts = unsafePerformIO (readIORef the_ihc_opts)

expandDerivedArgs :: [String] -> [String]
expandDerivedArgs ls =
  case ls of
    []     -> []
    (x:xs) ->
      case (lookup x derivedArgs) of
	Nothing -> x : expandDerivedArgs xs
	Just ys -> let xs' = expandDerivedArgs xs in ys ++ xs'

defaultArgs :: [String]
defaultArgs =
  case filter ((== "default").fst) registryOptions of
    ((_,o):_) -> o
    _         -> []

derivedArgs :: [(String, [String])]
derivedArgs = 
  ("-fautomation", autoOptions):
  filter ((/= "default").fst) registryOptions

autoOptions :: [String]
autoOptions = words $
{- BEGIN_USE_REGISTRY
  unsafePerformIO $
  catch 
    (do
      hk <- regOpenKeyEx hKEY_LOCAL_MACHINE hdirect_root kEY_READ
      v <- regQueryValue hk (Just "CurrentVersion") 
      bailIf (null v) (return default_opts) $ do
       -- figure out where the latest dist resides.
      hk <- regOpenKeyEx hKEY_LOCAL_MACHINE (hdirect_root ++ "options\\" ++ v) kEY_READ
       -- lookup the import dir for the latest version
      v <- regQueryValue hk (Just "-fautomation")
      bailIf (null v) (return default_opts) (return v))
    (\ _ -> return default_opts)
 END_USE_REGISTRY -}
{- BEGIN_NOT_USE_REGISTRY -}
 default_opts
{- END_NOT_USE_REGISTRY -}
  where
   default_opts = "-fno-export-list -fcom -fcoalesce-methods -fno-gen-binary-interface"

registryOptions :: [(String,[String])]
{- BEGIN_NOT_USE_REGISTRY -}
registryOptions = []
{- END_NOT_USE_REGISTRY   -}
{- BEGIN_USE_REGISTRY
registryOptions = unsafePerformIO $
  catch 
    (do
      hk <- regOpenKeyEx hKEY_LOCAL_MACHINE hdirect_root kEY_READ
      v <- regQueryValue hk (Just "CurrentVersion") 
      hk <- regOpenKeyEx hKEY_LOCAL_MACHINE (hdirect_root ++ '\\':v ++ "\\options") kEY_READ
      vs <- regEnumKeyVals hk
      let
        readOne (s,v,_) = do
	 return (s,words v)
      ls <- mapM readOne vs
      return ls)
    (\ _ -> return [])
   END_USE_REGISTRY -}

set_ihc_opts :: [String] -> IO ()
set_ihc_opts args = writeIORef the_ihc_opts args
\end{code}

\begin{code}
dumpIDL, dumpDesugar, dumpRenamer, dumpAbstractH :: Bool
dumpIDL        = any (DumpIDL==)     ihc_opts
dumpDesugar    = any (DumpDesugar==) ihc_opts
dumpRenamer    = any (DumpRenamer==) ihc_opts
dumpAbstractH  = any (DumpAbsH==)    ihc_opts

optGreenCard, optTargetGhc, optCpp, optNoOutput :: Bool
optGreenCard   = any (OptGreenCard==) ihc_opts
optTargetGhc   = any (OptTargetGhc==) ihc_opts
optCpp         = any (OptCpp==)      ihc_opts
optNoOutput    = any (OptNoOutput==) ihc_opts

optVersion, optHelp, optDebug, optVerbose, optGenHeader :: Bool
optVersion     = any (DumpVersion==) ihc_opts
optHelp	       = any (DumpHelp==)    ihc_opts
optDebug       = any (DumpDebug==)   ihc_opts
optVerbose     = any (DumpVerbose==) ihc_opts
optGenHeader   = any (OptGenHeader==) ihc_opts

optExportListWithTySig, optNoExportList, optNoModuleHeader :: Bool
optExportListWithTySig = any (OptExportTySig==) ihc_opts
optNoExportList   = any (OptNoExportList==)   ihc_opts
optNoModuleHeader = any (OptNoModuleHeader==) ihc_opts

optExpandInheritedInterface, optCoalesceIsomorphicMethods :: Bool
optExpandInheritedInterface = not (any (OptNoExpandInherit==) ihc_opts)
optCoalesceIsomorphicMethods = any (OptCoalesceIsomorphicMethods==) ihc_opts

optNoDllName :: Bool
optNoDllName = any (OptNoDllName==) ihc_opts

optKeepHRESULT, optUseDispIDs, optIntsEverywhere, optIntAsWord :: Bool
optKeepHRESULT    = any (OptKeepHResult==) ihc_opts
optUseDispIDs     = any (OptUseDispIDs==)  ihc_opts
optIntsEverywhere = any (OptUseInts==)     ihc_opts
optIntAsWord      = any (OptIntAsWord==)   ihc_opts

optShowPasses, optConvertImportLibs, optSortDefns, optWinnowDefns :: Bool
optShowPasses     = any (OptShowPasses==)  ihc_opts
optConvertImportLibs = any (OptConvertImportLibs==) ihc_opts
optSortDefns       = any (OptSortDefns==) ihc_opts
optWinnowDefns     = any (OptWinnowDefns==) ihc_opts

optOnlyRemoveDefns, optDon'tGenBinaryComInterfaces :: Bool
optOnlyRemoveDefns = any (OptOnlyRemoveDefns==) ihc_opts
optDon'tGenBinaryComInterfaces = any (OptDon'tGenBinaryComInterfaces==) ihc_opts

optOutPointersAreRefs, optQualInstanceMethods :: Bool
optOutPointersAreRefs    = not $ any (OptOutPointersAreNotRefs==) ihc_opts
optQualInstanceMethods   = any (OptQualInstanceMethods==) ihc_opts

optNoDerefRefs :: Bool
optNoDerefRefs = any (OptNoDerefRefs==) ihc_opts

optIntIsInt, optIgnoreSourceIfaces, optNoEnumMagic, optEnumsAsFlags :: Bool
optIntIsInt                = any (OptIntIsInt==) ihc_opts
optIgnoreSourceIfaces      = any (OptIgnoreSourceIfaces==) ihc_opts || optHugs
optNoEnumMagic             = any (OptNoEnumMagic==) ihc_opts
optEnumsAsFlags            = any (OptEnumsAsFlags==) ihc_opts

optGenBitsInstance, optGenNumInstance :: Bool
optGenBitsInstance        = any (OptGenBitsInstance==) ihc_opts
optGenNumInstance         = any (OptGenNumInstance==) ihc_opts

optLongLongIsInteger, optNukeEmptyStructs, optShortHeader, optIntCoercesInPrelude :: Bool
optLongLongIsInteger	   = any (OptLongLongIsInteger==) ihc_opts
optNukeEmptyStructs	   = any (OptNukeEmptyStructs==) ihc_opts
optShortHeader		   = any (OptShortHeader==) ihc_opts
optIntCoercesInPrelude     = any (OptIntCoercesInPrelude==) ihc_opts

optServer, optOneModulePerInterface, optShowIDLInComments :: Bool
optServer       = any (OptServer==) ihc_opts
optOneModulePerInterface = any (OptOneModulePerInterface==) ihc_opts
optShowIDLInComments = any (OptShowIDLInComments==) ihc_opts

optUnparamedInterfacePointers :: Bool
optUnparamedInterfacePointers = any (OptUnParam'dIPointers==) ihc_opts

-- Subtyped interface pointers are the default, only way turn this off
-- is via the use of -funparamed-interface-pointers. (or -fhs-to-c.)
optSubtypedInterfacePointers :: Bool
optSubtypedInterfacePointers = 
    any (OptSubTypedInterfacePointers==) ihc_opts ||
    (not optUnparamedInterfacePointers && not optHaskellToC)

optNoImportLists, optNoImports, optNoQualNames :: Bool
optNoImportLists   = any (OptNoImportLists==) ihc_opts
optNoImports       = any (OptNoImports==) ihc_opts
optNoQualNames     = any (OptNoQualNames==) ihc_opts

optNoDependentArgs, optNoLibIds :: Bool
optNoDependentArgs = any (OptNoDependentArgs==) ihc_opts
optNoLibIds	   = any (OptNoLibraryIds==)    ihc_opts

optPrefixIfaceName, optAppendIfaceName, optDeepMarshall :: Bool
optPrefixIfaceName = any (OptPrefixIfaceName==) ihc_opts
optAppendIfaceName = any (OptAppendIfaceName==) ihc_opts
optDeepMarshall    = not (any (OptShallowMarshall==) ihc_opts)

optNoMangleIfaceNames, optExportAbstractly :: Bool
optNoMangleIfaceNames = any (OptNoMangleIfaceNames==) ihc_opts
optExportAbstractly   = any (OptExportAbstractly==) ihc_opts

optIgnoreDispInterfaces, optDualVtbl, optCompilingDceIDL :: Bool
optIgnoreDispInterfaces = any (OptIgnoreDispInterfaces==) ihc_opts
optDualVtbl		= any (OptDualVtbl==) ihc_opts
optCompilingDceIDL	= any (OptCompilingDceIDL==) ihc_opts
-- MsIDL is currently the default.
optCompilingMsIDL, optCompilingOmgIDL :: Bool
optCompilingMsIDL	= any (OptCompilingMsIDL==) ihc_opts 
			    || (not optCompilingDceIDL && not optCompilingOmgIDL)
optCompilingOmgIDL	= any (OptCompilingOmgIDL==) ihc_opts

optHaskellToC, optIgnoreHelpstring, optTlb :: Bool
optHaskellToC		= any (OptHaskellToC==) ihc_opts
optIgnoreHelpstring     = any (OptIgnoreHelpstring==) ihc_opts
optTlb			= any (OptTLB==)  ihc_opts

optIgnoreImpLibs, optHugs, optSkel, optUnsafeCalls :: Bool
optIgnoreImpLibs	= any (OptIgnoreImpLibs==)  ihc_opts
optHugs			= any (OptHugs==) ihc_opts
optSkel                 = any (OptSkel==) ihc_opts
optUnsafeCalls	        = any (OptUnsafeCalls==) ihc_opts

optH1_4, optIgnoreHiddenMeths, optIgnoreRestrictedMeths :: Bool
optH1_4			= any (OptH1_4==) ihc_opts
optIgnoreHiddenMeths    = any (OptIgnoreHiddenMeths==) ihc_opts
optIgnoreRestrictedMeths = any (OptIgnoreRestrictedMeths==) ihc_opts

optOptionalAsMaybe, optNoVariantInstance, optVariantInstance :: Bool
optOptionalAsMaybe       = any (OptOptionalAsMaybe==) ihc_opts
optNoVariantInstance     = any (OptNoVariantInstance==) ihc_opts
optVariantInstance       = any (OptVariantInstance==) ihc_opts

optOutputTlb, optPatternAsLambda, optClassicNameMangling :: Bool
optOutputTlb             = any (OptOutputTlb==) ihc_opts
optPatternAsLambda       = any (OptPatternAsLambda==) ihc_opts
optClassicNameMangling   = any (OptClassicNameMangling==) ihc_opts

optGenDefs, optOverloadVariant, optNoOverloadVariant :: Bool
optGenDefs               = any (OptGenDefs==) ihc_opts
optOverloadVariant       = any (OptOverloadVariant==) ihc_opts
optNoOverloadVariant     = any (OptNoOverloadVariant==) ihc_opts

optGenCStubs, optExplicitIPointer, optAnonTLB :: Bool
optGenCStubs		 = any (OptGenCStubs==) ihc_opts
optExplicitIPointer      = any (OptExpIPointer==) ihc_opts
optAnonTLB               = any (OptAnonTLB==) ihc_opts

optUnwrapSingletonStructs, optJNI, optCorba :: Bool
optUnwrapSingletonStructs = any (OptUnwrapSingletonStructs==) ihc_opts
optJNI			 = any (OptJNI==) ihc_opts
optCorba		 = any (OptCorba==) ihc_opts

optCharPtrIsString, optInlineTypes :: Bool
optCharPtrIsString       = any (OptCharPtrIsString==) ihc_opts
optInlineTypes           = any (OptInlineTypes==) ihc_opts

optIncludeAsImport, optExcludeSysIncludes :: Bool
optIncludeAsImport       = any (OptIncludeAsImport==) ihc_opts
optExcludeSysIncludes    = any (OptExcludeSysIncludes==) ihc_opts

optVoidTydefIsAbstract, optNoWarnMissingMode :: Bool
optVoidTydefIsAbstract   = any (OptVoidTydefIsAbstract==) ihc_opts
optNoWarnMissingMode     = any (OptNoWarnMissingMode==) ihc_opts

optDon'tTidyDefns, optSmartEnums, optNoShareFIDs, optUseStdDispatch, optUseIIDIs :: Bool
optDon'tTidyDefns        = any (OptDon'tTidyDefns==) ihc_opts
optSmartEnums            = any (OptSmartEnums==) ihc_opts
optNoShareFIDs           = any (OptNoShareFIDs==) ihc_opts
optUseStdDispatch        = any (OptUseStdDispatch==) ihc_opts
optUseIIDIs              = any (OptUseIIDIs==) ihc_opts

optNoWideStrings :: Bool
optNoWideStrings         = any (OptNoWideStrings==) ihc_opts

optCom :: Bool
optCom = not optJNI && not optCorba && not optHaskellToC && not optCompilingOmgIDL

optIgnoreMethsUpto :: Maybe String
optIgnoreMethsUpto       = 
   case [ p | OptIgnoreMethsUpto p <- ihc_opts ] of
     [] -> Nothing
     ls -> Just (last ls)  -- last entry on the command line wins.

optPointerDefault :: Maybe String
optPointerDefault        = 
   case [ p | OptPointerDefault p <- ihc_opts ] of
     [] -> Nothing
     ls -> Just (last ls)  -- last entry on the command line wins.

optOutputDumpTo :: Maybe String
optOutputDumpTo          = 
   case [ p | OptOutputDump p <- ihc_opts ] of
     [] -> Nothing
     ls -> Just (last ls)  -- last entry on the command line wins.

optOutputModules :: [String]
optOutputModules = [ f | OptOutputModule f <- ihc_opts ]

optOutputHTo :: [String]
optOutputHTo   = [ f | OptOutputHTo f <- ihc_opts ]
optOutputTlbTo :: [String]
optOutputTlbTo = [ f | OptOutputTlbTo f <- ihc_opts ]

optFiles :: [String]
optFiles       = [ f | OptFile f <- ihc_opts]
optAsfs  :: [String]
optAsfs        = [ f | OptAsf f  <- ihc_opts]
optUseAsfs :: Bool
optUseAsfs     = notNull optAsfs
optOFiles :: [String]
optOFiles      = [ o | OptOutputFile o <- ihc_opts] 
optODirs  :: [String]
optODirs       = [ o | OptOutputDir o <- ihc_opts ] -- a smelly one (sorry, couldn't resist :)

optincludedirs :: [String]
optincludedirs = 
{- BEGIN_SUPPORT_TYPELIBS 
  concat [split ';' d | OptIncludeDirs d <- reverse ihc_opts]
   END_SUPPORT_TYPELIBS -}
{- BEGIN_NOT_SUPPORT_TYPELIBS -}
  concat [split ':' d | OptIncludeDirs d <- reverse ihc_opts]
{- END_NOT_SUPPORT_TYPELIBS -}

optinclude_cppdirs :: [String]
optinclude_cppdirs = 
  concat [split ':' d | OptIncludeCppDirs d <- reverse ihc_opts]

optcpp_defines :: [String]
optcpp_defines = [ d | OptCppDefine d <- reverse ihc_opts]

optIncludeHeaders, optIncludeCHeaders :: [String]
optIncludeHeaders = [ d | OptIncludeHeader d <- reverse ihc_opts]
optIncludeCHeaders = [ d | OptIncludeCHeader d <- reverse ihc_opts]
\end{code}

Printing out some info about the package in question:

\begin{code}
name, version :: String
name    = pkg_name
version = pkg_version

version_msg :: String
version_msg = 
 unlines
 [ name ++ ", " ++ version
 , ""
 , "Report bugs to <sof@galconn.com>"
 ]

usage_msg :: String -> String
usage_msg pgm = 
 unlines
 [ "Usage: " ++ pgm ++ " [OPTION]... SOURCE"
 , ""
 , "Run H/Direct, an IDL compiler for Haskell, over SOURCE"
 , ""
 , " -h, --help      print out this help message and exit"
 , " -v, --version   output version information and exit"
 , " -o <file>       write H/Direct output to <file>"
 , " -noC            don't generate any output files."
 , " -i<dirs>, --include-dir <dirs>"
 , "                 Add <dirs> to the include search path"
 , "                 (<dirs> is colon separated.)"
 , ""
 , " -cpp            run the C pre-processor over input prior to"
 , "                 before processing input"
 , " -I<dirs>        Add <dirs> to C pre-processor's search path"
 , "                 (<dirs> is colon separated.)"
 , " --gc            output GreenCard stubs"
 , " --hugs          output Hugs compatible stubs"
 , " -g              generate GHC specific FFI code (i.e., _casm_ and _ccall_) "
 , " --h1.4          generate Haskell 1.4 code (plus whateve FFI extension you're using)"
 , " --gen-headers   output C header file corresponding to IDL spec."
 , " --gen-tlb       generate type library/ies."
 , ""
 , " -fone-mod-per-iface  split output up into one Haskell module per (disp)interface"
 , " -fuse-dispids        for Automation interfaces, generate stubs that use DISPIDs "
 , "                      instead of method names"
 , " -fkeep-hresult         don't squirrel away HRESULTs, but return them (COM specific.)"
 , " -fno-export-list       generated Haskell module(s) should export everything"
 , " -fno-expand-inherited  don't include methods of inherited interface"
 , " -fno-module-header     don't emit module header"
 , " -fcoalesce-methods     coalesce isomorphic methods."
 , " -fexport-with-tysig    emit verbose export list containing function type signatures"
 , " -fexport-transparent   export defined data types non-abstractly"
 , " -fshow-idl-in-comments display original IDL in comments next to Haskell generated source"
 , ""
 , " -fmaybe-optional-params represent optional parameters via Maybe types (Automation specific)."
 , " -fpattern-as-lambda     in the generated Haskell source, lambda bind parameters to toplevel"
 , "                         parameters instead of using patterns (Meijer-style)."
 , ""
 , " -ftyped-interface-pointers    strongly type COM interface pointers"
 , " -funtyped-interface-pointers  treat Automation interface pointers as"
 , "                               generic IDispatch pointers"
 , " -fsubtyped-interface-pointers encode interface inheritance in type of"
 , "                               interface pointer (allows an interface that"
 , "                               inherit to reuse methods of super interface.)"
 , ""
 , " -fignore-helpstrings          don't include helpstrings as comments in generated"
 , "                               source."
 , " -fignore-dispinterfaces       don't generate stub code for any dispinterface"
 , "                               declarations encountered."
 , " -fno-library-ids              don't bother generating libid declarations for library"
 , "                               uuids."
 , " -fignore-hidden-methods       don't generate stubs for [hidden] methods"
 , " -fignore-restricted-methods   don't generate stubs for [restricted] methods"
 , " -ftreat-importlibs-as-imports try to convert importlib() statements into imports."
 , ""
 , " -fsort-defns                  re-order the input to make definition occur be any use."
 , ""
 , " -fdual-vtbl                   for dual interfaces, generate stubs that invoke"
 , "                               methods via vtbl entries rather than via IDispatch" 
 , ""
 , " -fno-import-lists             don't generate detailed import lists"
 , " -fno-imports                  leave out import section alltogether"
 , " -fno-qualified-names          don't qualify import names"
 , " -fqualified-instance-methods"
 , "                               qualify method names in instance decls"
 , ""
 , "-fno-dependent-args            turn off clever-mode for dependent args/fields"
 , "-fno-gen-binary-interface      don't bother generating stubs for binary Com interfaces"
 , "                               (useful when you're just interested in Automation)"
 , "-fno-gen-variant-instances     don't generate Variant instances for enums"
 , "                               (only of interest with Automation)."
 , "-fgen-variant-instances        generate Variant instances for enums"
 , ""
 , "-fprefix-interface-name        to avoid name clashes, prefix interface name to methods"
 , "-fappend-interface-short-name  to avoid name clashes, append upper case letters of"
 , "                               interface name to methods"
 , "-fprefix-interface-name        to avoid name clashes, prefix interface name to methods"
 , "-fdeep-marshall                (un)marshall as much as possible"
 , "-fout-pointers-are-not-refs    don't enforce the rule that says that [out] params have"
 , "                               to be references."
 , ""
 , "-fuse-ints-everywhere          gen. code that use Int instead of sized Int/Word types"
 , "-fint-as-word                  combined with previous, map all unsigned ints to Int"
 , ""
 , "-fstring-as-widestring         map wide strings to Haskell Strings"
 , " -s                    server mode, generate stubs for Haskell COM objects"
 , "                       (untested & support not yet finished.)"
 , " --skel                generate skeleton implementation of coclasses"
 , ""
 , " -d, --debug     output extra debug information to stderr"
 , " -ddump-idl      dump abstract syntax tree to stderr"
 , " -ddump-ds       dump desugared/core IDL to stderr"
 , " -ddump-absH     dump abstract Haskell to stderr"
 , ""
 , "Home page: http://www.haskell.org/hdirect/"
 ]
\end{code}

\begin{code}
options :: Opt [Option] ()
options = 
  (prefixed "-" $ 
   opts
      [ prefixed "-" $
         opts 
           [ "version"	    -=     DumpVersion
   	   , "gc"	    -=     OptGreenCard
	   , "hugs"	    -=     OptHugs
	   , "gen-headers"  -=     OptGenHeader
	   , "gen-defs"     -=     OptGenDefs
           , "help"	    -=     DumpHelp
           , "verbose"	    -=     DumpVerbose
           , "debug"	    -=     DumpDebug
           , "include-dir"  -===   OptIncludeDirs
           , "include-header=" -== OptIncludeHeader
           , "include-c-header=" -== OptIncludeCHeader
	   , "jni"	    -=	   OptJNI
	   , "corba"	    -=	   OptCorba
	   , "skeleton"     -=     OptSkel
	   , "tlb"	    -=     OptTLB
	   , "unsafe-calls" -=     OptUnsafeCalls
	   , "h1.4"         -=     OptH1_4
	   , "gen-tlb"      -=     OptOutputTlb
	   , "gen-c-stubs"  -=     OptGenCStubs
	   , "output-tlb="    -==  OptOutputTlbTo
	   , "output-h="      -==  OptOutputHTo
	   , "output-module=" -==  OptOutputModule
	   , "output-dump="   -==  OptOutputDump
	   , "asf="           -==  OptAsf
           ]
      , prefixed "f" $
         opts 
	   [ "anon-typelib"                -= OptAnonTLB
	   , "com"			   -= OptCompilingMsIDL
	   , "char-ptr-is-string"	   -= OptCharPtrIsString
	   , "classic-name-mangling"       -= OptClassicNameMangling
	   , "dual-vtbl"		   -= OptDualVtbl
	   , "export-with-tysig"           -= OptExportTySig
	   , "export-abstractly"           -= OptExportAbstractly
	   , "enums-as-flags"              -= OptEnumsAsFlags
	   , "gen-bits-instance"           -= OptGenBitsInstance
	   , "gen-num-instance"            -= OptGenNumInstance
           , "keep-hresult"                -= OptKeepHResult
	   , "maybe-optional-params"       -= OptOptionalAsMaybe
           , "no-export-list"              -= OptNoExportList
	   , "no-module-header"            -= OptNoModuleHeader
	   , "no-tidy-defns"               -= OptDon'tTidyDefns
	   , "no-share-ffi-decls"          -= OptNoShareFIDs
           , "no-expand-inherited"         -= OptNoExpandInherit
	   , "no-foreign-dll-name"         -= OptNoDllName
           , "use-dispids"                 -= OptUseDispIDs
	   , "ignore-importlibs"           -= OptIgnoreImpLibs
	   , "ignore-methods-upto="        -== OptIgnoreMethsUpto
	   , "int-is-int"                  -= OptIntIsInt
	   , "int-prelude-coercions"       -= OptIntCoercesInPrelude
           , "one-mod-per-iface"	   -= OptOneModulePerInterface
	   , "coalesce-methods"		   -= OptCoalesceIsomorphicMethods
	   , "show-idl-in-comments"	   -= OptShowIDLInComments
	   , "compress-enums"              -= OptSmartEnums
	   , "unparamed-interface-pointers"  -= OptUnParam'dIPointers
	   , "subtyped-interface-pointers"  -= OptSubTypedInterfacePointers
	   , "no-import-lists"		   -= OptNoImportLists
	   , "no-imports"		   -= OptNoImports
	   , "no-qualified-names"	   -= OptNoQualNames
	   , "qualified-instance-methods"  -= OptQualInstanceMethods
	   , "no-dependent-args"	   -= OptNoDependentArgs
	   , "no-overload-variant"	   -= OptNoOverloadVariant
	   , "no-enum-magic"               -= OptNoEnumMagic
	   , "pattern-as-lambda"           -= OptPatternAsLambda
	   , "prefix-interface-name"       -= OptPrefixIfaceName
	   , "append-interface-short-name" -= OptAppendIfaceName
	   , "shallow-marshall"		   -= OptShallowMarshall
	   , "no-mangle-interface-names"   -= OptNoMangleIfaceNames
	   , "no-gen-variant-instances"    -= OptNoVariantInstance
	   , "gen-variant-instances"       -= OptVariantInstance
	   , "ignore-dispinterfaces"       -= OptIgnoreDispInterfaces
	   , "ignore-helpstrings"	   -= OptIgnoreHelpstring
	   , "ignore-hidden-methods"       -= OptIgnoreHiddenMeths
	   , "ignore-restricted-methods"   -= OptIgnoreRestrictedMeths
	   , "ignore-source-interfaces"    -= OptIgnoreSourceIfaces
	   , "include-as-import"           -= OptIncludeAsImport
	   , "exclude-system-includes"     -= OptExcludeSysIncludes
	   , "inline-synonyms"             -= OptInlineTypes
	   , "longlong-as-integer"	   -= OptLongLongIsInteger
	   , "explicit-i-pointer"          -= OptExpIPointer
	   , "no-library-ids"              -= OptNoLibraryIds
	   , "no-warn-missing-mode"        -= OptNoWarnMissingMode
	   , "omg"			   -= OptCompilingOmgIDL
	   , "pointer-default="            -== OptPointerDefault
	   , "dce"			   -= OptCompilingDceIDL
	   , "hs-to-c"			   -= OptHaskellToC
	   , "use-ints-everywhere"         -= OptUseInts
	   , "int-as-word"                 -= OptIntAsWord
	   , "treat-importlibs-as-imports" -= OptConvertImportLibs
	   , "sort-defns"		   -= OptSortDefns
	   , "string-as-widestring"        -= OptNoWideStrings
	   , "overload-variant"		   -= OptOverloadVariant
	   , "remove-empty-structs"        -= OptNukeEmptyStructs
	   , "only-remove-defns"	   -= OptOnlyRemoveDefns
	   , "short-header"		   -= OptShortHeader
	   , "no-gen-binary-interface"     -= OptDon'tGenBinaryComInterfaces
	   , "out-pointers-are-not-refs"   -= OptOutPointersAreNotRefs
	   , "no-deref-refs"               -= OptNoDerefRefs
	   , "unwrap-singleton-structs"    -= OptUnwrapSingletonStructs
	   , "use-std-dispatch"            -= OptUseStdDispatch
	   , "use-iid-is"                  -= OptUseIIDIs
	   , "void-typedef-is-abstract"    -= OptVoidTydefIsAbstract
	   , "winnow-defns"		   -= OptWinnowDefns
	   ]
	   
      , prefixed "ddump-" $
               opts [ "ds"    -= DumpDesugar
	            , "idl"   -= DumpIDL
		    , "abs"   -= DumpAbsH
		    , "rn"    -= DumpRenamer
                    ],
      "dshow-passes"    -= OptShowPasses,
      "cpp"		-= OptCpp,
      "h"		-= DumpHelp,
      "d"		-= DumpDebug,
      "v"		-= DumpVerbose,
      "c"		-= OptIgnore,   -- accept, but vacuous.
      "g"		-= OptTargetGhc,
      "noC"             -= OptNoOutput,
      "odir"            -=== OptOutputDir,
      "otlb"            -=== OptOutputTlbTo,
      "oh"		-=== OptOutputHTo,
      "odump"           -=== OptOutputDump,
      "o"		-=== OptOutputFile,
      "s"               -= OptServer,
      "i"		-== OptIncludeDirs,
      "I"	        -== OptIncludeCppDirs,
      "D"	        -== (\ s -> OptCppDefine ('-':'D':s)),
      "U"	        -== (\ s -> OptCppDefine ('-':'U':s))
     ]) `orOpt`
    (((\ opt -> head opt == '-' && notNull (tail opt)) -? 
		(\ opt -> trace ("Unrecognised option: " ++ opt ++ ", ignoring.") OptIgnore))
		  `orOpt`
    ((const True)  -? OptFile))

data Option
 = DumpVersion
 | DumpHelp
 | DumpDebug
 | DumpVerbose
 | DumpDesugar
 | DumpRenamer
 | DumpIDL
 | DumpAbsH
 | OptNoOutput
 | OptCpp
 | OptGreenCard
 | OptHugs
 | OptH1_4
 | OptTargetGhc
 | OptGenHeader
 | OptSkel
 | OptExportTySig
 | OptNoExportList
 | OptEnumsAsFlags
 | OptNoModuleHeader
 | OptNoExpandInherit
 | OptNoDllName
 | OptCoalesceIsomorphicMethods
 | OptShowIDLInComments
 | OptKeepHResult
 | OptUseDispIDs
 | OptServer
 | OptOneModulePerInterface
 | OptUnParam'dIPointers
 | OptSubTypedInterfacePointers
 | OptNoImportLists
 | OptNoImports
 | OptNoQualNames
 | OptNoDependentArgs
 | OptNoLibraryIds
 | OptPrefixIfaceName
 | OptAppendIfaceName
 | OptShallowMarshall
 | OptNoMangleIfaceNames
 | OptExportAbstractly
 | OptIgnoreDispInterfaces
 | OptDualVtbl
 | OptCompilingDceIDL
 | OptCompilingMsIDL
 | OptCompilingOmgIDL
 | OptHaskellToC
 | OptUseInts
 | OptIgnore
 | OptIntAsWord
 | OptShowPasses
 | OptConvertImportLibs
 | OptIgnoreHelpstring
 | OptSortDefns
 | OptWinnowDefns
 | OptOnlyRemoveDefns
 | OptDon'tGenBinaryComInterfaces
 | OptOutPointersAreNotRefs
 | OptNoDerefRefs
 | OptQualInstanceMethods
 | OptTLB
 | OptUnsafeCalls
 | OptOutputTlb
 | OptIgnoreHiddenMeths
 | OptIgnoreRestrictedMeths
 | OptIgnoreImpLibs
 | OptOptionalAsMaybe
 | OptNoVariantInstance
 | OptVariantInstance
 | OptPatternAsLambda
 | OptGenDefs
 | OptGenCStubs
 | OptOverloadVariant
 | OptNoOverloadVariant
 | OptExpIPointer
 | OptAnonTLB
 | OptIntIsInt
 | OptIntCoercesInPrelude
 | OptNoEnumMagic
 | OptGenBitsInstance
 | OptGenNumInstance
 | OptLongLongIsInteger
 | OptUnwrapSingletonStructs
 | OptIgnoreSourceIfaces
 | OptNukeEmptyStructs
 | OptShortHeader
 | OptJNI
 | OptCorba
 | OptCharPtrIsString
 | OptInlineTypes
 | OptPointerDefault String
 | OptOutputDump   String
 | OptIncludeAsImport
 | OptExcludeSysIncludes
 | OptVoidTydefIsAbstract
 | OptNoWarnMissingMode
 | OptDon'tTidyDefns
 | OptNoShareFIDs
 | OptSmartEnums
 | OptUseStdDispatch
 | OptUseIIDIs
 | OptNoWideStrings
 | OptIgnoreMethsUpto String
 | OptOutputModule String
 | OptClassicNameMangling
 | OptOutputTlbTo  String
 | OptOutputHTo    String
 | OptCppDefine    String
 | OptIncludeDirs  String
 | OptIncludeHeader String
 | OptIncludeCHeader String
 | OptIncludeCppDirs String
 | OptOutputFile   String
 | OptOutputDir    String
 | OptAsf          String
 | OptFile         String 
   deriving ( Eq )

\end{code}

