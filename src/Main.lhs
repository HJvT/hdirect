% 
% (c) The Foo Project, Universities of Glasgow & Utrecht, 1997-8
%
% @(#) $Docid: Jun. 9th 2003  12:58  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

The toplevel driver for the IDL compiler

\begin{code}
module Main
       ( main
       , hdirectHelp
       ) where

import LexM
import qualified Parser (parseIDL)
import qualified OmgParser (parseIDL)
import Opts
import PpCore   ( ppCore, showCore, showHeader, ppHeaderDecl )
import CoreUtils ( getInterfaceIds )
import PpIDLSyn ( ppIDL, showIDL )
import IDLSyn   ( Attribute, Defn )
import IDLUtils ( sortDefns, winnowDefns )
import CoreIDL  ( Decl )
import AbstractH  ( HTopDecl(..) )
import PpAbstractH ( ppHTopDecls, showAbstractH )
import PreProc
import Desugar
import Rename
import System.IO  ( hPutStr, hPutStrLn, stderr, stdout, hPutChar,
	     openFile, IOMode(..), hClose, Handle, hFlush
	   )
import Control.Monad      ( when )
import System.Environment ( getProgName )
import System.Exit        ( exitWith, ExitCode(..) )
import CodeGen
import Utils ( dropSuffix, basename, notNull )
import Data.Time
import Data.List  ( partition )
import Version
import System.Locale
import HugsCodeGen
import DefGen
import CStubGen
import JavaProxy
import Env ( newEnv, addListToEnv_C )
import Control.Exception
import System.Time


{- BEGIN_SUPPORT_TYPELIBS
import 
       TLBWriter
import 
       ImportLib
import 
       Com   ( coRun )
   END_SUPPORT_TYPELIBS -}
{- BEGIN_USE_REGISTRY
import 
       Win32Registry  ( hKEY_LOCAL_MACHINE, regQueryValue, regOpenKeyEx, kEY_READ )
import 
       StdDIS	      ( MbString )
   END_USE_REGISTRY -}
\end{code}

\begin{code}
main :: IO ()
main
 | optVersion  = putStrLn version_msg
 | optHelp     = do { pgm <- getProgName ; putStrLn (usage_msg pgm) }
 | otherwise   = do
{- BEGIN_SUPPORT_TYPELIBS
   coRun $ do   
   END_SUPPORT_TYPELIBS -}
    file <- getInpFile
    case file of 
      Nothing     -> do { pgm <- getProgName ; putStrLn (usage_msg pgm) }
      Just fnames -> do
	case optOFiles of
	   (_:_) | length fnames > 1 -> do
	      pgm <- getProgName
	      hFlush stdout
	      hPutStr stderr pgm
	      hPutStrLn stderr (": you cannot use -o if you have multiple input files")
	      hPutStrLn stderr (usage_msg pgm)
	      exitWith (ExitFailure 1)
	   _ -> do
	     let
	       oFileFun | length fnames == 1 = \ n -> (oFile n, oModNm n)
	                | otherwise          = \ n -> (Right (oFileFromInput n), oModNm n)
               incls = optincludedirs ++ ["."]

	     if not optTlb 
	      then do
	       sequence (map (\ f -> processFile incls f (oFileFun f)) fnames)
	       return ()
	      else do
{- BEGIN_SUPPORT_TYPELIBS
                ds <- mapM (importLib) fnames
		let inp = Right ds
   END_SUPPORT_TYPELIBS -}

{- BEGIN_NOT_SUPPORT_TYPELIBS -}
		let src     = unlines $ map (\ x -> "importlib(" ++ show x ++ ");") fnames
		    inp     = Left src
	        hPutStrLn stderr "WARNING: Type library reading code not compiled in; Ignoring --tlb option"
{- END_NOT_SUPPORT_TYPELIBS -}
                let o_fnm   = oFile  (head fnames)
		    o_modnm = oModNm (head fnames)
		processSource incls "<typelib>" inp (o_fnm, o_modnm)

 where
   oModNm _  = case optOutputModules of { (x:_) -> Just x ; _ -> Nothing }
   oFile nm  = case optOFiles        of { (x:_) -> Left x ; _ -> Right (oFileFromInput nm) }

   oFileFromInput nm =
     case nm of
        "-" -> nm
	_
	 | optServer -> (dropSuffix nm) ++ "Proxy.hs"
         | otherwise -> (dropSuffix nm) ++ ".hs"

   getInpFile = 
         case optFiles of
            [] -> return Nothing
            _  -> return (Just (reverse optFiles))
              
    
hdirectHelp :: IO ()
hdirectHelp = do
  pgm <- getProgName
  putStrLn (usage_msg pgm)

\end{code}

\begin{code}
processFile :: [String] 
	    -> String
	    -> (Either String String, Maybe String)
	    -> IO ()
processFile path fname ofname = do
  fname'     <- preProcessFile fname
  ls         <- 
     case fname' of
        "-" -> getContents
	_   -> readFile fname'
  processSource path fname (Left ls) ofname

processAsf :: [String] 
	   -> String
	   -> IO [(String,Bool,[Attribute])]
processAsf path fname = do
  when optVerbose (hFlush stdout >> hPutStrLn stderr ("Processing ASF: " ++ fname))
  ls         <- 
     case fname of
        "-" -> getContents
	_   -> readFile fname
  Right x  <- runLexM path fname ('=':ls) Parser.parseIDL
  return x
  
showPassMsg :: String -> IO ()
showPassMsg msg = do
  hFlush stdout
  hPutStrLn stderr ("***" ++ msg)
  hFlush stderr

processSource :: [String]
	      -> String
	      -> Either String [Defn]
	      -> (Either String String, Maybe String)
	      -> IO ()
processSource path fname ls ofname = do
  when optShowPasses (showPassMsg "Reader")
  defs <- 
     case ls of
       Left str -> 
          catch 
             (runLexM path fname str parseIDL)
	     (\ err -> removeTmp >> ioError err)
       Right ds -> return ds
  when (optShowPasses && notNull optAsfs)
       (showPassMsg "Asf reader")
  asfs       <- mapM (processAsf path) optAsfs
  let 
       {-
        Definitions are sorted either on the command of 
	the user, or if we're operating in 'winnow'ing mode.
       -}
      s_defs
       | optWinnowDefns || optSortDefns || optJNI = sortDefns defs
       | otherwise    			= defs

      w_defs
       | optWinnowDefns = winnowDefns asf_env s_defs
       | otherwise      = s_defs

      combineAsf (f1, old) (f2, new) = (f1 && f2, old ++ new)

      asf_env   = addListToEnv_C combineAsf newEnv 
      				 (map (\ (x,y,z) -> (x, (y,z))) (concat asfs))

      os = showIDL (ppIDL fname w_defs)

  dumpPass dumpIDL     "Parsed IDL" os
  when optShowPasses (showPassMsg "Desugarer")
  (core_decls, tenv, tg_env, senv, ifenv) <- desugar fname asf_env w_defs
  let cs = showCore (ppCore core_decls)

  dumpPass dumpDesugar "Desugared IDL" cs

  let (renamed_decls, iso_env, iface_env) 
         = renameDecls tenv tg_env senv ifenv core_decls
      rs = showCore (ppCore renamed_decls)

  dumpPass dumpRenamer "Renamed Core IDL" rs
  
  when (optOutputTlb || notNull optOutputTlbTo) 
{- BEGIN_SUPPORT_TYPELIBS
       (coRun $ writeTLB optOutputTlbTo renamed_decls)
   END_SUPPORT_TYPELIBS -}
{- BEGIN_NOT_SUPPORT_TYPELIBS -}
       (hPutStrLn stderr "WARNING: type library handling code not compiled in; ignoring --output-tlb= option")
{- END_NOT_SUPPORT_TYPELIBS   -}
  when optShowPasses (showPassMsg "CodeGen")
  let (header, code) = codeGen ofname iso_env iface_env renamed_decls
      code_str	     = unlines (map showCode code)

  dumpPass dumpAbstractH "Abstract Haskell" code_str
  when optShowPasses (showPassMsg "CodeOutput")
  when (optJNI && optServer) (writeJava renamed_decls)
  writeHeader header
  let def_files = defGen code
  when (not optNoOutput && optGenDefs)
       (sequence_ (map (uncurry writeOutStuff) def_files))
  when (not optNoOutput) 
       (writeCode True code)
  removeTmp

{-  Invoke the right parser. -}
parseIDL :: LexM [Defn]
parseIDL
 | optCompilingOmgIDL = OmgParser.parseIDL 
 | otherwise	      = Parser.parseIDL >>= \ (Left x) -> return x

\end{code}

Bunch of small helper functions to write out and show code.

\begin{code}
showCode :: (String, Bool, [HTopDecl]) -> String
showCode (nm, _, ds) = file_msg (showAbstractH (ppHTopDecls ds))
 where
  file_msg cont
    | generateGreenCard = "File: "++ show (dropSuffix nm ++ ".gc") ++ '\n':cont
    | otherwise		= "File: "++ show nm ++ '\n':cont

generateGreenCard :: Bool
generateGreenCard = not optNoOutput && optGreenCard

writeCode :: Bool -> [(String, Bool, [HTopDecl])] -> IO ()
writeCode _          []                 = return ()
writeCode is_haskell ((nm, flg, md):rs) = do
  writeOutCode
  when (is_haskell && flg) $ do
        hFlush stdout
        hPutStrLn stderr 
		  ("Notice: Need to generate C stubs as well for module " ++ show (dropSuffix nm) ++ ",")
	hPutStrLn stderr
	          ("        since it contains methods that passes structs/unions")
	hPutStrLn stderr
	          ("        by value.")
  when (not optNoOutput && is_haskell && (optHugs || optGenCStubs || flg))
       (writeCode False [(nm,flg,md)])
  writeCode is_haskell rs
 where
  writeOutCode  = writeOut incs is_haskell False out_nm (showDecls non_incs)

  showDecls
    | is_haskell   = showAbstractH.ppHTopDecls
    | not optHugs  = cStubGen out_nm
    | otherwise    = hugsCodeGen out_nm

  incs             = map (\ (HInclude s) -> s) incs'
  (incs',non_incs) = partition filterIncludes md
  
  filterIncludes HInclude{}     = is_haskell -- C'ish backends deal with 
  					     -- the includes directly.
  filterIncludes _              = False

  out_nm = 
   case nm of
     "-" -> nm
     _ 
      | generateGreenCard -> dropSuffix nm ++ ".gc"
      | not is_haskell    -> dropSuffix nm ++ ".c"
      | otherwise	  -> nm

writeOut :: [String] -> Bool -> Bool -> String -> String -> IO ()
writeOut _    _          _       _             "" = return ()
writeOut incs is_haskell no_hdrs fname_prim stuff = do
  when (optVerbose && fname /= "-") (hFlush stdout >> hPutStr stderr ("Writing " ++ fname))
  case fname of
    "-" -> do
      hPutBanner embed_comment stdout
      includes_at_top stdout
      putStrLn stuff
    _   -> do
      hp <- openFile fname WriteMode
      when is_haskell       (includes_at_top hp)
      hPutBanner embed_comment hp
      when (not is_haskell) (includes_at_top hp)
      hPutStrLn hp stuff
      hClose hp
  when optVerbose (hFlush stdout >> hPutChar stderr '\n')
 where
   fname = 
    case fname_prim of
      "-" -> fname_prim
      _   ->
        case optODirs of
         (x:_) -> x ++ '/':basename fname_prim
         _     -> fname_prim

   dirs 
     | no_hdrs    = incs
     | is_haskell = optIncludeHeaders ++ incs
     | otherwise  = optIncludeCHeaders ++ optIncludeHeaders ++ incs

   embed_comment ls
    | is_haskell = unlines (map (\ x -> '-':'-':' ':x) ls)
    | otherwise  = block_comment ls ++
    		   unlines
		     [ ""
		     , if optHugs then "#include \"HDirect.h\"" else ""
		     , "#ifndef __INT64_DEFINED__"
		     , "#ifdef __GNUC__"
		     , "typedef long long int64;"
		     , "typedef unsigned long long uint64;"
		     , "#else"
		     , "#ifdef _MSC_VER"
		     , "typedef __int64 int64;"
		     , "typedef unsigned __int64 uint64;"
		     , "#else"
		     , "/* Need some help here, please. */"
		     , "#endif"
		     , "#endif"
		     , "#define __INT64_DEFINED__"
		     , "#endif"
		     , ""
		     ]

   includes_at_top hp
     | null dirs || optGreenCard = return ()
     | otherwise = do
            -- one {-# OPTIONS ... #-} per include file.
           sequence (map (gen_options hp) dirs)
	   return ()

   gen_options hp hfile = 
       case hfile of
          []     -> return ()
	  _      -> hPutStrLn hp (gen_include (showFn hfile))
           where
	    gen_include fn
	      | is_haskell = "{-# OPTIONS -#include " ++ fn ++ " #-}"
	      | otherwise  = "#include " ++ fn

            showFn ls@('"':_) = ls
            showFn ls@('<':_) = ls
	    showFn ls	      = show ls

hPutBanner :: ([String] -> String) -> Handle -> IO ()
hPutBanner comment hp = do
  ls <- mkBanner comment 
  hPutStrLn hp ls

mkBanner :: ([String] -> String) -> IO String
mkBanner comment = do
  cal <- getClockTime >>= toCalendarTime
  let date = formatCalendarTime defaultTimeLocale "%H:%M %Z, %A %d %B, %Y" cal
  return (comment
      [ "Automatically generated by " ++ pkg_name ++ ", " ++ pkg_version
      , "Created: " ++ date
      , "Command line: " ++ unwords theOpts
      ])

block_comment :: [String] -> String
block_comment ls = unlines ("/*":ls ++ ["*/"])

writeOutStuff :: String -> String -> IO ()
writeOutStuff fname stuff = do
  when optVerbose (hFlush stdout >> hPutStr stderr ("Writing " ++ fname))
  hp <- openFile fname WriteMode
  hPutStrLn hp stuff
  hClose hp
  when optVerbose (hPutChar stderr '\n')

writeHeader :: [(String, Decl)] -> IO ()
writeHeader ls = do
   sequence (map (\ (fname, d) -> 
   		    writeOut [] False True
		             fname
			     (showHeader fname (ppHeaderDecl (getInterfaceIds d) d)))
		 ls)
   return ()

writeJava :: [Decl] -> IO ()
writeJava ds = do
   sequence (map (\ (fname, d) -> do
                        b <- mkBanner block_comment
   			writeOutStuff (dropSuffix fname ++ ".java")
   						(b ++ javaProxyGen d))
		 ls)
   return ()
 where
  ls = prepareDecls ds

dumpPass :: Bool -> String -> String -> IO ()
dumpPass True hdr stuff =
  case optOutputDumpTo of
    Nothing -> do
      putStrLn ("**** " ++ hdr ++ " ****")
      hPutStrLn stdout stuff
    Just x  -> writeFile x stuff
dumpPass False _ _ = return ()
\end{code}
