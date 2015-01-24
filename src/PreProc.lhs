% 
% (c) The Foo Project, University of Glasgow 1998
%
% @(#) $Docid: Jun. 7th 2001  17:02  Sigbjorn Finne $
% @(#) $Contactid: sof@galconn.com $
%

Running cpp over a file:

\begin{code}
module PreProc 
        (
          preProcessFile
        , removeTmp
        ) where

import Data.IORef
import System.IO.Unsafe
import System.CPUTime
import System.Environment ( getEnv )
import Control.Exception
import System.Process     ( system )
import Opts    ( optDebug, optCpp, optinclude_cppdirs, optcpp_defines )
import Data.List    ( intersperse )
import Utils   ( prefixDir )
import System.IO
import Control.Monad

count :: IORef Int
count = unsafePerformIO (newIORef 0)

prefix :: IORef Integer
prefix = unsafePerformIO (newIORef 0)

preProcessFile :: String      -- file to run cpp over.
               -> IO String   -- where the result is stored.
preProcessFile fname 
 | not (optCpp)  = return fname
 | otherwise     = do
  pt  <- getCPUTime
  writeIORef prefix pt
  v   <- readIORef count
  writeIORef count (v+1)
--  tmp <- catch (getEnv "TMPDIR") 
--               (\ _ -> return "/tmp/")
  tmp <- getEnv "TMPDIR" 
                `onException` return "/tmp/"
  let tmpnam = prefixDir tmp ("ihc" ++ show pt ++ show v)
  let
      tmpnam1 = tmpnam ++ ".c"
      tmpnam2 = tmpnam ++ ".i"
      -- In case the CPP we're about to run is insistent
      -- on the input file ending in .c, we create a
      -- little temporary file here.
      oput    = "#include "++show fname ++ "\n"
      incls   = 
         " -I. " ++
         case optinclude_cppdirs of
            [] -> []
            ls -> '-':'I':'"': concat (intersperse ":" ls) ++ "\""

      defines = 
        " -D__midl"         ++ 
        " -D__restrict="    ++    -- pesky GNU extensions.
        " -D__restrict__="  ++
        " -D__extension__=" ++
        " -D__const__=const" ++
        " -D__const=const" ++
        ' ':unwords optcpp_defines

--  cpp <- catch (getEnv "CPP")
--               (\ _ -> return ("gcc -E -x c"))
  cpp <- getEnv "CPP"
                `onException` return ("gcc -E -x c")
  hdl <- openFile tmpnam1 WriteMode
  hPutStrLn hdl oput
  hClose hdl
  let cmd = (cpp ++ incls ++ defines ++ ' ':tmpnam1 ++ " -o " ++ tmpnam2)
  when optDebug (hPutStrLn stderr ("Pre-processing file: "++fname ++ '\n':cmd))
  res <- system cmd
  return tmpnam2

removeTmp :: IO ()
removeTmp = do
  pt <- readIORef prefix
--  tmp <- catch (getEnv "TMPDIR")
--               ( \ _ -> return "/tmp/")
  tmp <- getEnv "TMPDIR"
                `onException` return "/tmp/"
  let tmpnam = prefixDir tmp ("ihc" ++ show pt ++ "*")
--  del_cmd <- catch (getEnv "DELPROG")
--                   ( \ _ -> return "rm -f")
  del_cmd <- getEnv "DELPROG"
                    `onException` return "rm -f"
  let cmd    = del_cmd ++ ' ':tmpnam
  when optDebug (hPutStrLn stderr ("Clearing out temporary files: " ++ cmd))
  system cmd
  return ()

\end{code}
