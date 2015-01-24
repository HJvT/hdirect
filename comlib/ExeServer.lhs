
\begin{code}
{-# OPTIONS -#include "comPrim.h" #-}
module ExeServer 
	(
	  runExeServer       -- :: ComponentInfo -> IO ()
	, registerExeServer  -- :: String -> ComponentInfo -> IO ()
	) where

import ClassFactory
import ComDll
import ComPrim
import Com
import HDirect
import Int
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.ForeignPtr
import Char ( toLower )

\end{code}

\begin{code}
runExeServer :: ComponentInfo -> IO ()
runExeServer c = do
  newStablePtr c
  ev <- mkEvent
  let c'=c{componentFinalise=componentFinalise c >> signalEvent ev}
  ip <- createClassFactory (newInstance c' "" (componentFinalise c'))
  pip    <- marshallIUnknown ip
  pclsid <- marshallCLSID (componentCLSID c)
  dw    <- coRegisterClassObject (castForeignPtr pclsid) pip
  			(fromIntegral (fromEnum CLSCTX_LOCAL_SERVER))
  			(fromIntegral (fromEnum REGCLS_SINGLEUSE))
  waitForEvent ev
  coRevokeClassObject dw
  return ()
  
registerExeServer :: String -> ComponentInfo -> IO ()
registerExeServer arg ci
  | null arg  = return ()
  | otherwise = do
   let
    arg_low  = map toLower (tail arg)
    is_unreg = arg_low == "unregserver"
    is_reg   = arg_low == "regserver"

   if not is_unreg && not is_reg then
      return ()
    else do
      path <- getModuleFileName nullPtr
      (registerComponent ci) ci path is_reg
      (if is_reg then stdRegComponent else stdUnRegComponent) ci False path
\end{code}

\begin{code}
data REGCLS 
 = REGCLS_SINGLEUSE      --   = 0, 
 | REGCLS_MULTIPLEUSE    --   = 1, 
 | REGCLS_MULTI_SEPARATE --   = 2, 
 | REGCLS_SUSPENDED      --   = 4, 
 | REGCLS_SURROGATE      --   = 8, 

instance Enum REGCLS where
  fromEnum ctx =
    case ctx of
      REGCLS_SINGLEUSE      -> 0
      REGCLS_MULTIPLEUSE    -> 1
      REGCLS_MULTI_SEPARATE -> 2
      REGCLS_SUSPENDED      -> 4
      REGCLS_SURROGATE      -> 8
 
  toEnum c =
    case c of
      0 -> REGCLS_SINGLEUSE
      1 -> REGCLS_MULTIPLEUSE
      2 -> REGCLS_MULTI_SEPARATE
      4 -> REGCLS_SUSPENDED
      8 -> REGCLS_SURROGATE
 

foreign import ccall "mkEvent" mkEvent :: IO (Ptr ())

foreign import ccall "signalEvent" signalEvent :: Ptr () -> IO ()

foreign import ccall "waitForEvent" waitForEvent :: Ptr () -> IO ()

foreign import stdcall "CoRevokeClassObject"
	coRevokeClassObject :: Word32 -> IO HRESULT

\end{code}

