{-# OPTIONS -#include "PointerSrc.h" #-}
-- Automatically generated by HaskellDirect (ihc.exe), version 0.20
-- Created: 20:10 Pacific Standard Time, Tuesday 16 December, 2003
-- Command line: -fno-qualified-names -fkeep-hresult -fout-pointers-are-not-refs -c PointerPrim.idl -o PointerPrim.hs

module PointerPrim
       ( primNoFree
       , primFreeBSTR
       , primFreeMemory
       , finalNoFree
       , finalFreeMemory
       , primAllocMemory
       , primFinalise
       ) where
       
import Prelude
import Foreign.Ptr (Ptr)
import IOExts (unsafePerformIO)
import Word (Word32)

primNoFree :: Ptr ()
           -> IO ()
primNoFree p =
  prim_PointerPrim_primNoFree p

foreign import stdcall "primNoFree" prim_PointerPrim_primNoFree :: Ptr () -> IO ()
primFreeBSTR :: Ptr ()
             -> IO ()
primFreeBSTR p =
  prim_PointerPrim_primFreeBSTR p

foreign import stdcall "primFreeBSTR" prim_PointerPrim_primFreeBSTR :: Ptr () -> IO ()
primFreeMemory :: Ptr ()
               -> IO ()
primFreeMemory p =
  prim_PointerPrim_primFreeMemory p

foreign import stdcall "primFreeMemory" prim_PointerPrim_primFreeMemory :: Ptr () -> IO ()
finalNoFree :: Ptr ()
finalNoFree = unsafePerformIO (prim_PointerPrim_finalNoFree)

foreign import stdcall "finalNoFree" prim_PointerPrim_finalNoFree :: IO (Ptr ())
finalFreeMemory :: Ptr ()
finalFreeMemory =
  unsafePerformIO (prim_PointerPrim_finalFreeMemory)

foreign import stdcall "finalFreeMemory" prim_PointerPrim_finalFreeMemory :: IO (Ptr ())
primAllocMemory :: Word32
                -> IO (Ptr ())
primAllocMemory sz = prim_PointerPrim_primAllocMemory sz

foreign import stdcall "primAllocMemory" prim_PointerPrim_primAllocMemory :: Word32 -> IO (Ptr ())
primFinalise :: Ptr ()
             -> Ptr ()
             -> IO ()
primFinalise finaliser finalisee =
  prim_PointerPrim_primFinalise finaliser
                                finalisee

foreign import stdcall "primFinalise" prim_PointerPrim_primFinalise :: Ptr () -> Ptr () -> IO ()

