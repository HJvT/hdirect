
Copyright (c) 1998,  Daan Leijen, leijen@@fwi.uva.nl

This module is part of HaskellDirect (H/Direct).

\begin{code}
{-# OPTIONS -#include "PointerSrc.h" #-}
module Pointer  
	( 
	  Ptr
	
	, allocMemory
	, stackFrame

	, writeSeqAtDec
{-
	, ptrInc
	, writeSeqAt
-}

        , freeMemory
	, freeBSTR
	, freeWith
	, freeWithC
	
	, primNoFree
	
	, finalNoFree
	, finalFreeMemory
	
	, makeFO

       ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import PointerPrim
import Word     ( Word32 )
import Monad
\end{code}

The Pointer module provides helper functions over Ptrs +
allocation/freeing of memory via malloc/free or the COM task
allocator.

\begin{code}
type Finalizer a  = Ptr a -> IO ()

makeFO :: Ptr a -> FunPtr (Ptr a -> IO ()) -> IO (ForeignPtr b)
{- BEGIN_GHC_ONLY 
#if __GLASGOW_HASKELL__ > 601
makeFO obj finaliser = newForeignPtr (mkFinal finaliser obj) obj >>= return.castForeignPtr
#else
makeFO obj finaliser = newForeignPtr obj (mkFinal finaliser obj) >>= return.castForeignPtr
#endif
   END_GHC_ONLY -}
{- BEGIN_NOT_FOR_GHC -}
-- Versionitis. If your version of Hugs is troubled by this one, simply enable the other version.
makeFO obj finaliser = newForeignPtr (mkFinal finaliser obj) obj >>= return.castForeignPtr
--makeFO obj finaliser = newForeignPtr obj (mkFinal finaliser obj) >>= return.castForeignPtr
{- END_NOT_FOR_GHC -}

{- BEGIN_GHC_ONLY
#if __GLASGOW_HASKELL__ < 505
mkFinal final obj = ap0 final obj
foreign import ccall "dynamic" ap0 :: FunPtr (Ptr a -> IO()) -> (Ptr a -> IO ())
#else
mkFinal final _ = final
#endif
   END_GHC_ONLY -}
{- BEGIN_NOT_FOR_GHC -}
mkFinal final _ = final
{- END_NOT_FOR_GHC -}

\end{code}

Helpers.

\begin{code}
writeSeqAtDec :: Word32 -> [Ptr a -> IO ()] -> Ptr a -> IO ()
writeSeqAtDec size ws ptr = go init_ptr ws
  where
   len           = fromIntegral (length ws - 1)
   init_ptr      = ptr `plusPtr` (size_i * len)
   size_i        = fromIntegral size

   go _   []     = return ()
   go ptr (x:xs) = do
      x ptr
      let ptr_next = ptr `plusPtr` (-size_i)
      go ptr_next xs

{-
ptrInc i p        = p `plusAddr` (fromIntegral i)
ptrDec d p        = p `plusAddr` (-(fromIntegral  d))

writeSeqAt :: Word32 -> [Ptr a -> IO ()] -> Ptr a -> IO ()
writeSeqAt size ws ptr = go ptr ws
 where
   go ptr []     = return ()
   go ptr (x:xs) = do
      x ptr
      let ptr_next = ptrInc size ptr
      go ptr_next xs
-}

\end{code}

Use @stackFrame@ when you know the allocated chunk have
a fixed extent.

\begin{code}
stackFrame :: Word32 -> (Ptr a -> IO b) -> IO b
stackFrame size f
      = do p <- allocMemory size
           f p `always` primFreeMemory (castPtr p)

\end{code}

Special free routines for pointers. Use them to manually free pointers.

\begin{code}
freeMemory            = freeWithC primFreeMemory
freeBSTR              = freeWithC primFreeBSTR

freeWithC :: Finalizer () -> Ptr a -> IO ()
freeWithC final p = final (castPtr p)

freeWith :: (Ptr a -> IO ()) -> Ptr a -> IO ()
freeWith free p = free p 
\end{code}

Helper functions that doesn't really have a good home to go to:

\begin{code}
always :: IO a -> IO () -> IO a
always io action
      = do x <- io `catch` (\ err -> do { action; ioError err })
           action
           return x

\end{code}

Primitives/helpers:

\begin{code}
allocMemory :: Word32 -> IO (Ptr a)
allocMemory sz = do
  a <- primAllocMemory sz
  if a == nullPtr then
     ioError (userError "allocMemory: not enough memory")
   else
     return (castPtr a)

\end{code}
