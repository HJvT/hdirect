%
% (c) 1999, sof
%

Generic implementation of Com-style enumeration
interfaces - give it the list to enumerate & you're there!

\begin{code}
module EnumInterface 
        ( 
          mkEnumInterface  -- :: [a]
                           -- -> Int
                           -- -> (Ptr a -> a -> IO ())
                           -- -> IO (ComVTable iid objState)
        ) where

import Word
import Int
import ComServ
import ComException
import Com ( HRESULT, writeIUnknown, IUnknown )
import HDirect
import Foreign.Ptr
import IOExts
import Monad ( when )

type ThisPtr = Ptr (IUnknown ())
\end{code}

The state kept by each IEnum* instance:

\begin{code}
data EnumState a
 = EnumState 
     { elt      :: IORef ([a], Int)
     , origElts :: [a]
     , writeElt :: (Ptr (Ptr a) -> a -> IO ())
     , sizeof   :: Int
     }
\end{code}

The constructor doesn't have to do much, initialise the state
shared by the different methods and create a method table
containing them:

\begin{code}
mkEnumInterface :: [a]
                -> Int
                -> (Ptr (Ptr a) -> a -> IO ())
                -> IO (ComVTable iid objState)
mkEnumInterface ls sizeof write = do
  ref <- newIORef (ls, length ls)
  let st = EnumState ref ls write sizeof
  m_enumNext  <- export_enumNext  (enumNext st)
  m_enumSkip  <- export_enumSkip  (enumSkip st)
  m_enumReset <- export_enumReset (enumReset st)
  m_enumClone <- export_enumClone (enumClone st)
  createComVTable [m_enumNext, m_enumSkip, m_enumReset, m_enumClone]
\end{code}

An IEnum* interface allows you to iterate over a sequence *once*,
i.e., it doesn't wrap around (but you can rewind the enumeration
back to the beginning with IEnum::Reset()):

\begin{code}
enumNext :: EnumState a
         -> ThisPtr
         -> Word32
         -> Ptr (Ptr a)
         -> Ptr Word32
         -> IO HRESULT
enumNext st this c pFetched pcFetched 
  | pcFetched == nullPtr && c /= 1 = return e_INVALIDARG
  | pFetched == nullPtr            = return e_POINTER
  | otherwise                      = do
     let ref = elt st
     (elts, eltsLeft) <- readIORef ref
     let
      c_int = fromIntegral c
      (hr, elts_to_fetch)
       | c_int > eltsLeft  = (s_FALSE, eltsLeft)
       | otherwise         = (s_OK,    c_int)
       
      elts_left = eltsLeft - elts_to_fetch

     elts' <- fillIn pFetched elts_to_fetch  elts
     when (pcFetched /= nullPtr)
          (writeWord32 pcFetched (fromIntegral elts_to_fetch))
     writeIORef ref (elts', elts_left)
     return hr
 where
   wr_marshall = writeElt st

   fillIn ptr 0 ls     = return ls
   fillIn ptr n (x:xs) = do
       wr_marshall ptr x
       let ptr' = ptr `plusPtr` (fromIntegral (sizeof st))
       fillIn ptr' (n-1) xs

foreign export stdcall dynamic
   export_enumNext :: (ThisPtr -> Word32 -> Ptr a -> Ptr Word32 -> IO HRESULT) -> IO (Ptr ())

enumSkip :: EnumState a
         -> ThisPtr
         -> Word32
         -> IO HRESULT
enumSkip st this c
  | c == 0     = return e_INVALIDARG
  | otherwise  = do
     (elts, eltsLeft) <- readIORef (elt st)
     let
      c_int = fromIntegral c
      (hr, elts_to_fetch)
       | c_int > eltsLeft = (s_FALSE, eltsLeft)
       | otherwise        = (s_OK,    c_int)

      elts_left = take elts_to_fetch elts
      x         = eltsLeft - elts_to_fetch

     writeIORef (elt st) (elts_left, x)
     return hr

foreign export stdcall dynamic
   export_enumSkip :: (ThisPtr -> Word32 -> IO HRESULT) -> IO (Ptr ())

enumReset :: EnumState a
          -> ThisPtr
          -> IO HRESULT
enumReset st _ = do
  let ls = origElts st
  writeIORef (elt st) (ls, length ls)
  return s_OK

foreign export stdcall dynamic
   export_enumReset :: (ThisPtr -> IO HRESULT) -> IO (Ptr ())

enumClone :: EnumState a
          -> ThisPtr
          -> Ptr (Ptr (IUnknown b))
          -> IO HRESULT
enumClone st this out = do
   vtbl <- mkEnumInterface (origElts st) (sizeof st) (writeElt st)
   ip   <- cloneIPointer_prim this vtbl
   writeIUnknown False out ip
   return s_OK

foreign export stdcall dynamic
   export_enumClone :: (ThisPtr -> Ptr (Ptr (IUnknown b)) -> IO HRESULT) -> IO (Ptr ())
\end{code}
