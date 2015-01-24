
Haskell implementation of a COM class factory / component instance creator.

\begin{code}
{-# OPTIONS -#include "ClassFactory_stub.h" #-}
module ClassFactory 
	(
	  createClassFactory -- :: (IID a -> IO (PrimIP a)) -> IO (PrimIP ())

        , iidIClassFactory
	) where

import Com
import ComServ hiding ( createInstance )
import Foreign.Ptr
import ComException ( cLASS_E_NOAGGREGATION )
import IOExts
\end{code}

\begin{code}
data ClassFactory a
 = ClassFactory {
         new_instance :: (IID (IUnknown ()) -> IO (IUnknown ())),
	 lockCount    :: (IORef Int)
   }

type IClassFactory a = IUnknown (ClassFactory a)

type This_ClassFactory = Ptr (IClassFactory ())

iidIClassFactory :: IID (IClassFactory ())
iidIClassFactory = mkIID "{00000001-0000-0000-C000-000000000046}"
\end{code}


Class factory implementation:

\begin{code}
createInstance :: This_ClassFactory 
               -> Ptr (IUnknown a)
	       -> Ptr (IID (IUnknown ()))
	       -> Ptr (Ptr (IUnknown b))
	       -> IO HRESULT
createInstance this punkOuter riid ppv
 | punkOuter /= nullPtr = return cLASS_E_NOAGGREGATION
 | otherwise            = do
    st  <- getObjState this
    iid <- unmarshallIID False riid
    unk <- (new_instance st) iid
    writeIUnknown True ppv unk
    return s_OK

lockServer :: This_ClassFactory -> Int -> IO HRESULT
lockServer this ilock = do
    st    <- getObjState this
    let ref = lockCount st
    count <- readIORef ref
    if lock then
      writeIORef ref (count+1)
     else
      writeIORef ref (count-1)
    return s_OK
  where
   lock 
    | ilock == 0 = False
    | otherwise  = True

foreign export stdcall dynamic
   export_createInstance :: (This_ClassFactory -> Ptr (IUnknown a) -> Ptr (IID (IUnknown ())) -> Ptr (Ptr (IUnknown b)) -> IO HRESULT) -> IO (Ptr ())

foreign export stdcall dynamic
   export_lockServer :: (Ptr (IUnknown a) -> Int -> IO HRESULT) -> IO (Ptr ())
\end{code}

\begin{code}
createClassFactory :: (IID (IUnknown ()) -> IO (IUnknown ()))
		   -> IO (IClassFactory ())
createClassFactory mkInst = do
   lcount <- newIORef 0
   let cf_state = ClassFactory mkInst lcount
   createComInstance ""
                     cf_state (return ())
		     [mkIface iidIClassFactory iClassFactory_vtbl]
		     iidIClassFactory
 where
  guidIClassFactory = iidToGUID iidIClassFactory

iClassFactory_vtbl :: VTable (IClassFactory ()) (ClassFactory ())
iClassFactory_vtbl = unsafePerformIO $ do
   addrOf_cI <- export_createInstance createInstance
   addrOf_lS <- export_lockServer     lockServer
   createComVTable [addrOf_cI, addrOf_lS]
\end{code}
