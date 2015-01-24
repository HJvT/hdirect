%
% (c), 1999 - sof
%

Generic implementation of COM connectable objects / connection points,
server side.

The connection between this framework impl. and the Haskell object
responsible for firing events on the registered sinks, is still
up in the air. The current arrangement is for the object and a
particular connection point to share an IORef holding the current
set of registered sinks. The object will then fire the events
by using the (generated) stubs for that particular event interface.

Probably want to abstract away the details of how sink broadcasting
is done.

\begin{code}
module ConnectionPoint 
                (
                  mkConnectionContainer
                ) where

import ComServ
import Connection ( iidIConnectionPointContainer, iidIConnectionPoint,
                    IConnectionPointContainer, IConnectionPoint,
                    iidIEnumConnections, iidIEnumConnectionPoints
                  )
                    
import Com
import HDirect ( writeWord32, Ptr, sizeofPtr )
import EnumInterface ( mkEnumInterface )
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import IOExts
import Word
import Int
import ComException

type ThisPtr = Ptr (IUnknown ())
\end{code}


\begin{code}
mkConnectionContainer :: [(IID (IUnknown ()), IORef [(Word32, IUnknown ())])]
                      -> IO (IConnectionPointContainer ())
mkConnectionContainer ls = fixIO $ \ ip -> do
  let ils = unsafePerformIO (mapM (mkConnection ip) ls)
  addrOf_eCP <- export_enumCP (enumConnectionPoints (map snd ils))
  addrOf_fCP <- export_fCP    (findConnectionPoint ils)
  vtbl       <- createComVTable [ addrOf_eCP, addrOf_fCP ]
  createComInstance "" () (return ())
                    [mkIface iidIConnectionPointContainer vtbl]
                    iidIConnectionPointContainer
  
mkConnection :: IConnectionPointContainer ()
             -> (IID (IUnknown ()), IORef [(Word32, IUnknown ())])
             -> IO (IID (IUnknown ()), IConnectionPoint ())
mkConnection ip (iid, regd_sinks) = do
  vtbl <- mkConnectionPointVTBL ip iid regd_sinks
  ip   <- createComInstance "" () (return ())
                            [ mkIface iidIConnectionPoint vtbl ]
                            iidIConnectionPoint
  return (iid, ip)

\end{code}

\begin{code}
mkConnectionPointVTBL :: IConnectionPointContainer ()
                      -> IID (IUnknown iid)
                      -> IORef [(Word32, IUnknown ())]
                      -> IO (ComVTable (IConnectionPoint a) objState)
mkConnectionPointVTBL ip iid sinks = do
   addrOf_gi    <- export_gi    (getConnectionInterface iid)
   addrOf_gcpc  <- export_gcpc  (getConnectionPointContainer ip)
   cookie_ref   <- newIORef (0::Word32)
   addrOf_adv   <- export_adv   (advise sinks cookie_ref iid)
   addrOf_unadv <- export_unadv (unadvise sinks)
   addrOf_eC    <- export_eCP   (enumConnections sinks)
   createComVTable
         [ addrOf_gi , addrOf_gcpc , addrOf_adv, addrOf_unadv, addrOf_eC ]

getConnectionInterface :: IID iid
                       -> ThisPtr
                       -> Ptr GUID
                       -> IO HRESULT
getConnectionInterface iid _ piid 
  | piid == nullPtr  = return e_POINTER
  | otherwise        = do
     writeGUID piid (iidToGUID iid)
     return s_OK

foreign export stdcall dynamic
    export_gi :: (ThisPtr -> Ptr GUID -> IO HRESULT) -> IO (Ptr ())

getConnectionPointContainer :: IConnectionPointContainer ()
                            -> ThisPtr
                            -> Ptr (Ptr (IUnknown b))
                            -> IO HRESULT
getConnectionPointContainer ip _ pip = do
   writeIUnknown True{-addRef-} pip ip
   return s_OK

foreign export stdcall dynamic
    export_gcpc :: (ThisPtr -> Ptr (Ptr (IUnknown b)) -> IO HRESULT) -> IO (Ptr ())

advise :: IORef [(Word32,IUnknown ())]
       -> IORef Word32
       -> IID (IUnknown iid)
       -> ThisPtr
       -> PrimIP ()
       -> Ptr Word32
       -> IO HRESULT
advise sinks cookie_ref iid this pUnkSink pdwCookie = do
  ls      <- readIORef sinks
  cookie  <- readIORef cookie_ref
  ip      <- unmarshallIUnknown False pUnkSink
  catch (do
     ip2 <- ip # queryInterface iid
     if nullPtr == pdwCookie then
        return e_POINTER
      else do
        writeIORef cookie_ref (cookie+1)
        writeIORef sinks ((cookie,castIface ip2):ls)
        writeWord32 pdwCookie cookie
        return s_OK
   )(\ _ -> return cONNECT_E_CANNOTCONNECT)

foreign export stdcall dynamic
   export_adv :: (ThisPtr -> PrimIP () -> Ptr Word32 -> IO HRESULT) -> IO (Ptr ())

unadvise :: IORef [(Word32,IUnknown ())]
         -> ThisPtr
         -> Word32
         -> IO HRESULT
unadvise sinks this dwCookie = do
  ls     <- readIORef sinks
  case break ((==dwCookie).fst) ls of
    (ls,[])    -> return cONNECT_E_NOCONNECTION
    (ls, _:rs) -> do
         -- just drop the interface pointer and let
         -- the GC release it.
       writeIORef sinks (ls++rs)
       return s_OK

foreign export stdcall dynamic
    export_unadv :: (ThisPtr -> Word32 -> IO HRESULT) -> IO (Ptr ())

enumConnections :: IORef [(Word32,IUnknown ())]
                -> ThisPtr
                -> Ptr (Ptr (IUnknown a))
                -> IO HRESULT
enumConnections sinks this ppCP
  | ppCP == nullPtr  = return e_POINTER
  | otherwise        = do
    ls   <- readIORef sinks
    vtbl <- mkEnumInterface (map snd ls) (fromIntegral sizeofPtr) (writeIUnknown True)
    ip   <- createComInstance "" () (return ())
                              [mkIface iidIEnumConnections vtbl]
                              iidIEnumConnections
    writeIUnknown True ppCP ip
    return s_OK

foreign export stdcall dynamic
    export_enumCP :: (ThisPtr -> Ptr (Ptr (IUnknown b)) -> IO HRESULT) -> IO (Ptr ())
\end{code}

\begin{code}
enumConnectionPoints :: [IConnectionPoint ()]
                     -> ThisPtr
                     -> Ptr (Ptr (IUnknown b))
                     -> IO HRESULT
enumConnectionPoints ls this ppEnum 
  | ppEnum == nullPtr  = return e_POINTER
  | otherwise          = do
     vtbl <- mkEnumInterface ls (fromIntegral sizeofPtr) (writeIUnknown True)
     ip   <- createComInstance "" () (return ())
                                   [mkIface iidIEnumConnectionPoints vtbl]
                                   iidIEnumConnectionPoints
     writeIUnknown True ppEnum ip
     return s_OK

foreign export stdcall dynamic
    export_eCP :: (ThisPtr -> Ptr (Ptr (IUnknown b)) -> IO HRESULT) -> IO (Ptr ())

findConnectionPoint :: [(IID (IUnknown ()), IConnectionPoint ())]
                    -> ThisPtr
                    -> Ptr GUID
                    -> Ptr (Ptr (IUnknown ()))
                    -> IO HRESULT
findConnectionPoint ls this riid ppCP 
  | ppCP == nullPtr  = return e_POINTER
  | otherwise        = do
     guid <- unmarshallGUID False riid
     let iid = guidToIID guid
     case (lookup iid ls) of
       Nothing -> do
          poke ppCP nullPtr
          return cONNECT_E_NOCONNECTION
       Just i  -> do
          writeIUnknown True ppCP i
          return s_OK

foreign export stdcall dynamic
    export_fCP :: (ThisPtr -> Ptr GUID -> Ptr (Ptr (IUnknown b)) -> IO HRESULT) -> IO (Ptr ())

\end{code}

\begin{code}
cONNECT_E_NOCONNECTION :: HRESULT
cONNECT_E_NOCONNECTION = 0x80040200

cONNECT_E_ADVISELIMIT :: HRESULT
cONNECT_E_ADVISELIMIT  = 0x80040201

cONNECT_E_CANNOTCONNECT :: HRESULT
cONNECT_E_CANNOTCONNECT = 0x80040202

cONNECT_E_OVERRIDDEN :: HRESULT
cONNECT_E_OVERRIDDEN = 0x80040203
\end{code}
