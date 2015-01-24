%
% (c) Sigbjorn Finne, 1998-99
%

Support code for writing Haskell COM components (yay!)

The library code for Haskell COM components (aka. servers),
support the wrapping up of a bunch of Haskell function values
into the binary representation that the COM spec mandates.

The library has two classes of 'users':

  - HaskellDirect generated stubs for interfaces representing
    Haskell COM components.
  - on-the-fly generation of COM interface pointers by a Haskell
    application.

i.e., in short, we care about having a simple, programmer-useable, API :-)

\begin{code}
{-# OPTIONS -#include "ComServ_stub.h" #-}
{-# OPTIONS -#include "comPrim.h" #-}
module ComServ 
	(
	  createComInstance  -- :: String
	                     -- -> objState
			     -- -> IO ()
			     -- -> [ComInterface objState]
			     -- -> IID iid
			     -- -> IO (IUnknown iid)

        , createInstance     -- :: objState
			     -- -> VTable iid objState
			     -- -> IO (IUnknown iid)
			  
	, createVTable	    -- :: [Ptr ()] -> IO (VTable iid objState)
           -- prefixes the three IU methods.
	, createComVTable   -- :: [Ptr ()] -> IO (ComVTable iid objState)

	, createIPointer  -- :: StablePtr a
	                  -- -> (VTable b)
			  -- -> IO (PrimIP b)
	, cloneIPointer      -- :: IUnknown a -> VTable b -> IO (PrimIP b)
	, cloneIPointer_prim -- :: PrimIP a -> VTable b -> IO (PrimIP b)

        , getObjState      -- :: PrimIP a -> IO b
        , getRealObjState  -- :: PrimIP a -> IO b

	, createDualInterface -- :: [Ptr ()]
		              -- -> IID iid
		              -- -> Either LIBID String
		              -- -> IUnknown a
		              -- -> IO (IDispatch ())

        , createDispInterface -- :: IUnknown iid        -- the interface to delegate to
		              -- -> Either LIBID String -- libid of type library to use.
		              -- -> IID iid      
		                         -- what interface it implements (needed to
			                 -- get at the type library which drives
			                 -- the dispatch interface.)
		              -- -> IO (IDispatch ()) 
			                 -- the dispatch implementation handed back.

	, VTable
	, ComVTable
	, PrimIP

	, ComInterface
	, mkIface
	, mkDispIface
	, mkDualIface
	
	, export_getTypeInfoCount
	, export_getTypeInfo
	, export_getIDsOfNames
	, export_invoke
	) where

import HDirect
import Word
import Int
import IOExts
import Com     hiding ( queryInterface, addRef, release )
import qualified Com ( addRef, release )
import Automation ( IDispatch, VARIANT, iidIDispatch, DISPID )
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Maybe   ( fromMaybe )
import Bits ( (.&.) )
import ComException
import WideString

import Monad
import List
\end{code}

Basic types - we're essentially untyped here; safety is provided
by the abstraction levels above us.

\begin{code}
type PrimIP iid   = Ptr (Ptr ())
type VTBL	  = (Ptr (Ptr ()), Int)
type VTable    iid objState = VTBL
type ComVTable iid objState = VTable iid (objState, IUnkState)

data ComInterface objState 
  = Iface     { ifaceGUID :: GUID
              , ifaceVTBL  :: VTBL
	      }
  | DispIface { ifaceGUID  :: GUID
	      , ifaceLIBID :: Either LIBID String
	      , ifaceVTBL  :: VTBL
	      , isDual     :: Bool
	      }

mkIface :: IID iid -> VTable iid objState -> ComInterface objState
mkIface iid a = Iface (iidToGUID iid) a

mkDispIface :: Maybe LIBID -> IID iid -> VTable iid objState -> ComInterface objState
mkDispIface l iid v = DispIface (iidToGUID iid) l' v False
 where
  l' = fromMaybe (Right "") (fmap Left l)

mkDualIface :: Maybe LIBID -> IID iid -> VTable iid objState -> ComInterface objState
mkDualIface l iid v = DispIface (iidToGUID iid) l' v True
 where
  l' = fromMaybe (Right "") (fmap Left l)

\end{code}

=== Creating a component instance ===

`createComInstance' creates a new COM component instance, given
the initial state together with the (implementation) of the
interfaces that the component supports. The interface pointer
that it returns is at the one requested.

\begin{code}
createComInstance :: String                   -- DLL path.
                  -> objState		      -- initial state
		  -> IO ()                    -- action to perform when releasing object.
	          -> [ComInterface objState]  -- supported interfaces
	          -> IID (IUnknown iid)
	          -> IO (IUnknown iid)
createComInstance dll_path initState releaseAction supported_ifaces initial_iid = do
  ip_state  <- mkInstanceState supported_ifaces dll_path releaseAction initState
  (_,iu)    <- deRefStablePtr  ip_state
  res	    <- lookupInterface initial_iid (iu_ifaces iu)
  case res of
    (_,_,ip) -> do
--       putMessage ("createComInstance: " ++ show initial_iid)
       return (castIface ip)

createInstance :: objState -> VTable (IUnknown iid) objState  -> IO (IUnknown iid)
createInstance initState vtable = do
  ip_state     <- newStablePtr initState
  createIPointer ip_state vtable

\end{code}

=== Creating the component instance specific state ===

Internal function which allocates an (immovable) chunk of
memory which holds the instance-specific state of a component.
For each component instance we keep a stable pointer to the
component instance state. In all likelihood, this object state
also contains enough information for the <tt/IUnknown/ methods
to operate correctly.

\begin{code}
createIPointer :: StablePtr a
	       -> VTBL
	       -> IO (IUnknown b)
createIPointer iface_st (vtbl,_) = do
   pre   <- alloc (sizeofIfaceHeader)
   poke pre vtbl
   poke (pre `plusPtr` fromIntegral sizeofPtr) iface_st
--   writeAddrOffAddr      pre 0 vtbl
--   writeStablePtrOffAddr pre 1 iface_st
   unmarshallIUnknown False{-finalise-} pre

sizeofIfaceHeader :: Word32  -- in bytes.
sizeofIfaceHeader =
       sizeofPtr  -- lpVtbl
   +   sizeofPtr  -- interface pointer state

-- for convenience.
cloneIPointer :: IUnknown iid_old -> VTable (IUnknown iid_new) objState -> IO (IUnknown iid_new)
cloneIPointer iptr vtbl = do
  stable_state <- getIPointerState_stbl (ifaceToAddr iptr)
  createIPointer stable_state vtbl

cloneIPointer_prim :: Ptr (IUnknown a) -> VTable (IUnknown iid_new) objState -> IO (IUnknown iid_new)
cloneIPointer_prim iptr vtbl = do
  stable_state <- getIPointerState_stbl iptr
  createIPointer stable_state vtbl
\end{code}

<tt/findInterface/ is used by both the class factory
and <tt/queryInterface/ to check whether the component
implements a particular interface.

\begin{code}
findInterface :: IUnkIfaceInfo
	      -> Ptr GUID
	      -> Ptr (Ptr (IUnknown b))
	      -> IO HRESULT
findInterface ls piid ppv = do
    iid		 <- unmarshallGUID False piid
    if (iid == iidToGUID iidIUnknown) then 
      case ls of
        []	      -> return e_NOINTERFACE
	((_,_,ip):_)  -> realiseIPointer ip
     else if (iid == iidToGUID iidIDispatch) then
      case filter (isIDispatch) ls of
         []	      -> return e_NOINTERFACE
	 ((_,_,ip):_) -> realiseIPointer ip

     else
      let
       findIt [] = do
         poke (castPtr ppv) nullPtr
         return e_NOINTERFACE
       findIt ((x,_,ip):xs)
         | x == iid  = realiseIPointer ip
         | otherwise = findIt xs
      in
      findIt ls
  where
    -- 'realiseIPointer' fills in the [out] ptr and return.
    -- This has the desired effect of forcing the evaluation
    -- of the interface pointer itself.
   realiseIPointer newip = do
      primip <- marshallIUnknown newip
      writefptr ppv primip
--      writeForeignObj ppv primip
      addRef (ifaceToAddr newip)
      return s_OK

    -- 
    -- Note: there's currently no way of indicating that
    -- an interface IA (which derives from IDispatch) has
    -- a 'idispatch' nature here other than go via the
    -- route of using the tlb marshaller.
    -- 
   isIDispatch (iid, flg, _) = flg || iid == iidToGUID iidIDispatch

lookupInterface :: IID iid 
		-> IUnkIfaceInfo -- [(GUID, Bool, IUnknown ())]
		-> IO (GUID, Bool, IUnknown ())
lookupInterface iid []       = ioError (userError "lookupInterface: interface not supported")
lookupInterface iid ls@(i:_) =
  case (find (\ (i,_,_) -> i == guid) ls) of
    Nothing   -> return i
    Just i    -> return i
 where
  guid = iidToGUID iid

\end{code}

\begin{code}
data IUnkState
 = IUnkState
     { iu_ifaces   :: IUnkIfaceInfo
     , iu_release  :: IO ()
     , iu_refcnt   :: IORef Int
     }

type IUnkIfaceInfo = [(GUID, Bool, IUnknown ())]

mkInstanceState :: [ComInterface objState] 
                -> String
		-> IO ()
                -> objState
		-> IO (StablePtr (objState, IUnkState))
mkInstanceState iface_list dll_path releaseAction objState = do
  fixIO (\ stbl_st -> do
     ref_cnt <- newIORef 1
     let iptrs = map (mkIf stbl_st) iface_list
         iu_st = IUnkState iptrs releaseAction ref_cnt
     newStablePtr (objState, iu_st)
   )
 where
  mkIf st (Iface iid vtbl)              = 
     (iid, False, unsafePerformIO (createIPointer st vtbl))
  mkIf st (DispIface guid libid vtbl is_dual) =
     (guid, True, unsafePerformIO $ do 
		    let iid     = guidToIID guid
		        lib_loc = 
			  case libid of
			    Right "" -> Right dll_path
			    _        -> libid
		    if is_dual then 
		       createDualInterface st vtbl lib_loc iid
		     else do
   		       ip <- createIPointer st vtbl
		       createDispInterface ip lib_loc iid
     )
\end{code}

'Standard' IUnknown implementation:

Implementation assumes that the object state is of
the form :

   StablePtr ((real_obj_state::a), (iu_state :: IUnkState))

(which it is if 'createCoClass' was used to create the component
instance.)

\begin{code}
queryInterface :: Ptr (IUnknown a)
	       -> Ptr GUID
	       -> Ptr (Ptr (IUnknown b))
	       -> IO HRESULT
queryInterface iptr riid ppvObject = do
  iid		 <- unmarshallGUID False riid
--  putMessage ("qi: " ++ show (iptr,iid))
  if_ls <- getSupportedInterfaces iptr
  hr    <- findInterface if_ls riid ppvObject
--  putMessage ("qi: " ++ show (iid,hr))
  return hr

addRef :: Ptr (IUnknown a) -> IO Word32
addRef iptr = do
--   putMessage "addRef"
   v <- readRefCount iptr
   writeRefCount iptr (v+1)
   return (fromIntegral v)

release :: Ptr (IUnknown a) -> IO Word32
release iptr = do
--   putMessage ("release: " ++ show iptr)
   v <- readRefCount iptr
--   putMessage ("release: " ++ show (iptr,v))
   let v' = v-1
   writeRefCount iptr v'
   if v' <= 0 then do
      releaseObj iptr
      let x = (fromIntegral 0) 
      return x
    else do
      let x = (fromIntegral (v-1))
      return x

\end{code}

\begin{code}
foreign export stdcall dynamic  export_queryInterface :: (Ptr (IUnknown a) -> Ptr GUID -> Ptr (Ptr (IUnknown b)) -> IO Int32) -> IO (Ptr ())
foreign export stdcall dynamic  export_addRef  :: (Ptr (IUnknown a) -> IO Word32) -> IO (Ptr ())
foreign export stdcall dynamic  export_release :: (Ptr (IUnknown a) -> IO Word32) -> IO (Ptr ())
\end{code}

\begin{code}
releaseObj :: Ptr (IUnknown a) -> IO ()
releaseObj iptr = do
--   putMessage "releaseObj"
     -- * invoke user-supplied finaliser.
   r <- iptr # getReleaseAction
   r
   -- * free up mem allocated to hold interface pointers (and vtbls?).
   -- * free embedded stable pointers.
   stbl <- iptr # getIPointerState_stbl
   freeStablePtr stbl
    -- and the GC will take care of the rest..   
   return ()

\end{code}

Accessing data accessible via a Haskell i-pointer - this stuff

\begin{code}
readRefCount :: Ptr (IUnknown a) -> IO Int
readRefCount ptr = do
  iu <- getIUnkState ptr
  readIORef (iu_refcnt iu)

writeRefCount :: Ptr (IUnknown a) -> Int -> IO ()
writeRefCount ptr v = do
  iu <- getIUnkState ptr
  writeIORef (iu_refcnt iu) v

getReleaseAction :: Ptr (IUnknown a) -> IO (IO ())
getReleaseAction ptr = do
  iu <- getIUnkState ptr
  return (iu_release iu)

getSupportedInterfaces :: Ptr (IUnknown a)  -> IO IUnkIfaceInfo
getSupportedInterfaces ptr = do
  iu_state <- getIUnkState ptr
  return (iu_ifaces iu_state)

getIUnkState :: Ptr (IUnknown a) -> IO IUnkState
getIUnkState iptr = do
  stbl  <- getIPointerState_stbl iptr
  (_,x) <- deRefStablePtr stbl
  return x

-- users of 'createCoClass' *must* use this and not
-- the one below!
getObjState :: Ptr (IUnknown a) -> IO b
getObjState iptr = do
  stbl  <- getIPointerState_stbl iptr
  (x,_) <- deRefStablePtr stbl
  return x

getRealObjState :: Ptr (IUnknown a) -> IO b
getRealObjState iptr = do
  stbl  <- getIPointerState_stbl iptr
  deRefStablePtr stbl

getIPointerState_stbl :: Ptr (IUnknown a) -> IO (StablePtr b)
getIPointerState_stbl iptr = peek (iptr `plusPtr` fromIntegral sizeofPtr)
--readStablePtrOffAddr iptr 1

\end{code}

Dispatch interface support:

\begin{code}
createDualInterface :: StablePtr objState
		    -> ComVTable (IUnknown iid) objState
		    -> Either LIBID String
		    -> IID (IUnknown iid)
		    -> IO (IUnknown iid)
createDualInterface ip_state vtbl libid iid = do
    ip     <- createIPointer ip_state vtbl
    st     <- mkDispatchState libid ip iid
    meths  <- unmarshallVTable vtbl
    let real_meths = 
	    case meths of
	      (qi : ar : re : ls) -> ls
	      _			  -> error "createDualInterface: failed to strip of IU methods"
    vtable <- createDispVTable real_meths st 
    cloneIPointer ip vtable

createDispInterface :: IUnknown iid   -- the interface to delegate to
		    -> Either LIBID String
			   -- libid of type library to use / path to where the .tlb is stored.
		    -> IID (IUnknown iid)
		           -- what interface it implements (needed to
			   -- get at the type library which drives
			   -- the dispatch interface.)
		    -> IO (IUnknown iid)
			   -- the dispatch implementation handed back.
createDispInterface ip libid iid = do
--    putMessage ("createDispInterface: " ++ show (libid, iid))
    st     <- mkDispatchState libid ip iid
--    putMessage ("createDispInterface: " ++ show (libid, iid))
    vtable <- createDispVTable [] st
--    putMessage ("createDispInterface: " ++ show (libid, iid))
    i <- cloneIPointer ip vtable
--    putMessage ("createDispInterface: " ++ show (libid, iid))
    return i

mkDispatchState :: Either LIBID String
		-> IUnknown iid
		-> IID (IUnknown iid)
		-> IO DispState
mkDispatchState libid ip iid = do
   pTInfo_ref <- newIORef nullPtr
   return (DispState libid (coerceIID iid) (coerceIP ip) pTInfo_ref)

--sigh.
coerceIID :: IID a -> IID b
coerceIID iid = guidToIID (iidToGUID iid)

coerceIP :: IUnknown a -> IUnknown b
coerceIP x = castIface x

data DispState
 = DispState {
       disp_libid :: Either LIBID String,
       disp_iid   :: (IID ()),
       disp_ip    :: (IUnknown ()),
       disp_ti    :: (IORef (PrimIP (ITypeInfo ())))
   }

type DISPPARAMS = Ptr () -- abstract, really.
type EXCEPINFO  = Ptr () -- abstract, really.

createDispVTable :: [Ptr ()] 
	         -> DispState
		 -> IO (ComVTable (IDispatch ()) DispState)
createDispVTable meths disp_st = do
  a_getTypeInfoCount <- export_getTypeInfoCount getTypeInfoCount
  a_getTypeInfo	     <- export_getTypeInfo      (getTypeInfo disp_st)
  a_getIDsOfNames    <- export_getIDsOfNames	(getIDsOfNames disp_st)
  a_invoke	     <- export_invoke		(invoke disp_st)
  createComVTable ([ a_getTypeInfoCount
		   , a_getTypeInfo
		   , a_getIDsOfNames
		   , a_invoke
		   ] ++ meths)

getTypeInfoCount :: Ptr () -> Ptr Word32 -> IO HRESULT
getTypeInfoCount iptr pctInfo = do
--  putMessage "getTypeInfoCount"
  writeWord32 pctInfo 1
  return s_OK

foreign export stdcall dynamic export_getTypeInfoCount
	    :: (Ptr () -> Ptr Word32 -> IO HRESULT) -> IO (Ptr ())

getTypeInfo :: DispState -> Ptr (IDispatch ()) -> Word32 -> LCID -> Ptr () -> IO HRESULT
getTypeInfo disp_state this iTInfo lcid ppTInfo
  | iTInfo /= 0         = return tYPE_E_ELEMENTNOTFOUND
  | ppTInfo == nullPtr  = return e_POINTER
  | otherwise		= do
--  putMessage "getTypeInfo"
  poke (castPtr ppTInfo) nullPtr
  let ppITInfo_ref = disp_ti disp_state
  (hr, pITInfo) <- do
    pITInfo     <- readIORef ppITInfo_ref
     -- load up the typelib is done the first time
     -- around. Cannot do it earlier (or lazily), since
     -- loading is dependent on the 'lcid'.
     --
     -- The caching of the ITypeInfo* only works because
     -- we keep a disp_state for each interface.
    if (pITInfo == nullPtr) then do
       ppITInfo <- allocOutPtr
       hr       <- loadTypeInfo (disp_libid disp_state) (disp_iid disp_state) lcid ppITInfo
--       putMessage ("getTypeInfo: " ++ show hr)
       if (failed hr) then
          return (hr, undefined)
        else do
         pITInfo  <- peek ppITInfo
         writeIORef ppITInfo_ref pITInfo
         return (s_OK, pITInfo)
     else
       return (s_OK, pITInfo)

    -- do an AddRef() since we're handing out a copy to it.
    -- => when the GetIDsOfNames() and Invoke() implementations
    --    below call getTypeInfo, they'll have to call Release()
    --    when finished with the result here.
    --
  if (failed hr) then
      return hr
   else do
     punk <- unmarshallIUnknown True{-addRef and finalise-} pITInfo
       -- to counter the effect of running the finaliser on pITInfo.
     Com.addRef punk
     poke (castPtr ppTInfo) pITInfo
     return s_OK

-- Loading the type info is lcid sensitive, so we
-- have to manually invoke this from within GetTypeInfo()
loadTypeInfo :: Either LIBID String
	     -> IID iid
	     -> LCID
	     -> Ptr (PrimIP (ITypeInfo ()))
	     -> IO HRESULT
loadTypeInfo tlb_loc iid lcid ppITI = do
   (hr, pITypeLib) <- 
    catch
     (case tlb_loc of
        Left libid -> do
	   ip <- loadRegTypeLib libid 1 0 (fromIntegral (primLangID lcid))
	   return (s_OK, ip)
	  -- load it in silently
        Right path -> do
           ip <- loadTypeLibEx path False{-don't register-}
	   return (s_OK, ip))
     (\ ex -> do
     	 putMessage "Failed to load typelib"
         return (fromMaybe e_FAIL (coGetErrorHR ex), interfaceNULL))
   if (failed hr) then
      return hr
    else do
      hr <- pITypeLib # getTypeInfoOfGuid iid ppITI
        -- pITypeLib is a finalised i-pointer, and will be
	-- released in due course.
	-- NOTE: potential bug farm.
      return hr

-- from oleauto.h
foreign import ccall "primLoadRegTypeLib" 
  primLoadRegTypeLib :: Ptr () -> Word16 -> Word16 -> Word32 -> Ptr () -> IO HRESULT

foreign export stdcall dynamic export_getTypeInfo
	    :: (Ptr (IDispatch ()) -> Word32 -> LCID -> Ptr () -> IO HRESULT) -> IO (Ptr ())

getIDsOfNames :: DispState
	      -> Ptr (IDispatch ())
	      -> Ptr (IID ())
	      -> Ptr WideString
	      -> Word32
	      -> LCID
	      -> Ptr DISPID
	      -> IO HRESULT
getIDsOfNames disp_state this riid rgszNames cNames lcid rgDispID = do
--    putMessage ("getIDs: " ++ show cNames)
    pti      <- allocOutPtr
    hr       <- getTypeInfo disp_state this 0 lcid pti
    if (failed hr) then do
       free pti
       return hr
     else do
       prim_ti <- peek (castPtr pti)
       free pti
--       pw      <- peek (castPtr rgszNames)
       hr      <- prim_ti # getIDsOfNamesTI rgszNames cNames rgDispID
       return hr

foreign export stdcall dynamic export_getIDsOfNames
	    :: (Ptr (IDispatch ()) -> Ptr (IID ()) -> Ptr WideString -> Word32 -> LCID -> Ptr DISPID -> IO HRESULT) -> IO (Ptr ())

invoke :: DispState
       -> Ptr (IDispatch ())
       -> DISPID
       -> Ptr (IID a)
       -> LCID
       -> Word32
       -> Ptr DISPPARAMS
       -> Ptr VARIANT
       -> Ptr EXCEPINFO
       -> Ptr Word32
       -> IO HRESULT
invoke disp_state this dispIdMember riid lcid wFlags pDispParams pVarResult pExcepInfo puArgErr = do
   iid <- unmarshallIID False riid
--   putMessage ("invoke: " ++ show (dispIdMember, iid))
   if (iid /= castIID iidNULL) then
      return dISP_E_UNKNOWNINTERFACE
    else do
      pti      <- allocOutPtr
      hr       <- getTypeInfo disp_state this 0 lcid pti
      if (failed hr) then do
         free pti
         return hr
       else do
        prim_ti  <- peek (castPtr pti)
        let ip = disp_ip disp_state
	  -- hand over to the typelib marshaller, but making sure that
	  -- any exceptions within the user code will be handled correctly.
        clearException
        hr <- prim_ti # invokeTI ip dispIdMember wFlags pDispParams pVarResult pExcepInfo puArgErr
        fillException pExcepInfo lcid
	ip <- unmarshallIUnknown False prim_ti
	ip # Com.release
        return hr

invokeTI :: IUnknown a
	 -> DISPID
	 -> Word32
	 -> Ptr DISPPARAMS
	 -> Ptr VARIANT
	 -> Ptr EXCEPINFO
	 -> Ptr Word32
	 -> Ptr (ITypeInfo a)
	 -> IO HRESULT
invokeTI ip dispIdMember wFlags pDispParams pVarResult pExcepInfo puArgErr this = do
  iptr_fo <- marshallIUnknown ip
  let offset    = (11::Int)
  lpVtbl  <- peek (castPtr this)
  methPtr <- indexPtr lpVtbl offset
  withForeignPtr iptr_fo $ \ iptr -> 
    prim_invokeTI methPtr (castPtr this) iptr dispIdMember wFlags pDispParams pVarResult pExcepInfo puArgErr

foreign import stdcall dynamic
  prim_invokeTI :: Ptr (ITypeInfo a) -> Ptr () -> Ptr () -> DISPID -> Word32 
                -> Ptr DISPPARAMS -> Ptr VARIANT -> Ptr EXCEPINFO -> Ptr Word32 -> IO HRESULT

getTypeInfoOfGuid :: IID iid -> Ptr (PrimIP (ITypeInfo ())) -> IUnknown a -> IO HRESULT
getTypeInfoOfGuid iid ppITI this = do
  let offset = (6::Int)
  pthis   <- marshallIUnknown this
  let a = foreignPtrToPtr pthis
  lpVtbl  <- peek (castPtr a)
  methPtr <- indexPtr lpVtbl offset
  piid	  <- marshallIID iid
  withForeignPtr pthis $ \ pthis -> 
   withForeignPtr piid  $ \ piid -> 
    prim_getTypeInfoOfGuid methPtr pthis piid ppITI

foreign import stdcall dynamic
  prim_getTypeInfoOfGuid :: Ptr () -> Ptr (Ptr a)
  			 -> Ptr (IID iid) -> Ptr (PrimIP (ITypeInfo ())) -> IO HRESULT

getIDsOfNamesTI :: Ptr WideString -> Word32 -> Ptr DISPID -> Ptr (ITypeInfo ()) -> IO HRESULT
getIDsOfNamesTI rgszNames cNames rgDispID this = do
  let offset = (10::Int)
  lpVtbl  <- peek (castPtr this)
  methPtr <- indexPtr lpVtbl offset
  prim_getIDsOfNamesTI methPtr (castPtr this) rgszNames cNames rgDispID

foreign import stdcall dynamic
  prim_getIDsOfNamesTI :: Ptr (ITypeInfo ()) -> Ptr () -> Ptr WideString -> Word32 -> Ptr DISPID -> IO HRESULT

clearException :: IO ()
clearException = return ()

fillException :: Ptr EXCEPINFO
	      -> LCID
	      -> IO ()
fillException _ _ = return ()
	      

foreign export stdcall dynamic export_invoke
   :: (Ptr (IDispatch ()) -> DISPID -> Ptr (IID a) -> LCID -> Word32 -> Ptr DISPPARAMS
   -> Ptr VARIANT -> Ptr EXCEPINFO -> Ptr Word32 -> IO HRESULT) -> IO (Ptr ())

data TypeInfo  a = TypeInfo__
type ITypeInfo a = IUnknown (TypeInfo a)

--makeLangID :: Word16 -> Word16 -> Word16
--makeLangID p s = (shiftL p 10) .|. s

primLangID :: Word32 -> Word32
primLangID w = (w .&. 0x3ff)
\end{code}

Manufacturing method tables out of a list of method pointers.

\begin{code}
createVTable :: [Ptr ()] -> IO (VTable iid objState)
createVTable methods = do
  vtbl <- alloc (sizeofPtr * fromIntegral no_meths)
  sequence (zipWith (pokeElemOff vtbl) [(0::Int)..] methods)
  return (vtbl, no_meths)
 where
  no_meths = length methods

unmarshallVTable :: VTable iid objState -> IO [Ptr ()]
unmarshallVTable (vtbl, no_meths) =
  mapM (peekElemOff vtbl) [(0::Int)..no_meths]

createComVTable :: [Ptr ()] -> IO (ComVTable iid objState)
createComVTable methods = do
  m_queryInterface <- export_queryInterface queryInterface
  m_addRef         <- export_addRef  addRef
  m_release        <- export_release release
  createVTable (m_queryInterface: m_addRef: m_release: methods)

\end{code}
