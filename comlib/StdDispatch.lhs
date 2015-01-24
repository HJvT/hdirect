%
% (c), Sigbjorn Finne, 1999-
%

Implementation of IDispatch in Haskell; pretty useful
when creating/registering dynamic, on-the-fly event
sinks.

ToDo: have HaskellDirect generate code which targets
this implementation, and (maybe) make this impl. the
default instead of the typelib-driven one.

\begin{code}
module StdDispatch
        ( createStdDispatchVTBL
        , createStdDispatchVTBL2
        
        , createStdDispatch

        , DispMethod(..)

        , inArg
        , inIUnknownArg
        , inoutArg
        , outArg
        , retVal

        , apply_0
        , apply_1
        , apply_2
        , apply_3
        , apply_4
        , apply_5
        , apply_6
        , apply_7

        , mkDispMethod
        , dispmethod_0_0
        , dispmethod_1_0
        , dispmethod_2_0
        , dispmethod_3_0

        , dispmethod_0_1
        , dispmethod_0_2

        ) where

import TypeLib hiding (DISPID,invoke, getIDsOfNames)
import ComServ
import Com
import Automation
import HDirect
import Foreign.Ptr
import Foreign.Storable
import ComException
import WideString
import Bits
import IOExts
import Int
import List ( find )

\end{code}

\begin{code}
createStdDispatch :: objState
                  -> IO ()
                  -> [DispMethod objState]
                  -> IID (IUnknown iid)
                  -> IO (IUnknown iid)
createStdDispatch objState final meths iid = do
  vtbl <- createStdDispatchVTBL2 meths
  createComInstance "" objState final
                       [ mkIface iid vtbl
                       , mkIface iidIDispatch vtbl
                       ]
                       iid
\end{code}


This presents a simple, no-fuss implementation of a custom
IDispatch interface. It takes care of the minutiae of
implementing IDispatch, but leaves the programmer with the
task of specifying the instance specific bits of: mapping
from method names to DISPIDs + the invocation function which
performs the action associated with a particular DISPID,
including the marshaling and unmarshaling of VARIANT args.

Abstractions with varying degress of sophistication can be
layered on top of this basic IDispatch impl - see 
<tt/createStdDispatchVTBL2/ for one way of doing this.

\begin{code}
createStdDispatchVTBL :: (String -> Maybe DISPID)
                      -> (DISPID -> MethodKind -> [VARIANT] -> objState -> IO (Maybe VARIANT))
                      -> IO (ComVTable (IDispatch iid) objState)
createStdDispatchVTBL meths fun = do
  a_getTypeInfoCount <- export_getTypeInfoCount getTypeInfoCount_none
  a_getTypeInfo      <- export_getTypeInfo      getTypeInfo_none
  a_getIDsOfNames    <- export_getIDsOfNames    (getIDsOfNames meths)
  a_invoke           <- export_invoke           (invoke fun)
  createComVTable 
           [ a_getTypeInfoCount
           , a_getTypeInfo
           , a_getIDsOfNames
           , a_invoke
           ]
\end{code}

evDisp = 
{-
  simpleDisp mapDISPIDs
             invokeDISPIDs
-}
  simpleDisp2 
     [ dispmethod_0_0 "DownloadComplete" 1 onDownloadComplete
     , dispmethod_0_0 "DownloadBegin"    2 onDownloadBegin
     ]
 where
  mapDISPIDs x =
    case x of
          "DownloadComplete" -> Just 1
          "DownloadBegin"    -> Just 2
          "ProgressChange"   -> Just 3
          _                  -> Nothing

  invokeDISPIDs st d mk args =
    case d of
          1 -> st # onDownloadComplete
          2 -> st # onDownloadBegin
          3 -> st # onProgressChange
          _ -> return Nothing -- silently ignore.
          
\begin{code}
data DispMethod objState
  = DispMethod {
       disp_method_name :: String,     -- method name
       disp_method_id   :: DISPID,     -- its ...
       disp_method_kind :: MethodKind, -- bit of a misnomer.
       disp_method_act  :: ([VARIANT] -> objState -> IO (Maybe VARIANT))
    }

mkDispMethod :: String
             -> DISPID
             -> ([VARIANT] -> objState -> IO (Maybe VARIANT))
             -> DispMethod objState
mkDispMethod nm d f = DispMethod nm d Method f

dispmethod_0_0 :: String -> DISPID -> (objState -> IO ()) -> DispMethod objState
dispmethod_0_0 name id f 
  = DispMethod name id Method (apply_0 f)
dispmethod_1_0 name id f 
  = DispMethod name id Method (inArg $ \ x -> apply_0 (f x))
dispmethod_2_0 name id f 
  = DispMethod name id Method 
               (inArg $ \ x -> inArg $ \ y -> apply_0 (f x y))
dispmethod_3_0 name id f 
  = DispMethod name id Method 
               (inArg $ \ x -> inArg $ \ y -> inArg $ \ z -> apply_0 (f x y z))

dispmethod_0_1 name id f 
  = DispMethod name id Method
               (outArg $ \ ret_x -> apply_1 f ret_x)

dispmethod_0_2 name id f 
  = DispMethod name id Method
               (outArg $ \ ret_x -> outArg $ \ ret_y -> apply_2 f ret_x ret_y)

-- and so on..

createStdDispatchVTBL2 :: [DispMethod objState]
                       -> IO (ComVTable (IDispatch iid) objState)
createStdDispatchVTBL2 assoc = createStdDispatchVTBL lookup_dispid invoke_meths
 where
  lookup_dispid m_nm = 
    case (find ((==m_nm).disp_method_name) assoc) of
          Nothing      -> Nothing
          Just d       -> Just (disp_method_id d)

  invoke_meths did mkind args obj_st = 
     case (find ((==did).disp_method_id) assoc) of
           Nothing      -> return Nothing
           Just d       -> (disp_method_act d) args obj_st

\end{code}

When using <tt/createStdDispatchVTBL2/, here's a couple
of helper functions to let you wrap up 

  HRESULT f([in]int i,[in,out]int* j,[out]int* pres, [out,retval]int* retval);

 wrap_f = 
  inArg     $ \ i       ->
  inArg     $ \ j j_out ->
  resArg    $ \ pres    ->
  retVal    $ \ retval  ->
  apply_3 (f i j) j_out pres retval

\begin{code}
inArg :: ( Variant a )
      => (a -> [VARIANT] -> objState -> IO b)
      -> [VARIANT]
      -> objState
      -> IO b
inArg cont (a:args) objState = do
   x <- resVariant a
   cont x args objState

inIUnknownArg :: (IUnknown a -> [VARIANT] -> objState -> IO b)
              -> [VARIANT]
              -> objState
              -> IO b
inIUnknownArg cont (a:args) objState = do
   x <- resIUnknown a
   cont x args objState

inoutArg :: ( Variant a, Variant b )
         => ( a -> (b -> IO ()) -> [VARIANT] -> objState -> IO c)
         -> [VARIANT]
         -> objState
         -> IO c
inoutArg cont (a:args) objState = do
   x <- resVariant a
   cont x (\ x -> inVariant x a) args objState

outArg    :: ( Variant a )
          => ((a -> IO ()) -> [VARIANT] -> objState -> IO b)
          -> [VARIANT]
          -> objState
          -> IO b
outArg cont (a:args) objState = cont (\ x -> inVariant x a) args objState

retVal  :: ( Variant a )
        => ((a -> IO ()) -> [VARIANT] -> objState -> IO (Maybe VARIANT))
        -> [VARIANT]
        -> objState
        -> IO (Maybe VARIANT)
retVal cont args objState = do
   -- what a hack.
  res_ref <- newIORef Nothing
  cont (\ x -> writeIORef res_ref (Just x)) args objState 
  retv <- readIORef res_ref
  case retv of
    Nothing -> return Nothing
    Just x  -> do
      pVarResult <- allocBytes (fromIntegral sizeofVARIANT)
      inVariant x pVarResult
      return (Just pVarResult)

apply_0 :: (objState -> IO ())
        -> [VARIANT]
        -> objState
        -> IO (Maybe VARIANT)
apply_0 f _ objState = f objState >> return Nothing

apply_1 :: (Variant a)
        => (objState -> IO a)
        -> (a -> IO ())
        -> [VARIANT]
        -> objState
        -> IO (Maybe VARIANT)
apply_1 f res_1 _ objState = do
   x <- f objState
   res_1 x
   return Nothing

apply_2 :: (Variant a0, Variant a1)
        => (objState -> IO (a0,a1))
        -> (a0 -> IO ())
        -> (a1 -> IO ())
        -> [VARIANT]
        -> objState
        -> IO (Maybe VARIANT)
apply_2 f res_1 res_2 _ objState = do
   (x,y) <- f objState
   res_1 x
   res_2 y
   return Nothing

apply_3 :: (Variant a0, Variant a1, Variant a2)
        => (objState -> IO (a0,a1,a2))
        -> (a0 -> IO ())
        -> (a1 -> IO ())
        -> (a2 -> IO ())
        -> [VARIANT]
        -> objState
        -> IO (Maybe VARIANT)
apply_3 f res_1 res_2 res_3 _ objState = do
   (a0,a1,a2) <- f objState
   res_1 a0
   res_2 a1
   res_3 a2
   return Nothing

apply_4 :: (Variant a0, Variant a1, Variant a2, Variant a3)
        => (objState -> IO (a0,a1,a2,a3))
        -> (a0 -> IO ())
        -> (a1 -> IO ())
        -> (a2 -> IO ())
        -> (a3 -> IO ())
        -> [VARIANT]
        -> objState
        -> IO (Maybe VARIANT)
apply_4 f res_1 res_2 res_3 res_4 _ objState = do
   (a0,a1,a2,a3) <- f objState
   res_1 a0
   res_2 a1
   res_3 a2
   res_4 a3
   return Nothing

apply_5 :: (Variant a0, Variant a1, Variant a2, Variant a3, Variant a4)
        => (objState -> IO (a0,a1,a2,a3,a4))
        -> (a0 -> IO ())
        -> (a1 -> IO ())
        -> (a2 -> IO ())
        -> (a3 -> IO ())
        -> (a4 -> IO ())
        -> [VARIANT]
        -> objState
        -> IO (Maybe VARIANT)
apply_5 f res_1 res_2 res_3 res_4 res_5 _ objState = do
   (a0,a1,a2,a3,a4) <- f objState
   res_1 a0
   res_2 a1
   res_3 a2
   res_4 a3
   res_5 a4
   return Nothing

apply_6 :: (Variant a0, Variant a1, Variant a2, Variant a3, Variant a4, Variant a5)
        => (objState -> IO (a0,a1,a2,a3,a4,a5))
        -> (a0 -> IO ())
        -> (a1 -> IO ())
        -> (a2 -> IO ())
        -> (a3 -> IO ())
        -> (a4 -> IO ())
        -> (a5 -> IO ())
        -> [VARIANT]
        -> objState
        -> IO (Maybe VARIANT)
apply_6 f res_1 res_2 res_3 res_4 res_5 res_6 _ objState = do
   (a0,a1,a2,a3,a4,a5) <- f objState
   res_1 a0
   res_2 a1
   res_3 a2
   res_4 a3
   res_5 a4
   res_6 a5
   return Nothing

apply_7 :: (Variant a0, Variant a1, Variant a2, Variant a3, Variant a4, Variant a5, Variant a6)
        => (objState -> IO (a0,a1,a2,a3,a4,a5,a6))
        -> (a0 -> IO ())
        -> (a1 -> IO ())
        -> (a2 -> IO ())
        -> (a3 -> IO ())
        -> (a4 -> IO ())
        -> (a5 -> IO ())
        -> (a6 -> IO ())
        -> [VARIANT]
        -> objState
        -> IO (Maybe VARIANT)
apply_7 f res_1 res_2 res_3 res_4 res_5 res_6 res_7 _ objState = do
   (a0,a1,a2,a3,a4,a5,a6) <- f objState
   res_1 a0
   res_2 a1
   res_3 a2
   res_4 a3
   res_5 a4
   res_6 a5
   res_7 a6
   return Nothing

\end{code}


\begin{code}
type ThisPtr a = Ptr a

invoke :: (DISPID -> MethodKind -> [VARIANT] -> objState -> IO (Maybe VARIANT))
       -> Ptr (IDispatch ())
       -> DISPID
       -> Ptr (IID ())
       -> LCID
       -> Word32
       -> Ptr (Ptr ()) --DISPPARAMS
       -> Ptr VARIANT
       -> Ptr (Ptr ()) --EXCEPINFO
       -> Ptr Word32
       -> IO HRESULT
invoke dispMeth this dispIdMember riid lcid wFlags pDispParams
       pVarResult pExcepInfo puArgErr = do
   iid <- unmarshallIID False riid
   if (iid /= iidNULL) then
      return dISP_E_UNKNOWNINTERFACE
    else do
      let mkind = toMethodKind wFlags
          args  = unsafePerformIO (unmarshallArgs (castPtr pDispParams))
      st  <- getObjState this
      catch (do
        res <- dispMeth dispIdMember mkind args st
        case res of 
          Nothing -> do
            if pVarResult == nullPtr then
               return ()
             else
               poke pVarResult nullPtr
          Just x  -> do
--           putMessage ("Invoke: " ++ show dispIdMember ++ " interested")
            writeVARIANT pVarResult x
        return s_OK) 
       (\ err -> 
          case coGetErrorHR err of
            Nothing -> do
              if pExcepInfo == nullPtr then
                 return dISP_E_EXCEPTION
               else do
                 poke (castPtr pExcepInfo) nullPtr
                 return dISP_E_EXCEPTION
            Just hr ->
              -- ToDo: a lot better.
              if pExcepInfo == nullPtr then
                 return hr
               else do
                 poke (castPtr pExcepInfo) nullPtr
                 return hr)
              

unmarshallArgs :: Ptr DISPPARAMS -> IO [VARIANT]
unmarshallArgs ptr 
  | ptr == nullPtr = return []
  | otherwise       = do
     dp <- readDISPPARAMS (castPtr ptr)
     case dp of
       (TagDISPPARAMS rs _) -> return rs


data MethodKind = Method | PropertyGet | PropertyPut

toMethodKind :: Word32 -> MethodKind
toMethodKind x
  | x .&. dISPATCH_METHOD         /= 0 = Method
  | x .&. dISPATCH_PROPERTYGET    /= 0 = PropertyGet
  | x .&. dISPATCH_PROPERTYPUT    /= 0 = PropertyPut
  | x .&. dISPATCH_PROPERTYPUTREF /= 0 = PropertyPut
  | otherwise                          = Method

dISPATCH_METHOD :: Word32
dISPATCH_METHOD = 0x1
dISPATCH_PROPERTYGET :: Word32
dISPATCH_PROPERTYGET = 0x2
dISPATCH_PROPERTYPUT :: Word32
dISPATCH_PROPERTYPUT = 0x4
dISPATCH_PROPERTYPUTREF :: Word32
dISPATCH_PROPERTYPUTREF = 0x8

\end{code}

\begin{code}
getIDsOfNames :: (String -> Maybe DISPID)
              -> ThisPtr (IDispatch ())
              -> Ptr (IID ())
              -> Ptr WideString
              -> Word32
              -> LCID
              -> Ptr DISPID
              -> IO HRESULT
getIDsOfNames lookup_dispid this riid rgszNames cNames lcid rgDispID
  | cNames /= 1 = return e_FAIL
  | otherwise   = do
      pwide     <- peek (castPtr rgszNames)
      (pstr,hr) <- wideToString pwide
      checkHR hr
      str       <- unmarshallString (castPtr pstr)
      case lookup_dispid str of
        Nothing -> return e_FAIL
        Just v  -> do
           writeInt32 rgDispID v
           return s_OK

\end{code}

We don't expose type information of any sort, so 
here's the negative versions of the two IDispatch
methods.

\begin{code}
getTypeInfoCount_none :: Ptr () -> Ptr Word32 -> IO HRESULT
getTypeInfoCount_none iptr pctInfo = do
  writeWord32 pctInfo 0
  return s_OK

getTypeInfo_none :: Ptr (IDispatch ()) -> Word32 -> LCID -> Ptr () -> IO HRESULT
getTypeInfo_none this iTInfo lcid ppTInfo
  | iTInfo /= 0         = return tYPE_E_ELEMENTNOTFOUND
  | ppTInfo == nullPtr = return e_POINTER
  | otherwise           = do
     poke (castPtr ppTInfo) nullPtr
     return s_OK

\end{code}
