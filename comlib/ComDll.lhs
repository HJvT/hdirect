
Support for sealing up Haskell code as an in-proc COM server.

The export list is odd in that it doesn't export the Haskell functions 
required to implement the class factory, but rather an action which
exposes them via a COM-like interface.

\begin{code}
{-# OPTIONS -#include "ComDll_stub.h" -#include <windows.h> -#include "Registry.h" #-}
module ComDll
        (  
           ComponentInfo(..)
        ,  mkComponentInfo
        ,  withComponentName
        ,  withProgID
        ,  withVerIndepProgID
        ,  onRegister
        ,  onFinalize
        ,  hasTypeLib

        ,  createIComDll   -- :: Ptr (){-HMODULE-} -> [ComponentInfo] -> IO

        ,  regAddEntry
        ,  regRemoveEntry
        ,  RegHive(..)
        
        ,  stdRegComponent
        ,  stdUnRegComponent
        ) where

import ClassFactory
import Com
import ComException
import ComServ
import ComPrim ( getModuleFileName )
import HDirect ( Ptr, marshallString )

import Foreign  hiding ( Ptr )
import Word ( Word32 )
import HDirect ( marshallMaybe )
import IOExts

import Monad
import List  ( find )

\end{code}

The information an implementation of a Haskell COM component
needs to supply in order to hook into the machinery provided
here for interfacing to how COM does activation for in-proc
components:

\begin{code}
data ComponentInfo
  = ComponentInfo
      { newInstance       :: String -> IO () -> IID (IUnknown ()) -> IO (IUnknown ())
      , componentFinalise :: IO ()
      , componentName     :: String
      , componentProgID   :: String
      , componentVProgID  :: String
      , componentTLB      :: Bool
      , registerComponent :: ComponentInfo -> String -> Bool -> IO ()
      , componentCLSID    :: CLSID
      }

withProgID :: String -> ComponentInfo -> ComponentInfo
withProgID p info = info{componentProgID=p}

onRegister :: (ComponentInfo -> String -> Bool -> IO ()) -> ComponentInfo -> ComponentInfo
onRegister reg info = 
  info{registerComponent= \ a b c -> reg a b c >> (registerComponent info) a b c}

onFinalize :: IO () -> ComponentInfo -> ComponentInfo
onFinalize act info = info{componentFinalise= act >> (componentFinalise info)}

withVerIndepProgID :: String -> ComponentInfo -> ComponentInfo
withVerIndepProgID p info = info{componentVProgID=p}

withFinaliser :: IO () -> ComponentInfo -> ComponentInfo
withFinaliser act info = info{componentFinalise=act}

withComponentName :: String -> ComponentInfo -> ComponentInfo
withComponentName n info = info{componentName=n}

hasTypeLib :: ComponentInfo -> ComponentInfo
hasTypeLib info = info{componentTLB=True}

 -- constructor used to lessen the reliance on concrete rep.
mkComponentInfo :: CLSID
                -> (String -> Bool -> IO ())
                -> (String -> IO () -> IID (IUnknown ()) -> IO (IUnknown ()))
                -> ComponentInfo
mkComponentInfo cls reg n = ComponentInfo n (return ()) "" "" "" False (\ _ -> reg) cls
\end{code}

The state maintained by each instance of a 'ComDll' wrapper:

\begin{code}
data ComDllState
  = ComDllState {
      dllPath    :: String,
      components :: IORef [ComponentInfo],
      lockCount  :: IORef Int
     }
\end{code}


\begin{code}
dllGetClassObject :: ComDllState -> Ptr CLSID -> Ptr (IID a) -> Ptr (Ptr (IUnknown a)) -> IO HRESULT
dllGetClassObject comDll rclsid riid ppvObject = do
  iid <- unmarshallIID False (castPtr riid)
  let g = iidToGUID iid
  if ( not (g == iidToGUID iidIClassFactory || g == iidToGUID iidIUnknown) ) then
     return e_NOINTERFACE
   else do
    clsid <- unmarshallCLSID False rclsid
    cs    <- readIORef (components comDll)
    case lookupCLSID clsid cs of
      Nothing -> return cLASS_E_CLASSNOTAVAILABLE
      Just i  -> do
         ip <- createClassFactory (newInstance i (dllPath comDll) (componentFinalise i))
         writeIUnknown False ppvObject ip
         return s_OK

lookupCLSID :: CLSID -> [ComponentInfo] -> Maybe ComponentInfo
lookupCLSID clsid cs = find (\ x -> clsidToGUID (componentCLSID x) == guid) cs
 where
  guid = clsidToGUID clsid

dllCanUnloadNow :: ComDllState -> IO HRESULT
dllCanUnloadNow state = do
   c <- readIORef (lockCount state)
   if c == 0 then 
     return s_OK
    else
     return s_FALSE

dllRegisterServer :: ComDllState -> IO HRESULT
dllRegisterServer = registerServer True

dllUnregisterServer :: ComDllState -> IO HRESULT
dllUnregisterServer = registerServer False

registerServer :: Bool -> ComDllState -> IO HRESULT
registerServer isReg st = do
  cs   <- readIORef (components st)
  let
   path = dllPath st
   regComponent info
     | not isReg = do
         -- give the user-supplied un-reg action the opportunity
         -- to delete some entries first.
       (registerComponent info) info path isReg
       stdUnRegComponent info True path
     | otherwise = do
       stdRegComponent info True path
       (registerComponent info) info path isReg


  mapM_ regComponent cs
  case s_OK of
    14 -> return s_OK
    x  -> return x

dllUnload :: ComDllState -> IO ()
dllUnload st = return ()

\end{code}

Creating + manipulating the state of a Haskell DLL containing
Haskell COM server(s).

\begin{code}
newComDllState :: Ptr (){-HANDLE-} -> [ComponentInfo] -> IO ComDllState
newComDllState hMod cs = do
  path   <- getModuleFileName hMod
  ref_cs <- newIORef cs
    -- The lock count is intended used by DllCanUnloadNow() to keep
    -- track of when the Com DLL can safely be unloaded. It is only
    -- safe to do so if no COM interface pointers handed out by the 
    -- component are currently alive.
    --
    -- The Com library doesn't yet try to keep track of this, so the
    -- lock count is always left at 1 (==> DllCanUnloadNow() always returns
    -- S_FALSE.)
  lc     <- newIORef 1
  return (ComDllState path ref_cs lc)
\end{code}

\begin{code}
createIComDll :: Ptr (){-HMODULE-} -> [ComponentInfo] -> IO (VTable iid_comDllState ComDllState)
createIComDll hMod components = do
   state       <- newComDllState hMod components
   meths       <- iComDllEntryPoints state
   createVTable meths

iComDllEntryPoints :: ComDllState -> IO [Ptr ()]
iComDllEntryPoints state = do
  addrOf_DllUnload           <- export_DllUnload   (dllUnload state)
  addrOf_DllCanUnloadNow     <- export_nullaryMeth (dllCanUnloadNow state)
  addrOf_DllRegisterServer   <- export_nullaryMeth (dllRegisterServer state)
  addrOf_DllUnregisterServer <- export_nullaryMeth (dllUnregisterServer state)
  addrOf_DllGetClassObject   <- export_dllGetClassObject (dllGetClassObject state)
  return [ addrOf_DllUnload
         , addrOf_DllCanUnloadNow
         , addrOf_DllRegisterServer
         , addrOf_DllUnregisterServer
         , addrOf_DllGetClassObject
         ]

foreign export ccall dynamic
   export_DllUnload :: (IO ()) -> IO (Ptr ())
foreign export ccall dynamic 
   export_nullaryMeth :: (IO HRESULT) -> IO (Ptr ())

foreign export ccall dynamic
   export_dllGetClassObject :: (Ptr CLSID -> Ptr (IID a) -> Ptr (Ptr (IUnknown a)) -> IO HRESULT) -> IO (Ptr ())

\end{code}

\begin{code}
data RegHive
 = HKEY_CLASSES_ROOT
 | HKEY_CURRENT_USER
 | HKEY_LOCAL_MACHINE
 | HKEY_USERS
 | HKEY_CURRENT_CONFIG
   deriving ( Eq, Ord, Enum )

regAddEntry :: RegHive
            -> String
            -> Maybe String
            -> IO ()
regAddEntry hive path value = do
   m_path  <- marshallString path
   m_value <- marshallMaybe marshallString nullPtr value
   hr      <- primRegAddEntry (fromEnum hive) m_path m_value
   checkHR hr

regRemoveEntry :: RegHive
               -> String
               -> String
               -> Bool
               -> IO ()
regRemoveEntry hive path value removeKey = do
   m_path  <- marshallString path
   m_value <- marshallString value
   let m_removeKey
        | removeKey = (1::Int)
        | otherwise = 0
   hr      <- primRegRemoveEntry (fromEnum hive) m_path m_value m_removeKey
   checkHR hr

foreign import ccall "primRegAddEntry" 
   primRegAddEntry :: Int -> Ptr String -> Ptr String -> IO HRESULT
foreign import ccall "primRegRemoveEntry" 
   primRegRemoveEntry :: Int -> Ptr String -> Ptr String -> Int -> IO HRESULT

\end{code}

\begin{code}
stdRegComponent :: ComponentInfo -> Bool -> String -> IO ()
stdRegComponent info isInProc path = do
   let clsid_path = "CLSID\\" ++ clsid_str
       progid     = componentProgID info
       vprogid    = componentVProgID info
       clsid_str  = show (componentCLSID info)

        -- Add CLSID\{clsid}\friendly name
   regAddEntry HKEY_CLASSES_ROOT clsid_path (Just (componentName info))
        -- Add CLSID\{clsid}\ProgID (if any.)
   when (not (null progid)) (regAddEntry HKEY_CLASSES_ROOT (clsid_path++"\\ProgID") (Just progid))
        -- Add CLSID\{clsid}\VersionIndependentProgID (if any.)
   when (not (null vprogid)) (regAddEntry HKEY_CLASSES_ROOT (clsid_path++"\\VersionIndependentProgID") (Just vprogid))
        -- Add CLSID\{clsid}\{Inproc,Local}Server32\path
   regAddEntry HKEY_CLASSES_ROOT (clsid_path ++ (if isInProc then "\\InprocServer32" else "\\LocalServer32")) (Just path)

      -- register the type library; we don't care if it fails, or not.
   when (componentTLB info)
        (catch (loadTypeLibEx path True{-register-} >>= \ p -> p # release >> return ())
               (\ _ -> return ()))

        -- Add the ProgID entries

   when (not (null progid))  (regAddEntry HKEY_CLASSES_ROOT (progid  ++ "\\CLSID") (Just clsid_str))
   when (not (null vprogid)) (regAddEntry HKEY_CLASSES_ROOT (vprogid ++ "\\CLSID") (Just clsid_str))
   when (not (null vprogid) && not (null progid))
                             (regAddEntry HKEY_CLASSES_ROOT (progid ++ "\\CurVer") (Just vprogid))
   return ()

\end{code}

The removal of entries isn't quite right

\begin{code}
stdUnRegComponent :: ComponentInfo -> Bool -> String -> IO ()
stdUnRegComponent info isInProc path = do
   let clsid_path = "CLSID\\" ++ clsid_str
       progid     = componentProgID info
       vprogid    = componentVProgID info
       clsid_str  = show (componentCLSID info)

        -- Remove CLSID\{clsid}\{Local,Inproc}Server32
   regRemoveEntry HKEY_CLASSES_ROOT clsid_path (if isInProc then "InprocServer32" else "LocalServer32") True
        -- Remove CLSID\{clsid}\VersionIndependentProgID (if any.)
   when (not (null vprogid)) (regRemoveEntry HKEY_CLASSES_ROOT clsid_path "VersionIndependentProgID" True)
        -- Remove CLSID\{clsid}\ProgID (if any.)
   when (not (null progid)) (regRemoveEntry HKEY_CLASSES_ROOT clsid_path "ProgID" True)
        -- Remove CLSID\{clsid}\friendly name
   regRemoveEntry HKEY_CLASSES_ROOT "CLSID" clsid_str True

        -- Remove the ProgID entries
   when (not (null progid))  (regRemoveEntry HKEY_CLASSES_ROOT (progid  ++ "\\CLSID") clsid_str False)
   when (not (null progid))  (regRemoveEntry HKEY_CLASSES_ROOT progid  "CLSID"  True)
   when (not (null progid) && not (null vprogid)) 
                             (regRemoveEntry HKEY_CLASSES_ROOT  progid "CurVer" True)
   when (not (null vprogid)) (regRemoveEntry HKEY_CLASSES_ROOT (vprogid ++ "\\CLSID") clsid_str False)
   when (not (null vprogid)) (regRemoveEntry HKEY_CLASSES_ROOT vprogid  "CLSID" True)
   return ()

\end{code}
