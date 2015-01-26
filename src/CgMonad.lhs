%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Feb. 9th 2003  14:49  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

The code generator carries around information about
the abstract Haskell code that's being generated:

    - the export list so far.
    - the context we're in (e.g., are we processing
      a COM method)
    - etc.

\begin{code}
module CgMonad 
        (
          CgM
        , IfaceType(..)
        , runCgM

        , getDllName            -- :: CgM String
        , setDllName            -- :: String -> CgM a -> CgM a

           -- decl name is the name of the IDL unit (i.e.,
           -- module/interface etc.) being translated.
           --
        , getDeclName           -- :: (String -> CgM a) -> CgM a
        , withDeclName          -- :: String -> CgM a -> CgM a
        , withIfaceDeclName     -- :: String -> CgM a -> CgM a
        
        , needStubs             -- :: Bool -> CgM ()
        , hasPrims              -- :: CgM ()

        , setInterfaceFlag      -- :: IfaceType -> CgM a -> CgM a
        , getInterfaceFlag      -- :: CgM IfaceType

        , setSourceIfaceFlag    -- :: Bool -> CgM a -> CgM a
        , getSourceIfaceFlag    -- :: CgM Bool

        , setClientFlag         -- :: Bool -> CgM a -> CgM a
        , getClientFlag         -- :: CgM Bool

        , getIfaceName          -- :: CgM String
        , setIfaceName          -- :: String -> CgM a -> CgM a
        
        , inDispInterface       -- :: CgM a -> CgM a
        , isInDispInterface     -- :: CgM Bool
        
        , setIfaceAttributes    -- :: [Attribute] -> CgM a -> CgM a
        , getIfaceAttributes    -- :: CgM [Attribute]
        
        , getIfaceInherit       -- :: CgM [QualName]
        , withIfaceInherit      -- :: [QualNam] -> CgM a -> CgM a
        
        , IsoEnv
        , getIsoEnv             -- :: CgM IsoEnv
        , setIsoEnv             -- :: IsoEnv -> CgM ()
        
        , getIEnumFlag          -- :: CgM Bool
        , setIEnumFlag          -- :: Bool -> CgM a -> CgM a
        
        , addDynStub            -- :: String -> String -> CgM ()
        , lookupDynStub         -- :: String -> Maybe String

        , addExport             -- :: HIEEntity -> CgM ()
        , addVitalExport        -- :: HIEEntity -> CgM ()
        , addExportWithComment  -- :: HIEEntity -> String -> CgM ()
        , exportDecl            -- :: (String, HDecl) -> CgM HDecl

        , addExplicitImports    -- :: [(Bool,String)] -> CgM ()
        
        , hoistInClass          -- :: String -> (String -> CgM a) -> CgM a
        
        , getMethodNumber       -- :: Maybe Int -> CgM Int
        , setMethodNumber       -- :: Int -> CgM ()
        , incMethodNumber       -- :: CgM ()

        ) where

import Env
import AbstractH
import CoreIDL    ( Result, Param, Id, Attribute )
import Opts       ( optServer, optOneModulePerInterface )
import Data.Maybe         ( fromMaybe )
import BasicTypes ( QualName )
import Control.Applicative
import Control.Monad ( ap, liftM )

\end{code}

Information is carried both down and along:

\begin{code}
newtype CgM a = CgM ( CgDown -> CgState -> (a, CgState) )

data CgDown 
 = CgDown {
      if_ty     :: IfaceType,
      if_client :: Bool,   -- True => generating client stubs.
      if_source :: Bool,   -- True => processing outgoing interface
      if_ienum  :: Bool,   -- True => processing IEnum interface
      if_disp   :: Bool,
      dll_nm    :: String,
      mod_nm    :: String,
      if_nm     :: String,
      if_attrs  :: [Attribute],
      if_inh    :: [QualName],
      iface_env :: Env String (Maybe Id)
   }

data IfaceType
 = StdFFI
 | VTBLObject         -- client
 | ComIDispatch Bool  -- client, True => [dual]
   deriving Eq

type IsoEnv = Env String [(Bool, Result, [Param])]

data CgState = 
   CgState {
     exp_list   :: [(HIEEntity, Bool, Maybe String)],  -- export list
     imp_list   :: [(String,Bool,[HIEEntity])],
     dyn_env    :: Env String{-'signature' string of a type-} (Bool, String),
     iso_env    :: IsoEnv,
     meth_no    :: !Int,
     need_stubs :: Bool,
     has_prims  :: Bool
   }

runCgM :: Env String [(Result, [Param])]
       -> Env String (Maybe Id)
       -> CgM a
       -> ( a
          , [(HIEEntity, Bool, Maybe String)]
          , [(String, Bool, [HIEEntity])]
          , Bool
          , Bool
          )
runCgM isoEnv ifaceEnv (CgM act) = 
  case (act (CgDown iface_flg is_client is_source is_ienum 
                    is_disp "" "" "" [] [] ifaceEnv)
            (CgState [] [] newEnv iso_env' 0 False False)) of
    (v,CgState expo imps _ _ _ flg1 flg2) -> (v, reverse expo, imps, flg1, flg2)
 where
  is_client     = not optServer
  is_ienum      = False
  is_source     = False
  is_disp       = False
  iface_flg     = StdFFI

  iso_env' = mapEnv (\ _ ls -> map (\ (as,bs) -> (True, as, bs)) ls) isoEnv

getDllName :: CgM String
getDllName = CgM (\ env st -> (dll_nm env, st))

setDllName :: String -> CgM a -> CgM a
setDllName dname (CgM a) = CgM (\ env st -> a (env{dll_nm=dname}) st)

getDeclName :: (String -> CgM a) -> CgM a
getDeclName cont = CgM (\ env st -> let (CgM a) = cont (mod_nm env) in a env st)

needStubs :: Bool -> CgM ()
needStubs flg = CgM (\ _ st -> ((), st{need_stubs=need_stubs st || flg}))

hasPrims :: CgM ()
hasPrims = CgM (\ _ st -> ((), st{has_prims=True}))

withDeclName :: String -> CgM a -> CgM a
withDeclName mname (CgM a) = CgM (\ env st -> a (env{mod_nm=mname}) st)

withIfaceDeclName :: String -> CgM a -> CgM a
withIfaceDeclName mname act
  | not optOneModulePerInterface = act
  | otherwise                    = withDeclName mname act

setInterfaceFlag :: IfaceType -> CgM a -> CgM a
setInterfaceFlag flg (CgM a) = CgM (\ env st -> a (env{if_ty=flg}) st)

setClientFlag :: Bool -> CgM a -> CgM a
setClientFlag flg (CgM a) = CgM (\ env st -> a (env{if_client=flg}) st)

getClientFlag :: CgM Bool
getClientFlag = CgM (\ env st -> (if_client env, st))

getInterfaceFlag :: CgM IfaceType
getInterfaceFlag = CgM (\ env st -> (if_ty env, st))

getIfaceName :: CgM String
getIfaceName = CgM (\ env st -> (if_nm env, st))

getIfaceAttributes :: CgM [Attribute]
getIfaceAttributes = CgM (\ env st -> (if_attrs env, st))

getIfaceInherit :: CgM [QualName]
getIfaceInherit = CgM (\ env st -> (if_inh env, st))

withIfaceInherit :: [QualName] -> CgM a -> CgM a
withIfaceInherit ls (CgM a) =
  CgM (\ env st -> a (env{if_inh=ls}) st)

setIfaceName :: String -> CgM a -> CgM a  
setIfaceName iface (CgM a) =
  CgM (\ env st -> a (env{if_nm=iface}) st)

setIfaceAttributes :: [Attribute] -> CgM a -> CgM a  
setIfaceAttributes as (CgM a) = CgM (\ env st -> a (env{if_attrs=as}) st)

getIsoEnv :: CgM IsoEnv
getIsoEnv = CgM (\ _ st -> (iso_env st, st))

setIsoEnv :: IsoEnv -> CgM ()
setIsoEnv env = CgM (\ _ st -> ((),st{iso_env=env}))

getIEnumFlag :: CgM Bool
getIEnumFlag = CgM (\ env st -> (if_ienum env, st))

setIEnumFlag :: Bool -> CgM a -> CgM a
setIEnumFlag i (CgM a) = CgM (\ env st -> a (env{if_ienum=i}) st)

getSourceIfaceFlag :: CgM Bool
getSourceIfaceFlag = CgM (\ env st -> (if_source env, st))

setSourceIfaceFlag :: Bool -> CgM a -> CgM a
setSourceIfaceFlag i (CgM a) = CgM (\ env st -> a (env{if_source=i}) st)

addExport :: HIEEntity -> CgM ()
addExport nm = CgM ( \ _ st -> ((), st{exp_list=(nm, False, Nothing):exp_list st}))

addVitalExport :: HIEEntity -> CgM ()
addVitalExport nm = CgM ( \ _ st -> ((), st{exp_list=(nm, True, Nothing):exp_list st}))

hoistInClass :: String -> (Maybe Id -> CgM a) -> CgM a 
hoistInClass nm cont =  
  CgM (\ env st -> 
         let 
           (CgM a) = cont (fromMaybe Nothing (lookupEnv (iface_env env) nm))
         in
         a env st)

addExportWithComment :: HIEEntity -> String -> CgM ()
addExportWithComment nm comm =
 CgM ( \ _ st -> ((), st{exp_list=(nm, False, Just comm):exp_list st}))

addExplicitImports :: [(Bool,String)] -> CgM ()
addExplicitImports imps = CgM ( \ _ st -> ((), st{imp_list= map (\ (x,y) -> (y,x,[])) imps ++imp_list st}))

exportDecl :: (String, HDecl) -> CgM HDecl
exportDecl (nm,d) = do
   addExport (IEVal nm)
   return d

-- 'convenient' interface that combines the result of
-- looking up whether 
getMethodNumber :: Maybe Int -> CgM Int
getMethodNumber (Just i) = return i
getMethodNumber Nothing  = CgM (\ _ st -> (meth_no st, st))

incMethodNumber :: CgM ()
incMethodNumber = CgM (\ _ st -> ((), st{meth_no=1+meth_no st}))

setMethodNumber :: Int -> CgM ()
setMethodNumber n = CgM (\ _ st -> ((), st{meth_no=n}))
\end{code}

\begin{code}
inDispInterface :: CgM a -> CgM a
inDispInterface (CgM a) = CgM (\ env st -> a (env{if_disp=True}) st)

isInDispInterface :: CgM Bool
isInDispInterface = CgM (\ env st -> (if_disp env, st))

\end{code}

Within a Haskell module we share 'foreign export dynamic'
(or equivalent) stubs, if possible.

\begin{code}
addDynStub :: String -> String -> Bool -> CgM ()
addDynStub nm sig is_exp = CgM $ \ _ st ->
   ((),st{dyn_env=addToEnv (dyn_env st) sig (is_exp,nm)})

lookupDynStub :: String -> CgM (Maybe (Bool, String))
lookupDynStub sig = CgM $ \ _ st ->
   (lookupEnv (dyn_env st) sig, st)

\end{code}


\begin{code}
instance Monad CgM where
  (>>=) (CgM a) f = 
    CgM (\ env st -> 
           case a env st of
             (v, st1) -> let CgM b = f v in b env st1)
  return v = CgM (\ _ st -> (v, st))

instance Applicative CgM where
    pure  = return
    (<*>) = ap

instance Functor CgM where
  fmap = liftM

\end{code}

