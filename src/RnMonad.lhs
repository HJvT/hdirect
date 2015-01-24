% 
% (c) sof 1999-
%
% @(#) $Docid: Sep. 18th 2001  09:28  Sigbjorn Finne $
% @(#) $Contactid: sof@galconn.com $
%

The 'Renamer' monad - carrying around the environments needed to
turn a set of IDL decls into a set of uniquely named Haskell decls.

\begin{code}
module RnMonad 
        (
          RnM
        , runRnM           -- :: TypeEnv -> TagEnv -> SourceEnv 
                           -- -> RnM a -> (a, IsoEnv, IfaceNukeEnv)
        , lookupTypeId     -- :: Name -> RnM (Maybe (Maybe String, Type))
        , lookupIface      -- :: Name -> RnM (Maybe Decl)
        , lookupTag        -- :: Name -> RnM (Maybe (Maybe String, String))

        , getMethOffset    -- :: RnM (Maybe Int)
        , setMethOffset    -- :: Maybe Int -> RnM a -> RnM a
        , incMethOffset    -- :: RnM ()

        , withNewVarIdEnv  -- :: RnM a -> RnM a
        , inNewModule      -- :: RnM a -> RnM a

        , isSourceIface           -- :: Name  -> RnM Bool

        , lookupVarIdAndAddEnv    -- :: String -> (String -> RnM a) -> RnM a
        , lookupTypeIdAndAddEnv   -- :: String -> (String -> RnM a) -> RnM a
        , lookupTyConAndAddEnv    -- :: String -> (String -> RnM a) -> RnM a
        , lookupModIdAndAddEnv    -- :: String -> (String -> RnM a) -> RnM a
        , lookupClassIdAndAddEnv  -- :: String -> (String -> RnM a) -> RnM a

        , lookupTyConEnv    -- :: String -> (String -> RnM a) -> RnM a
        , lookupTypeIdEnv   -- :: String -> (String -> RnM a) -> RnM a
        , lookupVarIdEnv    -- :: String -> (String -> RnM a) -> RnM a
        , varIdInScope      -- :: String -> RnM Bool

        , addIface          -- :: String -> Decl -> RnM ()
        , addNukeIface      -- :: String -> Id -> RnM ()
        , addMethod         -- :: String -> (Maybe Int, Result, [Param]) -> RnM ()
        , addIsoMethod      -- :: String -> (Result,[Param]) -> RnM ()
        , lookupMethod      -- :: String -> RnM (Maybe [(Maybe Int, Result, [Param])])

        , setIfaceName      -- :: String -> RnM a -> RnM a
        , setModuleName     -- :: String -> RnM a -> RnM a
        , withDependers     -- :: [String] -> RnM a -> RnM a
        , getIfaceName      -- :: (String -> RnM a) -> RnM a
        , getModuleName     -- :: (String -> RnM a) -> RnM a
        , getDependers      -- :: ([String] -> RnM a) -> RnM a

        , IsoEnv
        , IfaceNukeEnv
        ) where

import qualified Env
import DsMonad   ( TypeEnv, SourceEnv, TagEnv, IfaceEnv )
import CoreIDL
import CoreUtils
import BasicTypes
import Data.Maybe ( isJust )
import Utils
\end{code}

\begin{code}
newtype RnM a  = RnM (RnEnv -> RnState -> (a, RnState))

type RnEnv = ( String    -- current interface name
             , String    -- current Haskell module name
             , [String]  -- list of interface/modules depending on
                         -- iface/module being currently processed.
                         -- (Need to record this when fighting Haskell modules
                         -- in one-module-per-interface mode.)
             )

  -- name environments are used to map from the name that occurred
  -- in the IDL input to the unique name&module to use when generating Haskell.
  -- 
  -- while renaming, when a name is encountered, we first check to see whether
  -- it has got a mapping in the NameEnv. If so, reuse it.
type NameEnv       = Env.Env String String

  -- UniqueNameEnvs are used to record how many defns of a name N there has been
  -- /in the current scope/. 
type UniqueNameEnv = Env.Env String Int

  -- the renaming pass will optionally also spot isomorphic methods, that is,
  -- methods with the same name, same method table offset (if any -- IDispatch
  -- methods doesn't have any), result and parameter types.
  -- 
  -- The underlying idea is that in many cases interfaces mapped to the same Haskell
  -- module have identical methods. Spotting the ones that are shared allows us to later
  -- generate just the one stub, rather than N. 
type MethodEnv = Env.Env String [(Maybe Int,Result,[Param])]
type IsoEnv    = Env.Env String [(Result,[Param])]

  -- the 'nuke' interface environment is used for two purposes:
  --   * some interfaces we may simply want to ignore. Period.
  --   * sometimes you see IDL input of the form 
  --            "interface _A {....}; coclass A { interface A; }"
  --     If you've got the one-module-per-iface/class option turned on,
  --     you really don't want to generate two modules for this (assuming
  --     the _A isn't used by anyone else, of course.) We support this by
  --     slurping the interface into the class' Haskell module, and dropping
  --     the generation of the interface's module alltogether.
type IfaceNukeEnv = Env.Env String (Maybe Id)
                            -- True => don't bother generating code for this iface.
                            -- False (or not in env) => do generate.
\end{code}

Carry around a set of environments that keep the various namespaces clean.
The n-spaces are:

 + typedef'ed names      (turns into type names in Haskell)
 + constructed tag names (turns into data cons in Haskell)
 + field labels     
        IDL mimics C's rules for overloading field labels, they
        only have to be unique within a constructed type declaration, not
        across all definitions in scope. 
            
        Since we're mapping field labels to Haskell record field
        labels, we have to ensure that a label is unique within the scope
        of one module (best we can do.)

         => Field labels, method names and constants are all in the same
            Haskell namespace, so we rename all of these wrt. to one environment.

 + A method's parameter labels is also renamed, although we can assume that
   they by this stage have been checked to have unique (IDL) names. Why? Because
   of the potential clash with Haskell keywords, e.g.,

       void foo([in]int _data, [in]int __data);

   should turn into 

       void foo([in]int data0, [in]int data1);


   To this, we use a per-method name mapping environment for these.

\begin{code}

type NameSpaceEnv = 
  ( NameEnv        -- current set of forward/unbound IDL names.
  , NameEnv        -- mapping from IDL names to (unique) Haskell name
  , UniqueNameEnv  -- mapping from Haskell names to next unique tag.
  )

-- big,fat&ugly state:
data RnState
 = RnState 
     { type_env         :: TypeEnv
     , tg_env           :: TagEnv
     , src_env          :: SourceEnv
     , tycon_env        :: NameSpaceEnv
     , modid_env        :: NameSpaceEnv
     , varid_env        :: NameSpaceEnv
     , clsid_env        :: NameSpaceEnv  -- a class' scope is essentially global in Haskell
     , tyid_env         :: NameSpaceEnv
     , meth_env         :: MethodEnv
     , iso_meths        :: IsoEnv
     , meth_offset      :: Maybe Int
     , iface_env        :: IfaceEnv
     , iface_nuke_env   :: IfaceNukeEnv
     }

runRnM :: TypeEnv
       -> TagEnv
       -> SourceEnv
       -> IfaceEnv
       -> RnM a -> (a, IsoEnv, IfaceNukeEnv)
runRnM tenv tgenv senv ienv (RnM act) = 
  case (act ("","",[]) envs) of 
    (v, RnState{iso_meths=i,iface_nuke_env=e}) -> (v, i, e)
 where
  n_env = (newINameEnv, newINameEnv, newNameEnv)
  envs = RnState
            { type_env   = tenv
            , tg_env     = tgenv
            , src_env    = senv
            , tycon_env  = n_env
            , modid_env  = n_env
            , varid_env  = n_env
            , clsid_env  = n_env
            , tyid_env   = n_env
            , meth_env   = Env.newEnv
            , iso_meths  = Env.newEnv
            , meth_offset = Nothing
            , iface_env  = ienv
            , iface_nuke_env = Env.newEnv
            }

\end{code}

\begin{code}
lookupTypeId :: Name -> RnM (Maybe (Maybe String, Type))
lookupTypeId nm = RnM $ \ _ st -> 
  ( mapMb (\ (mod,t,_) -> (mod,t))
          (Env.lookupEnv (type_env st) nm)
  , st
  )

lookupIface :: Name -> RnM (Maybe Decl)
lookupIface nm = RnM ( \ _ st -> (Env.lookupEnv (iface_env st) nm, st))

lookupTag :: Name -> RnM (Maybe (Maybe String, String))
lookupTag nm = RnM ( \ _ st -> (Env.lookupEnv (tg_env st) nm, st))

getMethOffset :: RnM (Maybe Int)
getMethOffset = RnM ( \ _ st -> (meth_offset st, st))

setMethOffset :: Maybe Int -> RnM a -> RnM a
setMethOffset no (RnM a) = RnM ( \ env st -> a env (st{meth_offset=no}))

incMethOffset :: RnM ()
incMethOffset = RnM $ \ _ st -> 
            let
              st' =
               case meth_offset st of
                 Nothing -> st
                 Just x  -> st{meth_offset=Just (x+1)}
            in
            ((), st')

withNewVarIdEnv :: RnM a -> RnM a
withNewVarIdEnv (RnM act) = RnM $ \ env st -> 
     let old = varid_env st in
     case act env st of
       (v, st') -> (v, st'{varid_env=old})

inNewModule :: RnM a -> RnM a
inNewModule (RnM act) = RnM $ \ env st ->
   let
    ds   = tycon_env st
    vs   = varid_env st
    ts   = tyid_env st
   in
   case act env st of
     (v, new_st) -> (v, new_st{tycon_env=ds,varid_env=vs,tyid_env=ts})

isSourceIface :: Name -> RnM Bool
isSourceIface nm =
  RnM ( \ _ st -> (isJust (Env.lookupEnv (src_env st) nm), st))

newINameEnv :: NameEnv
newINameEnv = Env.newEnv

newNameEnv :: UniqueNameEnv
newNameEnv = Env.addListToEnv Env.newEnv builtins
 where
  builtins = zip builtin_names (repeat 0)
  
  builtin_names = haskellKeywords

haskellKeywords :: [String]
haskellKeywords =
 [ "case", "class", "data", "default", "deriving", "do"
 , "else", "if", "import", "in", "infix", "infixl", "infixr"
 , "instance", "let", "module", "newtype", "of", "then", "type", "where"
 , "do"
 , "as", "qualified", "hiding" -- special ids the last three, so strictly not necessary to include them.
 ]

lookupAndAddEnv2 :: (RnState -> NameSpaceEnv)
                 -> (RnState -> NameSpaceEnv -> RnState)
                 -> String 
                 -> String
                 -> (String -> RnM a) 
                 -> RnM a
lookupAndAddEnv2 get upd nm nm_to_use cont = RnM $ \ rn_env st ->
        let (fwdMap, idlMap, env) = get st in
        case Env.lookupEnv fwdMap nm of
          Just x -> -- a mention has already been made of this IDL name in
                    -- this scope, just reuse it.
                let
                 (RnM act) = cont x
                in
                act rn_env st
          Nothing -> -- no forward mention, try the IDL->HS map.
             case Env.lookupEnv idlMap nm of
               Just x -> -- IDL name in scope, use it's unique name. 
                        let
                         (RnM act) = cont x
                        in
                        act rn_env st
               Nothing ->
                   -- Not seen, add it to the 'forward map', allocating
                   -- a unique name for it.
                case Env.lookupEnv env nm_to_use of
                  Nothing -> 
                    let env'      = Env.addToEnv env nm_to_use 0
                        fwdMap'   = Env.addToEnv fwdMap nm nm_to_use
                        (RnM act) = cont nm_to_use
                    in
                    act rn_env (upd st (fwdMap', idlMap, env'))
                  Just i -> 
                    -- Find a new unique and use it.
                   let 
                       (env',nm') = addNewName env nm_to_use i
                       fwdMap'    = Env.addToEnv fwdMap nm nm'
                       (RnM act)  = cont nm'
                   in
                   act rn_env (upd st (fwdMap',idlMap,env'))

lookupAndAddEnv :: (RnState -> NameSpaceEnv)
                 -> (RnState -> NameSpaceEnv -> RnState)
                 -> String 
                 -> String
                 -> (String -> RnM a) 
                 -> RnM a
lookupAndAddEnv get upd nm nm_to_use cont = RnM $ \ rn_env st ->
        let (fwdMap,idlMap, env) = get st in
        case Env.lookupEnv fwdMap nm of
          Just x -> -- a mention has been made of this IDL name in
                    -- this scope, incorporate it in the 'IDL map'
                    -- and remove it from the 'forward map'.
                case Env.lookupEnv env x of
                    -- This case could should never happen, as the
                    -- addition of a name to the 'forward map' will
                    -- have allocated a unique name using 'env'. (It's
                    -- no problem to handle it correctly though.)
                  Nothing -> let
                              env'      = Env.addToEnv env x 0
                              fwdMap'   = Env.delFromEnv fwdMap nm
                              idlMap'   = Env.addToEnv idlMap nm x
                              (RnM act) = cont x
                             in
                             act rn_env (upd st (fwdMap',idlMap', env'))
                  Just _ -> 
                    -- 
                   let 
                       fwdMap'   = Env.delFromEnv fwdMap nm
                       idlMap'   = Env.addToEnv idlMap nm x
                       (RnM act) = cont x
                   in
                   act rn_env (upd st (fwdMap',idlMap',env))
          Nothing -> -- there's been no forward reference, 
                     -- 'simply' find a unique name and upd. the IDL map.
             case Env.lookupEnv env nm_to_use of
                Nothing -> 
                  let env'       = Env.addToEnv env nm_to_use 0
                      idlMap'    = Env.addToEnv idlMap nm nm_to_use
                      (RnM act)  = cont nm_to_use
                  in
                  act rn_env (upd st (fwdMap, idlMap', env'))
                Just i -> 
                   let (env',nm') = addNewName env nm_to_use i
                       idlMap'    = Env.addToEnv idlMap nm nm'
                       (RnM act)  = cont nm'
                   in
                   act rn_env (upd st (fwdMap,idlMap',env'))

addNewName :: Env.Env String Int
           -> String
           -> Int
           -> (Env.Env String Int, String)
addNewName env nm v =
  let nm' = nm ++ show v in
  case Env.lookupEnv env nm' of
   Nothing -> 
              let env'     = Env.addToEnv env  nm  (v+1)
                  env''    = Env.addToEnv env' nm' 0
              in
              (env'', nm')
   Just _  -> addNewName env nm (v+1)


lookupVarIdAndAddEnv :: String -> (String -> RnM a) -> RnM a
lookupVarIdAndAddEnv nm cont = 
    lookupAndAddEnv (varid_env) (\ st env' -> st{varid_env=env'}) nm (mkHaskellVarName nm) cont

lookupTypeIdAndAddEnv :: String -> (String -> RnM a) -> RnM a
lookupTypeIdAndAddEnv nm cont = 
    lookupAndAddEnv (tyid_env) (\ st env' -> st{tyid_env=env'}) nm (mkHaskellTyConName nm) cont

lookupTyConAndAddEnv :: String -> (String -> RnM a) -> RnM a
lookupTyConAndAddEnv nm cont = 
    lookupAndAddEnv (tycon_env) (\ st env' -> st{tycon_env=env'}) nm (mkHaskellTyConName nm) cont

lookupModIdAndAddEnv :: String -> (String -> RnM a) -> RnM a
lookupModIdAndAddEnv nm cont = 
    lookupAndAddEnv (modid_env) (\ st env' -> st{modid_env=env'}) nm (mkHaskellTyConName nm) cont

lookupClassIdAndAddEnv :: String -> (String -> RnM a) -> RnM a
lookupClassIdAndAddEnv nm cont = 
    lookupAndAddEnv (clsid_env) (\ st env' -> st{clsid_env=env'}) nm (mkHaskellTyConName nm) cont

lookupTyConEnv :: String -> (String -> RnM a) -> RnM a
lookupTyConEnv nm cont = 
    lookupAndAddEnv2 (tycon_env) (\ st env' -> st{tycon_env=env'}) nm (mkHaskellTyConName nm) cont

lookupTypeIdEnv :: String -> (String -> RnM a) -> RnM a
lookupTypeIdEnv nm cont = 
    lookupAndAddEnv2 (tyid_env) (\ st env' -> st{tyid_env=env'}) nm (mkHaskellTyConName nm) cont

lookupVarIdEnv :: String -> (String -> RnM a) -> RnM a
lookupVarIdEnv nm cont = 
    lookupAndAddEnv2 (varid_env) (\ st env' -> st{varid_env=env'}) nm (mkHaskellVarName nm) cont

varIdInScope :: String -> RnM Bool
varIdInScope nm = RnM $ \ _ st ->
   let (_,idlMap, env) = varid_env st in
   case Env.lookupEnv idlMap nm of
     Nothing -> (isJust (Env.lookupEnv env nm), st)
     Just _  -> (True, st)

addIface :: String -> Decl -> RnM ()
addIface nm d = RnM (\ _ st -> ((), st{iface_env=Env.addToEnv (iface_env st) nm d}))

addNukeIface :: String -> Id -> RnM ()
addNukeIface nm i =
    RnM
      (\ _ st ->
        case Env.lookupEnv (iface_nuke_env st) nm of
           Nothing -> ((), st{iface_nuke_env=Env.addToEnv (iface_nuke_env st) nm (Just i)})
           Just _  -> ((), st{iface_nuke_env=Env.addToEnv (iface_nuke_env st) nm Nothing}))

addMethod :: String -> (Maybe Int, Result, [Param]) -> RnM ()
addMethod nm it = RnM (\ _ st -> ((), st{meth_env=Env.addToEnv_C (++) (meth_env st) nm [it]}))

addIsoMethod :: String -> (Result,[Param]) -> RnM ()
addIsoMethod nm it =
   RnM (\ _ st -> 
          ((), st{iso_meths=Env.addToEnv_C (++) (iso_meths st) nm [it]}))

lookupMethod :: String -> RnM (Maybe [(Maybe Int, Result, [Param])])
lookupMethod nm = RnM (\ _ st -> (Env.lookupEnv (meth_env st) nm, st))

setIfaceName :: String -> RnM a -> RnM a
setIfaceName nm (RnM act) = RnM (\ (_,hmod,ls) st -> act (nm,hmod,ls) st)

setModuleName :: String -> RnM a -> RnM a
setModuleName nm (RnM act) = RnM (\ (inm,_,ls) st -> act (inm,nm,ls) st)

withDependers :: [String] -> RnM a -> RnM a
withDependers nms (RnM act) = RnM (\ (inm,nm,_) st -> act (inm,nm,nms) st)

getIfaceName :: (String -> RnM a) -> RnM a
getIfaceName f = RnM (\ env@(nm,_,_) st -> let (RnM act) = f nm in act env st)

getModuleName :: (String -> RnM a) -> RnM a
getModuleName f = RnM (\ env@(_,nm,_) st -> let (RnM act) = f nm in act env st)

getDependers :: ([String] -> RnM a) -> RnM a
getDependers f = RnM (\ env@(_,_,nms) st -> let (RnM act) = f nms in act env st)

\end{code}

And, finally, let's have a look at tomorrow's weather..

\begin{code}
instance Monad RnM where
  (>>=) (RnM m) n =
     RnM (\ env st ->
            case m env st of
              (v, st') -> let (RnM act) = n v in
                           act env st')
  return v = RnM (\ _ st -> (v,st))

\end{code}
