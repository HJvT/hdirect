%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Feb. 9th 2003  15:08  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

The desugar monad helps carry around environments
that maps user defined type names to their type and
attributes.

\begin{code}
module DsMonad 
        (
          DsM
        , runDsM                -- :: String 
                                -- -> Env String TypeInfo
                                -- -> Env String (Bool, [IDL.Attribute])
                                -- -> String DsM a
                                -- -> IO a

        , mapDsM                -- :: (a -> b) -> DsM a -> DsM b

        , lookupType            -- :: String -> DsM (Maybe (String,Type, [Attribute]))
        , lookupTypeInfo        -- :: String -> DsM (Maybe TypeInfo)
        , lookupAsf             -- :: String -> DsM (Maybe (Bool,[IDL.Attribute]))
        , lookupConst           -- :: String -> DsM (Maybe Int32)
        , lookupIface           -- :: String -> DsM (Maybe Decl)
        , lookupTag             -- :: String -> DsM (Maybe (String, String))

        , getAttributes         -- :: DsM [Attribute]
        , propagateAttributes   -- :: [Attribute] -> DsM a -> DsM a
        , withAttributes        -- :: [Attribute] -> DsM a -> DsM a
        
        , getSrcFilename        -- :: DsM String

        , pushPack              -- :: Maybe (Maybe (String, Maybe Int)) -> DsM ()
        , popPack               -- :: Maybe (String, Maybe Int) -> DsM ()
        , getCurrentPack        -- :: DsM (Maybe Int)

        , openUpScope           -- :: DsM a -> DsM a

        , addToTypeEnv          -- :: String -> (Type, [Attribute]) -> DsM ()
        , addToIfaceEnv         -- :: String -> Decl   -> DsM ()
        , addToConstEnv         -- :: String -> Int32  -> DsM ()
        , getConstEnv           -- :: DsM ConstEnv
        , addToTagEnv           -- :: String -> String -> DsM ()
        , addSourceIface        -- :: String -> DsM ()

        , getFilename           -- :: DsM (Maybe String)
        , setFilename           -- :: Maybe String -> DsM ()
        
        , getInterface          -- :: DsM (Maybe String)
        , withInterface         -- :: String -> DsM a -> DsM a
        
        , addToPath             -- :: String -> DsM a -> DsM a
        , getPath               -- :: DsM String
        
        , inLibrary             -- :: DsM a -> DsM a
        , isInLibrary           -- :: DsM Bool
        
        , inImportedContext     -- :: DsM a -> DsM a
        , isInImportedContext   -- :: DsM Bool
        
        , addWarning            -- :: String -> DsM ()
        
        , ioToDsM               -- :: IO a -> DsM a
        
        , TypeEnv
        , SourceEnv
        , ConstEnv
        , TagEnv
        , IfaceEnv
        , TypeInfo

        ) where

import CoreIDL
import qualified IDLSyn as IDL ( Attribute )
import CoreUtils ( childAttributes )
import Env
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import System.IO  ( hPutStrLn, stderr )
import Data.Int ( Int32 )
import Control.Monad ( when )
import Opts  ( optVerbose, optDebug )
import Data.Maybe ( catMaybes )
import TypeInfo

\end{code}

Lots of stuff being plumbed here...maybe I qualify for a plumber's
diploma on the grounds of this monad? 

\begin{code}
type TypeEnv      = Env String (Maybe String, Type, [Attribute])
type IfaceEnv     = Env String Decl{-Decl.{Disp}Interface constructor only-}
type ConstEnv     = Env String (Either Int32 Expr)
type SourceEnv    = Env String ()
type TagEnv       = Env String (Maybe String, String) {- struct/union/enum tag to name of typedef -}

data EnvChain
 = EnvChain
      TypeEnv  -- default/builtins
      [DsEnvs]

data DsEnvs
 = DsEnvs 
     { type_env :: TypeEnv
     , if_env   :: IfaceEnv
     , co_env   :: ConstEnv
     , tg_env   :: TagEnv
     , src_env  :: SourceEnv
     }

freshDsEnv :: DsEnvs
freshDsEnv = DsEnvs newEnv newEnv newEnv newEnv newEnv

-- As an experiment, this monad uses an environment containing 
-- IORefs rather than thread the information they carry. It is
-- questionable whether this is more efficient, and the result
-- is less than pretty => switch to threading the info sometime
-- in the future.


data DsMEnv
 = DsMEnv 
    { env_ref    :: IORef EnvChain
    , at_ref     :: (IORef [Attribute])    -- the current interface's attributes
    , fn_ref     :: (IORef (Maybe String)) -- current file/module name
    , current_if :: (IORef (Maybe String)) -- current (disp)interface
    , in_lib     :: (IORef Bool)
    , in_import  :: (IORef Bool)
    , tinfo_env  :: Env String TypeInfo
    , asf_env    :: Env String (Bool,[IDL.Attribute])
    , pack_stk   :: IORef [(Maybe String, Int)]
    , nm_path    :: String
    , src_name   :: String
    }

newtype DsM a = DsM (DsMEnv -> IO a)
\end{code}

Types out of the way, here's the big ugly action for performing @DsM@
actions:

\begin{code}
runDsM :: String
       -> Env String TypeInfo
       -> Env String (Bool,[IDL.Attribute])
       -> [(String, Maybe String, Type)]
       -> DsM a
       -> IO (a, TypeEnv, TagEnv, SourceEnv, IfaceEnv)
runDsM srcFileName tInfo aenv defs (DsM m) = do
  at       <- newIORef []
  pck      <- newIORef []
  md       <- newIORef Nothing
  cur_if   <- newIORef Nothing
  in_l     <- newIORef False
  in_im    <- newIORef False
  let tenv = addListToEnv newEnv (map (\(n,mo,t) -> (n, (mo,t,[]))) defs)
      cha  = EnvChain tenv [freshDsEnv]
  chain    <- newIORef cha
  let denv = DsMEnv chain at md cur_if in_l in_im tInfo
                    aenv pck "" srcFileName
  a        <- m denv
  cha1     <- readIORef chain      
  let (EnvChain t ds) = cha1
      ty = unionEnvs (t:map type_env ds)
      ta = unionEnvs (map tg_env ds)
      sr = unionEnvs (map src_env  ds)
      ir = unionEnvs (map if_env ds)
  return (a, ty, ta, sr, ir)

thenDsM :: DsM a -> (a -> DsM b) -> DsM b
thenDsM (DsM m) n =
 DsM (\ te -> do
       v <- m te
       case n v of
         DsM k -> k te)

returnDsM :: a -> DsM a
returnDsM v = DsM (\ _ -> return v)

openUpScope :: DsM a -> DsM a
openUpScope (DsM a) = liftDsM $ \ dse -> do
         let ref = env_ref dse
         cha <- readIORef ref
         let (EnvChain t (x:ls)) = cha
         writeIORef ref (EnvChain t [freshDsEnv])
         v    <- a dse
         cha1 <- readIORef ref
         let (EnvChain t1 nls) = cha1
             ls' = x:nls ++ ls
         writeIORef ref (EnvChain t1 ls')
         return v

liftDsM :: (DsMEnv -> IO a) -> DsM a
liftDsM a = DsM a

lookupType :: String -> DsM (Maybe (Maybe String, Type, [Attribute]))
lookupType str = liftDsM $ 
   \ (DsMEnv{env_ref=ref}) -> do
        chain <- readIORef ref
        let (EnvChain t ds) = chain
        case lookupEnv t str of
           x@(Just _) -> return x
           _ -> case catMaybes (map (\ d -> lookupEnv (type_env d) str) ds) of
                  []    -> return Nothing
                  (x:_) -> return (Just x)

lookupTypeInfo :: String -> DsM (Maybe TypeInfo)
lookupTypeInfo str = liftDsM (\ (DsMEnv{tinfo_env=ti})  -> return (lookupEnv ti str))

lookupAsf :: String -> DsM (Maybe (Bool, [IDL.Attribute]))
lookupAsf str = liftDsM (\ (DsMEnv{asf_env=as})  -> return (lookupEnv as str))

lookupConst :: String -> DsM (Maybe (Either Int32 Expr))
lookupConst str = liftDsM $ 
    \ (DsMEnv{env_ref=ref}) -> do
        chain <- readIORef ref
        let (EnvChain _ ls) = chain
        case catMaybes (map (\ d -> lookupEnv (co_env d) str) ls) of
           []    -> return Nothing
           (x:_) -> return (Just x)

lookupIface :: String -> DsM (Maybe Decl)
lookupIface str = liftDsM $ 
   \ (DsMEnv{env_ref=ref}) -> do
        chain <- readIORef ref
        let (EnvChain _ ls) = chain
        case catMaybes (map (\ d -> lookupEnv (if_env d) str) ls) of
           []    -> return Nothing
           (x:_) -> return (Just x)

lookupTag :: String -> DsM (Maybe (Maybe String, String))
lookupTag str = liftDsM $ 
   \ (DsMEnv{env_ref=ref}) -> do
        chain <- readIORef ref
        let (EnvChain _ ls) = chain
        case catMaybes (map (\ d -> lookupEnv (tg_env d) str) ls) of
           []    -> return Nothing
           (x:_) -> return (Just x)

getAttributes :: DsM [Attribute]
getAttributes = liftDsM (\ (DsMEnv{at_ref=at_v})  -> readIORef at_v)

{- UNUSED
getInheritedAttributes :: DsM [Attribute]
getInheritedAttributes = 
   liftDsM (\ (DsMEnv{at_ref=at_v})  -> do
                ls <- readIORef at_v
                return (childAttributes ls))
-}

getSrcFilename :: DsM String
getSrcFilename = liftDsM (\ (DsMEnv{src_name=s})  -> return s)

withAttributes :: [Attribute] -> DsM a -> DsM a
withAttributes ats (DsM act) = liftDsM $
   \ env@(DsMEnv{at_ref=at_v}) -> do
        old_at <- readIORef at_v
        writeIORef at_v ats
        v      <- act env
        writeIORef at_v old_at
        return v

-- like withAttributes, but filter out the non-inheritable ones.
propagateAttributes :: [Attribute] -> DsM a -> DsM a
propagateAttributes ats (DsM act) = liftDsM $
   \ env@(DsMEnv{at_ref=at_v}) -> do
        old_at <- readIORef at_v
        writeIORef at_v (childAttributes ats)
        v      <- act env
        writeIORef at_v old_at
        return v

{-
 An IDL specification may import a number of other specs. The meaning
 of an import is simply to bring the definitions of types and interfaces
 into scope, no code is generated for the imported entities (you'd
 use #include to (optionally) do literal code inclusion.)

 When desugaring, we work our way through the imports, stashing information
 about types, constants and interfaces into appropriate environments.
-}
addToTypeEnv :: String -> Maybe String -> (Type, [Attribute]) -> DsM ()
addToTypeEnv str md (ty,at) = liftDsM $
    \ (DsMEnv{env_ref=ref}) -> do
--        hPutStrLn stderr ("Adding: " ++ str)
        chain <- readIORef ref
        let (EnvChain t (d:ds)) = chain
            ty_env = type_env d
            d' =d{type_env=addToEnv ty_env str (md,ty,at)}
        writeIORef ref (EnvChain t (d':ds))

addToIfaceEnv :: String -> Decl -> DsM ()
addToIfaceEnv str val = liftDsM $
   \ (DsMEnv{env_ref=ref}) -> do
        chain <- readIORef ref
        let (EnvChain t (d:ds)) = chain
            ienv = if_env d
            d' =d{if_env=addToEnv ienv str val}
        writeIORef ref (EnvChain t (d':ds))

addSourceIface :: String -> DsM ()
addSourceIface str = liftDsM $
   \ (DsMEnv{env_ref=ref}) -> do
        chain <- readIORef ref
        let (EnvChain t (d:ds)) = chain
            senv = src_env d
            d' =d{src_env=addToEnv senv str ()}
        writeIORef ref (EnvChain t (d':ds))

addToConstEnv :: String -> Either Int32 Expr -> DsM ()
addToConstEnv str val = liftDsM $
    \ (DsMEnv{env_ref=ref}) -> do
        chain <- readIORef ref
        let (EnvChain t (d:ds)) = chain
            cenv = co_env d
            d' =d{co_env=addToEnv cenv str val}
        writeIORef ref (EnvChain t (d':ds))

addToTagEnv :: String -> String -> DsM ()
addToTagEnv str val = liftDsM $ 
  \ (DsMEnv{env_ref=ref,fn_ref=fe}) -> do
        chain <- readIORef ref
        md    <- readIORef fe
        let (EnvChain t (d:ds)) = chain
            tenv = tg_env d
            d' =d{tg_env=addToEnv tenv str (md,val)}
        writeIORef ref (EnvChain t (d':ds))

getConstEnv :: DsM ConstEnv
getConstEnv = liftDsM $
  \ DsMEnv{env_ref=ref} -> do
      chain <- readIORef ref
      let (EnvChain _ ds) = chain
      return (unionEnvs (map co_env ds))

getFilename :: DsM (Maybe String)
getFilename = liftDsM (\ (DsMEnv{fn_ref=md}) -> readIORef md)

setFilename :: Maybe String -> DsM ()
setFilename nm = liftDsM ( \ (DsMEnv{fn_ref=md}) -> writeIORef md nm)

getInterface :: DsM (Maybe String)
getInterface = liftDsM ( \ (DsMEnv{current_if=cur_i_ref}) -> readIORef cur_i_ref)

getPath :: DsM String
getPath = liftDsM ( \ (DsMEnv{nm_path=nm}) -> return nm)

setInterface :: Maybe String -> DsM ()
setInterface nm = liftDsM ( \ (DsMEnv{current_if=cur_i_ref}) -> writeIORef cur_i_ref nm)

addToPath :: String -> DsM a -> DsM a
addToPath nm (DsM x) = 
  DsM (\ (env@DsMEnv{nm_path=onm}) ->
           let new_nm = 
                case onm of
                  "" -> nm
                  _  -> onm ++ '.':nm
           in
           x (env{nm_path=new_nm}))

withInterface :: String -> DsM a -> DsM a
withInterface nm act = do
   old_nm <- getInterface
   setInterface (Just nm)
   v      <- act
   setInterface old_nm
   return v

inLibrary :: DsM a -> DsM a
inLibrary (DsM act) = liftDsM $ 
 \ env@(DsMEnv{in_lib=in_lib_ref}) -> do
     writeIORef in_lib_ref True
     v      <- act env
     writeIORef in_lib_ref False
     return v

isInLibrary :: DsM Bool
isInLibrary = liftDsM (\ (DsMEnv{in_lib=in_lib_ref}) -> readIORef in_lib_ref)

inImportedContext :: DsM a -> DsM a
inImportedContext (DsM act) = liftDsM $
 \ env@(DsMEnv{in_import=in_import_ref}) -> do
     x <- readIORef in_import_ref
     writeIORef in_import_ref True
     v      <- act env
     writeIORef in_import_ref x
     return v

isInImportedContext :: DsM Bool
isInImportedContext = liftDsM (\ (DsMEnv{in_import=in_import_ref}) -> readIORef in_import_ref)

pushPack :: Maybe (Maybe (String, Maybe Int)) -> DsM ()
pushPack mb_val = liftDsM $ \ (DsMEnv{pack_stk=ps_ref}) -> do
   ls <- readIORef ps_ref
   case mb_val of
     Nothing      -> writeIORef ps_ref ((Nothing,8):ls) -- default packing is 8. (ToDo: param out.)
     Just Nothing ->
        case ls of
          ((_,x):_) -> writeIORef ps_ref ((Nothing,x):ls)
          []        -> writeIORef ps_ref [(Nothing,8)] 
     Just (Just ("", Just x)) -> writeIORef ps_ref ((Nothing,x):ls)
     Just (Just (nm, Nothing)) -> 
        case ls of
          ((_,x):_) -> writeIORef ps_ref ((Just nm,x):ls)
          []        -> writeIORef ps_ref [(Just nm,8)]
     Just (Just (nm, Just x)) -> writeIORef ps_ref ((Just nm,x):ls)

getCurrentPack :: DsM (Maybe Int)
getCurrentPack = liftDsM $ \ (DsMEnv{pack_stk=ps_ref}) -> do
   ls <- readIORef ps_ref
   case ls of
     []        -> return Nothing
     ((_,x):_) -> return (Just x)

popPack :: Maybe (String, Maybe Int) -> DsM ()
popPack mb_i = liftDsM $ \ (DsMEnv{pack_stk=ps_ref}) -> do
   ls <- readIORef ps_ref
   let ls' = 
        case mb_i of
          Nothing           -> case ls of { [] -> [] ; (_:xs) -> xs }
          Just ("", Just v) -> case ls of { [] -> [] ; (_:xs) -> ((Nothing,v):xs) }
          Just (x,_)        -> scramble x ls ls
   writeIORef ps_ref ls'
 where
   scramble _ ls [] = ls
   scramble x ls ((Nothing,_):xs) = scramble x ls xs
   scramble x ls ((Just y,_):xs) 
       | x == y     = xs
       | otherwise  = scramble x ls xs

\end{code}

\begin{code}
addWarning :: String -> DsM ()
addWarning msg =  liftDsM ( \ _ -> when (optVerbose || optDebug ) (hPutStrLn stderr msg))

ioToDsM :: IO a -> DsM a
ioToDsM act = liftDsM ( \ _ -> act)

instance Monad DsM where
  (>>=)  = thenDsM
  return = returnDsM

mapDsM :: (a -> b) -> DsM a -> DsM b
mapDsM f m =  m >>= \ v -> return (f v)

\end{code}
