% 
% (c) The Foo Project, University of Glasgow 1998-99
%
% @(#) $Docid: Dec. 27th 2001  00:14  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Prior to converting (core) IDL declarations to Haskell, we
run a renaming pass over the IDL, performing the following
tasks:

 - fields labels, method names and types are renamed so
   as to be unique within the scope of a Haskell module.
 - For imported types, adjust the module name to point to
   the Haskell interface/module we're importing from.

To be more precise, IDL has three namespaces:

  * struct/union/enum tags
  * component names (fields, parameter names)
  * other (typedef names, method names and enumeration constants)

which are mapped onto the following Haskell namespaces:

  * tags            => type constructors (unique per module.)
  * field labels    => varids (unique per module.)
  * parameter names => varids (unique per function.)
  * typedef nms.    => type ids (unique per module.)
  * method names    => varids (unique per module.)
  * module names    => modids (unique per translation unit.)
  * interface names => varids / modids (unique per translation unit.)

An added wrinkle when mapping from IDL into Haskell namespaces is that
we have to make sure we don't map IDL names onto Haskell keywords/reserved ids.

\begin{code}
module Rename ( renameDecls, IsoEnv, IfaceNukeEnv ) where

import RnMonad
import CoreIDL
import BasicTypes
import CoreUtils ( mkHaskellTyConName, isMethod, mkIfaceTypeName, getTypeAttributes,
                   localiseTypes )
import Attribute ( isConstantAttribute, hasAttributeWithName, hasAttributeWithNames,
                   sourceAttribute, filterAttributes, filterOutAttributes,
                   findAttribute, findStringAttributes
                 )
import Utils     ( dropSuffix, mapMbM, splitLast )
import DsMonad   ( TypeEnv, SourceEnv, TagEnv, IfaceEnv )
import Opts      ( optOneModulePerInterface, optCoalesceIsomorphicMethods,
                   optPrefixIfaceName, optAppendIfaceName, optInlineTypes,
                   optCharPtrIsString, optUseStdDispatch
                 )

import Data.Maybe    ( isJust, fromMaybe )
import Data.List     ( isPrefixOf )
import Control.Monad ( when, mplus )
import Data.Char     ( isUpper, toUpper )
import Literal

\end{code}

This module's shopfront is @renameDecls@, renaming a list of declarations.
Along with the renamed decls, @renameDecls@ returns an environment of
method names. The environment contains signatures of methods for which
there exist isomorphic definitions, .e.g.,

@
  interface IA { void foo(); };
  interface IB { void foo(); };
@

The @foo@ method in @IB@ is isomorphic to the one in @IA@, a fact that may
be exploited when generating code, creating just an overloaded version
of @foo@ (or one that is just not as strongly typed.)

\begin{code}
renameDecls :: TypeEnv
            -> TagEnv
            -> SourceEnv
            -> IfaceEnv
            -> [Decl]
            -> ([Decl], IsoEnv, IfaceNukeEnv)
renameDecls tenv tgenv senv ienv ds =
   runRnM tenv tgenv senv ienv $ mapM renameDecl (localiseTypes ds)

\end{code}

Renaming individual definitions:

\begin{code}
renameDecl :: Decl -> RnM Decl
renameDecl (Typedef i t orig_ty) = do
  i'   <- renameTypeId (normaliseIdName i)
  t'   <- renameType t
  getModuleName $ \ x -> do
  n_ty <- normaliseTy x [] t'
  return (Typedef i' n_ty orig_ty)
renameDecl (Constant i t o_t e) = do
  i'   <- renameVarId (normaliseIdName i)
  t'   <- renameType t
  o_t' <- relabelType False o_t
  return (Constant i' t' o_t' e)
renameDecl (Interface i is_ref inh ds) = do
  i'        <- renameConId (normaliseIdName i)
  i''       <- renameTypeId i'
  is_source <- isSourceIface (idName i)
  let
   i'''
    | is_source = i''{idAttributes=sourceAttribute:idAttributes i''}
    | otherwise = i''

   nm = mkHaskellTyConName (idName i''')

   startOffset
    | nm == "IUnknown" = Just 0
    | otherwise        = Just (sum (map snd inh))

   rnDecl d
    | isMethod d = do
        d' <- renameDecl d
        incMethOffset
        return d'
    | otherwise  = renameDecl d

   deps = map remove $
          filterAttributes (idAttributes i) ["depender"]
     where
      remove (Attribute _ [ParamLit (LitLit s)]) = mkHaskellTyConName (snd (splitLast "." s))
      remove _                                   = ""

  ds' <- 
    setMethOffset startOffset $
    setIfaceName nm (
    if optOneModulePerInterface 
     then withDependers deps (setModuleName nm (withNewVarIdEnv (mapM rnDecl ds)))
     else mapM rnDecl ds)

  sane_inh <-
    if optOneModulePerInterface then
       normaliseInh nm deps inh
     else
       getModuleName $ \ mod_nm -> do
       normaliseInh mod_nm [] inh
  i_final <-
     if optOneModulePerInterface then
        return (i'''{idModule=Nothing})
      else 
        normaliseModName i'''

  let d = Interface i_final is_ref sane_inh ds'
  when (not is_ref) (addIface (idName i_final) d)
  return d

renameDecl (Module i ms) = do
  i'  <- renameModId (normaliseIdName i)
  ms' <- setModuleName (idName i') (inNewModule (mapM renameDecl ms))
  return (Module i' ms')
renameDecl (Library i ls) = do
  i'  <- renameModId (normaliseIdName i)
  ls' <- setModuleName (idName i') (inNewModule (mapM renameDecl ls))
  return (Library i' ls')
renameDecl (DispInterface i ii ps ms) = do
  i'   <- renameTypeId (normaliseIdName i) >>= \ x -> renameConId x >>= normaliseModName
  is_source <- isSourceIface (idName i)
  let
   i''
    | is_source = i'{idAttributes=sourceAttribute:idAttributes i'}
    | otherwise = i'

   nm = mkHaskellTyConName (idName i'')
   
   is_wrapper = isJust ii && not optUseStdDispatch
   
   reDecl 
     | is_wrapper = relabelDecl
     | otherwise  = renameDecl

  (ps',ms') <- 
     setIfaceName nm (
     withNew nm ( do
                   ps' <- mapM reDecl ps
                   ms' <- mapM reDecl ms
                   return (ps', ms')))

  ii' <- 
    case ii of
      Nothing -> return Nothing
      Just d  -> do
         d' <- relabelDecl d
         return (Just d')
  let d = DispInterface i'' ii' ps' ms'
  addIface (idName i'') d
  return d

 where
  withNew nm
    | optOneModulePerInterface = (setModuleName nm).withNewVarIdEnv
    | otherwise                = id

renameDecl (CoClass i ls) = do
  i' <- 
    if optOneModulePerInterface then
       renameConId (normaliseIdName i)  >>= renameClassId
     else
       renameClassId (normaliseIdName i)
  ls' <- mapM renameCoCDecl ls
  case ls' of
    [x] -> do
       when optOneModulePerInterface (addNukeIface (idName (coClassId x)) i')
       return (CoClass i' ls')
    _   -> return (CoClass i' ls')

renameDecl (Method i cc res ps offs) =
  getIfaceName $ \ if_name -> do
  let 
    -- We pin on the prefixes of propgetters and putters
    -- here, and rename that.
    attrs = idAttributes i
    
    i'    = normaliseIdName i

    real_i 
       | attrs `hasAttributeWithName` "propget" 
       = i'{idName=if_mangle ("get"++mkHaskellTyConName (idName i))}

       | attrs `hasAttributeWithNames` ["propput", "propputref"]
       = i'{idName=if_mangle ("set"++mkHaskellTyConName (idName i))}

       | optPrefixIfaceName 
       = i'{idName=if_mangle (idName i)}

       | otherwise = i'

    h_if_name = mkIfaceTypeName if_name

    if_mangle
     | optPrefixIfaceName = \ x -> h_if_name ++ "_" ++ x
     | optAppendIfaceName = \ x -> x ++ shorten_if h_if_name
     | otherwise          = id

    mangled_i 
     | optAppendIfaceName && not optOneModulePerInterface
                          = real_i{idName=if_mangle (idName i')}
     | otherwise          = real_i


    -- IFooBar => FB
    -- FooBar  => FB
    -- IntFoo  => IF
    shorten_if ls = 
      case ls of
        'I':x:xs | isUpper x -> shorten (x:xs)
        xs     -> shorten xs

    shorten [] = []
    shorten (x:xs) = toUpper x : filter (isUpper) xs

  the_i <- renameVarId2 mangled_i real_i
    -- fish out the attributes from the result type, so
    -- that all of them are attached to the method Id.
    -- (this is done chiefly to cope with the [ignore]
    --  attribute, which causes type names to be shorted
    --  during type renaming and relabelling.)
    -- 
    -- see relabelDecl.Method comment as to why [ignore]s
    -- are filtered out here.
  let ty_attrs = filterOutAttributes (getTypeAttributes (resultOrigType res))
                                     ["ignore"]
  res'  <- renameResult res
  ps'   <- withNewVarIdEnv $ do
            ps' <- mapM renameParam ps
            mapM renameParamAttr ps'
  off   <- getMethOffset
  when optCoalesceIsomorphicMethods (checkIsomorphicMeth real_i off res' ps')
  return (Method the_i{idAttributes=idAttributes the_i ++ ty_attrs} cc res' ps' offs)

renameDecl (Property i ty mb_off set_i get_i) = do
  set_i' <- renameId (normaliseIdName set_i)
  get_i' <- renameId (normaliseIdName get_i)
  ty'    <- renameType ty
  return (Property i ty' mb_off set_i' get_i')

renameDecl d@(HsLiteral _) = return d
renameDecl d@(CInclude  _) = return d
renameDecl d@(CLiteral  _) = return d
\end{code}

\begin{code}
renameCoCDecl :: CoClassDecl -> RnM CoClassDecl
renameCoCDecl d = do
   i'   <- relabelTyConId i >>= normaliseModName
   mb_d <- lookupIface (idName i')
   return (d{coClassId=i',coClassDecl=mb_d `mplus` coClassDecl d})
 where
  i  = normaliseIdName (coClassId d)
  
\end{code}

\begin{code}
relabelDecl :: Decl -> RnM Decl
relabelDecl (Interface i is_ref inh ds) = do
  i'        <- relabelTyConId (normaliseIdName i)
  ds'       <- mapM relabelDecl ds
  i''       <- normaliseModName i'
  getModuleName $ \ mod_nm -> do
  inh'      <- normaliseInh mod_nm [] inh
  return (Interface i'' is_ref inh' ds')

relabelDecl (DispInterface i ii ps ms) = do
  i'  <- relabelTyConId i
  i'' <- normaliseModName i'
  ms' <- mapM relabelDecl ms
  return (DispInterface i'' ii ps ms')

relabelDecl (Method i cc res ps offs) = do
  i'   <- relabelVarId i
  ps'  <- mapM relabelParam ps
    -- [ignore] attributes attached to the result type are *not* 
    -- propagated to the method Id.
  let ty_attrs = filterOutAttributes (getTypeAttributes (resultOrigType res))
                                     ["ignore"]
  res' <- relabelResult res
  return (Method i'{idAttributes=idAttributes i' ++ ty_attrs} cc res' ps' offs)

relabelDecl (Property i ty mb_off set_i get_i) = do
  set_i' <- relabelVarId (normaliseIdName set_i)
  get_i' <- relabelVarId (normaliseIdName get_i)
  ty'    <- relabelType False ty
  return (Property i ty' mb_off set_i' get_i')

relabelDecl d = return d

\end{code}


\begin{code}
renameVarId :: Id -> RnM Id
renameVarId i = 
  lookupVarIdAndAddEnv (idName i) $ \ nm ->
  return (i{idName=nm})

renameModId :: Id -> RnM Id
renameModId i = 
  lookupModIdAndAddEnv (idName i) $ \ nm ->
  return (i{idName=nm})

normaliseModName :: Id -> RnM Id
normaliseModName i = 
   getModuleName $ \ mod_nm -> 
   case (idModule i) of
     Just x | x == mod_nm -> return (i{idModule=Nothing})
            | otherwise   -> return (i{idModule=Just (mkHaskellTyConName (dropSuffix x))})
     _ -> return (i{idModule=Nothing})

normaliseIdName :: Id -> Id
normaliseIdName i = 
  case findAttribute "hs_name" (idAttributes i) of
    Just (Attribute _ [ParamLit (StringLit s)]) -> i{idName=s}
    _ -> adjustHsNameId i

renameId :: Id -> RnM Id
renameId i = 
  lookupVarIdAndAddEnv (idName i) $ \ nm ->
  return (i{idName=nm})

renameVarId2 :: Id -> Id -> RnM Id
renameVarId2 i2 i = do
  flg <- varIdInScope (idName i)
   -- if it's already there, try using i2 instead.
  if flg then
     renameVarId i2
   else
     renameVarId i

renameConId :: Id -> RnM Id
renameConId i = 
  lookupTyConAndAddEnv (idName i) $ \ nm ->
  return (i{idName=nm})

renameClassId :: Id -> RnM Id
renameClassId i = 
  lookupClassIdAndAddEnv (idName i) $ \ nm ->
  return (i{idName=nm})

renameTyConId :: Id -> RnM Id
renameTyConId i = 
  lookupTyConAndAddEnv (idName i) $ \ nm ->
  return (i{idName=nm})

renameTypeId :: Id -> RnM Id
renameTypeId i = 
  lookupTypeIdAndAddEnv (idName i) $ \ nm ->
  return (i{idName=nm})
\end{code}

Check to see if a method's result and parameters are
isomorphic (upto parameter names) of any others:

\begin{code}
checkIsomorphicMeth :: Id -> Maybe Int -> Result -> [Param] -> RnM ()
checkIsomorphicMeth i mem_off res params = do
  r <- lookupMethod (idOrigName i)  -- use original names here.
  case r of
    Nothing -> -- none yet, add an entry for the method and return.
       addMethod (idOrigName i) (mem_off,res,params)

    Just alts 
      | any checkOne alts ->  -- name & params matched, store it.
            addIsoMethod (idOrigName i) (res, params)
      | otherwise ->
            addMethod (idOrigName i) (mem_off, res,params)

 where
  checkOne (off,r,ps) =
    off == mem_off &&
    resultType r == resultType res && 
    all (\ (p1,p2) -> paramMode p1 == paramMode p2 &&
                      paramType p1 == paramType p2)  -- ToDo: check attributes too.
        (zip ps params)

\end{code}

\begin{code}
renameType :: Type -> RnM Type
renameType ty = 
 case ty of
   Struct i [] mbsz     -> do
       r  <- lookupTag (idName i)
       case r of
         Just (mod,nm) -> do
            mb_r <- lookupTypeId nm
            let r' = fmap (snd) mb_r
            return (Name nm nm mod Nothing r' Nothing)
         Nothing -> do
               --i' <- renameTagId i
               return (Struct i [] mbsz)
   Struct i fields mbsz -> do
     i'        <- renameTyConId i
      {-
       Need to rename all the fields ids first, since
       field types may have attributes that refer to
       another field (and these might be forward refs).
       Sigh.
      -}
     fields'   <- mapM renameFieldId fields
     fields''  <- mapM renameField fields'
     fields''' <- mapM relabelFieldAttr fields''
     return (Struct i' fields''' mbsz)
   Enum i flg vals -> do
     i_r   <- renameTyConId i
     i'    <- normaliseModName i_r
     i''   <- 
         {- Make sure we've got the right module. -} 
       case idModule i' of
         Nothing -> do
            r <- lookupTag (idName i)
            case r of
              Just (Just mod, _) -> setModuleName mod (normaliseModName i')
                    -- drop the module qualifier if it's the same as the name of the
                    -- containing module [this prunage is only done in the one-mod-per-iface
                    -- setting at the moment.]
              _     -> return i'
         Just _ -> return i'

     vals' <- mapM renameEnumTag vals
     return (Enum i'' flg vals')
   Union i t struct_tg un_tg switches -> do
     i'         <- renameTyConId i
     struct_tg' <- renameTyConId struct_tg
     un_tg'     <- renameTyConId un_tg
     switches'  <- mapM renameSwitch switches
     return (Union i' t struct_tg' un_tg' switches')
   UnionNon i switches -> do
     i'         <- renameTyConId i
     switches'  <- mapM renameSwitch switches
     return (UnionNon i' switches')
   CUnion i fields mbsz -> do
     i'         <- renameTyConId i
     fields'    <- mapM renameFieldId fields
     fields''   <- mapM renameField fields'
     return (CUnion i' fields'' mbsz)
   FunTy cc res ps -> do
     res'       <- renameResult res
     ps'        <- mapM renameParam ps
     return (FunTy cc res' ps')

   Pointer _ _ x@(Char _) 
     | optCharPtrIsString -> return (String x False Nothing)

   Pointer pt isExp t -> do
     t'         <- renameType t
     return (Pointer pt isExp t')
   Array t es   -> do
     t'         <- renameType t
     return (Array t' es)
   SafeArray t  -> do
     t'         <- renameType t
     return (SafeArray t')

   Sequence t mb_sz mb_term -> do
     t' <- renameType t
     return (Sequence t' mb_sz mb_term)

    {-
     We adjust the module part of a name (if any) from the name of the IDL source file
     to the Haskell module that contains it.
    -}
   Name nm orig_nm mod mb_attrs mb_ty mb_ti ->
      getModuleName      $ \ mod_nm ->
      getDependers       $ \ deps   ->
      lookupTypeIdEnv (adjustHsName (fromMaybe [] mb_attrs) nm) $ \ nm'    -> do
      mb_attrs' <- mapMbM renameAttrs mb_attrs
      ren_ty <- 
         case mb_ty of
           Nothing   -> do
              r <- lookupTypeId (adjustHsName (fromMaybe [] mb_attrs) nm)
              case r of
                Nothing -> return (Name nm' orig_nm mod mb_attrs' (fmap snd r) mb_ti)
                  -- avoid 'obvious' loops.
                Just (_, t) -> do
                   t' <- relabelType True t
                   return (Name nm' orig_nm mod mb_attrs' (Just t') mb_ti)

           Just ty1   -> do
             ty' <- relabelType True ty1
               -- optionally shortening out imported synonyms
               -- can sometimes reduce external dependencies.
             
             if (optInlineTypes && isJust mod && nm /= "HRESULT") ||
                ((getTypeAttributes ty1 ++ fromMaybe [] mb_attrs) `hasAttributeWithName`
                        "ignore") then
                return ty'
              else
                return (Name nm' orig_nm mod mb_attrs' (Just ty') mb_ti)
      normaliseTy mod_nm deps ren_ty

   Iface{} ->
     getModuleName $ \ if_name ->
     getDependers  $ \ deps   -> do
     normaliseTy if_name deps ty
   _ -> return ty


-- drop the module qualifier if it's the same as the name of the
-- containing module.
normaliseTy :: String -> [String] -> Type -> RnM Type
normaliseTy mod_nm ls t = 
  case t of
    Iface nm (Just mod) onm attrs is_idis inh
       | mkHaskellTyConName mod == mod_nm -> do
               inh1 <- getBestInheritInfo nm inh
               inh' <- normaliseInh mod_nm ls inh1
               return (Iface (mkHaskellTyConName nm) Nothing
                             onm attrs is_idis inh')
    Iface nm mod onm attrs is_idis inh -> do
               inh1 <- getBestInheritInfo nm inh
               inh' <- normaliseInh mod_nm ls inh1
               return (Iface (mkHaskellTyConName nm) mod'
                             onm attrs is_idis inh')
      where
        mod' = 
         case mod of
           Nothing -> mod
           Just x  -> 
             let h_mod = mkHaskellTyConName x in
             if (optOneModulePerInterface && h_mod `elem` ls) then
                Just (h_mod ++ "Ty")
             else
                Just h_mod

    Name nm orig_nm mb_mod mb_attrs mb_ty mb_ti -> do
       mb_ty' <- mapMbM (normaliseTy mod_nm ls) mb_ty
       let
        mb_mod' =
         case mb_mod of
           Just x  | mkHaskellTyConName x == mod_nm -> Nothing
                   | otherwise                      -> Just (mkHaskellTyConName (dropSuffix x))
           _ -> Nothing

       return (Name (mkHaskellTyConName nm) orig_nm mb_mod'
                    mb_attrs mb_ty' mb_ti)
    Pointer x isExp ty   -> do
        ty' <- normaliseTy mod_nm ls ty
        return (Pointer x isExp ty')
    SafeArray ty   -> do
        ty' <- normaliseTy mod_nm ls ty
        return (SafeArray ty')
    Sequence ty mb_sz mb_term -> do
        ty' <- normaliseTy mod_nm ls ty
        return (Sequence ty' mb_sz mb_term)
    Array ty e     -> do
        ty' <- normaliseTy mod_nm ls ty
        return (Array ty e)
    _ -> return t

normaliseInh :: String -> [String] -> InterfaceInherit -> RnM InterfaceInherit
normaliseInh mod_nm ls inh = do
   inh' <- updateMethodCount inh
   mapM tweakMod inh'
  where
   tweakMod (q,n) = do
     m    <- adjustName optOneModulePerInterface q (qModule q)
     mbNm <- adjustName False q (Just (qName q))
     let nm = fromMaybe (qName q) mbNm
     case m of
       Just x | x == mod_nm -> return (q{qModule=Nothing,qName=nm}, n)
       _                    -> return (q{qModule=m,qName=nm}, n)

   adjustName isModule q nm =
    case nm of
      Nothing
        | isModule && (qName q) `elem` ls
          -> return (Just ((qName q) ++ "Ty"))
        | otherwise -> return nm
      Just h_mod ->
        lookupTyConEnv h_mod $ \ _ -> 
                -- 11/00:
                -- suspicious lack of use of the result, but i'm leaving it
                -- as is, for fear of perturbing anything right now.
        if (isModule && h_mod `elem` ls) then
           return (Just (h_mod ++ "Ty"))
         else
           return (Just (mkHaskellTyConName h_mod))

{-
  In the following setting:

         interface IB;
         interface IA : IB { ... };
         interface IB { ... };

  we need to know how many methods there are in IB when generating
  the stubs for the IA methods. 'updateMethodCount' updates this info
  for IA's inherited interfaces, using the type environment that was
  gathered during desugaring.

  [ -fsort-defns will in most cases sort this one out for us, but 
    this fwd. ref. situation may still occur in a strongly connected
    group of ifaces.
  ]

-}
updateMethodCount :: InterfaceInherit -> RnM InterfaceInherit
updateMethodCount is = mapM updateMethod is
 where
  updateMethod (i,n)
    | n /= 0    = return (i,n) -- non-zero method count means
                               -- that it is up-to-date.
    | otherwise = do
        res <- lookupIface (qName i) 
        case res of
          Just DispInterface{} -> return (i, 7)
          Just iface@Interface{declInherit=inhs,declDecls=ds} -> do
                -- make sure the parent info is up-to-date.
              is' <- updateMethodCount inhs
              let inh_meths = sum (map snd is')
                  no_meths  = length (filter isMethod ds)
                -- update info in env for the benefit of others.
              addIface (qName i) (iface{declInherit=is'})
                -- ToDo: attribute Interface decls with vtbl sizes.
              return (i, no_meths + inh_meths)
          _  -> return (i,n)
{-
   In case we were processing something like:
    
      interface A;
      interface B { ... f(...A* x...); }
      interface A : X { ... };
                       
   The inheritance info 'X' for A isn't known when processing
   interface B. Rectify that here.
-}
getBestInheritInfo :: Name -> InterfaceInherit -> RnM InterfaceInherit
getBestInheritInfo nm inh = do
   res  <- lookupTypeId nm
   case res of
     Just (_, Iface _ _ _ _ _ inh2) | not (null inh2) -> return inh2
     _                    -> return inh

\end{code}

\begin{code}
relabelType :: Bool -> Type -> RnM Type
relabelType derefTy ty = 
 case ty of
   Struct i [] mbsz     -> do
       r  <- lookupTag (idName i)
       case r of
         Just (mod,nm) -> do
            r1 <- lookupTypeId nm
            case r1 of
             Just (_,t)  -> return (Name nm nm mod Nothing (Just t) Nothing)
             Nothing -> do
               return (Name nm nm mod Nothing Nothing Nothing)
         Nothing -> do
            --i' <- relabelTyConId i
            return (Struct i [] mbsz)

   Struct i fields mbsz -> do
     i'       <- relabelTyConId i
     fields'  <- mapM relabelField fields
     fields'' <- mapM relabelFieldAttr fields'
     return (Struct i' fields' mbsz)
   Enum i flg vals -> do
     i'    <- relabelTyConId i >>= normaliseModName
     vals' <- mapM relabelEnumTag vals
     return (Enum i' flg vals')
   Union i t struct_tg un_tg switches -> do
     i'         <- relabelTyConId i
     struct_tg' <- relabelTyConId struct_tg
     un_tg'     <- relabelTyConId un_tg
     switches'  <- mapM relabelSwitch switches
     return (Union i' t struct_tg' un_tg' switches')
   UnionNon i switches -> do
     i'         <- relabelTyConId i
     switches'  <- mapM relabelSwitch switches
     return (UnionNon i' switches')
   CUnion i fields mbsz -> do
     i'         <- relabelTyConId i
     fields'    <- mapM relabelField fields
     return (CUnion i' fields' mbsz)
   Pointer _ _ x@(Char _) 
     | optCharPtrIsString -> return (String x False Nothing)
   Pointer pt isExp t -> do
     t'         <- relabelType derefTy t
     return (Pointer pt isExp t')
   Array t es   -> do
     t'         <- relabelType derefTy t
     return (Array t' es)
   SafeArray t  -> do
     t'         <- relabelType derefTy t
     return (SafeArray t')
   Sequence t mb_sz mb_term -> do
     t'         <- relabelType derefTy t
     return (Sequence t' mb_sz mb_term)
   FunTy cc res ps -> do
     res'       <- relabelResult res
     ps'        <- mapM relabelParam ps
     return (FunTy cc res' ps')
   
    {-
     We adjust the module part of a name (if any) from the name of the IDL source file
     to the Haskell module that contains it.
    -}
   Name nm orig_nm mod mb_attrs mb_ty mb_ti ->
     getModuleName $ \ mod_nm ->
     getDependers  $ \ deps   -> do 
     lookupTypeIdEnv (adjustHsName (fromMaybe [] mb_attrs) nm) $ \ nm'    -> do
     mb_attrs' <- mapMbM renameAttrs mb_attrs
     ren_ty <-
       case mb_ty of
          Nothing   -> do
            r <- lookupTypeId (adjustHsName (fromMaybe [] mb_attrs) nm)
            case r of
                -- avoid chains of type names..
                -- iff there's no TypeInfo attached to the outermost, since
                -- it takes precedence later on as it completely describes the ty.
              Just (_, t) | derefTy -> do
                   t' <- relabelType False t
                   return (Name nm' orig_nm mod mb_attrs' (Just t') mb_ti)
              Just (_,(Name _ _ _ a o_t mb_ti2)) 
                  | isJust mb_ti      -> return (Name nm' orig_nm mod a o_t mb_ti2)
              _                       -> return (Name nm' orig_nm mod mb_attrs' (fmap snd r) mb_ti)

          Just ty1
               | optInlineTypes && isJust mod && nm /= "HRESULT" -> return ty1
               | (fromMaybe [] mb_attrs ++ getTypeAttributes ty1) 
                       `hasAttributeWithName` "ignore"  -> return ty1
               | otherwise -> do
                   ty' <- if False && derefTy then
                             relabelType False ty1
                           else
                             return ty1     
                   return (Name nm' orig_nm mod mb_attrs' (Just ty') mb_ti)
     normaliseTy mod_nm deps ren_ty

   Iface{} ->
     getModuleName $ \ if_name ->
     getDependers  $ \ deps   -> do 
     normaliseTy if_name deps ty

   _ -> return ty

\end{code}

\begin{code}
renameParam :: Param -> RnM Param
renameParam (Param i m ty orig_ty has_dep) = do
  i'        <- renameVarId i
  ty'       <- renameType ty
  orig_ty'  <- relabelType True orig_ty
  return (Param i' m ty' orig_ty' has_dep)
  
relabelParam :: Param -> RnM Param
relabelParam (Param i m ty orig_ty has_dep) = do
  i'        <- relabelVarId i
  ty'       <- relabelType False ty
  orig_ty'  <- relabelType False orig_ty
  return (Param i' m ty' orig_ty' has_dep)
  
relabelField :: Field -> RnM Field
relabelField (Field i ty orig_ty mb_sz mb_off) = do
  i'        <- relabelVarId i
  ty'       <- relabelType False ty
  orig_ty'  <- relabelType False orig_ty
  return (Field i' ty' orig_ty' mb_sz mb_off)

relabelResult :: Result -> RnM Result
relabelResult (Result ty orig_ty) = do
  ty'       <- relabelType False ty
  orig_ty'  <- relabelType False orig_ty
  return (Result ty' orig_ty')

relabelSwitch :: Switch -> RnM Switch
relabelSwitch (Switch i labs ty orig_ty) = do
  i'        <- relabelVarId i
  ty'       <- relabelType False ty
  orig_ty'  <- relabelType False orig_ty
  return (Switch i' labs ty' orig_ty')
relabelSwitch s = return s

renameFieldId :: Field -> RnM Field
renameFieldId f = do
  i'        <- renameVarId (fieldId f)
  return (f{fieldId=i'})

renameField :: Field -> RnM Field
renameField f@Field{fieldType=ty,fieldOrigType=o_ty} = do
  ty'    <- renameType ty
  o_ty'  <- relabelType True o_ty
  return f{fieldType=ty',fieldOrigType=o_ty'}

{- UNUSED
renameFieldAttr :: Field -> RnM Field
renameFieldAttr f@Field{fieldId=i} = do
 attrs    <- mapM renameAttribute (idAttributes i)
 return (f{fieldId=i{idAttributes=attrs}})
-}

relabelFieldAttr :: Field -> RnM Field
relabelFieldAttr f@Field{fieldId=i} = do
 attrs    <- mapM relabelAttribute (idAttributes i)
 return (f{fieldId=i{idAttributes=attrs}})

renameResult :: Result -> RnM Result
renameResult (Result ty orig_ty) = do
  ty'       <- renameType ty
  orig_ty'  <- relabelType True orig_ty
  return (Result ty' orig_ty')

renameParamAttr :: Param -> RnM Param
renameParamAttr p = do
 attrs <- mapM renameAttribute (idAttributes (paramId p))
 return (p{paramId=(paramId p){idAttributes=attrs}})

renameAttrs :: [Attribute] -> RnM [Attribute]
renameAttrs = mapM renameAttribute

renameAttribute :: Attribute -> RnM Attribute
renameAttribute at 
 | isConstantAttribute at = return at  -- NB: not just an optimisation, 
                                       -- atParams will fail when given a (moded) constant attributes!
 | otherwise              = do
    params <- mapM renameAttrParam (atParams at)
    return (at{atParams=params})

renameAttrParam :: AttributeParam -> RnM AttributeParam
renameAttrParam p = 
 case p of
  ParamVar nm -> lookupVarIdEnv nm $ \ v -> return (ParamVar v)
  ParamType t -> do
     t' <- renameType t
     return (ParamType t')
  ParamLit _ -> return p
  ParamVoid  -> return p
  ParamPtr ptr -> do
     p' <- renameAttrParam ptr
     return (ParamPtr p')
  ParamExpr e -> do
     e' <- renameExpr e
     return (ParamExpr e')

relabelAttribute :: Attribute -> RnM Attribute
relabelAttribute at 
 | isConstantAttribute at = return at  -- NB: not just an optimisation, 
                                       -- atParams will fail when given a (moded) constant attributes!
 | otherwise              = do
    params <- mapM relabelAttrParam (atParams at)
    return (at{atParams=params})

relabelAttrParam :: AttributeParam -> RnM AttributeParam
relabelAttrParam p = 
 case p of
  ParamVar nm -> lookupVarIdEnv nm $ \ v -> return (ParamVar v)
  ParamType t -> do
     t' <- relabelType True t
     return (ParamType t')
  ParamLit _ -> return p
  ParamVoid  -> return p
  ParamPtr ptr -> do
     p' <- relabelAttrParam ptr
     return (ParamPtr p')
  ParamExpr e -> do
     e' <- renameExpr e
     return (ParamExpr e')

renameSwitch :: Switch -> RnM Switch
renameSwitch (Switch i labs ty orig_ty) = do
  i'        <- renameVarId i
  ty'       <- renameType ty
  orig_ty'  <- relabelType True orig_ty
  return (Switch i' labs ty' orig_ty')
renameSwitch s = return s

renameEnumTag :: EnumValue -> RnM EnumValue
renameEnumTag (EnumValue nm (Left v)) = do
 nm' <- renameTyConId (adjustHsNameId nm)
 return (EnumValue nm' (Left v))
renameEnumTag (EnumValue nm (Right e)) = do
 nm' <- renameTyConId (adjustHsNameId nm)
 e' <- renameExpr e
 return (EnumValue nm' (Right e'))

relabelEnumTag :: EnumValue -> RnM EnumValue
relabelEnumTag ev = 
 let i = enumName ev in
 lookupTyConEnv (idName i) $ \ nm -> return (ev{enumName=i{idName=nm}})

relabelVarId :: Id -> RnM Id
relabelVarId i = lookupVarIdEnv (idName i) $ \ v -> return (i{idName=v})

relabelTyConId :: Id -> RnM Id
relabelTyConId i =
  lookupTyConEnv (idName i) $ \ v -> return (i{idName=v})

\end{code}

\begin{code}
renameExpr :: Expr -> RnM Expr
renameExpr e = 
 case e of
   Binary bop e1 e2 -> do
     e1' <- renameExpr e1
     e2' <- renameExpr e2
     return (Binary bop e1' e2')
   Cond e1 e2 e3 -> do
     e1' <- renameExpr e1
     e2' <- renameExpr e2
     e3' <- renameExpr e3
     return (Cond e1' e2' e3')
   Unary op e1 -> do
     e1'  <- renameExpr e1
     return (Unary op e1')
   Var nm -> lookupVarIdEnv nm $ \ v -> return (Var v)
   Lit _  -> return e
   Cast t e1 -> do
    e' <- renameExpr e1
    return (Cast t e')
   Sizeof t -> do
      t' <- relabelType True t
      return (Sizeof t')
     
\end{code}

\begin{code}
adjustHsNameId :: Id -> Id
adjustHsNameId i = i{idName=adjustHsName (idAttributes i) (idName i)}

adjustHsName :: [Attribute] -> Name -> Name
adjustHsName attr nm = rmPrefix prefixes
 where
  prefixes = findStringAttributes "hs_prefix" attr
  
  rmPrefix [] = nm
  rmPrefix (x:xs) | x `isPrefixOf` nm = drop (length x) nm
                  | otherwise         = rmPrefix xs


\end{code}
