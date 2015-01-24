%
% (c) University of Glasgow, 1998-1999
%     Sigbjorn Finne, 2000-
%
% @(#) $Docid: Nov. 24th 2003  07:50  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

\begin{code}
module Desugar ( desugar ) where

import qualified IDLSyn as IDL
import qualified PpIDLSyn as PpIDL ( ppType )
import IDLUtils hiding ( childAttributes, getTyTag )
import qualified IDLUtils ( childAttributes, getTyTag )
import qualified CoreIDL as Core
import CoreUtils ( getTyTag, simpRedExpr, mkHaskellTyConName
                 , mkId, removePtr, findPtrType, isMethod
                 , iUnknownTy, iDispatchTy, childAttributes
                 , int16Ty, currencyTy
                 , dateTy, dummyMethod, intTy, variantTy, bstrTy
                 , mkRefPointer, rawPointerToIP, isIfacePtr, getIfaceTy
                 )
import Attribute ( stringToDepReason, hasStringAttribute, 
                   hasSeqAttribute, getLengthAttribute,  hasModeAttribute,
                   findAttribute, hasAttributeWithName,  hasUniqueAttribute,
                   hasDependentAttrs, hasSourceAttribute, getDefaultCConv
                 )
import DsMonad
import Env

import BasicTypes
import Literal
import Opts ( optOneModulePerInterface, optVerbose,
              optExpandInheritedInterface, optIgnoreDispInterfaces,
              optCompilingMsIDL, optOutPointersAreRefs, 
              optSubtypedInterfacePointers, optTlb, dumpIDL,
              optIgnoreImpLibs, optUnwrapSingletonStructs,
              optNukeEmptyStructs, optJNI, optCompilingOmgIDL,
              optCorba, optHaskellToC, optVoidTydefIsAbstract,
              optNoWarnMissingMode, optUseAsfs, optDon'tTidyDefns,
              optTlb, optServer, optUseStdDispatch
            )

import Utils
import NormaliseType
import ImportLib ( importLib )
import PpIDLSyn ( showIDL, ppDefn )
import PpCore   ( ppType, showCore )
import LibUtils ( defaultCConv, prelude, autoLib, comLib, 
                  iUnknown, iDispatch, jObject, cObject, jniLib,
                  intLib, wordLib, hdirectLib, wStringLib
                )
import NameSupply

import Data.Int
import Control.Monad
import Data.Maybe    ( isJust, fromJust, fromMaybe )
import Data.Char     ( toLower, isSpace )
import Data.List     ( partition, sort, sortBy, isPrefixOf )
import TypeInfo
import Validate
\end{code}

The store front is @desugar@, which converts a set of definitions
into the form expected by the code generator. By this stage, we
assume that the definitions have been checked for `well-formedness'
(legal types, definitions/types in scope etc.), so that we can
just go about doing the transformation from IDLSyn to Core.

\begin{code}
desugar :: String
        -> Env String (Bool, [IDL.Attribute])
        -> [IDL.Defn]
        -> IO ([Core.Decl], TypeEnv, TagEnv, SourceEnv, IfaceEnv)
desugar srcFileName aenv defs = 
     runDsM srcFileName tenv_to_use aenv def_types
                        (desugarer srcFileName defs)
 where
   def_types
    | optCompilingMsIDL = ms_idl_def_types
    | otherwise         = []

   tenv_to_use
    | optCompilingMsIDL = 
        addListToEnv newEnv
                     [ ("VARIANT", variant_ti)
                     , ("IID",   iid_ti)
                     , ("CLSID", clsid_ti)
                     , ("GUID",  guid_ti)
                     , ("VARIANT_BOOL", v_bool_ti)
                     , ("BSTR", bstr_ti)
                     ]
    | otherwise = newEnv

   ms_idl_def_types
    = [ ("IUnknown",     comLib,  Core.Iface "IUnknown"  comLib  "IUnknown"  [] False [])
      , ("IDispatch",    autoLib, Core.Iface "IDispatch" autoLib "IDispatch" [] True  [(iUnknown,3)])
      , ("CURRENCY",     autoLib, currencyTy)
      , ("DATE",         autoLib, dateTy)
      , ("BSTR",         comLib,  bstrTy)
      , ("VARIANT_BOOL", autoLib, Core.Name  "VARIANT_BOOL" "VARIANT_BOOL" 
                                             autoLib Nothing (Just int16Ty) (Just v_bool_ti))
      , ("IID",          comLib,  Core.Name  "IID" "IID" comLib Nothing Nothing (Just iid_ti))
      , ("CLSID",        comLib,  Core.Name  "CLSID" "CLSID" comLib Nothing Nothing (Just clsid_ti))
      , ("GUID",         comLib,  Core.Name  "GUID" "GUID" comLib Nothing Nothing (Just guid_ti))
      , ("VARIANT",      autoLib, variantTy)
      , ("int64",        intLib,  Core.Integer LongLong True)
      , ("uint64",       wordLib, Core.Integer LongLong False)
      , ("HRESULT",      comLib,  Core.Integer Long True)
      , ("LPSTR",        comLib,  Core.String  (Core.Char False) False Nothing)
      , ("LPWSTR",       wStringLib,  Core.WString False Nothing)
      , ("bool",         prelude, Core.Bool)
      , ("wchar_t",      hdirectLib, Core.WChar)
      , ("octet",        hdirectLib, Core.Octet)
      ]


{-
  desugarer is the entry point for the translation of a spec, be
  it imported or at the root. 
  
  ToDo: the unique names needs to be moved into DsM - this is going
  to break in mysterious ways..
-}
desugarer :: String
          -> [IDL.Defn]
          -> DsM [Core.Decl]
desugarer src defs =  do
  let defs' =  tidyDefns (concat (runNS (mapM fillInDefn defs) names))
  (res, _) <- desugarIncludedDecls src Nothing [] defs'
  return (reverse res)
 where
  names  = [ prefix ++ show i | i <- [(0::Int)..]]
  prefix = "__IHC_TAG_"

{- it is rather unfortunate that since we're folding over
    the sequence of declarations, desugarDefn cannot make
    'global' decisions - introducing a new scope by the
    presence of IncludeStart (and IncludeEnd.), for example.

    Instead we're forced to lift this out to here, using
    explicit recursion & testing to handle this. 
      
    ToDo: Re-think this structure sometime in the future.
-} 
desugarIncludedDecls _   _      acc     []  = return (acc, [])
desugarIncludedDecls src keepIt acc (x:xs)  =
    case x of
      IDL.IncludeStart headerFileName 
             -- delay this from kicking in until we see the first occurrence of
             -- a '#line' for the source file (avoids running into trouble with
             -- other line gunk the CPP may emit.)
         | isJust keepIt || headerFileName == src -> do
          old_nm <- getFilename
          let mod' = mkHaskellTyConName (dropSuffix (basename headerFileName))
          mod <- nameOfImport mod'
          setFilename (Just mod)
          let
             -- we're only interested in generating code for the portions
             -- that belong to the source .idl.
             -- ToDo: conditionalise this and optionally be interested in
             --       contents of #include files.
            forKeeps = fromMaybe True keepIt && headerFileName == src
          
          (new_acc, xs') <- desugarIncludedDecls src (Just forKeeps) acc xs
          setFilename old_nm
          desugarIncludedDecls src keepIt new_acc xs'
          
      IDL.IncludeEnd -> return (acc, xs)
      _ -> do
        new_acc <- desugarDefn acc x
        let
         keep = fromMaybe True keepIt

         the_acc
           | keep      = new_acc
           | otherwise = acc
        desugarIncludedDecls src keepIt the_acc xs
\end{code}

%*
%
\section[desugarDefn]{Converting a definition into CoreIDL}
%
%*

\begin{code}
desugarDefn :: [Core.Decl] -> IDL.Defn -> DsM [Core.Decl]

{-
 When parsing typedefs like

    typedef [foo]bar* baz;

 The attribute part is parsed as part of the type, and the
 pointer is parsed as being part of the declarator `baz'.

 When desugaring into Core form, attributes are pinned onto
 Core Ids, and the pointer (or array) nature of an Id is
 recorded in its associated Core type.

 Types aren't currently pinned directly onto Ids, instead
 the context in which they appear records their type.

 Notice that we currently enter the new type name plus its definition
 into a type environment *and* return a Core.Typedef decl. The type
 declaration is stored in an environment so that we later can reduce
 a type down to its primitive form (i.e., expand out synonyms.) Reducing
 types avoid having to create marshalling code for the typedefs themselves.

-}
desugarDefn acc (IDL.Typedef ty tdef_attrs ids)
  | optNukeEmptyStructs  && isEmptyStructTy ty = return acc
  | optVoidTydefIsAbstract && isVoidTyDef ty ids = desugarDefn acc (IDL.Interface (head ids) [] [])
  | otherwise = do
     core_tdef_attrs      <- addToPath (iName (head ids)) $ idlToCoreAttributes tdef_attrs
     if hasAttributeWithName core_tdef_attrs "abstract" then
        -- 
        -- decls of the forms: typedef [abstract,...] struct foo Foo; 
        -- are equal to interface Foo{};
        -- 
        withAttributes core_tdef_attrs $ desugarDefn acc (IDL.Interface (head ids) [] [])
      else do
       mod                  <- getFilename
       inherited_attrs      <- getAttributes
       let 
        child_attrs = childAttributes inherited_attrs

         {-
          A typedef such as:

             typedef struct _tag { ... } *foo;
       
          is problematic, since it doesn't have a particularly descriptive
          Haskell type:
     
             type Foo = Pointer Addr {- or maybe just Addr -}
       
          To solve this, we introduce a dummy typedef:

             typedef struct _tag { ... } structTag, *foo;
       
          and as a result will generate
     
             data StructTag = Tag ...
             type Foo = Pointer StructTag
 
           [10/10/98: drop the typePrefix on the generated type's name, i.e.,
            it's now data Tag = Tag ... , rather than data StructTag = Tag ...
             -- sof]

          (this will only happen for typedefs on structs, unions and enums.)
         -}
        fixed_ids = 
         case filter isUnpointedId ids of
          []    -> let t = tyTag ty in(IDL.Id t:ids)
          (x:_) -> 
            {- In the case where you've got  *foo,*bar,baz - ensure
               that the unpointed id is processed first, so that
               we don't get any forward type references.
            -}
           x: filter (\ i -> iName i /= iName x) ids

         -- unpointed synonym that all pointer and array syns can `point' to.
        ground_syn =
          case map removeIdAttrs (filter isUnpointedId fixed_ids) of
            (IDL.Id x:_) -> x

        mkCoreTypeDef accum i
          | isUnpointedId i && (iName i == ground_syn) = addToPath (iName i) $ do
             -- notice that we augment the path (for the benefit of ASFs) before
             -- the attributes are converted, which is why the conversion cannot be
             -- lifted out of this action.
            core_tdef_attrs1     <- idlToCoreAttributes (tdef_attrs ++ idAttrs i)
            let 
             core_tdef_attrs' = childAttributes core_tdef_attrs1
             final_attrs      = core_tdef_attrs1 ++ inherited_attrs
             
             asNewType = final_attrs `hasAttributeWithName` "hs_newtype"
             
            when (not (isEmptyStructTy ty))
                 (addToTagEnv (IDLUtils.getTyTag (iName i) ty) (iName i))
              -- add tag name to environment, so that recursive types can
              -- be handled correctly.
              --        
              -- Not a principled approach to recursive types, this.
            (nm, core_ty, real_ty)   <- withAttributes child_attrs
                                                       (mkCoreIdTy ty i True core_tdef_attrs')
            the_mod            <- getFilename
            let 
               core_id = mkCoreTypeId nm the_mod final_attrs
               core_ty'
                 | asNewType && not (isConstructedTy ty) =
                     Core.Struct core_id [Core.Field core_id core_ty real_ty Nothing Nothing] Nothing
                 | otherwise = core_ty
                                                       
            addToTypeEnv nm mod (core_ty', final_attrs)
            return (Core.Typedef core_id core_ty' core_ty' : accum)

          | otherwise= addToPath (iName i) $ do
             -- don't redeclare, just make the synonym point to
             -- the ground one.
            core_tdef_attrs1     <- idlToCoreAttributes (tdef_attrs ++ idAttrs i)
            let core_tdef_attrs' = childAttributes core_tdef_attrs1
            (nm, core_ty, real_ty)   <- 
              withAttributes child_attrs
                             (mkCoreIdTy (IDL.TyName ground_syn Nothing) i True core_tdef_attrs')
            addToTypeEnv nm mod (core_ty, core_tdef_attrs1 ++ child_attrs)
            return (Core.Typedef (mkCoreTypeId nm mod core_tdef_attrs1) core_ty core_ty : accum)
        
        mkCoreSimpleTypeDef accum i = addToPath (iName i) $ do
           core_local_attrs      <- idlToCoreAttributes (tdef_attrs ++ idAttrs i)
           let 
            local_inh_attrs  = childAttributes core_local_attrs
            the_tdef_attrs   = core_local_attrs ++ inherited_attrs

            asNewType = the_tdef_attrs `hasAttributeWithName` "hs_newtype"
             
           (nm, core_ty, real_ty)    <- withAttributes inherited_attrs
                                                       (mkCoreIdTy ty i True local_inh_attrs)
           let 
             core_id = mkCoreTypeId nm mod the_tdef_attrs
             core_ty'
              | asNewType = Core.Struct core_id
                                        [Core.Field core_id core_ty real_ty Nothing Nothing]
                                        Nothing
              | otherwise = core_ty
                                                       
           addToTypeEnv nm mod (core_ty', the_tdef_attrs)
           return (Core.Typedef core_id core_ty' core_ty' : accum)

        mkCoreTypeId nm modu attr = mkId nm nm modu attr

       if not (isConstructedTy ty) then
          foldM mkCoreSimpleTypeDef acc ids
        else
          foldM mkCoreTypeDef acc fixed_ids

-- an enum,struct or union are the only legal type declarations.
--  - add to type environment and return.
desugarDefn acc (IDL.TypeDecl ty) = do
   attrs   <- getAttributes
   core_ty <- propagateAttributes attrs (idlToCoreTy ty)
   let nm    = Core.idName (getTyTag core_ty)
   mod     <- getFilename 
   addToTypeEnv nm mod (core_ty, attrs)
   return (Core.Typedef (mkId nm nm mod attrs) core_ty core_ty : acc)

desugarDefn acc (IDL.ExternDecl ty [i])
  | optHaskellToC = desugarDefn acc (IDL.Operation (mkMethodId i) ty Nothing Nothing)
  | otherwise     = return acc

desugarDefn acc IDL.ExternDecl{} = return acc

desugarDefn acc (IDL.Constant i as ty e) = addToPath (iName i) $ do
  core_as                <- idlToCoreAttributes (as ++ idAttrs i)
  attrs                  <- getAttributes
  let child_attrs = core_as ++ childAttributes attrs
  (nm, core_ty, real_ty) <- withAttributes child_attrs (mkCoreIdTy ty i True [])
  core_expr              <- idlToCoreExpr e
  cenv                   <- getConstEnv
  let core_expr' = simpRedExpr cenv core_ty core_expr

      core_int   = 
        case core_expr' of
          Core.Lit (IntegerLit x) -> Left (iLitToIntegral x)
          _                       -> Right core_expr'

  addToConstEnv (iName i) core_int
  mod                    <- getFilename 
  return (Core.Constant (mkId nm (iName i) mod child_attrs) real_ty core_ty core_expr' : acc)

desugarDefn acc IDL.Attribute{} = do
  addWarning ("desugarDefn: attribute not implemented!")
  return acc

desugarDefn acc (IDL.Attributed attrs d) 
{-  | isLeafDefn d = do -- if method or typedef, aggregate attributes..
     as          <- getAttributes
     core_attrs  <- idlToCoreAttributes attrs
     withAttributes (core_attrs ++ as) (desugarDefn acc d)
  | otherwise  -} = do -- ..if interface/module/library/dispinterface/coclass, don't.
     as          <- getAttributes
     core_attrs  <- idlToCoreAttributes attrs
     withAttributes (core_attrs ++ as) (desugarDefn acc d)

desugarDefn acc (IDL.Operation i ty _ _) = addToPath (iName i) $ do
   -- attrs will contain the attributes pinned onto method result type (if any.)
  inh_attrs <- getAttributes
  attrs     <- augmentAttributes inh_attrs
  in_import <- isInImportedContext
  mb_iface  <- getInterface
  let isWithinIface = isJust mb_iface
    {- when processing a COM method call, we drop methods which have
       call_as() attributes (the remotable cousin of some other [local]
       interface method.) It's a bit unfortunate that we cannot make
       use of this remotable version, since it is often specified more
       precisely (e.g., [size_is()] and friends are used), which is
       helpful when trying to generating Haskell friendly signatures.
       However, since the parameters that the remotable version of 
       a method takes doesn't have to have any correlation to the 
       parameters of the [local] method, we're stuck and have to
       drop the [call_as()] version. Such is life.    sof 11/98

    -}
  if ( isWithinIface && attrs `hasAttributeWithName` "call_as" ) then
     return acc
   else if in_import then
     {-
       If the method occurs in an imported context, don't bother
       desugaring it, since we're not going to generate code for
       it anyway. Insert a dummy method so that the computation of
       vtbl offsets don't go bad as a result.
     -}
     
     return (dummyMethod:acc)
    else do
     let
      (fun_id, mb_cc, fun_params) = 
         case i of
           IDL.FunId f cc ps -> (f, cc, ps)
           x                 -> (x, Nothing, [])
    
     (nm, core_ty, real_ty) <- mkCoreIdTy ty fun_id True attrs
     propagateAttributes attrs $ do
     core_args <- idlToCoreParams (iName i) fun_params
     mod       <- getFilename
     let
      callconv = fromMaybe defaultCConv (mb_cc `mplus` getDefaultCConv attrs)
      (meth_nm, orig_nm) = 
          case findAttribute "call_as" attrs of
            Just (Core.Attribute _ [Core.ParamVar v]) | isWithinIface -> (v, v)
            Just (Core.Attribute _ [Core.ParamLit (TypeConst v)]) | isWithinIface -> (v, v)
            Just (Core.Attribute _ [Core.ParamLit (StringLit v)]) | isWithinIface -> (v, v)
            _ -> (nm, iName i)

      meth = Core.Method (mkId meth_nm orig_nm mod attrs) callconv 
                      (Core.Result real_ty core_ty) core_args
                      Nothing

     return (meth:acc)

desugarDefn acc (IDL.Interface (IDL.Id nm) inherits defs) = addToPath nm $ do
  inh_attrs  <- getAttributes
  attrs      <- augmentAttributes inh_attrs
  mod        <- getFilename
  when optOneModulePerInterface (setFilename (Just nm))
  withInterface nm $ do
  let
   iface_nm 
    | optJNI     = mkHaskellTyConName (snd (splitLast "." nm))
    | otherwise  = nm

   isClass    = attrs `hasAttributeWithName` "jni_class"

    {-
      As an experimental hack, we support the [ty_params("args")] attribute
      which is appended to the Haskell type name of 

   iface_args = 
     case findAttribute "ty_params" attrs of
       Just (Core.Attribute _ [Core.ParamLit (StringLit s)]) -> s
       _ -> []
    -}

   home_mod
     | optOneModulePerInterface = Just iface_nm
     | otherwise                = mod

  (inherited_decls, inherited_ifaces) <- do
    stuff <- mapM expandIface inherits
    let (iss, core_inheritss) = unzip stuff
        core_inherits         = concat core_inheritss
        the_core_inherits
         | optJNI && null core_inherits && isClass = [(jObject,0)]
         | optCorba && null core_inherits = [(cObject,0)]
         | otherwise          = core_inherits

        is
         | (not optExpandInheritedInterface) || null inherits = []
         | otherwise = concat iss
    return (is, the_core_inherits)

  let is_idispatch = 
          any (\ x -> "IDispatch" == qName (fst x)) inherited_ifaces &&
          iface_nm /= "IDispatchEx"
     -- Insert a typedef that says that the interface name is an interface, so
     -- that interface pointers can be marshalled properly at a later stage.
     --  (including the methods of this very interface).
  addToTypeEnv nm home_mod (Core.Iface iface_nm home_mod nm attrs is_idispatch (reverse inherited_ifaces)
                                       , attrs)
  decls        <- propagateAttributes attrs (foldM desugarDefn [] defs)
  let 
      core_decls = inherited_decls ++ reverse decls
      iface = Core.Interface (mkId  iface_nm nm home_mod attrs)
                              False inherited_ifaces core_decls
  addToIfaceEnv nm iface
  setFilename mod
  return (iface : acc)

 where
   expandIface iface = do
      mb_decls <- lookupIface iface
      case mb_decls of
          Nothing -> do
             res <- lookupType iface
             case res of
               Just (_,Core.Iface iface_nm home_mod inm [] False _, _) -> 
                 return ([], [(setOrigQName inm (mkQualName home_mod iface_nm), adjMethodCount iface_nm 0)])
               _ -> do
                when (iface /= "IDispatch" && iface /= "IUnknown")
                     (addWarning ("failed to find inherited interface: "++ iface ++ " - for interface " ++ nm))
                let
                  q_iface_name = 
                    case iface of
                     "IUnknown"  -> iUnknown
                     "IDispatch" -> iDispatch
                     _           -> mkQualName Nothing iface

                return ([], [(q_iface_name, adjMethodCount iface 0)])
          Just (Core.Interface id _ inhs i_ds)
             | not optSubtypedInterfacePointers &&
               (Core.idName id /= "IUnknown" && Core.idName id /= "IDispatch") -> do
                  return ( i_ds'
                         , (setOrigQName (Core.idOrigName id)
                                         (mkQualName (Core.idModule id)
                                                     (Core.idName id))
                            , no_methods):inhs
                         )
             | otherwise   -> 
                    -- want to make sure that we're referring to IUnknown and IDispatch
                    -- in the proper manner, since the marshaling of these are in a sense
                    -- built-in.
                  case Core.idName id of
                    "IDispatch" -> return ([], [(iDispatch, 4),(iUnknown, 3)])
                    "IUnknown"  -> return ([], [(iUnknown, 3)])
                    _ -> 
                      return ( []
                             , ( setOrigQName (Core.idOrigName id)
                                              (mkQualName (Core.idModule id)
                                                          (Core.idName id))
                               , no_methods):inhs
                             )
            where
             i_ds'      = filter isMethod i_ds
             no_methods = adjMethodCount (Core.idName id) (length i_ds')

   adjMethodCount iface_nm no_meths =
      case iface_nm of
        "IUnknown"  -> 3
        "IDispatch" -> 7
        _           -> no_meths

desugarDefn acc (IDL.Module (IDL.Id nm) decls) = addToPath nm $ do
  inh_attrs  <- getAttributes
  attrs      <- augmentAttributes inh_attrs
  old_mod <- getFilename
  let mod = Just (mkHaskellTyConName (dropSuffix (basename nm)))
  setFilename mod
  (decls',_)  <- propagateAttributes attrs (desugarIncludedDecls nm (Just True) [] decls)
  setFilename old_mod
  return (Core.Module (mkId (mkHaskellTyConName nm) nm mod attrs) (reverse decls') : acc)

desugarDefn acc (IDL.Library (IDL.Id nm) decls) = addToPath nm $ do
  inh_attrs  <- getAttributes
  attrs      <- augmentAttributes inh_attrs
  old_mod <- getFilename
  let mod = Just (mkHaskellTyConName (dropSuffix (basename nm)))
       -- Hack to make sure we don't depend on the older stdole.
       -- Ugly, but a win from the user's point of view if we
       -- take care of this.
      the_mod = 
        case fmap (map toLower) old_mod of
          Just "stdole32" -> Just "Stdole32"
          Just "stdole2"  -> Just "Stdole32"
          _ ->  mod
  setFilename the_mod
  decls'  <- inLibrary (propagateAttributes attrs (foldM desugarDefn [] decls))
  setFilename old_mod
  return (Core.Library (mkId (mkHaskellTyConName nm) nm Nothing attrs) (reverse decls') : acc)

desugarDefn acc (IDL.CoClass (IDL.Id nm) decls) = addToPath nm $ do
  inh_attrs  <- getAttributes
  attrs      <- augmentAttributes inh_attrs
  core_decls  <- propagateAttributes attrs (mapM desugarCoClassMember decls)
  mod         <- getFilename
  return (Core.CoClass (mkId nm nm mod attrs) core_decls : acc)

desugarDefn acc (IDL.DispInterface (IDL.Id nm) props meths)
  | optIgnoreDispInterfaces = return acc
  | otherwise               = addToPath nm $ do
  inh_attrs  <- getAttributes
  attrs      <- augmentAttributes inh_attrs
  old_nm      <- getFilename
  when optOneModulePerInterface (setFilename (Just nm)  )
   -- Insert a typedef that says that the interface name is an interface, so
   -- that interface pointers can be marshalled properly.
  withInterface nm $ do
  let
   home_mod
     | optOneModulePerInterface = Just nm
     | otherwise                = old_nm

  addToTypeEnv nm home_mod (Core.Iface nm home_mod nm [] True [(iUnknown,3),(iDispatch,4)], attrs)
  (core_props, core_meths) <- propagateAttributes attrs $ do
      ps <- mapM desugarProp props
      ms <- foldM desugarDefn [] meths
      return (ps, reverse ms)
  setFilename old_nm
  let iface = Core.DispInterface (mkId nm nm home_mod attrs) Nothing core_props core_meths
  addToIfaceEnv nm iface
  return (iface : acc)

desugarDefn acc (IDL.DispInterfaceDecl (IDL.Id nm) (IDL.Id i_nm)) = do
  mb_iface <- lookupIface i_nm
  case mb_iface of
    Just d@(Core.Interface _ _ _ i_ds) -> do
        attrs       <- getAttributes 
        home_mod    <- getFilename
--      let d_ds = map toDispInterfaceMethod i_ds
        addToTypeEnv nm home_mod (Core.Iface nm home_mod nm [] True
                                             [(iUnknown,3),(iDispatch,4)], attrs)
        let iface = Core.DispInterface (mkId nm nm home_mod attrs) (Just d) [] (if optServer && optUseStdDispatch then i_ds else []) --i_ds
        addToIfaceEnv nm iface
        return (iface : acc)
    _-> do
        addWarning ("desugarDefn.DispInterfaceDecl: failed to find interface " ++ show i_nm)
        return acc

desugarDefn acc (IDL.Exception _ _) = do
  addWarning ("desugarDefn: Exception not handled")
  return acc

desugarDefn acc (IDL.Forward (IDL.Id nm)) = do
  attrs <- getAttributes
  mod   <- getFilename
  let
   iface_nm 
    | optJNI     = mkHaskellTyConName (snd (splitLast "." nm))
    | otherwise  = nm

   home_mod
     | optOneModulePerInterface = Just iface_nm
     | otherwise                = mod
     
   inherit
     | optCorba  = [(cObject,0)]
     | otherwise = []

  addToTypeEnv nm home_mod (Core.Iface iface_nm home_mod nm [] False inherit, attrs)
  flg   <- isInLibrary
  if flg then do
     mb_iface <- lookupIface nm
     case mb_iface of
       Nothing -> return acc -- warn?
       Just (Core.Interface i _ inhs ds) -> 
          return (Core.Interface i True inhs ds : acc)
   else
     return acc

desugarDefn acc (IDL.Import ls) = do
  -- store the imported entities away in the various environments we're carrying.
  old_nm <- getFilename
  openUpScope $ 
     (if (isJust old_nm) then
        inImportedContext
      else
        id) (sequence (map (\ (nm,ds) -> do
                            let the_nm = dropSuffix nm
                            nm_to_use <- nameOfImport the_nm
                            setFilename (Just nm_to_use)
                            src <- getSrcFilename
                            addToPath the_nm $ desugarer src ds) ls))
  setFilename old_nm
  return acc

desugarDefn acc (IDL.ImportLib nm)
 | not optIgnoreImpLibs = do
    old_nm <- getFilename
    let nm' = dropSuffix (basename nm)
     -- We only set the filename, if it hasn't been set before
     -- i.e., we're not processing a toplevel declaration.
    nm_to_use <- nameOfImport nm'
    when (isJust old_nm) (setFilename (Just nm_to_use))
    d <- ioToDsM (importLib nm)
    when (dumpIDL && optVerbose) (ioToDsM (putStrLn (showIDL (ppDefn d))))
    ls <- 
     addToPath nm' $
     openUpScope   $
       (if (isJust old_nm) || not optTlb then
           inImportedContext
        else
           id)
       (foldM desugarDefn acc [d])
    setFilename old_nm
    if (isJust old_nm) then
       return acc
     else
       return ls
 | otherwise = do
    addWarning ("desugarDefn: ignoring importlib("++show nm++");\n Type library imports (via importlib) not handled yet")
    return acc

desugarDefn acc (IDL.Pragma str)   =  do
   handlePackPragma (dropWhile isSpace str)
   return acc

desugarDefn acc (IDL.CppQuote str) = return (Core.CLiteral str : acc)
desugarDefn acc (IDL.HsQuote str)  = return (Core.HsLiteral str : acc)
desugarDefn acc (IDL.CInclude s)   = return (Core.CInclude s    : acc)
desugarDefn acc IDL.IncludeStart{} = return acc  -- shouldn't occur, but no harm done.
desugarDefn acc IDL.IncludeEnd     = return acc  -- shouldn't occur, but no harm done.

\end{code}

\begin{code}
desugarProp :: ([IDL.Attribute], IDL.Type, IDL.Id) -> DsM Core.Decl
desugarProp (attrs, ty, i) = addToPath (iName i) $ do
  core_attrs <- idlToCoreAttributes (attrs ++ idAttrs i)
  (nm, core_ty, real_ty) <- mkCoreIdTy ty i True []
  home_mod   <- getFilename
  let 
      prop_i = mkId nm (iName i) home_mod core_attrs
      set_i  = prop_i{ Core.idName="set" ++ mkHaskellTyConName (Core.idName prop_i)
                     , Core.idOrigName="set" ++ mkHaskellTyConName (Core.idOrigName prop_i)
                     }
      get_i  = prop_i{ Core.idName="get" ++ mkHaskellTyConName (Core.idName prop_i)
                     , Core.idOrigName="get" ++ mkHaskellTyConName (Core.idOrigName prop_i)
                     }
  return (Core.Property prop_i
                        core_ty
                        Nothing
                        set_i
                        get_i
                        )

desugarCoClassMember :: IDL.CoClassMember -> DsM Core.CoClassDecl
desugarCoClassMember (isInterface, IDL.Id nm, attrs) = addToPath nm $ do
  core_attrs <- idlToCoreAttributes attrs
  when (hasSourceAttribute core_attrs) (addSourceIface nm)
  home_mod  <- getFilename
  mb_iface  <- lookupIface nm
  let 
    attrs_to_pin_on = 
      core_attrs ++ 
      case mb_iface of
        Just d -> Core.idAttributes (Core.declId d)
        _      -> []

    i = mkId nm nm the_mod attrs_to_pin_on
    
    the_mod = 
      case mb_iface of
        Just (Core.Interface{Core.declId=ii})     -> Core.idModule ii
        Just (Core.DispInterface{Core.declId=ii}) -> Core.idModule ii
        _ -> home_mod

  if isInterface 
   then return (Core.CoClassInterface     i mb_iface)
   else return (Core.CoClassDispInterface i mb_iface)
\end{code}

\begin{code}
mkCoreIdTy :: IDL.Type 
           -> IDL.Id
           -> Bool
           -> [Core.Attribute] 
           -> DsM (String, Core.Type, Core.Type)
mkCoreIdTy ty i isTopLev attrs = do
   (n,t) <- mkCoreIdTy' ty' i' isTopLev attrs
   return (n, t, normaliseType t)
 where
   (qual, ty') = getTyQual ty

   i' = 
     case (prec i) of
       IDL.Pointed ([]:qs) pi -> IDL.Pointed (qual:qs) pi
       oi -> oi

   -- ToDo: implement precedences properly.
   prec (IDL.Pointed q pi)  = prec1 (IDL.Pointed q) pi
   prec (IDL.ArrayId ai es) = prec2 (\ z -> IDL.ArrayId z es) ai
   prec pi = pi

   prec1 cont (IDL.ArrayId ai es) = prec1 (\ x -> IDL.ArrayId (cont x) es) ai
   prec1 cont pi = cont pi

   prec2 cont (IDL.Pointed q pi) = prec2 (\ x -> IDL.Pointed q (cont x)) pi
   prec2 cont pi = cont pi

mkCoreIdTy' :: IDL.Type 
            -> IDL.Id
            -> Bool
            -> [Core.Attribute] 
            -> DsM (String, Core.Type)
mkCoreIdTy' ty (IDL.Id nm) _ attrs = do
  as        <- getAttributes
  real_ty   <- withAttributes (attrs ++ as) (idlToCoreTy ty)
  return (nm, {-core_ty,-} real_ty)

mkCoreIdTy' ty (IDL.AttrId as i) isTopLev attrs = do
  core_as   <- idlToCoreAttributes as
  mkCoreIdTy' ty i isTopLev (core_as ++ attrs)

mkCoreIdTy' ty (IDL.CConvId _ i) isTopLev attrs = mkCoreIdTy' ty i isTopLev attrs

mkCoreIdTy' ty (IDL.ArrayId i es) _ attrs = do
  (nm, core_ty) <- mkCoreIdTy' ty i False attrs
  core_exprs    <- mapM idlToCoreExpr es
  cenv          <- getConstEnv
  let core_exprs' = map (simpRedExpr cenv intTy) core_exprs
  return (nm, {-Core.Array core_ty core_exprs,-} Core.Array core_ty core_exprs')

mkCoreIdTy' ty (IDL.Pointed quals i) isTopLev local_attrs = do
    (nm, real_ty) <- mkCoreIdTy' ty i False local_attrs
    let ty_nm = showCore (ppType (removePtr real_ty))
    core_ty              <- mkPointer ty_nm real_ty quals
--    orig_ty            <- mkPointer ty_nm orig_ty' quals
    return (nm, core_ty) --, orig_ty)
 where

    {- 
     + toplevel pointer receives its pointer attribute
       from its local attributes.
     + [string] is handled specially, it applies to the
       innermost (char) pointer.
    -}
  mkPointer _  tty []       = return tty
  mkPointer nm tty ls@(x:xs)
   | hasStringAttribute local_attrs = mkStringTy nm tty ls
   | hasSeqAttribute local_attrs    = do
        let
         mb_expr = 
          case getLengthAttribute local_attrs of
             Just (Core.ParamLit l)  -> Just (Core.Lit l)
             Just (Core.ParamExpr e) -> Just e
             Just (Core.ParamVar v)  -> Just (Core.Var v)
             _                       -> Nothing
         mb_term =
          case findAttribute "terminator" local_attrs of
            Just (Core.Attribute _ (ap:_)) ->
               case ap of
                 Core.ParamLit l  -> Just (Core.Lit l)
                 Core.ParamExpr e -> Just e
                 Core.ParamVar v  -> Just (Core.Var v)
                 _                -> Nothing
                 
            _ -> Nothing

        core_ty <- foldM (mkPtr nm) tty xs
        return (Core.Sequence core_ty mb_expr mb_term)

   | otherwise = do
        let
         attrs_to_use
           = case x of
              (Const:_)    -> local_attrs ++ [Core.Attribute "ref" []]
              (Volatile:_) -> local_attrs ++ [Core.Attribute "ptr" []]
              _            -> local_attrs

        core_ty <- foldM (mkPtr nm) tty xs
        return ((findPtrType isTopLev attrs_to_use) core_ty)

  mkStringTy _  tty []     = return tty
  mkStringTy _  tty [_] 
    | normaliseType tty == Core.WChar   = return (Core.WString (hasUniqueAttribute local_attrs)
                                                               Nothing)
    | otherwise                         = return (Core.String tty --(Core.Char False) 
                                                              (hasUniqueAttribute local_attrs)
                                                              Nothing)
  mkStringTy nm tty (x:xs) = do
       ty' <- mkStringTy nm tty xs
       mkPtr nm ty' x

   {- assigning the right pointer type goes as follows:
        * if type definition specifies a pointer type (this includes
          its definition context) - use it.
        * if the current attribute context specifies one - use it.
        * if all of the above fails, use *ref* for parameters, and *unique*
          for fields/ function results.
   -}
  mkPtr nm tty [] = do
        mb_res <- lookupType nm
        case mb_res of 
          Just (_,_,attrs) -> 
            return ((findPtrType False attrs) tty)
          Nothing -> findDef tty

   {- Note: if you specify 

         typedef const const volatile const volatile int* foo;

      you'll get a const (aka ref) pointer, i.e., leftmost
      qualifier overrides whatever comes after.
   -}
  mkPtr _ acc (Const:_) = return (Core.Pointer Ref True acc)
  mkPtr _ acc (_:_)     = return (Core.Pointer Ptr True acc)

  findDef t =  do
     attrs <- getAttributes
     return ((findPtrType False attrs) t)
       
mkCoreIdTy' ty (IDL.FunId i mb_cc params) isTopLev attrs = do
  (nm,core_ty) <- mkCoreIdTy' ty i isTopLev attrs
  core_params  <- idlToCoreParams (iName i) params
  let cc = fromMaybe defaultCConv mb_cc
  let t  = Core.FunTy cc (Core.Result (normaliseType core_ty) core_ty) core_params
  return ( nm, t)
\end{code}

\begin{code}
idlToCoreParams :: String -> [IDL.Param] -> DsM [Core.Param]
idlToCoreParams meth ps = zipWithM (idlToCoreParam meth) [(1::Int)..] ps

idlToCoreParam :: String -> Int -> IDL.Param -> DsM Core.Param
idlToCoreParam meth idx (IDL.Param i ty attrs) = 
   -- We allow either the use of "arg<X>" or the parameter name here,
   -- so hackily we check whether the first alt. is in scope, before
   -- plumping for the second.
   --
  let
   configPath d
    | optUseAsfs = do
       ls   <- getPath
       let alt1 = "arg" ++ show idx
       res  <- lookupAsf (ls ++ '.':alt1)
       if (isJust res) then
          addToPath alt1 d
        else 
          addToPath (iName i) d
    | otherwise = d

  in
  configPath $ do
  core_attrs  <- idlToCoreAttributes attrs
  let 
      withIn     = hasModeAttribute In    core_attrs
      withOut    = hasModeAttribute Out   core_attrs
      withInOut  = hasModeAttribute InOut core_attrs || (withIn && withOut)

      core_attrs2 
        | withOut && not withInOut && optOutPointersAreRefs 
        = (Core.Attribute "ref" []):core_attrs -- toplevel pointers
        | otherwise 
        = core_attrs

      (p_ty, p_i) = movePointers ty i

  (nm, core_ty, real_ty) <- mkCoreIdTy p_ty p_i True core_attrs2
  mb_if                  <- getInterface
  let
    if_prefix x = 
      case mb_if of
       Nothing -> meth ++ '.':x
       Just y  -> y ++ '.':meth ++ '.':x

     {-
      Defaulting the parameter mode and type.
     -}
    (mode, real_ty', core_ty')
     | withInOut  = (InOut, real_ty, core_ty)
     | withOut    = 
          case real_ty of
             Core.Pointer _ _ (Core.Pointer _ _ Core.Void)
                 | core_attrs `hasAttributeWithName` "iid_is" -> 
                        -- normalise double-pointed out args with iid_is(); ignore
                        -- the supplied pointer modifiers and insist on ref-ref.
                        ( Out
                        , Core.Pointer Ref True (Core.Pointer Ref True iUnknownTy)
                        , mkRefPointer (rawPointerToIP core_ty)
                        )
             Core.Pointer pt isExp t 
                    -- insist on a [ref] here.
              | optOutPointersAreRefs -> (Out, Core.Pointer Ref isExp t', mkRefPointer c_ty')
              | otherwise -> (Out, Core.Pointer pt isExp t', core_ty)
                 where
                   (t', c_ty') 
                        -- we insist that out i-pointers are {r}*{r}*.
                     | isIfacePtr t = (Core.Pointer Ref isExp (getIfaceTy t), 
                                       Core.Pointer Ref isExp (getIfaceTy core_ty))
                     | otherwise    = (t, core_ty)

             _ | optCompilingOmgIDL -> (Out, Core.Pointer Ref True real_ty, mkRefPointer core_ty)
               | otherwise          -> (Out, real_ty, core_ty)

     | withIn     = (In, real_ty, core_ty)
     | optVerbose && not optNoWarnMissingMode
                  = trace ("Warning: no mode for parameter " ++ 
                           show (if_prefix (iName i))        ++
                           " (defaulting it to [in].)")
                          (In, real_ty, core_ty)
     | otherwise  = (In, real_ty, core_ty)

    is_dependent = hasDependentAttrs core_attrs2

     {-
       Duplicating the default'ed parameter mode in the attribute list makes
       it possible to pretty print a param without looking at its mode field.
     -}
    core_attrs_final 
      | not (withIn || withOut || withInOut) = (Core.AttrMode mode:core_attrs2)
      | otherwise = core_attrs2
      
    core_param = 
        Core.Param (mkId nm (iName i) Nothing core_attrs_final)
                   mode real_ty' core_ty' is_dependent
                   
  return (validateParam (if_prefix (iName i)) core_param) 

movePointers :: IDL.Type -> IDL.Id -> (IDL.Type, IDL.Id)
movePointers (IDL.TyPointer t) i = movePointers t (IDL.Pointed [[]] i)
movePointers t i = (t,i)

\end{code}

Having a front end type representation that differs slightly 
from the intermediate rep., is somewhat tedious.

\begin{code}
idlToCoreTy :: IDL.Type -> DsM Core.Type
idlToCoreTy ty = 
 case ty of
  IDL.TyInteger sz -> return (Core.Integer sz True)
  IDL.TyFloat sz   -> return (Core.Float sz)
  IDL.TyStable     -> return (Core.StablePtr)
  IDL.TyChar       -> return (Core.Char False)
  IDL.TyWChar      -> return (Core.WChar)
  IDL.TyBool       -> return (Core.Bool)
  IDL.TyOctet      -> return (Core.Octet)
  IDL.TyAny        -> return (Core.Any)
  IDL.TyObject | optJNI    -> return (Core.Iface "JObject"  jniLib  "java.lang.Object" [] False [])
               | otherwise -> return Core.Object
  IDL.TyBString    -> return bstrTy
  IDL.TyFun mb_cc r_ty ps -> do
     core_ps  <- idlToCoreParams "" ps
     res_ty   <- idlToCoreTy r_ty
     let cc    = fromMaybe defaultCConv mb_cc
     return (Core.FunTy cc (Core.Result (normaliseType res_ty) res_ty) core_ps)

  IDL.TyVoid       -> return (Core.Void)
  IDL.TyIface nm   ->
     case nm of
       "IUnknown"  -> return iUnknownTy
       "IDispatch" -> return iDispatchTy
       _ -> do
         -- attribute right module for where the type was defined.
         -- Don't reduce the type here.
        res <- lookupType nm
        let
           inherit
             | optCorba  = [(cObject,0)]
             | otherwise = []
        attrs <- getAttributes
        case res of
          Nothing          -> return (Core.Iface nm Nothing nm attrs False inherit)
          Just (_, tty, _) -> return tty

  IDL.TyName "java.lang.String" _ | optJNI -> return (Core.String (Core.Char False) False Nothing)
  IDL.TyName nm mb_t -> do
     -- attribute it with the module where the type was defined.
     -- Don't reduce the type here, wait until the cleanup/renaming pass.
     let
       (qual, ty_nm)
          | optCompilingOmgIDL = splitLast "::" nm
          | otherwise          = ([], nm)
       mb_mod = toMaybe null qual

     res    <- do
         r <- lookupType ty_nm
         case r of
           Nothing -> lookupType nm
           Just _  -> return r
     as     <- getAttributes
     case res of
       Nothing -> do
           mb_ti  <- lookupTypeInfo ty_nm
           case mb_t of
             Nothing 
                | optJNI ->
                  case splitLast "." ty_nm of
                    (bef,aft) 
                      | notNull bef -> do 
                        -- strong indication of an object type, repr. it as an Iface.
                       attrs <- getAttributes
                       let iface_nm = mkHaskellTyConName aft
                       case ty_nm of
                         -- JNI lib has got special support for Strings.
                        "java.lang.String" -> return (Core.String (Core.Char False) False Nothing)
                        _ -> return (Core.Iface iface_nm (Just iface_nm) ty_nm attrs False [(jObject,0)])
                                
                      | otherwise      ->
                        return (Core.Name ty_nm ty_nm mb_mod Nothing Nothing mb_ti)
                | otherwise -> do
                    tg <- lookupTag nm
                    case tg of
                      Nothing -> return (Core.Name ty_nm ty_nm mb_mod
                                                   Nothing Nothing mb_ti)
                      Just (mod1,v) -> return (Core.Name v v mod1
                                                   Nothing Nothing mb_ti)
             Just it -> do
                ot  <- idlToCoreTy it
                case ot of
                  Core.Iface inm mod _ attrs is_idis inh | inm == ty_nm || optJNI -> 
                       return (Core.Iface inm mod ty_nm (attrs ++ as) is_idis inh)
--                     return (Core.Iface aft (Just aft) ty_nm (attrs ++ as) is_idis inh)
                    -- rid ourselves of compiler introduced synonyms, if
                    -- they just refer to another name.
                  Core.Name{} 
                     | "IHC_TAG" `isPrefixOf` ty_nm    -> return ot
                     | "__IHC_TAG" `isPrefixOf` ty_nm  -> return ot
                  _ -> return (Core.Name ty_nm ty_nm mb_mod (Just as) (Just ot) mb_ti)
       Just (mod, tty, attrs) ->
           -- Avoid creating (Name nm (Iface nm ..))
          case tty of
             Core.Iface "IUnknown" _ _ _ _ _  -> return iUnknownTy -- sigh.
             Core.Iface "IDispatch" _ _ _ _ _ -> return iDispatchTy
             Core.Iface "String" (Just _) _ _ _ _ | optJNI -> return (Core.String (Core.Char False) False Nothing)
             Core.Iface inm imod tnm iattrs is_idis inh 
               | optJNI || inm == ty_nm -> do
                  return (Core.Iface inm imod tnm (iattrs ++ ty_attrs) is_idis inh)
             Core.Name _ _ _ _ _ mb_ti 
                     | "IHC_TAG" `isPrefixOf` ty_nm    -> return tty
                     | "__IHC_TAG" `isPrefixOf` ty_nm  -> return tty
                     | otherwise -> return (Core.Name ty_nm ty_nm mod (Just ty_attrs)
                                                      the_ty mb_ti)
             _ -> 
               return (Core.Name ty_nm ty_nm mod (Just ty_attrs) the_ty Nothing)
         where
          the_ty   = Just tty
          ty_attrs = attrs ++ as

  IDL.TyPointer (IDL.TyName "wchar_t" Nothing)  -> do
     return (Core.WString False Nothing)
  IDL.TyPointer t  -> do
     core_ty <- idlToCoreTy t
     return (Core.Pointer Ref True core_ty)
{-
  IDL.TyFixed e i  -> do
     core_expr <- idlToCoreExpr e
     return (Core.Fixed core_expr i)
-}
  IDL.TyArray t es -> do
     core_ty <- idlToCoreTy t
     core_es <- mapM idlToCoreExpr es
     cenv    <- getConstEnv
     let core_exprs = map (simpRedExpr cenv intTy) core_es
     return (Core.Array core_ty core_exprs)
  IDL.TySafeArray t    -> do
     core_ty <- idlToCoreTy t
     return (Core.SafeArray core_ty)
  IDL.TyApply (IDL.TySigned s) (IDL.TyInteger sz) ->
     return (Core.Integer sz s)
  IDL.TyApply (IDL.TySigned s) IDL.TyChar ->
     return (Core.Char s)
  IDL.TyApply (IDL.TySigned s) t -> do
     t' <- idlToCoreTy t
     case t' of
       Core.Name _ _ _ _ (Just (Core.Integer i _)) _  -> return (Core.Integer i s)
       _                -> return (Core.Integer Long s)
  IDL.TySigned s -> return (Core.Integer Long s)
  IDL.TyApply (IDL.TyQualifier _) t -> idlToCoreTy t
  IDL.TyApply t (IDL.TyQualifier _) -> idlToCoreTy t
  IDL.TyString mb_expr     -> do
     core_expr <- mapFromMb (return Nothing) ((mapDsM Just) . idlToCoreExpr) mb_expr
     cenv      <- getConstEnv
     let core_expr' = fmap (simpRedExpr cenv intTy) core_expr
     return (Core.String (Core.Char False) False core_expr')
  IDL.TyWString mb_expr    -> do
     core_expr <- mapFromMb (return Nothing) ((mapDsM Just) . idlToCoreExpr) mb_expr
     cenv      <- getConstEnv
     let core_expr' = fmap (simpRedExpr cenv intTy) core_expr
     return (Core.WString False core_expr')
  IDL.TySequence t mb_expr -> do
     core_ty   <- idlToCoreTy t
     core_expr <- mapFromMb (return Nothing) ((mapDsM Just) . idlToCoreExpr) mb_expr
     cenv      <- getConstEnv
     let core_expr' = fmap (simpRedExpr cenv intTy) core_expr
     return (Core.Sequence core_ty core_expr' Nothing)

    -- the rest of the IDL.Type constructors have optional
    -- (Maybe-valued) fields - we here assume that a previous
    -- pass have filled these fields in with a value.

   -- an enum-reference; lookup real type in environment
  IDL.TyEnum (Just (IDL.Id nm)) [] -> do
     res       <- lookupType nm
     attrs     <- getAttributes
     home_mod  <- getFilename
     case res of
       Nothing             -> return (Core.Enum (mkId nm nm Nothing attrs) Unclassified [])
       Just (_,core_ty, _) -> idlToCoreTy (IDL.TyName (Core.idName (getTyTag core_ty)) Nothing)
  IDL.TyEnum (Just (IDL.Id nm)) enums -> do
    core_enums <- fillInEnums (Left (0::Int32)) enums
    attrs      <- getAttributes
    home_mod   <- getFilename
    let 
         {-
           Try to characterise the enumeration sequence as being
           an instance of a kind that's easy to generate code
           for in the end (e.g., if the tags start from zero and
           inc. by one, we can use Haskell's "deriving" mechanism
           to generate enum <--> Int mappings.
         -}
        kind
         | not (isJust mb_tags) = Unclassified
         | otherwise            = classifyProgression (sort tags)

        mb_tags = getEnumTags [] core_enums
        (Just tags) = mb_tags
        
        core_enums_to_use
          | isJust mb_tags = sortBy cmpTag core_enums
          | otherwise      = core_enums

        cmpTag (Core.EnumValue _ (Left t1))
               (Core.EnumValue _ (Left t2)) = compare t1 t2

        getEnumTags acc [] = Just (reverse acc)
        getEnumTags acc ((Core.EnumValue _ (Left x)):xs) = getEnumTags (x:acc) xs
        getEnumTags _   _  = Nothing

    return (Core.Enum (mkId nm nm home_mod attrs) kind core_enums_to_use)
   where
       fillInEnums _ [] = return []
       fillInEnums n ((IDL.Id tnm, attrs, Nothing):xs) = do
          addToConstEnv tnm n
          ls          <- fillInEnums (addOne n) xs
          inh_attrs   <- getAttributes
          core_attrs  <- idlToCoreAttributes attrs
          home_mod    <- getFilename
          return ((Core.EnumValue (mkId tnm tnm home_mod (core_attrs ++ inh_attrs)) n) : ls)
       fillInEnums _ ((IDL.Id tnm, attrs, Just e):xs) = do
          n' <- reduceExpr (idlToCoreTy) e
          addToConstEnv tnm n'
          inh_attrs   <- getAttributes
          core_attrs  <- idlToCoreAttributes attrs
          home_mod    <- getFilename
          ls <- fillInEnums (addOne n') xs
          return ((Core.EnumValue (mkId tnm tnm home_mod (core_attrs ++ inh_attrs)) n'): ls)

       addOne (Left  n) = Left (n+1)
       addOne (Right e) = Right (Core.Binary Add 
                                             e 
                                             (Core.Lit (iLit (1::Int))))

  IDL.TyStruct (Just (IDL.Id nm)) [] mb_packed -> do
            tg <- lookupTag nm
            case tg of
              Just (_,v)  -> idlToCoreTy (IDL.TyName v Nothing)
              Nothing -> do
                 attrs    <- getAttributes
                 home_mod <- getFilename
                 mb_pck   <- getCurrentPack
                 return (Core.Struct (mkId nm nm home_mod attrs) [] (mb_packed `mplus` mb_pck))

  IDL.TyStruct (Just (IDL.Id _)) [(t,_,[i])] _
    | optUnwrapSingletonStructs && not (isAnonTy t) -> idlToCoreTy (transferPointedness i t)
  IDL.TyStruct (Just (IDL.Id nm)) mems mb_packed -> do
    core_mems <- mapM memberToField mems
    attrs     <- getAttributes
    home_mod  <- getFilename
    mb_pck   <- getCurrentPack
    return (Core.Struct (mkId nm nm home_mod attrs) (concat core_mems) (mb_packed `mplus` mb_pck))

  IDL.TyUnion (Just (IDL.Id nm1)) t 
              (IDL.Id nm2) (Just (IDL.Id nm3)) switches -> do
    core_ty   <- idlToCoreTy t
    core_sw   <- idlToCoreSwitches switches
    attrs     <- getAttributes
    home_mod  <- getFilename
    return (Core.Union (mkId nm1 nm1 home_mod attrs)
                       core_ty
                       (mkId nm2 nm2 home_mod attrs)
                       (mkId nm3 nm3 home_mod attrs)
                       core_sw)
  IDL.TyUnionNon (Just (IDL.Id nm1)) switches -> do
    core_sw   <- idlToCoreSwitches switches
    attrs     <- getAttributes
    home_mod  <- getFilename
    return (Core.UnionNon (mkId nm1 nm1 home_mod attrs) core_sw)
   -- a union-reference; lookup real type in environment
  IDL.TyCUnion (Just (IDL.Id nm)) [] mb_pack -> do
     res <- lookupType nm
     attrs <- getAttributes
     case res of
       Just (_,core_ty, _) -> return core_ty
       Nothing             -> do
           home_mod  <- getFilename
           mb_pck   <- getCurrentPack
           return (Core.CUnion (mkId nm nm home_mod attrs) [] (mb_pack `mplus` mb_pck))
  IDL.TyCUnion (Just (IDL.Id nm1)) members mb_pack -> do
    core_mems <- mapM memberToField members
    attrs     <- getAttributes
    home_mod  <- getFilename
    mb_pck   <- getCurrentPack
    return (Core.CUnion (mkId nm1 nm1 home_mod attrs) (concat core_mems) (mb_pack `mplus` mb_pck))
  _ -> error ("idlToCoreTy: " ++ showIDL (PpIDL.ppType ty))

\end{code}

\begin{code}
memberToField :: IDL.Member -> DsM [Core.Field]
memberToField (ty, attrs, ids) = do
  core_attrs <- idlToCoreAttributes attrs
  home_mod   <- getFilename
  let
   mkCoreField i = do
    let (f_ty, f_i, mb_sz) = 
           case (movePointers ty i) of
             (t, IDL.BitFieldId x bi) -> (t, bi, Just x)
             (t,fi) -> (t,fi,Nothing)
    (nm, orig_ty, core_ty) <- mkCoreIdTy f_ty f_i False core_attrs
    as <- getAttributes
    let as2 = core_attrs ++ as
    return (Core.Field (mkId nm nm home_mod as2)
                       core_ty orig_ty
                       mb_sz Nothing)

  mapM mkCoreField ids

idlToCoreSwitches :: [IDL.Switch] -> DsM [Core.Switch]
idlToCoreSwitches switches = mapM idlToCoreSwitch switches

{- 
   Since switches contain types and expressions,
   we cannot share the Switch type between Core and IDL.
-}
idlToCoreSwitch :: IDL.Switch -> DsM Core.Switch
idlToCoreSwitch (IDL.Switch labs (Just (IDL.Param i ty attrs))) = addToPath (iName i) $ do
  core_attrs             <- idlToCoreAttributes attrs
  (nm, orig_ty, core_ty) <- mkCoreIdTy ty i False core_attrs
  as                     <- getAttributes
  let as2 = core_attrs ++ as
  core_labs <- mapM idlToCoreCaseLabel labs
  home_mod  <- getFilename
  return (Core.Switch (mkId nm (iName i) home_mod as2)
                      (concat core_labs)
                      core_ty
                      orig_ty
                      )

idlToCoreSwitch (IDL.Switch [IDL.Default] Nothing) = return (Core.SwitchEmpty Nothing)
idlToCoreSwitch (IDL.Switch labs Nothing) = do
  core_labs <- mapM idlToCoreCaseLabel labs
  let tg_names = concatMap toLabel labs
  return (Core.SwitchEmpty (Just (zip (concat core_labs) tg_names)))
 where
  toLabel IDL.Default   = ["Anon"] -- good enough?
  toLabel (IDL.Case es) = map exprToName es

idlToCoreCaseLabel :: IDL.CaseLabel -> DsM [Core.CaseLabel]
idlToCoreCaseLabel IDL.Default  = return [Core.Default]
idlToCoreCaseLabel (IDL.Case es) =
  mapM (\ e -> do
          core_e <- idlToCoreExpr e
          cenv      <- getConstEnv
          let core_expr = simpRedExpr cenv intTy core_e
          return (Core.Case core_expr))
       es
\end{code}

%*
%
\section[expr]{Converting expressions}
%
%*

The only difference between @IDL.Expr@ and @Core.Expr@
is that they use different @Type@ types.

\begin{code}
idlToCoreExpr :: IDL.Expr -> DsM Core.Expr
idlToCoreExpr e =
 case e of
  IDL.Binary bop e1 e2 -> do
      c1 <- idlToCoreExpr e1
      c2 <- idlToCoreExpr e2
      return (Core.Binary bop c1 c2)
  IDL.Cond e1 e2 e3 -> do
      c1 <- idlToCoreExpr e1
      c2 <- idlToCoreExpr e2
      c3 <- idlToCoreExpr e3
      return (Core.Cond c1 c2 c3)
  IDL.Unary op e1 -> do
      c  <- idlToCoreExpr e1
      return (Core.Unary op c)
  IDL.Var nm -> do
      res <- lookupConst nm
      case res of
        Nothing         -> return (Core.Var nm)
        Just (Left v)   -> return (Core.Lit (iLit v))
        Just (Right e1) -> return e1
  IDL.Lit l  ->
      return (Core.Lit l)
  IDL.Cast t e1 -> do
      core_t   <- idlToCoreTy t
      c        <- idlToCoreExpr e1
      return (Core.Cast (normaliseType core_t) c)
  IDL.Sizeof t -> do
      core_t  <- idlToCoreTy t
      return (Core.Sizeof (normaliseType core_t))

\end{code}

%*
%
\subsection{Filling in}
%
%*

Before translating into the core syntax, we fill in the
tags and Ids that are optional.

\begin{code}
fillInDefn :: IDL.Defn -> NSM [IDL.Defn]
fillInDefn def =
 case def of
  IDL.Typedef ty attrs ids -> do
      let withName = 
            case ids of
              (IDL.Id s : _) -> withTyTag s
              _ -> id
      (ty', ds1) <- fillInType (withName ty)
      (ty'', ds) <- simplifyType False attrs ty'
       {- put the (type) declarations that have been lifted out of ty'
          before the typedef itself, so that they're in scope when
          processing it later. (Note: this isn't sufficient to deal
          with recursive defns.)
       -}
      let ids' = map massageId ids
      return (ds1 ++ ds ++ [IDL.Typedef ty'' attrs ids'])

  IDL.TypeDecl ty -> do
      (ty', ds1) <- fillInType ty
      (ty'', ds) <- simplifyType False [] ty'
      return (ds1 ++ ds ++ [IDL.TypeDecl ty''])

  IDL.Constant i attrs ty e -> do
      (ty',ds1) <- fillInType ty
      return (ds1 ++ [IDL.Constant i attrs ty' e])
  IDL.Attributed attrs d -> do
      ds' <- fillInDefn d 
      return (map (IDL.Attributed attrs) ds')
  IDL.Attribute ids read_only ty -> do
      (ty',ds) <- fillInType ty
      let ids' = map massageId ids
      return (ds ++ [IDL.Attribute ids' read_only ty'])
  IDL.Operation i ty mb_raise mb_context -> do
      (ty',ds1) <- fillInType ty
      let i' = massageId i
      (fi,ds)   <- fillInFunId i'
      return (ds1 ++ ds ++ [IDL.Operation fi ty' mb_raise mb_context])
  IDL.Interface i inherit defs -> do
      defs' <- mapM fillInDefn defs
      return [IDL.Interface i inherit (concat defs')]
  IDL.Module i defs -> do
      defs' <- mapM fillInDefn defs
      return [IDL.Module i (concat defs')]
  IDL.Library i defs -> do
      defs' <- mapM fillInDefn defs
      return [IDL.Library i (concat defs')]
  IDL.ExternDecl ty [i] | optHaskellToC -> do
      let i' = massageId i
      fillInDefn (IDL.Operation i' ty Nothing Nothing)   
  IDL.DispInterface i props meths -> do
      meths' <- mapM fillInDefn meths
      return [IDL.DispInterface i props (concat meths')]
  _ -> return [def]

{- "foo(void)" is the same as "foo()" - spot this here rather
   than in the parser, and remove the "void"
-}
removeVoidParam :: [IDL.Param] -> [IDL.Param]
removeVoidParam [IDL.Param (IDL.Id "") IDL.TyVoid _] = []
removeVoidParam ps = ps

fillInFunId :: IDL.Id -> NSM (IDL.Id, [IDL.Defn])
fillInFunId (IDL.FunId i mb_cc ps) = do
  let ps1 = removeVoidParam ps
  -- lift out non-trivial arguments from parameter positions
  -- and create typedefs for them. Needed to marshall them properly.
  stuff <- zipWithM fillInParam ps1 [(1::Int)..]
  let (ps2, dss) = unzip stuff
  return (IDL.FunId i mb_cc ps2, concat dss)
fillInFunId i = return (i,[])

fillInParam :: IDL.Param -> Int -> NSM (IDL.Param, [IDL.Defn])
fillInParam (IDL.Param i ty attrs) x = do
  (i', ty',  ds) <- fillInParamId i
  return (IDL.Param i' ty' attrs, ds)
 where
   fillInParamId (IDL.Id "")        = return (IDL.Id ("arg"++show x), ty, [])
   fillInParamId pi@(IDL.Id _)      = return (pi, ty, [])
   fillInParamId (IDL.AttrId as ai) = do
      (ai', ty', ds) <- fillInParamId ai
      return (IDL.AttrId as ai', ty', ds)
   fillInParamId pi@(IDL.ArrayId _ _) = return (pi, ty, [])
   fillInParamId (IDL.CConvId cc ci)  = do
      (i', ty', ds) <- fillInParamId ci
      return (IDL.CConvId cc i', ty', ds)
     -- just ignore CConvIds here.
   fillInParamId (IDL.BitFieldId _ bi) = fillInParamId bi
   fillInParamId (IDL.Pointed qs pi)   = do
      (i', ty', ds) <- fillInParamId pi
      return (IDL.Pointed qs i', ty', ds)
   fillInParamId (IDL.FunId fi cc_i ps)  = do
      let ps1 = removeVoidParam ps
      stuff <- zipWithM fillInParam ps1 [(1::Int)..]
      let (ps2, dss) = unzip stuff
      (i', ty', ds) <- fillInParamId fi
      new_nm        <- getNewName
      let new_def = [IDL.Typedef (IDL.TyFun cc_i ty' ps2) [] [IDL.Id new_nm] ]
      return (i', IDL.TyName new_nm Nothing, ds ++ concat dss++ new_def)

-- The enum, struct and union constructors may have optional
-- fields. fillInType decorates them.
--
-- ToDo: document the naming strategy.
--
-- The reason why we're floating out a bunch of defns too in
-- the result of 'fillInType' is that for function types we
-- have to introduce a 'typedef' in order to generate marshalling
-- code for it. A lot of plumbing for a not-too-common case.
-- ToDo: consider separating the hoisting of 'FunIds' into a
-- separate pass.
-- 
fillInType :: IDL.Type -> NSM (IDL.Type, [IDL.Defn])
fillInType ty =
 case ty of
   IDL.TyPointer t -> do 
       (t',ds) <- fillInType t
       return (IDL.TyPointer t', ds)

   IDL.TyArray t es -> do
       (t',ds) <- fillInType t
       return (IDL.TyArray t' es, ds)

   IDL.TyApply f a -> do
       (f',ds1) <- fillInType f
       (a',ds2) <- fillInType a
       return (IDL.TyApply f' a', ds1 ++ ds2)

   IDL.TySequence t mb_expr -> do
       (t',ds1) <- fillInType t
       return (IDL.TySequence t' mb_expr, ds1)

   IDL.TyEnum mb_id enums -> do
     id <- 
       case mb_id of
        Just _  -> return mb_id
        Nothing -> mapNSM (Just . (IDL.Id)) getNewName
     return (IDL.TyEnum id enums, [])

   IDL.TyStruct mb_tag structs mb_pack -> do
     tag <-                       
       case mb_tag of
         Just (IDL.Id v)  -> return (Just (IDL.Id v))
         Nothing          -> mapNSM (Just . (IDL.Id)) (getNewName)
     stuff <- mapM fillInMember structs
     let (structs', dss) = unzip stuff
     return (IDL.TyStruct tag structs' mb_pack, concat dss)

   IDL.TyUnion mb_tag t switch_tag mb_union_struct_tag switches -> do
     (t',ds)  <- fillInType t
     tag <-                       
       case mb_tag of
         Just (IDL.Id v)  -> return (Just (IDL.Id v))
         Nothing          -> mapNSM (Just . (IDL.Id)) (getNewName)
     union_struct_tag <-
       case mb_union_struct_tag of
         Just (IDL.Id v)  -> return (Just (IDL.Id v))
         Nothing          -> return (Just (IDL.Id "tagged_union"))
                   -- mimicing MIDL here

     stuff <- mapM fillInSwitch switches
     let (switches', dss) = unzip stuff
     return (IDL.TyUnion tag t' switch_tag union_struct_tag switches',
             ds ++ concat dss)

   IDL.TyUnionNon mb_tag switches -> do
     tag <-                       
       case mb_tag of
         Just (IDL.Id v)  -> return (Just (IDL.Id v))
         Nothing          -> mapNSM (Just . (IDL.Id)) (getNewName)
     stuff <- mapM fillInSwitch switches
     let (switches', dss) = unzip stuff
     return (IDL.TyUnionNon tag switches', concat dss)

   IDL.TyCUnion mb_tag members mb_pack -> do
     tag <-                       
       case mb_tag of
         Just (IDL.Id v) -> return (Just (IDL.Id v))
         Nothing         -> mapNSM (Just . (IDL.Id)) (getNewName)
     stuff <- mapM fillInMember members
     let (members', dss) = unzip stuff
     return (IDL.TyCUnion tag members' mb_pack, concat dss)

   _ -> return (ty, [])

fillInMember :: IDL.Member -> NSM (IDL.Member, [IDL.Defn])
fillInMember (ty, attrs, ids) = do
        (ty',ds1) <- fillInType ty
        (is,ds)   <- 
           case ids of
             [] -> do
               n <- getNewName
               return ([IDL.Id n], [])
             _  -> do
              stuff <- mapM fillInId ids
              let (is, dss) = unzip stuff
              return (is, concat dss)
        return ((ty', attrs, is), ds1 ++ ds)

fillInSwitch :: IDL.Switch -> NSM (IDL.Switch, [IDL.Defn])
fillInSwitch (IDL.Switch labs arm) = do
         (arm', ds)  <- fillInArm arm
         return (IDL.Switch labs arm', ds)

fillInArm :: Maybe IDL.SwitchArm -> NSM (Maybe IDL.SwitchArm, [IDL.Defn])
fillInArm Nothing = return (Nothing, [])
fillInArm (Just (IDL.Param i ty attr)) = do
  (ty',ds1) <- fillInType ty
  (i',ds2)  <- fillInId i
  return (Just (IDL.Param i' ty' attr), ds1 ++ ds2)

fillInId :: IDL.Id -> NSM (IDL.Id, [IDL.Defn])
fillInId (IDL.Id "") = do  -- ToDo: document exactly when an empty Id can occur.
   x <- getNewName
   return (IDL.Id x, [])
fillInId i@(IDL.Id _) = return (i, [])
fillInId (IDL.AttrId as i) = do
  (i', ds) <- fillInId i
  return (IDL.AttrId as i', ds)
fillInId (IDL.ArrayId i es)  = do
   (i',ds) <- fillInId i
   return (IDL.ArrayId i' es, ds)
fillInId (IDL.Pointed qs i)  = do
   (i',ds) <- fillInId i
   return (IDL.Pointed qs i', ds)
fillInId (IDL.CConvId c  i)  = do
   (i',ds) <- fillInId i
   return (IDL.CConvId c i', ds)
fillInId (IDL.BitFieldId x i)  = do
   (i',ds) <- fillInId i
   return (IDL.BitFieldId x i', ds)
fillInId (IDL.FunId i cc ps) = do
   let ps1 = removeVoidParam ps
   stuff    <- zipWithM fillInParam ps1 [(1::Int)..]
   let (ps2, dss) = unzip stuff
   (i',ds1) <- fillInId i
   return (IDL.FunId i' cc ps2, ds1 ++ concat dss)

\end{code}

\begin{code}
idlToCoreAttributes :: [IDL.Attribute] -> DsM [Core.Attribute]
idlToCoreAttributes attrs = do
  as <- 
    if not optUseAsfs then
       return attrs
     else do
       pth         <- getPath
       res         <- lookupAsf pth
       return $
        case res of
          Nothing -> attrs
          Just (False, ss) -> ss
          Just (_, ss) -> attrs ++ ss
       
  mapM idlToCoreAttribute as

augmentAttributes :: [Core.Attribute] -> DsM [Core.Attribute]
augmentAttributes inh_attrs
  | not optUseAsfs = return inh_attrs
  | otherwise      = do
      pth  <- getPath
      res  <- lookupAsf pth
      case res of
        Nothing -> return inh_attrs
        Just (False, ss) -> mapM idlToCoreAttribute ss
        Just (_,ss) -> do
          ss' <- mapM idlToCoreAttribute ss
          return (inh_attrs ++ ss')

idlToCoreAttribute :: IDL.Attribute -> DsM (Core.Attribute)
idlToCoreAttribute (IDL.Mode m) =
  case m of 
    In    -> return (Core.AttrMode m)
    Out   -> return (Core.AttrMode m)
    InOut -> return (Core.AttrMode m)
idlToCoreAttribute (IDL.Attrib i params) = do
  core_params <- mapM convParam params
  let
   nm        = iName i
   mb_reason = stringToDepReason nm

   attr_con
    | isJust mb_reason  = Core.AttrDependent (fromJust mb_reason)
    | otherwise         = Core.Attribute nm

  return (attr_con core_params)
 where
   convParam (IDL.AttrExpr (IDL.Lit l)) = return (Core.ParamLit l)
   convParam (IDL.AttrExpr (IDL.Var v)) = do
      res <- lookupConst v
      case res of
        Nothing        -> return (Core.ParamVar v)
        Just (Left v1) -> return (Core.ParamLit (iLit v1))
        Just (Right e) -> return (Core.ParamExpr e)

   convParam (IDL.AttrExpr e) = do
      core_e <- reduceExpr (\ x -> idlToCoreTy x) e
      case core_e of
         Left l   -> return (Core.ParamLit (iLit l))
         Right e1 -> return (Core.ParamExpr e1)

   convParam (IDL.EmptyAttr)  = return (Core.ParamVoid)
   convParam (IDL.AttrLit (TypeConst tc))  = do
        ty    <- lookupType tc
        mb_ti <- lookupTypeInfo tc
        let t = 
             case ty of
               Nothing        -> Core.Name tc tc Nothing Nothing Nothing mb_ti
               Just (_,t1,as) -> Core.Name tc tc Nothing (Just as) (Just t1) mb_ti
        return (Core.ParamType (normaliseType t))
   convParam (IDL.AttrLit l)  = return (Core.ParamLit l)
   convParam (IDL.AttrPtr a)  = do
        core_a <- convParam a
        return (Core.ParamPtr core_a)

\end{code}

Prior to translation into core, we simplify union and struct types,
lifting out any embedded enum/struct/union members they might have.

\begin{code}
simplifyType :: Bool -> [IDL.Attribute] -> IDL.Type -> NSM (IDL.Type, [IDL.Defn])
simplifyType liftOut attrs ty
  | liftOut && isConstructedTy ty = do
      nm        <- getNewName
      (ty', ds) <- simplifyType False [] ty
      return (IDL.TyName nm (Just ty'), ds ++ [IDL.Typedef ty' attrs [IDL.Id nm]])
  | otherwise = 
   case ty of
    IDL.TyStruct tag mems mb_pack -> do
      (mems', decls) <- simplifyMembers attrs mems mems
      let addFwdDecl = id
{-
           case tag of
             Just i -> ((IDL.Typedef (IDL.TyStruct tag [] Nothing) [] [i]):)
             _      -> id
-}
      return (IDL.TyStruct tag mems' mb_pack, addFwdDecl decls)

    IDL.TyUnion tag t switch_tag union_struct_tag switches -> do
      (switches', decls) <- simplifySwitches switches
      return (IDL.TyUnion tag t switch_tag union_struct_tag switches', decls)

    IDL.TyCUnion tag members mb_pack -> do
      (members', decls) <- simplifyMembers attrs members members
      return (IDL.TyCUnion tag members' mb_pack, decls)

    IDL.TyUnionNon tag switches -> do
      (switches', decls) <- simplifySwitches switches
      return (IDL.TyUnionNon tag switches', decls)

    _ -> return (ty, [])

simplifyMembers :: [IDL.Attribute] -> [IDL.Member] -> [IDL.Member] -> NSM ([IDL.Member], [IDL.Defn])
simplifyMembers _ _    [] = return ([], [])
simplifyMembers p_attrs mems ((ty, attrs, [IDL.FunId i cc ps]):ms) = do
   let ps' = removeVoidParam ps
   nm <- getNewName
   (ty', ds1) <- simplifyType True (IDLUtils.childAttributes (attrs ++ p_attrs)) ty
   let ty_nm = nm
       def   = IDL.Typedef (IDL.TyFun cc ty' ps') attrs [IDL.Id ty_nm]
   (ms', ds2) <- simplifyMembers p_attrs mems ms
   return ((IDL.TyName ty_nm Nothing, attrs, [i]):ms', ds1++def:ds2)
simplifyMembers p_attrs mems (m@(ty, attrs, is):ms)
 | isConstructedTy ty && any isUnpointedId is = do
   nm <- getNewName
   let 
        {-
         In case we're lifting a (non-encap) union out of
         a struct, make sure we record the type of the switch.
        -}
       attrs' =
        case ty of
          IDL.TyUnionNon{} -> attrs ++ switch_ty_attr
          IDL.TyCUnion{}   -> attrs ++ switch_ty_attr
          _                -> attrs

       switch_ty_attr 
         | any isSwitchType (attrs ++ p_attrs) = []
         | otherwise =
            case filter (isSwitchIs) (attrs ++ p_attrs) of
               (IDL.Attrib _ [l] : _) -> 
                    let n = fromMaybe "" (findName l) in
                    case (filter (isField n) mems) of
                       ((s_ty,_,_):_) -> 
                        [IDL.Attrib (IDL.Id "switch_type") 
                                    [IDL.AttrLit (TypeConst (showIDL (PpIDL.ppType s_ty)))]]
                       _              -> []
               _ -> []


        -- ToDo: lift out into utility module.
       findName (IDL.AttrExpr e) = findNameExpr e
       findName IDL.EmptyAttr    = Nothing
       findName (IDL.AttrLit (TypeConst tc)) = Just tc
       findName (IDL.AttrPtr a)  = findName a
       
       findNameExpr expr =
         case expr of
           IDL.Binary _ e1 e2 -> findNameExpr e1 `concMaybe` 
                                 findNameExpr e2
           IDL.Cond e1 e2 e3  -> findNameExpr e1 `concMaybe`
                                 findNameExpr e2 `concMaybe`
                                 findNameExpr e3
           IDL.Unary _ e1 -> findNameExpr e1
           IDL.Var v      -> Just v
           IDL.Cast _ e   -> findNameExpr e
           IDL.Sizeof _   -> Nothing
           IDL.Lit (TypeConst tc) -> Just tc
           IDL.Lit _      -> Nothing

       isSwitchIs (IDL.Attrib (IDL.Id "switch_is") _) = True
       isSwitchIs _                                   = False
       
       isSwitchType (IDL.Attrib (IDL.Id "switch_type") _) = True
       isSwitchType _                                     = False
       
       isField n (_,_,ss) = any isNm ss
         where
          isNm (IDL.Id inm) = inm == n
          isNm _            = False

   (ty', ds1) <- simplifyType True (attrs' ++ IDLUtils.childAttributes p_attrs) ty      
   let def   = IDL.Typedef ty' attrs [IDL.Id nm]
   (ms', ds2) <- simplifyMembers p_attrs mems ms
   return ((IDL.TyName nm Nothing, attrs', is):ms', ds1 ++ def:ds2)
 | otherwise = do  
   (ms', ds) <- simplifyMembers p_attrs mems ms
   return (m:ms', ds)

simplifySwitches :: [IDL.Switch] -> NSM ([IDL.Switch], [IDL.Defn])
simplifySwitches [] = return ([],[])
simplifySwitches ((IDL.Switch labs arm):ss) = do
  (arm', ds) <- simplifyArm arm
  (ss', ds') <- simplifySwitches ss
  return ((IDL.Switch labs arm'):ss', ds++ds')

simplifyArm :: Maybe IDL.SwitchArm -> NSM (Maybe IDL.SwitchArm, [IDL.Defn])
simplifyArm Nothing = return (Nothing,[])
simplifyArm (Just (IDL.Param i ty attrs)) = do
  (ty',ds) <- simplifyType True attrs ty
  return (Just (IDL.Param i ty' attrs), ds)
\end{code}

@tidyDefns@ takes care of moving typedefs for constructed type 
references to the site where the constructed type is actually defined.
Doing this is required to generate the right data type defns. for an
example like the following:

\begin{verbatim}
typedef struct foo bar;
typedef struct foo {
        bar *ptr;
        int i;
} *pbar;
\end{verbatim}

The two typedefs are combined into one (earlier passes will
have checked that "struct foo" is a valid structure reference.)

Need to cope with both forward and backward references, so we make
one pass over the decls trying to move forward references to their
definition site, followed by another pass trying to reposition the
backward type references.

This pass isn't required if you're processing already normalised
input, i.e., input coming from the TLB reader, so an option is
provided for turning this 2-pass off.

\begin{code}
tidyDefns :: [IDL.Defn] -> [IDL.Defn]
tidyDefns orig_ds
  | optTlb || optDon'tTidyDefns = orig_ds
  | otherwise =
  case (tidyDefns' True [] [] [] orig_ds) of
    ([], [], ds) -> ds
    (cands, removeds, ds') -> 
      case (tidyDefns' False cands removeds [] ds') of
          {- We remove a definition from its original site
             only if we can successfully move it to a more appropriate site.
             Leftovers in the candidate list that by now haven't found a
             better home are simply dropped, and the defns therein are thereby
             left at their original site.
          -}
        (_,_,ds'')  ->  ds''

  where
 {- Used by debugging code. 
   defTag (IDL.TypeDecl t) = tyTag t
   defTag (IDL.Typedef t _ is) = tyTag t ++ showList (map iName is) ""
   defTag _ = ""

   removeDefs rs ds = filter (\x -> not (x `elem` rs)) ds
 -}
   removeDef d ds = filter (/=d) ds
   
   tidyDefns' _ cands removeds acc_ds [] = (cands, removeds, reverse acc_ds)
   tidyDefns' newFlag cands removeds acc_ds (d:ds) =
    case d of
     IDL.Typedef ty as is
       |  isConstructedTy ty     -- enum/struct/union
       && (isReferenceTy ty  ||  -- "typedef enum foo bar;"
           any isMIDLishId is )  -- "typedef enum { ... } __MIDL__MIDL__.... ;"
       -> 
          if newFlag then
             tidyDefns' newFlag (d:cands) removeds
                                (d:acc_ds) ds
          else if d `elem` removeds then
             tidyDefns' newFlag cands removeds acc_ds ds
          else 
             tidyDefns' newFlag cands removeds (d:acc_ds) ds

       |  isConstructedTy ty
       && isCompleteTy ty
       && haveForwardRef (tyTag ty) cands 
       -> let 
            (new_cands, moved_to_new_home, d')   = moveForwardRef d cands
          in
          tidyDefns' newFlag
                     new_cands
                     (d:removeds)   -- (moved_to_new_home ++ removeds)
                     (d': acc_ds) ds
                        -- (d':removeDefs moved_to_new_home acc_ds) ds

       |  isMIDLishTy ty 
       && haveMIDLRef (tyTag ty) cands
       -> let 
            (new_cands, d')   = moveForwardMIDLRef d cands
          in
          tidyDefns' newFlag new_cands (d:removeds) (d':removeDef d acc_ds) ds

     IDL.TypeDecl ty
       | d `elem` removeds -> tidyDefns' newFlag cands removeds acc_ds ds
       |  isConstructedTy ty
       && isCompleteTy ty
       && haveForwardRef (tyTag ty) cands
       -> 
          let 
            (new_cands, moved_to_new_home, d')  = moveForwardRef d cands
          in
          tidyDefns' newFlag new_cands
                     (d:removeds) --removeds --(d:moved_to_new_home ++ removeds)
                     (d': acc_ds) ds
                        --(d':removeDefs moved_to_new_home acc_ds) ds

       |  isConstructedTy ty
       && haveForwardRef (tyTag ty) cands
          {-
            If we see "typedef struct _P p; struct _P", remove the 
            second decl entirely. 
          -}
       -> tidyDefns' newFlag cands removeds acc_ds ds

     IDL.Attributed a a_d -> 
          let
            (new_cands,rs,d') = tidyDefns' newFlag cands removeds [] [a_d]
            attr_d            = map (IDL.Attributed a) d'
          in
          tidyDefns' newFlag new_cands rs (attr_d ++ acc_ds) ds

     IDL.Interface i inh i_ds -> 
          let
            i_ds'      
             | newFlag    = tidyDefns i_ds
             | otherwise  = i_ds
          in
          tidyDefns' newFlag cands removeds ((IDL.Interface i inh i_ds'):acc_ds) ds

     IDL.Module i m_ds ->
          let
            m_ds'
             | newFlag    = tidyDefns m_ds
             | otherwise  = m_ds
          in
          tidyDefns' newFlag cands removeds ((IDL.Module i m_ds'):acc_ds) ds

     IDL.Library i l_ds ->
          let
            l_ds'
             | newFlag    = tidyDefns l_ds
             | otherwise  = l_ds
          in
          tidyDefns' newFlag cands removeds ((IDL.Library i l_ds'):acc_ds) ds

     IDL.DispInterface i a d_ds ->
          let
            d_ds'
             | newFlag   = tidyDefns d_ds
             | otherwise = d_ds
          in
          tidyDefns' newFlag cands removeds ((IDL.DispInterface i a d_ds'):acc_ds) ds

     _ -> 
       tidyDefns' newFlag cands removeds (d:acc_ds) ds

haveForwardRef :: String -> [IDL.Defn] -> Bool
haveForwardRef nm ls = go ls
  where
    go [] = False
    go ((IDL.Typedef t _ _):_) | tyTag t == nm = True
    go (_:ds) = go ds

-- to avoid (harmless) duplication later on, tag the
-- moved/introduced defn with an 'ignore' attribute.
-- ==> no Haskell code will be generated for it.
moveForwardRef :: IDL.Defn -> [IDL.Defn] -> ([IDL.Defn], [IDL.Defn], IDL.Defn)
moveForwardRef (IDL.TypeDecl t) ls =
  (ls', cs, IDL.Typedef t (concat as) (concat is))
 where
   nm       = tyTag t 
   (cs,ls') = partition (\ (IDL.Typedef ty _ _)     -> tyTag ty == nm) ls
   (as,is)  = unzip (map (\ (IDL.Typedef _ as1 is1) -> (as1,is1)) cs)

moveForwardRef (IDL.Typedef t attrs is1) ls =
  (ls', cs, IDL.Typedef t (attrs++concat as) (is1++map addIgnoreAttrib (concat is)))
 where
   nm       = tyTag t

   (cs,ls') = partition (\ (IDL.Typedef ty _ _)     -> tyTag ty == nm) ls
   (as,is)  = unzip (map (\ (IDL.Typedef _ as1 is2) -> (as1,is2)) cs)
   
   addIgnoreAttrib i = IDL.AttrId [IDL.Attrib (IDL.Id "ignore") []] i

-- should never happen
moveForwardRef d ls = trace "moveForwardRef: funny defn." (ls, [], d)

haveMIDLRef :: String -> [IDL.Defn] -> Bool
haveMIDLRef nm 
  = any (\ (IDL.Typedef _ _ is) ->  notNull (filter (midlLooking nm) is))

midlLooking :: String -> IDL.Id -> Bool
midlLooking nm x  =
 isMIDLishId x && nm == iName x

moveForwardMIDLRef :: IDL.Defn -> [IDL.Defn] -> ([IDL.Defn], IDL.Defn)
moveForwardMIDLRef (IDL.Typedef t attrs is) ls =
 case break isMIDLDefn ls of
   (as, (IDL.Typedef real_ty attrs1 _ :bs)) ->
     (as++bs, IDL.Typedef real_ty (attrs1 ++ attrs) is)
 where
  nm = tyTag t

  isMIDLDefn (IDL.Typedef _ _ t_is) = notNull (filter (midlLooking nm) t_is)
  isMIDLDefn _                      = False
-- should never happen
moveForwardMIDLRef d ls = trace "moveForwardMIDLRef: funny defn." (ls, d)

\end{code}

Ad-hac hockily, we allow a different name to be assocaiated with an
import name.

\begin{code}
nameOfImport :: String -> DsM String
nameOfImport nm 
 | not optUseAsfs = return nm
 | otherwise      = do
    x <- lookupAsf nm
    case x of
      Just (_,as) -> do
        c_as <- mapM idlToCoreAttribute as
        case findAttribute "hs_name" c_as of
          Just (Core.Attribute _ [Core.ParamLit (StringLit s)]) -> return s
          _ -> return nm
      _ -> return nm

\end{code}
