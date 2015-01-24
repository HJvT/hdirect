%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Feb. 2nd 2003  07:49  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Generating marshalling code for Haskell COM servers.

\begin{code}
module MarshallJNI 
	( 
	  cgJNIMethod
	, cgJNIInterface
	, cgJNIClass
	, cgClassNameDecl
	) where

import CoreIDL hiding ( Expr(..), CaseLabel(..) )
import BasicTypes
import AbstractH hiding ( Type(..) )
import AbsHUtils
import CoreUtils
import MarshallCore
import Attribute
import Literal
import CgMonad
import LibUtils
import PpCore ( showCore, ppType )
import Opts
	( optOneModulePerInterface
        )

import Monad ( when )
import Maybe ( mapMaybe )
import Utils ( splitLast, snoc )

\end{code}

\begin{code}
cgJNIMethod :: Id -> Result -> [Param] -> CgM HDecl
cgJNIMethod i res params = do
  iface <- getIfaceName
  as    <- getIfaceAttributes
  addExport (ieValue (idName i))
  return (mkJNIMethod i iface res params as)

mkJNIMethod :: Id -> String -> Result -> [Param] -> [Attribute] -> HDecl
mkJNIMethod i iface res params iface_attrs = m_decl
 where
  name        = idName i
  m_name      = name
  m_orig_name = idOrigName i
  attrs       = idAttributes i

  m_decl      = m_tysig `andDecl` m_def
  m_tysig     = mkTypeSig m_name param_tys (io res_ty)

  m_def       = funDef m_name m_pats m_rhs

  res_ty      = 
    let r_ty = toHaskellTy True (resultOrigType res) in
    if isCtor then
       groundTyVars r_ty
    else
       r_ty

  param_tys   = map (paramToHaskellType [] False False False) params `snoc` objParamTy

  isGetField  = attrs `hasAttributeWithName` "jni_get_field"
  isSetField  = attrs `hasAttributeWithName` "jni_set_field"
  isStatic    = attrs `hasAttributeWithName` "jni_static"
  isInterface = not (iface_attrs `hasAttributeWithName` "jni_class")
  isFinal     = attrs `hasAttributeWithName` "jni_final"
  isCtor      = attrs `hasAttributeWithName` "jni_ctor"

  isStaticGetField = isGetField && isStatic
  isStaticSetField = isSetField && isStatic

  objParamTy 
    | isStatic    = mkTyConst jniEnv
    | isCtor      = mkTyConst jniEnv
    | isInterface = let 
                       tyv = tyVar "a"
		       nm  = mkHaskellTyConName iface
		       qnm
		        | optOneModulePerInterface = mkQualName (Just nm) nm
			| otherwise		   = mkQualName Nothing nm
		    in
		    mkTyCon jObject
		       [ctxtTyApp (ctxtClass qnm [mkTyCon jObject [tyv]])
			          tyv]
    | otherwise   = tyQCon Nothing (mkHaskellTyConName iface) [obj_ty_arg]
   where  
    obj_ty_arg
     | isFinal   = tyUnit
     | otherwise = tyVar "a"

  m_rhs
    | isStaticGetField  = funApp getStaticField [ var cls_cls_name, meth_name, ty_spec ]
    | isStaticSetField  = funApp setStaticField [ var cls_cls_name, meth_name, ty_spec, tup m_args ]
    | isGetField        = funApp getField [ meth_name, ty_spec ]
    | isSetField        = funApp setField [ meth_name, ty_spec, tup m_args ]
    | isCtor      = funApp newObj    [ var cls_cls_name, ty_spec, tup m_args ]
    | isStatic    = funApp invokeStaticMethod invoke_static_args
    | isInterface = funApp invokeInterfaceMethod [ meth_name, ty_spec, tup m_args ]
    | otherwise   = funApp invokeMethod [ meth_name, ty_spec, tup m_args ]

  cls_cls_name = mkClassName (mkHaskellVarName iface)

  invoke_static_args = 
          [ var cls_cls_name
	  , meth_name
	  , ty_spec
	  , tup m_args
	  ]

  ty_spec
   | isGetField = stringLit (toTyDesc (resultType res))
   | otherwise  = stringLit (mkJavaTypeSpec params res)

  meth_name 
    | isGetField = stringLit stripped_m_name
    | isSetField = stringLit stripped_m_name
    | otherwise  = stringLit m_orig_name
   where
    (_:_:_:_:stripped_m_name) = m_orig_name

  m_args    = map (\ p -> var (idName (paramId p))) params
  m_pats    = map (patVar.idName.paramId) params

\end{code}

\begin{code}
mkJavaTypeSpec :: [Param] -> Result -> String
mkJavaTypeSpec ps res = '(':concatMap tyParam ps ++ ')':tyRes res
 where
   tyParam p = toTyDesc (paramType p)
   tyRes  r  = toTyDesc (resultType r)

toTyDesc :: Type -> String
toTyDesc ty = 
   case ty of
       Integer Short _     -> "S"
       Integer Long  _     -> "I"
       Integer Natural  _  -> "I"
       Integer LongLong  _ -> "J"
       String{}            -> "Ljava/lang/String;"
       Name _ _ _ _ (Just t) _ -> toTyDesc t
       Name n _ _ _ _ _    -> 'L':trans n ++ ";"
       Iface _ _ n _ _ _ -> 'L':trans n ++ ";"
       Float Short -> "F"
       Float Long  -> "D"
       Bool        -> "Z"
       Octet       -> "B"
       Char _      -> "C"
       Void	   -> "V"
       Array t _   -> '[':toTyDesc t
       _	   -> error ("toTyDesc: unknown type " ++ showCore (ppType ty))

 where
  trans x = map dotToSlash x

  dotToSlash '.' = '/'
  dotToSlash x   = x
\end{code}

\begin{code}
cgClassNameDecl :: Id -> (HDecl, Name)
cgClassNameDecl i = (cls_name_tysig `andDecl` cls_name_def, cls_cls_name)
 where
   name	          = idName i
   attrs	  = idAttributes i
   h_nm           = mkHaskellTyConName (snd (splitLast "." name))
   cls_cls_name   = mkClassName (mkHaskellVarName h_nm)
   cls_name_tysig = typeSig cls_cls_name (mkTyCon className [ty_arg])
   ty_arg
    | attrs `hasAttributeWithName` "jni_class" = mkTyCon (mkQualName Nothing h_nm) [tyUnit]
    | otherwise				       = tyUnit -- for an interface.

   cls_name_def   = funDef cls_cls_name [] cls_name_rhs
   cls_name_rhs   = funApp makeClassName [stringLit (idOrigName i)]

cgJNIInterface :: Id
	       -> Bool
	       -> CgM HDecl
cgJNIInterface i ignore_decls = do
   when (not ignore_decls) $ do
        addExport (ieClass (mkHaskellTyConName name))
        addExport (ieValue cls_cls_name)
   let
    ds 
     | ignore_decls = emptyDecl
     | otherwise    = class_decl `andDecl` cls_nm_decl

   return ds
 where
   class_decl = hClass ctxt cls_name [tvar] []

   name	      = idName i

   (cls_nm_decl, cls_cls_name) = cgClassNameDecl i

   cls_name   = mkClsName name

   mkClsName n = mkConName (mkHaskellTyConName n)

   tvar       = mkTyVar "a"
   attrs      = idAttributes i
   is         = 
    case (findAttribute "jni_implements" attrs) of
      Just (Attribute _ [ParamLit (StringLit s)]) -> words s
      _ -> []

   ctxt       = CtxtTuple (map (\ x -> CtxtClass (mkClsName x) [tyVar "a"]) is)

\end{code}

\begin{code}
cgJNIClass :: Id
	   -> Bool
	   -> CgM HDecl
cgJNIClass i incl_type_defs 
 | incl_type_defs = return iface_inst_decls
 | otherwise      = do
   addExport (ieValue cls_cls_name)
   addExport (ieValue ctor_name)
   return (andDecls [cls_name,default_ctor, iface_inst_decls])
  where
   attrs = idAttributes i
   nm    = idOrigName i
   h_nm  = mkHaskellTyConName (snd (splitLast "." nm))

   (cls_name, cls_cls_name) = cgClassNameDecl i
   obj_ty_open    = tyCon (mkHaskellTyConName h_nm) [tyVar "a"]

   default_ctor   = ctor_tysig `andDecl` ctor_def
   ctor_tysig = typeSig ctor_name
		        (funTy (mkTyConst jniEnv)
			       (io (tyCon h_nm [tyUnit])))

   ctor_def   = funDef ctor_name [] ctor_rhs
   ctor_rhs   = funApp newObj [ var cls_cls_name
			      , stringLit "()V"
			      , tup []
			      ]
   ctor_name  = "new" ++ h_nm 

   iface_inst_decls = andDecls (map declInstance ifaces_implemented)

   ifaces_implemented = mapMaybe toNm (filterAttributes attrs ["jni_interface"])
     where
      toNm  (Attribute _ [ParamLit (StringLit s)]) = Just s
      toNm  _					   = Nothing

   declInstance n = hInstance Nothing (mkQConName hmod haskell_nm) obj_ty_open []
     where
      hmod 
        | optOneModulePerInterface = Just haskell_nm
	| otherwise                = Nothing

      haskell_nm = mkHaskellTyConName (snd (splitLast "." n))

\end{code}
