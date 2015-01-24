%
% (c) 1999, sof
%
% @(#) $Docid: Feb. 9th 2003  15:02  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Generating Haskell-side proxies for HJOs (Haskell implemented
Java objects.)

\begin{code}
module MarshallJServ ( cgJServMethod, cgJClass ) where

import CoreIDL
import Attribute
import qualified AbstractH as Haskell ( Type )
import CoreUtils
import PpCore ( showCore, ppType )

-- code/defs to generate Haskell code:

import AbstractH   ( HDecl )
import AbsHUtils

import MarshallUtils
import MarshallJNI

import CgMonad

-- utility libraries:
import BasicTypes
import Utils   ( snoc )
import LibUtils

\end{code}

\begin{code}
cgJServMethod :: Id
              -> Result
              -> [Param]
              -> CgM HDecl
cgJServMethod i res params = do
  cls_name <- getIfaceName
  return ( helpStringComment i `andDecl` marshallMethod i cls_name params res )
 
\end{code}

The JNI library lets you export Haskell IO actions to Java by tupling
up the arguments. For the automatically generated proxies we 'un-curry'
the arguments before applying them to the Haskell method.

\begin{code}
marshallMethod :: Id
               -> String
               -> [Param]
               -> Result
               -> HDecl
marshallMethod i cls_name params res 
  | is_ignorable = emptyDecl
  | otherwise    = ty_sig `andDecl` def
 where
  nm           = mkWrapperName (idName i)
  attrs        = idAttributes i

  is_ignorable = hasAttributeWithNames 
                        attrs
                        [ "jni_get_field"
                        , "jni_set_field"
                        , "jni_ctor"
                        ]

  ty_sig      = genTypeSig nm ctxt
                              (funTy method_ty        $
                               funTy (tuple arg_tys') $
                               funTy thisTy (io res_ty'))
  def         = funDef nm pats rhs
  pats        = [patVar "meth", tuplePat arg_pats, patVar "this"]
  rhs         = funApp (mkVarName "meth") (arg_exprs `snoc` var "this")

  thisTy      = tyCon cls_name [tyVar "a"]

  (res_ty':arg_tys', ctxt) = generaliseTys (res_ty:arg_tys)

  method_ty   = foldr funTy (funTy thisTy (io res_ty')) arg_tys'

  res_ty      = toJNIType (resultOrigType res)

  arg_exprs   = map var arg_names
  arg_pats    = map patVar arg_names
  arg_names   = zipWith (\ _ x -> "arg" ++ show x) arg_tys [(0::Int)..]
  arg_tys     = map (toJNIType.paramType) params

\end{code}

Converting an IDL type to a corresponding Haskell type is
not too much work:

\begin{code}
toJNIType :: Type -> Haskell.Type
toJNIType t = 
  case t of
    Integer sz signed  -> mkIntTy sz signed
    Float Short        -> tyFloat
    Float Long         -> tyDouble
    Float _            -> error "toJNIType: unsupported Float size"
    Char _             -> tyWord16
    Bool               -> tyBool
    Octet              -> tyByte
    Object             -> mkTyCon jObject [tyVar "a"]
    String{}           -> tyString
    Name _ _ _ _ (Just ty) _ -> toJNIType ty
    Pointer _ _ ty     -> toJNIType ty
    Array ty []        -> mkTyCon jArray [toJNIType ty]
    Void                 -> tyUnit
    Iface nm imod _ attrs _ _
      | not (attrs `hasAttributeWithName` "jni_iface_ty") -> tyQCon imod nm [tyVar "a"]
      | otherwise  -> 
                    let 
                       i  = tyVar "a"
                    in
                    mkTyCon jObject
                       [ctxtTyApp (ctxtClass (mkQualName imod nm)
                                             [mkTyCon jObject [i]])
                                  i]

    _ -> error ("toJNIType: unknown type " ++ showCore (ppType t))

-- Convert a method's type into its Haskell equivalent.
toHaskellMethodTy :: Haskell.Type -> Decl -> Haskell.Type
toHaskellMethodTy obj_ty meth = funTys ps_tys (funTy obj_ty (io res_ty))
  where
    res_ty = toJNIType (resultType (methResult meth))
    ps_tys = map (toJNIType.paramType) (methParams meth)
\end{code}

\begin{code}
cgJClass :: Id
         -> [Decl]
         -> CgM HDecl
cgJClass i ds 
 | is_interface = cgJNIInterface i False
 | otherwise = do
  addExport (ieValue ctor_nm)
  addExport (ieValue cls_cls_name)
  return (cls_nm_decl `andDecl` ty_sig `andDecl` decl)
  where
    is_interface = not ((idAttributes i) `hasAttributeWithName` "jni_class")

    ty_sig = mkTypeSig ctor_nm ctor_param_tys ctor_res_ty
    decl   = funDef ctor_nm ctor_params rhs

    cls_nm = idName i ++ "Proxy"
    i'     = i{idOrigName=cls_nm}

    (cls_nm_decl, cls_cls_name) = cgClassNameDecl i'

    ctor_nm = "new_" ++ idName i

     -- only interested in the 'real' methods here.
    ms   = filter (\ d -> isMethod d && not (isIgnorable d)) ds

    ctor_param_tys = map (toHaskellMethodTy obj_ty) ms
    ctor_params    = map (varPat.methArg) meth_idxs `snoc` varPat env

    ctor_res_ty = funTy (mkTyConst jniEnv) (io fptr_ty)

    obj_ty  = mkTyCon (mkQualName (idModule i) (idName i)) [tyVar "a"]
    fptr_ty = mkTyCon (mkQualName Nothing  (idName i)) [tyUnit]

    rhs  = foldr (\ f acc -> f acc) res xs

    res  = funApp newObj [ clsName, tySpec, tup x_ms, env ]
    xs   = zipWith exportMethod ms meth_idxs

    clsName = var cls_cls_name
    tySpec  = stringLit tySpec_lit
    env     = var "env"
    
    tySpec_lit = '(':ty_args ++ ")V"

    ty_args = concat $ map (\ _ -> functionPtrTySpec) ms

    functionPtrTySpec = "LFunctionPtr;"

    x_ms = map xMethArg meth_idxs

    meth_idxs  = zipWith const [(1::Int)..] ms

    exportMethod d mi =
      bind (funApp newFPointer [funApp (mkWrapName d) [methArg mi], env])
           (xMethArg mi)
        

    xMethArg mi = var ("xm_" ++ show mi)
    methArg  mi = var ("meth_" ++ show mi)

    mkWrapName d = mkVarName (mkWrapperName (idName (declId d)))

    isIgnorable d = 
     hasAttributeWithNames (idAttributes (declId d))
                           ["jni_set_field", "jni_get_field", "jni_ctor"]

\end{code}
