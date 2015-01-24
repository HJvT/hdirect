%
% (c) The Foo Project, University of Glasgow, 1999
%
% @(#) $Docid: Nov. 24th 2003  09:56  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Marshalling procedures/function values


\begin{code}
module MarshallFun ( marshallFun ) where

import BasicTypes
import Attribute
import Literal
import LibUtils
import CgMonad

import qualified AbstractH as Haskell ( HDecl )
import AbsHUtils

import CoreIDL
import CoreUtils

import MarshallType
import MarshallServ   ( cgServMethod )
import MarshallMethod ( cgMethod )
import MarshallCore

import Data.Maybe

\end{code}


@marshallFun@ performs two major tasks:

+ generates the marshalling code reqd. to deal
  with function values / callbacks.
+ generates the wrapper code needed for exposing
  Haskell functions (i.e., non COM) to the outside
  world.

As elsewhere, the Haskell code generating code is pig ugly.

\begin{code}
marshallFun :: Maybe Name -> Id -> Type -> CgM Haskell.HDecl
marshallFun mb_mod_nm i (FunTy cc res ps)
  | exportFun = do
     h     <- setInterfaceFlag StdFFI (cgServMethod i real_result ps False False)
     return (andDecls [m_decl, e_decl, h])
  | otherwise = do
     h   <- setInterfaceFlag StdFFI (cgServMethod i real_result ps False False)
     d   <- setInterfaceFlag StdFFI (cgMethod (i{idName=uw_name}) cc real_result ps
                                              Nothing (Just i_name))
     let decl_list = imp_decls ++ exp_decls
     ds  <- mapM exportDecl decl_list
     return (andDecls (ds ++ [d,h]))
 where
  exportFun     = isJust mb_mod_nm

  imp_decls = 
    [ (u_name, u_decl)
    , (i_name, i_decl)
    , (re_name, re_decl)
    , (s_name,  s_decl)
    ]

  exp_decls = 
    [ (m_name, m_decl)
    , (e_name, e_decl)
    , (wr_name, wr_decl)
    ]

   {- If we're marshalling a function pointer, drop the pointer
      off the result.
   -}
  real_result = res{resultType=res_ty, resultOrigType=res_orig_ty}
  res_ty      = removePtr (resultType res)
  res_orig_ty = removePtr (resultOrigType res)

  name      = mkVarName (idName i)
  v_name    = mkHaskellVarName (idName i)

  m_name 
    | not exportFun = qName (prefix marshallPrefix name)
    | otherwise     = qName (appendStr "_proxy" name)

  e_name 
    | not exportFun = qName (prefix "export_" name)
    | otherwise     = m_name
  i_name    = qName (prefix "import_" name)
  w_name    = mkWrapperName (qName name)
  wr_name   = qName (prefix marshallRefPrefix name)
  re_name   = qName (prefix unmarshallRefPrefix name)
  uw_name   = qName (prefix "unwrap_" name)
  u_name    = qName (prefix unmarshallPrefix name)

  isPure    = not exportFun && (idAttributes i) `hasAttributeWithName` "pure"

  (ty, mb_c) = toHaskellMethodTy isPure False False Nothing ps real_result
  base_ty    = toHaskellBaseMethodTy True ps real_result
   -- base_ty is the type sig of the call-ins, i_base_ty is for
   -- the call-out. They may differ in that the latter can be
   -- passed ForeignObjs, while the former receives the FO args
   -- in Addr form.
  i_base_ty  = toHaskellBaseMethodTy False ps real_result

  m_decl    = m_tysig `andDecl` m_def
  m_tysig   = genTypeSig m_name mb_c m_type
  m_def     
    | not exportFun = funDef m_name [patVar v_name] m_rhs
    | otherwise     = funDef m_name [] m_rhs
  m_rhs     
    | not exportFun = funApply (var e_name) [funApply (var w_name) [var v_name]]
    | otherwise     = funApply (var w_name) [varName f_name]

  m_type
    | exportFun     = ty
    | otherwise     = funTy ty (io (tyPtr ty))

  wr_decl   = wr_tysig `andDecl` wr_def
  wr_tysig  = genTypeSig wr_name mb_c wr_type
  wr_type   = funTy (tyPtr (tyPtr ty)) $
              funTy ty                 $
              io_unit
  wr_def    = funDef wr_name [patVar "fptr", patVar v_name] wr_rhs
  wr_rhs    = 
     bind (funApply (var m_name) [var v_name]) (var "ptr") $
     funApp w_ptr [var "fptr", var "ptr"]

  e_decl    = fexport cc e_loc e_name e_ty

  e_prim_ty = funTy base_ty (io (tyPtr ty))

   {- Name of the *Haskell* function. -}
  f_name    = 
       case (findAttribute "entry" (idAttributes i)) of
         Just (Attribute _ (ParamLit (StringLit x) : _)) -> mkQVarName mb_mod_nm x
         _ -> mkQVarName mb_mod_nm (idName i)

  (e_loc, e_ty)
    | exportFun = (Just (idName i), base_ty)
    | otherwise = (Nothing, e_prim_ty)


  i_decl    = primcst cc i_name (funTy (tyPtr ty) i_base_ty)
                      has_structs c_ty_args c_res_ty

  u_decl    = u_tysig `andDecl` u_def
  u_tysig   = genTypeSig u_name mb_c (funTy (tyPtr ty) (io ty))
  u_def     = funDef u_name [patVar "fptr"] u_rhs
  u_rhs     = ret (funApply (var uw_name) [funApply (var i_name) [var "fptr"]])

  re_decl   = re_tysig `andDecl` re_def
  re_tysig  = genTypeSig re_name mb_c (funTy (tyPtr ty) (io ty))
  re_def    = funDef re_name [] re_rhs
  re_rhs    = var u_name

  s_name   = qName (prefix sizeofPrefix name)
  s_decl   = s_tysig `andDecl` s_def
  s_tysig  = typeSig s_name tyWord32
  s_def    = funDef s_name [] s_rhs
  s_rhs    = szType addrTy

  has_structs = any (fst) ls
  ls@(c_res_ty:c_ty_args) = map (isStruct.toCType) (res_ty:p_tys)
     where
       isStruct (Left x)  = (False, x)
       isStruct (Right x) = (True, x)

  p_tys     = map paramType ps

  w_ptr    = prefix marshallRefPrefix (mkQVarName hdirectLib ptrName)

marshallFun _ _ _ = error "marshallFun"
\end{code}
