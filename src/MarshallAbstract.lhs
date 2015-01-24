%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Feb. 9th 2003  06:49  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

<tt/MarshallAbstract/ generates the stubs for IDL types
that are to be represented as abstract Haskell types. These
abstract types are only generated when in 'Haskell-2-C' mode
for (empty?) interface declarations.

\begin{code}
module MarshallAbstract ( marshallAbstract ) where

import MarshallType
import CgMonad

import BasicTypes ( QualName, qName, CallConv(..)  )
import Literal    ( Literal(..)  )
import AbstractH  ( HDecl )
import AbsHUtils
import LibUtils
import Opts       ( optHugs )

import Attribute

import CoreIDL
import CoreUtils  ( mkHaskellTyConName, addrTy )

\end{code}


\begin{code}
marshallAbstract :: Id -> CgM HDecl
marshallAbstract i = do
  ds <- mapM exportify decl_list
  return (andDecls ds)
  where
    decl_list  = 
     (if isFinalised then
        ( ( fin_name, fin_decl ) :)
      else
        id) $
     (if isFinalised then
        id  -- ToDo: generate one (it's genuinely useful in some cases.)
      else 
        ( ( f_name, f_tysig `andDecl`  f_def) :)) 
     [ ( m_name, m_tysig `andDecl`  m_def)
     , ( u_name, u_tysig `andDecl`  u_def)
     , ( w_name, w_tysig `andDecl`  w_def)
     , ( r_name, r_tysig `andDecl`  r_def)
     , ( s_name, s_tysig `andDecl`  s_def)
     , ( l_name, l_tysig `andDecl`  l_def)
     ]

    exportify (nm, d) = do
      addExport (ieValue nm)
      return d

    attrs       = idAttributes i
    isFinalised = attrs `hasAttributeWithName` "finaliser"
    
    free_routine =
      case findAttribute "free" attrs of
        Just (Attribute _ [ParamLit (StringLit s)]) -> mkVarName s
        _ -> LibUtils.free

    abs_nm     = idOrigName i
    con_name   = mkHaskellTyConName abs_nm
    name       = mkVarName con_name

    fin_name  = finaliser
    fin_decl  
      | optHugs   = prim Cdecl (dname, Nothing, orig_fin_nm, Nothing)
                         fin_name (tyFunPtr (funTy (tyPtr t_ty) io_unit))
                         False [] (False,"void*")
      | otherwise = extLabel orig_fin_nm fin_name (tyFunPtr (funTy (tyPtr t_ty) io_unit))
      where
       dname = "" -- dll location.

    (finaliser, orig_fin_nm) = 
       case (findAttribute "finaliser" attrs) of
          Just (Attribute _ [ParamLit (StringLit s)]) -> ("addrOf_" ++ idName i ++ '_':s, s)
          _ -> let s = "no-finaliser" in (s,s)

    ty_args  = 
     case findAttribute "ty_args" attrs of
      Just (Attribute _ [ParamLit (StringLit s)]) -> map tyVar (words s)
      _ -> []

    v          = var "v"
    vptr       = var "ptr"
    t_ty       = tyCon con_name ty_args

    prim_ty    = tyPtr t_ty
    b_ty
     | isFinalised = tyForeignPtr t_ty
     | otherwise   = prim_ty

    -- ** Marshalling ** --
    m_name     = qName (prefix marshallPrefix name)
    m_tysig    = typeSig m_name (funTy t_ty (io b_ty))
    m_def      = funDef m_name [conPat (mkConName con_name) [varPat v]] m_rhs
    m_rhs      = ret v

    -- ** unmarshalling ** --
    u_name     = qName (prefix unmarshallPrefix name)
    u_tysig    = typeSig u_name u_type
    u_type
      | isFinalised = funTy tyBool (funTy prim_ty (io t_ty))
      | otherwise   = (funTy prim_ty (io t_ty))

    u_def      = funDef u_name u_pats u_rhs
    u_pats 
      | isFinalised = [patVar "finaliseMe__", varPat v]
      | otherwise   = [varPat v]
    u_rhs
      | isFinalised =
           bind (funApp mkForeignObj [v, hCase (var "finaliseMe__") 
                                               [alt (conPat false []) (varName nullFinaliser)
                                               ,alt (conPat true [])  (var finaliser)
                                               ]]) v $
           ret (dataCon (mkConName con_name) [v])
      | otherwise =
           ret (dataCon (mkConName con_name) [v])
    
    -- ** Reference marshalling 
    w_name   = qName (prefix marshallRefPrefix name)
    w_tysig  = typeSig w_name w_ty
    w_ty     = funTy (tyPtr (tyPtr t_ty)) (funTy t_ty io_unit)
    w_def    = funDef w_name [ varPat vptr
                             , conPat (mkConName con_name) [varPat v]] w_rhs
    w_rhs
      | isFinalised = funApp w_fptr [vptr , v]
      | otherwise   = funApp w_ptr  [vptr , v]
    
    -- ** Reference unmarshalling 
    r_name   = qName (prefix unmarshallRefPrefix name)
    r_tysig  = typeSig r_name r_ty
    r_ty 
      | isFinalised = funTy tyBool (funTy (tyPtr t_ty) (io t_ty))
      | otherwise   = funTy (tyPtr t_ty) (io t_ty)
    r_def    = funDef r_name r_pats r_rhs
    r_pats
      | isFinalised = [patVar "finaliseMe__", varPat vptr]
      | otherwise   = [varPat vptr]
    r_rhs
      | isFinalised =
           bind (funApp r_ptr [vptr]) v $
           bind (funApp mkForeignObj [v, hCase (var "finaliseMe__") 
                                               [alt (conPat false []) (varName nullFinaliser)
                                               ,alt (conPat true [])  (var finaliser)
                                               ]]) v $
           ret (dataCon (mkConName con_name) [v])
      | otherwise   = 
           bind (funApp r_ptr  [vptr]) v $
           ret (dataCon (mkConName con_name) [v])

    f_name   = qName (prefix freePrefix name)
    f_tysig  = typeSig f_name (funTy t_ty io_unit)
    f_def    = funDef f_name [conPat (mkConName con_name) [varPat v]] f_rhs
    f_rhs    = funApp free_routine [v]
    s_name   = qName (prefix sizeofPrefix name)
--     s_name   = sizeOfName
    s_tysig  = typeSig s_name tyWord32
    s_def    = funDef s_name [] s_rhs
    s_rhs    = szType addrTy

    -- addr --> ADT-type lifting fun.

    l_name   = qName (prefix "addrTo" name)
    l_tysig  = typeSig l_name (funTy anyTyPtr t_ty)
    l_def    = funDef l_name [varPat v] l_rhs
    l_rhs
      | isFinalised =
        funApp (mkQVarName ioExts "unsafePerformIO")
           [bind (funApp mkForeignObj [castPtr v, var finaliser]) v $
            ret (dataCon (mkConName con_name) [v])]
      | otherwise =
           dataCon (mkConName con_name) [castPtr v]

    w_ptr    = prefix marshallRefPrefix (mkQVarName hdirectLib ptrName)
    r_ptr    = prefix unmarshallRefPrefix (mkQVarName hdirectLib ptrName)
    w_fptr   = prefix marshallRefPrefix (mkQVarName hdirectLib fptr)

\end{code}
