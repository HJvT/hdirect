%
% (c) Sigbjorn Finne, 1999
%

Generation skeleton implementations of Haskell
servers:

\begin{code}
module Skeleton where

import AbstractH ( HDecl, HTopDecl )
import MarshallCore
import MarshallUtils
import MkImport
import AbsHUtils
import CoreIDL
import CoreUtils
import Attribute
import LibUtils

import Data.List ( partition )

\end{code}

Top-level chap, one skeleton per coclass.

\begin{code}
cgSkeleton :: [Decl] -> [(String, Bool, [HTopDecl])]
cgSkeleton decls = map genSkeleton coclasses_only
  where
   coclasses_only = filter isCoClass (reallyFlattenDecls decls)
\end{code}

\begin{code}
genSkeleton :: Decl -> (String, Bool, [HTopDecl])
genSkeleton (CoClass cid ds) = 
  ( name ++ ".hs"
  , False
  , [hModule name False exports imports mod_decls]
  )
 where
  exports   = []
  imports   = map (\ (nm,f,ls) -> hImport nm f ls)
                  (mkImportLists name (getHsImports cid) [mod_decls])

  mod_decls = 
          obj_state_decl           `andDecl`
          new_decl                 `andDecl`
          andDecls prop_selectors  `andDecl`
          andDecls stub_meths

  (props,meths) = partition isProperty coclass_decls
  coclass_decls = filter (isMethodOrProp) (concat (map (getDecls) ds))

  name          = mkHaskellTyConName (idName cid)

  prop_selectors = map mkPropSelect props
  stub_meths     = map mkStubMethod meths
  
  getDecls (CoClassInterface     _ (Just (Interface _ _ _ decls))) = decls
  getDecls (CoClassDispInterface _ (Just (DispInterface _ (Just (Interface{declDecls=decls})) _ _))) = decls
  getDecls (CoClassDispInterface _ (Just (DispInterface _ _ ps ms))) = ps ++ ms
  getDecls _ = []

  new_decl = new_tysig `andDecl` new_def
  new_nm    = "new"
  new_tysig = typeSig new_nm (io obj_ty)
  new_def   = valDef new_nm (ret (dataConst (mkQConName Nothing obj_dc)))

  obj_nm         = mkHaskellTyConName (idName cid)
  obj_state_decl = dataTy obj_dc [] [recConBanged obj_dc fields]
  obj_dc         = "State"
  obj_ty         = tyConst "State"

  fields = map mkField props
  mkField (Property propId ty _ _ _) = 
    ("prop"++idName propId, tyQCon ioExts "IORef" [toHaskellTy False ty])
  mkField _ = error "Skeleton.genSkeleton.mkField: it only groks Properties"


  mkPropSelect (Property i ty _ seti geti)
     | attrs `hasAttributeWithName` "readonly" = getter
     | otherwise                               = getter `andDecl` setter
    where
     attrs         = idAttributes i
     getter        = mkPropGet seti ty
     setter        = mkPropSet geti ty
  mkPropSelect (Method i _ res _ _) 
     | attrs `hasAttributeWithName` "propget" = getter
     | otherwise                              = setter
    where
     ty            = resultType res --toHaskellMethodTy (Just (tyConst name)) ps res
     attrs         = idAttributes i
     getter        = mkPropGet i ty
     setter        = mkPropSet i ty
  mkPropSelect _ = error "Skeleton.genSkeleton.mkPropSelect: it only groks Properties and Methods"

  mkPropGet i ty   = getter
   where
     prop_ty       = toHaskellTy False ty
     prop_field_nm = "prop"++idName i
     getter        = get_tysig `andDecl` get_def
     get_tysig     = typeSig get_name get_type
     get_name      = mkHaskellTyConName (idName i)
     get_type      = funTy obj_ty (io prop_ty)
     get_def       = funDef get_name [patRec (mkVarName obj_nm) 
                                          [(mkVarName prop_field_nm, patVar prop_field_nm)]]
                         get_rhs
     get_rhs    = funApp (mkQVarName ioExts "readIORef") [var prop_field_nm]

  mkPropSet i ty = setter
    where
     prop_ty    = toHaskellTy False ty
     prop_field_nm = "prop"++idName i
     setter     = set_tysig `andDecl` set_def
     set_tysig  = typeSig set_name set_type
     set_name   = mkHaskellTyConName (idName i)
     set_type   = funTy prop_ty (funTy obj_ty io_unit)
     set_def    = funDef set_name [ patVar "val___"
                                  , patRec (mkVarName obj_nm) 
                                           [ (mkVarName prop_field_nm
                                           , patVar prop_field_nm)
                                           ]
                                  ]
                         set_rhs
     set_rhs    = funApp (mkQVarName ioExts "writeIORef") [var prop_field_nm, var "val___"]

genSkeleton _ = error "Skeleton.genSkeleton: can only generate code skeletons from coclasses"

mkStubMethod :: Decl -> HDecl
mkStubMethod (Method i _ res ps _) =
   stub_tysig `andDecl`
   stub_def
 where
   name            = mkHaskellVarName (idName i)

   stub_tysig        = genTypeSig name mb_c stub_type
   stub_def          = funDef name stub_in_pats stub_rhs
   (stub_type, mb_c) = toHaskellMethodTy isPure
                                         True {- is server -}
                                         False
                                         (Just (tyConst "State"))
                                         ps res
   isPure          = (idAttributes i) `hasAttributeWithName` "pure"
   stub_rhs        = funApply (varName prelError) [stringLit "Your code goes here"]
   stub_in_pats    = map (varPat.mkHVar.paramId) meth_params

   (pars, _, _, _, _) = binParams ps
   meth_params        = real_params ++ [obj_param]
   (real_params, _)   = findParamDependents False pars
   obj_param          = objParam (idName i)
mkStubMethod _ = error "Skeleton.mkStubMethod: it only groks Methods"
\end{code}

