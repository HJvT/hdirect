%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Jun. 9th 2003  16:33  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

\begin{code}
module MarshallUtils 
	(
	  mkHVar
	, adjustField
	
	, prefixHTy
	, appHTy
	
	, infoHeader
	
	, helpStringComment
	
	, toHaskellIfaceTy

	, findParamDependents
	, findFieldDependents
	, removeDependees
	, removeDependents
	, removeDependers
	) where

import BasicTypes
import CoreUtils
import CoreIDL
import Attribute
import qualified AbstractH as Haskell
import AbsHUtils   ( var, prefix, prefixApp, mkVarName, comment,
		     andDecl, andDecls, emptyDecl, tyQCon, tyVar, mkTyCon,
		     tyQConst, ctxtTyApp, ctxtClass
		   )
import PpCore      ( ppDecl, showCore, setDebug )
import PpAbstractH ( ppType, showAbstractH )

import LibUtils

import Literal
import Data.List  ( intersperse )
import Utils ( notNull )
import Opts  ( optShowIDLInComments, optIgnoreHelpstring,
	       optCorba, optHaskellToC, optJNI, optSubtypedInterfacePointers )

\end{code}

\begin{code}
mkHVar :: Id -> Haskell.Expr
mkHVar i = var (mkHaskellVarName (idName i))
\end{code}

Fields and parameters with pointer types that are represented
as lists in Haskell land, are turned into raw([ptr]) pointers
prior to marshalling. Why? Because the marshalling of these pointers
has already been performed by the dependency marshalling. (Since
[ptr] marshalling is the identity operation at the moment, we
could equally well remove the dependent fields/params from the
marshalling lists.)

\begin{code}
adjustField :: Bool -> [(Id,[Dependent])] -> Field -> Maybe Field
adjustField forMarshalling dep_list f
--  isSwitchDependee dep_list (fieldId f) = Nothing
 | isDepender dep_list i && not (isArrayTy ty)
 = 
    if (forMarshalling && not (isIfaceTy ty)) || isSwitchDepender dep_list i then
       Just (f{fieldType=mkPtrPointer ty})
    else
       Just (f{fieldType=Pointer Ptr True Void})
 | otherwise
 = if not forMarshalling && isArrayTy ty && isDepender dep_list i then
      Just (f{fieldType=Pointer Ptr True Void})
   else
      Just f
 where
  i  = fieldId f
  ty = fieldType f

\end{code}

\begin{code}
prefixHTy :: String -> Haskell.Type -> QualName
prefixHTy pre ty = 
  case ty of
    Haskell.TyVar _ tv  -> prefix pre tv
    Haskell.TyCon tc    -> prefix pre tc
    Haskell.TyApply f _ -> prefixHTy pre f
    Haskell.TyList _    -> mkVarName (pre ++ list)
    _ -> error ("prefixHTy: unexpected type" ++ showAbstractH (ppType ty))

appHTy :: String -> Haskell.Type -> QualName
appHTy pre ty = 
  case ty of
    Haskell.TyVar _ tv  -> prefixApp pre tv
    Haskell.TyCon tc    -> prefixApp pre tc
    Haskell.TyApply f _ -> appHTy pre f
    Haskell.TyList _    -> mkVarName (pre ++ list)
    _ -> error ("prefixHTy: unexpected type" ++ showAbstractH (ppType ty))

\end{code}

Prefixing

\begin{code}
infoHeader :: Decl -> Haskell.HDecl
infoHeader d =
 case d of
  Interface i _ _ _ ->
	 header "interface" (idOrigName i)  `andDecl`
	 idlDecls			    `andDecl`
         line

  CoClass i _ ->
	 header "coclass" (idOrigName i)    `andDecl`
	 coIdlDecls			    `andDecl`
         line
 
  DispInterface i _ _ _ ->
	 header "dispinterface" (idOrigName i)  `andDecl`
	 idlDecls                               `andDecl`
         line

  _ -> emptyDecl
 where
  header pre nm = 
    line                   `andDecl`
    comment ""	           `andDecl`
    comment (pre++' ':nm)  `andDecl`
    comment ""
	   
  ifaces = 
    case d of
      CoClass _ ds -> map toStr ds
      _ -> error "MarshallUtils.infoHeader: Expected a coclass."
   
  toStr dcl = 
	let
	 i     = coClassId dcl
	 attrs = idAttributes i

	 if_source
	  | attrs `hasAttributeWithName` "source" = ("[source]" ++)
	  | otherwise = id
	in
	if_source (idOrigName i)

  coIdlDecls
   | optShowIDLInComments = idlDecls
   | otherwise		  =
        comment ("  implements: "  ++ unwords (intersperse "," ifaces))

  idlDecls
   | optShowIDLInComments = andDecls (map comment (lines (showCore (setDebug False $ ppDecl d))))
   | otherwise		  = emptyDecl

  line = comment "--------------------------------------------------"
 

\end{code}

\begin{code}
helpStringComment :: Id -> Haskell.HDecl
helpStringComment i
 | optIgnoreHelpstring = emptyDecl
 | otherwise           =
      case findAttribute "helpstring" (idAttributes i) of
	Just (Attribute _ (ParamLit (StringLit hs):_)) -> comment hs
        _ -> emptyDecl
\end{code}

The T-translation of interface types is provided as a separate function, as
its needed by both MarshallType.toHaskellBaseTy and MarshallCore.toHaskellTy

\begin{code}
toHaskellIfaceTy :: Type -> Haskell.Type
toHaskellIfaceTy (Iface nm mo _ attrs _ _)
   -- dear, oh dear: special treatment of IUnknown and IDispatch just to make
   -- AutoPrim.idl generate right looking code.
 | optCorba = tyQCon  mo nm [iface_ptr_ty_arg]
 | optHaskellToC  && nm `notElem` ["IUnknown", "IDispatch"] =
    case findAttribute "ty_args" attrs of
       Just (Attribute _ [ParamLit (StringLit s)]) -> tyQCon mo (mkHaskellTyConName nm) (map tyVar (words s))
       _ -> tyQConst mo (mkHaskellTyConName nm)
 | optJNI && attrs `hasAttributeWithName` "jni_iface_ty" =
        let i = tyVar "a" in
	mkTyCon jObject
		[ ctxtTyApp (ctxtClass (mkQualName mo nm) [mkTyCon jObject [i]]) i ]
 | optSubtypedInterfacePointers = tyQCon mo (mkHaskellTyConName nm) [iface_ptr_ty_arg]
 | otherwise		        = tyQConst mo (mkHaskellTyConName nm)
 where
  iface_ptr_ty_arg = tyVar "a"
toHaskellIfaceTy _ = error "toHaskellIfaceTy: not an interface type"
\end{code}

Given a parameter dependency list, figure out which parameters
should be represented as a Haskell list.

[This code is somewhat simplistic, as being a dependent doesn't necessarily 
 mean that a pair of parameters to an IDL method should be coalesced
 into a Haskell list.]

\begin{code}
findParamDependents :: Bool -> [Param] -> ( [Param], DependInfo )
findParamDependents isOut ps = (removeDependees deps ps, deps)
 where
  deps = filter (notNull.snd) $     -- 12/98: strengthened - deps list now 
				    -- only contain the real dependent args.
         (if isOut then (\ ls -> zipWith notVoidPtr ps ls) else id) $
	 findDependents (map paramId ps)

  notVoidPtr _ t@(_,[]) = t
  notVoidPtr p t@(x,_) 
    | isVoidPointerTy (paramType p) = (x,[])
    | otherwise = t

findFieldDependents :: [Field] -> DependInfo
findFieldDependents fs = deps
 where
  deps = filter (notNull.snd) $     -- 12/98: strengthened - deps list now 
				    -- only contain the real dependent args.
         (\ ls -> zipWith notVoidPtr fs ls) $
	 findDependents (map fieldId fs)

  notVoidPtr _ t@(_,[]) = t
  notVoidPtr f t@(x,_) 
    | isVoidPointerTy (fieldType f) = (x,[])
    | otherwise = t


removeDependees :: DependInfo -> [Param] -> [Param]
removeDependees ls ps = filter (not.(isDependee ls).paramId) ps

removeDependents :: DependInfo -> [Param] -> [Param]
removeDependents ls ps = filter (not.(isHParamDep ls)) ps

removeDependers :: DependInfo -> [Param] -> [Param]
removeDependers ls ps = filter (\ x -> not (isDependee ls (paramId x)) &&
				       not (isHParamDep ls x)) ps

-- local utility fun, used by the two previous defs. only.
isHParamDep :: DependInfo -> Param -> Bool
isHParamDep ls p = isDepender ls (paramId p)

\end{code}
