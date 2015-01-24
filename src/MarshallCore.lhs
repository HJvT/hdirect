%
% (c) The Foo Project, University of Glasgow, 1999
%
% @(#) $Docid: Dec. 9th 2003  09:07  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Higher-level marshalling code - working over Core IDL 
constructs. (That's the Official Line - the Real Reason
for this module is that it avoids creating a mutual
dependency between MarshallType and MarshallDep.)

\begin{code}
module MarshallCore
       (
         toHaskellMethodTy
       , toHaskellTy
       , paramToHaskellType

       , mkHStructDef
       , mkHEnumDef
       , mkHUnionDef
       , mkCUnionDef

       , toHaskellBaseTy
       , toBaseTy
       , toHaskellBaseMethodTy

       , mkMarshaller
       
       , autoTypeToHaskellTy
       , autoTypeToQName
       , mbAutoTypeToHaskellTy
       
       , constrainIIDParams
       
       ) where

import qualified AbstractH as Haskell ( Type, ConDecl, Context )
import AbsHUtils

import CoreIDL
import CoreUtils

import MarshallUtils

import BasicTypes
import Attribute
import Literal
import LibUtils
import PpCore
import Utils    ( notNull, trace )
import Opts
import TypeInfo ( TypeInfo(..) )

import Maybe
import List     ( nub )

\end{code}

Converting a interface method signature into its corresponding 
Haskell type. This means taking into consideration the presence
of [out] parameters plus dependent arguments etc.

\begin{code}
toHaskellMethodTy :: Bool
		  -> Bool
		  -> Bool
	          -> Maybe Haskell.Type
		  -> [Param]
		  -> Result
		  -> (Haskell.Type, Maybe Haskell.Context)
toHaskellMethodTy isPure isServer isAuto mb_iface_ty params result 
  = case generaliseTys (mb_io_res_ty: the_param_tys) of
     ((r:ps), mb_c) -> (funTys ps r, mb_c)
     _              -> error "MarshallCore.toHaskellMethodTy: unexpected result"		       
  where
   mb_io_res_ty
     | isPure    = the_res_ty
     | otherwise = io the_res_ty
     
   (pars, _, _, _,res)     = binParams params
   (real_params, par_deps) = findParamDependents False pars
   (_, res_deps)           = findParamDependents False res

   the_param_tys =
     case mb_iface_ty of
       Nothing -> param_tys
       Just x  -> param_tys ++ [x]

   (param_tys, res_tys) = 
       constrainIIDParams (paramToHaskellType par_deps isServer isAuto False)
       			  (paramToHaskellType res_deps isServer isAuto True)
			  real_params
			  res_params

   res_params = removeDependees res_deps res

   the_res_ty =
     tuple (
      case (resultOrigType result) of
	t  | isHRESULTTy t && not optKeepHRESULT -> res_tys
	   | not (isVoidTy (removeNames t)) -> (res_tys ++ [toHaskellTy False (resultOrigType result)])
	   | otherwise   -> res_tys)
     

constrainIIDParams :: (Param -> Haskell.Type)
		   -> (Param -> Haskell.Type)
		   -> [Param]
		   -> [Param]
		   -> ([Haskell.Type], [Haskell.Type])
constrainIIDParams paramToType resultToType params res 
  | optUseIIDIs = (param_tys, res_tys)
  | otherwise   = (param_tys_vanilla, res_tys_vanilla)
 where
   toIIDTyVar p ty = 
      case lookup (idName (paramId p)) iidIs_vars of
       Just x -> replaceTyVar (mkTyCon iUnknown [uniqueTyVar ('i':show x)]) ty
       _      -> toIIDTyVarRes p ty

   toIIDTyVarRes p ty =
       case findAttribute "iid_is" (idAttributes (paramId p)) of
	  Just (Attribute _ [ParamVar v]) -> 
	     case lookup v iidIs_vars of
	        Just x -> replaceTyVar (uniqueTyVar ('i':show x)) ty
		_ -> ty
          _ -> ty
	    
   iidIs_vars = zip (nub (catMaybes (map isIIDDep res)))
   		    [(0::Int)..]
     where
      isIIDDep p = 
        case findAttribute "iid_is" (idAttributes (paramId p)) of
	  Just (Attribute _ [ParamVar v]) -> Just v
	  _ -> Nothing

   param_tys = zipWith toIIDTyVar    params param_tys_vanilla
   res_tys   = zipWith toIIDTyVarRes res    res_tys_vanilla
   
   param_tys_vanilla = map paramToType  params
   res_tys_vanilla   = map resultToType res

\end{code}

@toHaskellTy@ takes care of implementing the T[] translation scheme. It differs
from @toHaskellBaseTy@ in that we're mapping to the user-level representation
of the IDL type in Haskell, not the type of its marshalled/packed representation.

\begin{code}
toHaskellTy :: Bool -> Type -> Haskell.Type
toHaskellTy isGround ty =
 case ty of
   Integer sz s    -> mkIntTy sz s
   Float sz        -> mkFloatTy sz
   Char signed
     | optJNI      -> tyWord16
     | otherwise   -> mkCharTy signed
   WChar           -> tyWChar
   Bool            -> tyBool
   Void            -> tyUnit
   Octet           -> tyWord8
   Any             -> tyAddr
   Object          -> tyAddr
   StablePtr	   -> tyStable
   FunTy _ res ps  -> 
   	case (toHaskellMethodTy False isGround False Nothing ps res') of
	  (t, Nothing) -> t
	  (t, Just c)  -> ctxtTyApp c t
     where
      res' = res{ resultType     = removePtr (resultType res)
	        , resultOrigType = removePtr (resultOrigType res)
	        }
 
   String _ isUnique _ -> (if isUnique then tyMaybe else id) tyString
   WString isUnique _  -> (if isUnique then tyMaybe else id) tyWString
   Sequence t _ _      -> tyList (toHaskellTy isGround t)
   Fixed{}             -> error "not implemented yet."
   Name _ _ _ _ (Just o@Iface{}) _ -> toHaskellTy isGround o
   Name _ _ _ _ _ (Just ti) ->
   	case mkTyConst (haskell_type ti) of
	   t | optCom && isTyVar t -> 
			if isGround then
			   mkTyConst vARIANT
			else
			   ctxtTyApp (ctxtClass variantClass [t]) t
	     | otherwise           -> t
   Name nm _ md _ _ _ -> tyQConst (fmap mkHaskellTyConName md) (mkHaskellTyConName nm)
   SafeArray t 
     | isGround  -> mkTyConst sAFEARRAY
     | otherwise -> tyQCon autoLib "SafeArray" [toHaskellTy isGround t]
   Array Void _ -> tyList (toHaskellTy isGround (Pointer Ptr True Void))
   Array t _ 
     | optJNI    -> mkTyCon jArray [toHaskellTy isGround t]
     | otherwise -> tyList (toHaskellTy isGround t)

   Pointer Unique isExp (Iface nm md _ _ _ _)
     | optCom && isExp -> tyQCon prelude "Maybe" [tyQCon md nm [mkTyConst groundInterface]]
     | optCom          -> tyQCon md nm [iface_ptr_ty_arg]
{-
   Pointer Unique isExp (Iface nm mod _ attrs _ _) 
     | optJNI && attrs `hasAttributeWithName` "jni_iface_ty" -> 
     	        let i = tyVar "a" in
		mkTyCon jObject
			[ ctxtTyApp (ctxtClass (mkQualName mod nm) [mkTyCon jObject [i]]) i]

     | optSubtypedInterfacePointers -> 
		     -- Pointer to anything interface'ish is an interface pointer. Period.
                     tyQCon prelude "Maybe" [tyQCon mod nm [mkTyConst groundInterface]]
		     --tyQCon mod nm [iface_ptr_ty_arg]
     | otherwise		    -> tyQCon prelude "Maybe" [tyQConst mod nm]
-}
{- moved down
   Pointer _ isExp (Iface nm mod _ attrs _ _)
     | optCorba         -> tyQCon  mod nm [iface_ptr_ty_arg]
	-- what's the IU/ID bit? Needed for processing AutoPrim.idl without
	-- a hitch. ToDo: remove it.
     | optHaskellToC && not (nm `elem` ["IUnknown", "IDispatch"]) -> tyQConst mod nm
     | optJNI && attrs `hasAttributeWithName` "jni_iface_ty" -> 
     	        let i = tyVar "a" in
		mkTyCon jObject
			[ ctxtTyApp (ctxtClass (mkQualName mod nm) [mkTyCon jObject [i]]) i ]
     | optSubtypedInterfacePointers -> tyQCon  mod nm [iface_ptr_ty_arg]
     | otherwise		    -> tyQConst mod nm
-}
   Pointer Unique _ (Iface nm md _ attrs _ _) 
     | optJNI && attrs `hasAttributeWithName` "jni_iface_ty" -> 
     	        let i = tyVar "a" in
		mkTyCon jObject
			[ ctxtTyApp (ctxtClass (mkQualName md nm) [mkTyCon jObject [i]]) i]

     | optSubtypedInterfacePointers -> 
		     -- Pointer to anything interface'ish is an interface pointer. Period.
                     tyQCon prelude "Maybe" [tyQCon md nm [mkTyConst groundInterface]]
		     --tyQCon md nm [iface_ptr_ty_arg]
     | otherwise		    -> tyQCon prelude "Maybe" [tyQConst md nm]
   Pointer _ _ (Iface nm md _ attrs _ _)
     | optCorba         -> tyQCon  md nm [iface_ptr_ty_arg]
	-- what's the IU/ID bit? Needed for processing AutoPrim.idl without
	-- a hitch. ToDo: remove it.
     | optHaskellToC && not (nm `elem` ["IUnknown", "IDispatch"]) -> tyQConst md nm
     | optJNI && attrs `hasAttributeWithName` "jni_iface_ty" -> 
     	        let i = tyVar "a" in
		mkTyCon jObject
			[ ctxtTyApp (ctxtClass (mkQualName md nm) [mkTyCon jObject [i]]) i ]
     | optSubtypedInterfacePointers -> tyQCon  md nm [iface_ptr_ty_arg]
     | otherwise		    -> tyQConst md nm
   Pointer pt _ (Name _ _ _ _ _ (Just ti))
     | pt /= Ptr && is_pointed ti ->
        (\ x -> 
	 if pt == Unique {-&& not (isVARIANTTy x)-} then 
	    tyQCon prelude "Maybe" [x] 
	 else x) $
   	case mkTyConst (haskell_type ti) of
	   t | optCom && isTyVar t -> 
			if isGround then
			   mkTyConst vARIANT
			else
			   ctxtTyApp (ctxtClass variantClass [t]) t
	     | otherwise           -> t

   Pointer pt _ t
     | pt /= Ptr && isFunTy t -> toHaskellTy isGround t

   Pointer Ptr _ (Name _ _ _ (Just as) _ _) 
     | as `hasAttributeWithName` "foreign" -> tyForeignObj

   Pointer _ _ t | isVoidTy t || (isConstructedTy t && isReferenceTy t) -> tyAddr
   Pointer pt _ t
      | pt == Ref || (optHaskellToC && isIfaceTy t) ->
		if isIfaceTy t then
		   toHaskellTy isGround (getIfaceTy t)
		else
		   toHaskellTy isGround t
      | pt == Unique ->
	     tyQCon prelude "Maybe" $
	     case t of
	       Void -> [tyAddr]
               _  
	       --  optDeepMarshall  -> [tyPtr (toHaskellTy isGround ty)]
	        | optCom && isIfaceTy t -> [toHaskellTy isGround (getIfaceTy t)]
		| otherwise	   -> [toHaskellTy isGround t]   -- is this right?

      | isVariantTy t -> toHaskellTy isGround t
      | otherwise -> -- assumed to be a pure/raw pointer
             tyPtr (toHaskellTy isGround t)
   Struct i _ _     -> tyConst' i
   Union u _ _ _ _  -> tyConst' u
   CUnion u  _ _    -> tyConst' u
   UnionNon u _     -> tyConst' u
   Enum   i _ _     -> tyConst' i
   Iface{}          -> toHaskellIfaceTy ty
   _		    -> error ("toHaskellTy: "++showCore (ppType ty))
   where
    iface_ptr_ty_arg = tyVar "a"

    tyConst' i = tyQConst 
		    (idHaskellModule i)
		    (mkHaskellTyConName (idName i))

\end{code}


%*
%
<sect>Mapping <tt/structs/ to Haskell types</sect>
<label id="sec:type:struct:translate">
%
%*

We derive a Haskell type from a @struct@ as follows:

 struct Tag { [a1]f_1 : t1; [a2]f_2:t2; [a_n]f_3:t_n; }

 ===>  data _ = Tag { h_1 :: T1, ... h_m :: Tm }

<itemize>
<item> if f_i is not the size/length specifier of another
field, it is added as a labelled field to the Haskell
data type (converting the field name and type, first)

<item>if f_i is a size/length specifier for another field, it
is not directly represented in the generated type.
Instead, the struct member that is the dependent of
f_i is turned into a list. (We probably will need to
add a flag that controls this rewrite.)

<item>If @f_i@ is a switch id/tag for a non-encapsulated union, we
leave the tag as a member of the struct, since it is useful on
its own, and does in some cases contain more tag values than that
used by the union (cf. the mysterious default union tag.)
</itemize>

\begin{code}
mkHStructDef :: Id -> [Field] -> Haskell.ConDecl
mkHStructDef tg fields =
    recCon (mkHaskellTyConName (idName tg))
           (map mkField fields')
  where
   mkField (i, t) = ( mkHaskellVarName (idName i), t)

   dep_list  = findFieldDependents fields

    -- any dependees apart from the switch_is() ones, which
    -- we keep.
   dependees = map    (idName.fieldId)  $
	       filter (\ f -> isNotSwitchDependee dep_list (fieldId f) &&
	       		      not (isVoidPointerTy (fieldType f)))
		      fields

   dependers = map (idName.fst) (filter (notNull.snd) dep_list)

   fields' = 
      map convDependees $
       -- remove the size fields
      filter (\ f -> notElem (idName (fieldId f)) dependees) fields

   convDependees f
       | (idName i) `elem` dependers = 
		    let r_ty = removeNames ty in
		    case r_ty of
		      Array _ _        -> (i, toHaskellTy True ty)
		      Pointer _ _ Void -> (i, toHaskellTy True r_ty)
		      Pointer{}        -> (i, tyList (toHaskellTy True (removeNames (removePtr r_ty))))
		      _		       -> (i, toHaskellTy True ty)
       | otherwise = (i, toHaskellTy True ty)
         where
	  i  = fieldId f
	  ty = fieldOrigType f

mkHEnumDef :: Name -> [Attribute] -> EnumKind -> [EnumValue] -> [Haskell.ConDecl]
mkHEnumDef enumTag attrs kind vals = addList (map mkCon vals)
 where
  addList =
    case kind of 
      EnumFlags{} -> (listCon:)
      _ 
        | optEnumsAsFlags || asFlag -> (listCon:)
	| otherwise -> id

  asFlag = attrs `hasAttributeWithName` "hs_flag"

  listCon = conDecl (mkHaskellTyConName (enumTag ++ "List__"))
      		    [tyList (tyCon (mkHaskellTyConName enumTag) [])]

  mkCon ev = conDecl (mkHaskellTyConName (idName i))
  		     tys
   where
     i   = enumName ev
     tys = 
     	(\ x -> 
	  case x of 
	   [] -> []
	   xs -> [tyCon xs []]) $
     	   unwords $
	   map (\ xs -> '(':xs ++ ")") $
	   map getStr $
           filterAttributes (idAttributes i)
     			    ["hs_tyarg"]
     getStr (Attribute _ [ParamLit (StringLit s)]) = s
     getStr _ = ""
     
mkHUnionDef :: Name -> [Switch] -> [Haskell.ConDecl]
mkHUnionDef nm switches = concatMap mkCon switches
  where
   mkCon (SwitchEmpty Nothing)   = [conDecl (mkHaskellTyConName (nm ++ "_Anon")) []]
   mkCon (SwitchEmpty (Just ls)) = 
      map (\ x -> conDecl (mkHaskellTyConName (nm ++ x)) []) ls_nm
       where
        ls_nm = map snd ls
   mkCon sw =
      [conDecl (mkHaskellTyConName (idName (switchId sw)))
               [toHaskellTy True (switchOrigType sw)]]

mkCUnionDef :: [Field] -> [Haskell.ConDecl]
mkCUnionDef fields = map mkCon fields
  where
   mkCon f = 
      conDecl (mkHaskellTyConName (idName (fieldId f)))
              [toHaskellTy True (fieldOrigType f)]

\end{code}


\begin{code}
paramToHaskellType :: DependInfo -> Bool -> Bool -> Bool -> Param -> Haskell.Type
paramToHaskellType deps isServer isAuto isResult p
  | no_dependees && not keep_external = mkHaskellTy ty
  | keep_external = mkHaskellTy ty'
  | otherwise     = real_ty
   where
    nm		 = paramId p
    attrs        = idAttributes nm

      -- Note: we need to use the paramType rather than the
      -- original type when peeling of a layer of pointers,
      -- because the orig. type might be a straight synonym.
      -- (the peeling function could look through names to avoid
      -- this, but it doesn't at the moment.)
    ty	
      | peel	  = removePtrAndArray (paramType p)
      | otherwise = paramOrigType p

    dependees    = fromMaybe [] (lookupDepender deps nm)
    no_dependees = null dependees

    ty'  = mkPtrPointer ty
    keep_external = 
        attrs `hasAttributeWithName` "ptr"  ||
	(not isResult && keepValueAsPointer ty)

    mkHaskellTy t
      | attrs `hasAttributeWithName` "foreign"  = tyForeignObj
      | isAuto && paramMode p == In &&
	attrs `hasAttributeWithName` "optional" = 
	    if optOptionalAsMaybe then
	       tyQCon prelude "Maybe" [autoTypeToHaskellTy pkind t]
	    else
	       overloadedTyVar variantClass "a"
      | isAuto     = autoTypeToHaskellTy pkind t
      | otherwise  = 
	 (if isOut &&
	     not (attrs `hasAttributeWithName` "iid_is")
	   then groundTyVars else id) $ toHaskellTy isServer t

    real_ty = go sizes ty

    go (_:xs) acc 
      | isVoidPointerTy (removeNames acc)    = tyAddr
      | isPointerOrArrayTy (removeNames acc) = tyList (go xs (removePtrAndArray (removeNames acc)))
    go _ acc     = 
      (if isOut &&
          not (attrs `hasAttributeWithName` "iid_is")
        then groundTyVars else id) $ toHaskellTy isServer acc

    (sizes, peel) = 
     case computeArrayConstraints False{-not unmarshalling-} dependees of
       (_,_,cs)   ->
          case cs of
	    (DepNone:cs1) | isOut  -> (cs1, True)
--	    (_:cs) | isOut  -> (cs, True)
	    _ -> (cs, False)

    pkind = paramMode p

    isOut = 
      case pkind of
        Out   -> True
	_     -> False

\end{code}


Convert an IDL type into its corresponding Haskell <em/primitive/ representation.
The primitive representation of a type is the representation expected externally.
For example,

    typedef [unique]char* pchar;

is to the Haskell programmer represented as

    type Pchar = Maybe Char

the primitive representation of this unique pointer is an Addr (not such a big surprise),
which is what we pass to an external function expecting a @pchar@ as argument.

@toHaskellBaseTy@ implements the B scheme in the H/Direct paper.

\begin{code}
toHaskellBaseTy :: Bool -> Type -> Haskell.Type
toHaskellBaseTy isResult ty =
 case ty of
   Integer sz s  -> mkIntTy sz s
   Float sz      -> mkFloatTy sz
   Char signed   -> mkCharTy signed
   WChar         -> tyWChar
   Bool          -> mkIntTy Long True
   Void          -> tyUnit
   Octet         -> tyWord8
   Any           -> tyAddr
   Object        -> tyAddr
   String{}      -> tyPtr tyString
   WString{}     -> tyPtr tyWString
   FunTy{}       -> tyPtr (toHaskellTy False ty) -- (-- the toplevel type --)
   StablePtr	 -> tyStable
   Sequence{}    -> tyAddr --error "toHaskellBaseTy.Sequence: not implemented yet."
   Fixed{}       -> error "toHaskellBaseTy.Fixed: not implemented yet."
   Name _ _ _ _ _ (Just ti)
     | not isResult && {- not (is_pointed ti) && -} finalised ti -> tyForeignObj

   Name n _ _ _ mb_orig_ty mb_ti ->
    case mb_ti of
      Just ti ->  (if isResult then toPtrTy else id) $ prim_type ti
      Nothing ->
       case mb_orig_ty of
         Nothing -> 
	  trace ("toHaskellBaseTy: Warning, defaulting " ++ show n ++ "to Addr")
	  tyAddr
         Just t  -> toHaskellBaseTy isResult t

   Pointer _ _ (Name _ _ _ _ _ (Just ti))
     | not isResult && is_pointed ti && finalised ti ->
	prim_type ti
     | otherwise    -> (if not (is_pointed ti) && not (isPtrTy (prim_type ti)) 
                        then tyPtr else id) $ ((if isResult then toPtrTy else id) $ prim_type ti)
   Pointer _ _ i@(Iface nm _ _ attrs _ _) 
        | not isResult ->
	   if not optHaskellToC || 
	      attrs `hasAttributeWithName` "finaliser" ||
	      nm `elem` ["IUnknown" , "IDispatch"] -- fudge
	        	    then
	      tyForeignPtr (toHaskellIfaceTy i)
	   else
	      tyPtr (toHaskellIfaceTy i)
	| otherwise    -> tyPtr (toHaskellIfaceTy i)

   Pointer _ _ (Name n _ m _ _ _) ->
      tyPtr (tyQConst (fmap mkHaskellTyConName m) (mkHaskellTyConName n))
   Pointer _ _ t  -> tyPtr (toHaskellBaseTy isResult t)
   Iface{}        -> toHaskellBaseTy isResult (Pointer Ref True ty)
   Array t _      -> tyPtr (toHaskellBaseTy isResult t)
   SafeArray _  
     | isResult     -> tyPtr (mkTyConst sAFEARRAY)
     | otherwise    -> tyForeignPtr (mkTyConst sAFEARRAY)
   Struct i [f] _
     | isSimpleTy (fieldType f) || 
       ((idAttributes i) `hasAttributeWithName` "hs_newtype")
     -> toHaskellBaseTy isResult (fieldType f)
   Struct{}         -> tyAddr -- liar!
   Union{}          -> tyAddr
   CUnion{}         -> tyAddr
   UnionNon{}       -> tyAddr
   Enum{}           -> tyInt32
   _		    -> error ("toHaskellBaseTy: not handled" ++ showCore (ppType ty))

toBaseTy :: Type -> Type
toBaseTy ty =
 case ty of
   Bool	        -> int32Ty
   Octet        -> charTy
   String{}     -> addrTy
   WString{}    -> addrTy
   Sequence{}   -> addrTy
   FunTy{}      -> addrTy
    -- ToDo: get rid of this.
   Name "VARIANT_BOOL" _ _ _ _ _ | optCom -> ty
   Name _ _ _ _ Nothing  _ -> addrTy -- your guess is as good as mine.
   Name _ _ _ _ (Just t) _ -> toBaseTy t
   Struct i [f] _
     | isSimpleTy (fieldType f) ||
       ((idAttributes i) `hasAttributeWithName` "hs_newtype")
     -> toBaseTy (fieldType f)
   Struct{}        -> addrTy
   Enum{}          -> int32Ty
   Union{}         -> int32Ty
   UnionNon{}      -> int32Ty
   CUnion{}	   -> int32Ty
   Pointer _ _ t
      | isIfaceTy t    -> ty
      | otherwise      -> addrTy
   Array{}	       -> addrTy
   SafeArray{}	       -> addrTy
   _		       -> ty
   
\end{code}

Provide the 'direct' mapping of a method/function.

\begin{code}
toHaskellBaseMethodTy :: Bool -> [Param] -> Result -> Haskell.Type
toHaskellBaseMethodTy isRes ps res 
  = case generaliseTys (res_ty : p_tys) of
      ((r:ps1), mb_c) -> mbCtxtTyApp mb_c (funTys ps1 r)
  where
    res_ty = io (toPtrTy (toHaskellBaseTy True (resultType res)))
    p_tys  = map (toPtrTy.(toHaskellBaseTy isRes).paramType) ps

\end{code}

\begin{code}
mkMarshaller :: String -> Type -> QualName
mkMarshaller pre ty =
 case ty of
   Name _ _ _ _ (Just t@(Name{}))  _ -> mkMarshaller pre t
   Name _ _ _ _ (Just t) _ | not (isConstructedTy t) -> mkMarshaller pre t
   Name n _ md _ _ _ -> mkQVarName md (pre ++ mkHaskellTyConName n)
   Struct i [] _     -> mkQVarName (idModule i) (pre ++ mkHaskellTyConName (idName i))
--   _ -> appHTy pre (toHaskellBaseTy False ty)
--   _ -> mkQVarName Nothing ("(" ++ pre ++ " " ++ (PPHaskell.showAbstractH (ppType ty)) ++ ")")
   _ -> prefixHTy pre (toHaskellBaseTy False ty)

\end{code}

\begin{code}
autoTypeToHaskellTy :: ParamDir -> Type -> Haskell.Type
autoTypeToHaskellTy pkind ty =
  case mbAutoTypeToHaskellTy pkind ty of
    Just x  -> x
    Nothing -> 
     trace ("autoTypeToHaskellType: unknown auto type "++ showCore (ppType ty) ++ "\n Giving it a variant type") $
     (overloadedTyVar variantClass "a")

mbAutoTypeToHaskellTy :: ParamDir -> Type -> Maybe Haskell.Type
mbAutoTypeToHaskellTy pkind ty = 
 case ty of
    -- Note: we disregard the '[unique]' flag on the string types here;
    -- Automation doesn't support [unique].
   String{}      -> Just $ mkTyConst $ mkQualName prelude stringName
   WString{}     -> Just $ mkTyConst $ mkQualName prelude stringName
   Integer sz isSigned ->
     case sz of
       Short
	 | isSigned  -> Just $ mkTyConst $ tyInt16Name
	 | otherwise -> Just $ mkTyConst $ tyWord16Name
       Long 
	 | isSigned  -> Just $ mkTyConst $ tyInt32Name
	 | otherwise -> Just $ mkTyConst $ tyWord32Name
       Natural
	 | optIntIsInt && isSigned -> Just $ mkTyConst $ tyIntName
	 | isSigned  -> Just $ mkTyConst $ tyInt32Name
	 | otherwise -> Just $ mkTyConst $ tyWord32Name
       LongLong
	 | optLongLongIsInteger -> Just $ mkTyConst $ tyIntegerName
	 | isSigned	        -> Just $ mkTyConst $ tyInt64Name
	 | otherwise		-> Just $ mkTyConst $ tyWord64Name

   Char{}        -> Just $ mkTyConst $ mkQualName prelude "Char" -- dodgy, but it works..
   Octet	 -> Just $ mkTyConst $ mkQualName prelude "Char"
   Bool		 -> Just $ mkTyConst $ mkQualName prelude "Bool"
   Float sz      ->
     case sz of
       Short     -> Just $ mkTyConst $ mkQualName prelude "Float"
       Long      -> Just $ mkTyConst $ mkQualName prelude "Double"
       LongLong	 -> Just $ mkTyConst $ mkQualName prelude "Double"
       Natural   -> Just $ mkTyConst $ mkQualName prelude "Float"

   Pointer _ _ (Iface "IUnknown" _ _ _ _ _)  -> Just (mkIType iUnknown)
   Pointer _ _ (Iface "IDispatch" _ _ _ _ _) -> Just (mkIType iDispatch)
   Pointer _ _ (Iface nm md _ _ _ _)         -> Just $ mkIType $ mkQualName md (mkIfaceTypeName nm)
   Pointer _ _ Void		 -> Just varTy
   Pointer _ _ (Name _ _ _ _ _ (Just ti)) 
      | is_pointed ti  -> Just (mkAutoTyConst (auto_type ti))
   Iface "IUnknown" _ _ _ _ _	 -> Just $ mkIType iUnknown
   Iface "IDispatch" _ _ _ _ _   -> Just $ mkIType iDispatch
   Iface nm md _ _ _ _           -> Just $ mkIType $ mkQualName md (mkIfaceTypeName nm)

   SafeArray t		   -> 
	    case mbAutoTypeToHaskellTy Out{-want to ground any embedded iface-pointers-} t of
	      Nothing -> Nothing
--	      Just x  -> Just $ mkTyCon (mkQualName autoLib "SafeArray") [groundTyVars x]
	      Just x  -> Just $ mkTyCon (mkQualName autoLib "SafeArray") [x]
   (Name "HRESULT" _ _ _ _ _)  -> Just $ mkTyConst $ mkQualName comLib "HRESULT"
   (Name "VARIANT_BOOL" _ _ _ _ _) -> Just $ mkTyConst $ mkQualName prelude "Bool"
--   (Name "VARIANT" _ _ _ _ _)  -> Just varTy
{- BEGIN_SUPPORT_TYPELIBS
   (Name _ _ _ _ _ (Just ti)) | isJust (auto_vt ti) -> Just $ mkAutoTyConst (auto_type ti)
   END_SUPPORT_TYPELIBS -}
   (Name nm _ md _ (Just Enum{}) _) -> Just $ mkTyConst $ mkQualName md (mkHaskellTyConName nm)
   (Name _ _ _ _ (Just orig_ty) _)   -> mbAutoTypeToHaskellTy pkind orig_ty
   (Name _ _ _ _ _ (Just ti))      -> Just (mkTyConst $ haskell_type ti)
   Name{}                          -> Nothing
   Pointer _ _ t                   -> mbAutoTypeToHaskellTy pkind t
   Enum{} 			   -> Just $ mkTyConst $ mkQualName prelude "Int"
     --As if by magic..
   (Struct (Id {idName="TagVARIANT"}) _ _) -> Just varTy
   _ -> Nothing
 where
   -- leave these as a type variable, later pass will constrain
   -- it with the approp. context.
  varTy = overloadedTyVar variantClass "a"

  isOut = pkind == Out

  mkIType qv
   | optSubtypedInterfacePointers && isOut = mkTyCon qv [mkTyConst groundInterface]
   | optSubtypedInterfacePointers          = mkTyCon qv [tyVar "a"]
   | otherwise                             = mkTyCon qv [tyVar "a"]	     


{-
 Type variables are Variant-overloaded.
-}
mkAutoTyConst :: QualName -> Haskell.Type
mkAutoTyConst q =
  case mkTyConst q of
    t | isTyVar t -> ctxtTyApp (ctxtClass variantClass [t]) t
      | otherwise -> t

autoTypeToQName :: Type -> QualName
autoTypeToQName ty =
  case ty of
   String{}    -> mkQualName prelude "String"
   WString{}   -> mkQualName prelude "String"
   Integer sz isSigned ->
     case sz of
       Short
	 | isSigned  -> tyInt16Name
	 | otherwise -> tyWord16Name
       Long 
	 | isSigned  -> tyInt32Name
	 | otherwise -> tyWord32Name
       Natural
	 | optIntIsInt && isSigned -> tyIntName
	 | isSigned  -> tyInt32Name
	 | otherwise -> tyWord32Name
       LongLong
	 | optLongLongIsInteger -> tyIntegerName
	 | isSigned	        -> tyInt64Name
	 | otherwise		-> tyWord64Name

   Char{}        -> mkQualName prelude "Char" -- dodgy, but it works.. (don't take signedness into account, nor the size of a Haskell Char)
   Octet         -> mkQualName prelude "Char"
   Bool		 -> mkQualName prelude "Bool"
   Float sz      ->
     case sz of
       Short     -> mkQualName prelude "Float"
       Long      -> mkQualName prelude "Double"
       LongLong  -> mkQualName prelude "Double"
       Natural   -> mkQualName prelude "Float"

   Iface _ _ _ _ is_idis _
        | is_idis   -> iDispatch
	| otherwise -> iUnknown
   Pointer _ _ Void -> mkQualName autoLib "Variant" 
   SafeArray{}		    -> mkQualName autoLib "SafeArray"
   (Name "HRESULT" _ _ _ _ _) -> mkQualName comLib "HRESULT"
    -- hack
--   (Name "LPSTR" _ _ _ _ _) -> mkQualName prelude stringName
   Name _ _ _ _ _ (Just ti)
   	| isTyVar (mkTyConst (auto_type ti)) -> mkQualName autoLib "Variant"
	| otherwise			     -> auto_type ti
   Name _ _ _ _ (Just orig_ty) _ -> autoTypeToQName orig_ty
   Name nm _ _ _ _ _            -> 
	trace ("warning: found type name " ++ show nm ++ " but not its defn.") $
	mkQualName autoLib "Variant"
   Pointer _ _ (Name _ _ _ _ _ (Just ti))
        | is_pointed ti ->
	   let at = auto_type ti in
	   if isTyVar (mkTyConst at) then
	      mkQualName autoLib "Variant"
	   else
	      at
   Pointer _ _ t               -> autoTypeToQName t
   Enum{}                      -> mkQualName autoLib "Enum"
   (Struct (Id {idName="TagVARIANT"}) _ _) -> mkQualName autoLib "Variant"  -- hmm..
   _ -> trace ("autoTypeToQName: unknown auto type "++ showCore (ppType ty) ++ "\n Giving it a variant type") $
	mkQualName autoLib "Variant"

\end{code}
