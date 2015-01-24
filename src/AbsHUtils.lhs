%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Dec. 1st 2003  06:57  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Disjoint set of utilities for working with the @AbstractH@
type.

\begin{code}
module AbsHUtils 
	(
	  tyConst
	, tyQConst
	, libTyQConst
	, mkTyConst
	, deTyCon
	, tyCon
	, tyQCon
	, mkTyCon
	, tyVar
	, uniqueTyVar
	, tyQVar
	, isTyVar

	, generaliseTys
	, overloadedTyVar
	, ctxtTyApp
	, ctxtClass
	, mbCtxtTyApp

	, tyList
	, tyMaybe
	, tyVariant
	, tuple
	, tyInt8Name, tyInt16Name, tyInt32Name, tyInt64Name, tyIntName
	, tyInt8, tyInt16, tyInt32, tyInt64, tyInt
	, tyInteger, tyIntegerName
	, tyFloat, tyDouble, tyLongDouble
	, tyAddr
	, tyPtr
	, anyTyPtr
	, tyFunPtr
	, tyForeignObj
	, tyForeignPtr
	, isFOTy
	, isPtrTy
	, isVARIANTTy
	, toPtrTy

	, tyStable
	, tyString
	, tyWString
	, tyByte, tyChar, tyWChar
	, tyBool
	, tyWord8Name, tyWord16Name, tyWord32Name, tyWord64Name
	, tyWord8, tyWord16, tyWord32, tyWord64
	, funTy
	, funTys
	, io
	, io_unit
	, tyUnit
	, purifyType
	, isIOTy

	, recCon
	, recConBanged
	, conDecl
	, recToConDecl

	, dataTy
	, newTy
	, tySyn
	, conDeclToCon
	, conDeclToPat
	, splitFunTys
	, hInstance
	, hClass

	, groundTyVars
	, unconstrainType

	, andDecl
	, andDecls
	, emptyDecl
	, comment
	, isEmptyDecl
	, cCode

	, typeSig
	, genTypeSig
	, mkTypeSig
	, funDef
	, valDef
	, methodDef
	, guardedFunDef
	, prim
	, primcst
	, fexport
	, extLabel

	, conPat
	, patVar
	, patRec
	, qpatVar
	, litPat
	, varPat
	, tuplePat
	, exprToPat
	, wildPat
	, isVarPat

	, ret
	, genBind
	, bind
	, bind_
	, var
	, varName
	, qvar
	, lam
	, lit
	, integerLit
	, dataConst
	, dataCon
	, funApp
	, contApply
	, funApply
	, infixOp
	, binOp
	, unaryOp
	, tup
	, hList
	, hCase
	, hIf
	, alt
	, genAlt
	, defaultAlt
	, equals
	, hLet
	, hLets

	, intLit
	, stringLit

	, addPtr
	, castPtr
	, nothing
	, just
	, unit

	, prefix
	, prefixApp
	, appendStr

	, isVarsEq
	
	, hModule
	, hMeta
	, cMeta
	, hInclude

	, hExport
	, hImport
	, hQImport
	, ieModule
	, ieValue
	, ieClass
	, ieType
	, subst

	, mkQVarName
	, mkVarName
	, mkConName
	, mkQConName
	, mkTyVar
	, mkQTyVar
	, mkQTyCon
	
	, mkIntTy
	, mkCharTy
	, mkFloatTy
	
	, findIncludes
	
	, mkTySig
	, replaceTyVar

	) where

import AbstractH
import Literal
import BasicTypes
import LibUtils
import Opts    ( optIntsEverywhere, optIntAsWord
	       , optIntIsInt, optLongLongIsInteger
	       , optNoWideStrings
	       )
import Data.Maybe   ( fromMaybe, isJust )
import Data.Char    ( isLower )
import Data.List    ( mapAccumL, intersperse )

-- This should be the default, but older versions (e.g., Jan 98) of
-- Hugs insist on this one..
infixl 9 `andDecl`
\end{code}

\begin{code}
tyConst :: String -> Type
tyConst con = TyCon (mkQualName Nothing con)

tyQConst :: Maybe String -> String -> Type
tyQConst m con = TyCon (mkQTyCon m con)

libTyQConst :: Maybe String -> Maybe String -> String -> Type
libTyQConst ty_mod marshall_mod con = TyCon ((mkQTyCon marshall_mod con){qDefModule=ty_mod})

libTyQName :: Maybe String -> Maybe String -> String -> QualName
libTyQName ty_mod marshall_mod con = (mkQTyCon marshall_mod con){qDefModule=ty_mod}

{-
 Slightly magic in that it transforms
 "Foo.Bar a" into a type application.
-}
mkTyConst :: QualName -> Type
mkTyConst qv 
  | not (isJust (qModule qv)) && 
    isLower (head (qName qv))
  = TyVar False (mkTyVar (qName qv))
  | length args > 1 
  = TyApply (TyCon (qv{qName=a})) (map ((TyVar False). mkTyVar) as)
  | otherwise = TyCon qv
 where
  args@(a:as) = words (qName qv)

deTyCon :: Type -> QualName
deTyCon (TyCon c) = c
deTyCon _         = error "AbsHUtils.deTyCon: expected a tycon"

tyCon :: String -> [Type] -> Type
tyCon con args = TyApply (TyCon (mkQualName Nothing con)) args

tyQCon :: Maybe String -> String -> [Type] -> Type
tyQCon ty_mod con args = TyApply (TyCon (mkQTyCon ty_mod con)) args

mkTyCon :: QualName -> [Type] -> Type
mkTyCon qv args = TyApply (TyCon qv) args

tyVar :: String -> Type
tyVar nm = TyVar False (mkTyVar nm)

uniqueTyVar :: String -> Type
uniqueTyVar nm = TyVar True (mkTyVar nm)

overloadedTyVar :: ClassName -> String -> Type
overloadedTyVar c_name tv = TyCtxt (CtxtClass c_name [tvar]) tvar
  where
   tvar = TyVar False (mkTyVar tv)

ctxtClass :: ClassName -> [Type] -> Context
ctxtClass c ts = CtxtClass c ts

ctxtTyApp :: Context -> Type -> Type
ctxtTyApp ctxt t = TyCtxt ctxt t

mbCtxtTyApp :: Maybe Context -> Type -> Type
mbCtxtTyApp Nothing t = t
mbCtxtTyApp (Just c) t = TyCtxt c t

tyQVar :: Maybe String -> String -> Type
tyQVar ty_mod nm = TyVar False (mkQTyVar ty_mod nm)

isTyVar :: Type -> Bool
isTyVar (TyVar _ _) = True
isTyVar _           = False

isNonUniqTyVar :: Type -> Bool
isNonUniqTyVar (TyVar False _) = True
isNonUniqTyVar _               = False

unconstrainType :: Type -> ([(Context,TyVar)], Type)
unconstrainType tx = go [] tx
 where
   go acc t = 
    case t of
      TyApply f args  -> 
		let
		 (acc1, f')    = go acc f
		 (acc2, args') = mapAccumL go acc1 args
		in
		(acc2, TyApply f' args')
      TyTuple ts      -> 
    		let
		 (acc1, ts') = mapAccumL go acc ts
		 in
		 (acc1, TyTuple ts')
      TyFun t1 t2     -> 
		let
		 (acc1, t1') = go acc  t1
		 (acc2, t2') = go acc1 t2
		in
		(acc2, TyFun t1' t2')
      TyList t1   ->
    		let
		 (acc1, t1') = go acc t1
		in
		(acc1, TyList t1')
      TyCtxt ctxt t1@(TyVar _ tv) -> ((ctxt,tv):acc, t1)
      _		    -> (acc, t)

groundTyVars :: Type -> Type
groundTyVars t =
  case t of
    TyVar{}         -> groundTyVar t
    TyApply (TyCon tc) args | qName tc == "Maybe" -> TyApply (TyCon tc) (map groundTyVars args)
    TyApply tc args -> TyApply tc (map groundTyVar args)
    TyTuple ts      -> TyTuple (map groundTyVars ts)
    TyFun t1 t2     -> TyFun (groundTyVars t1) (groundTyVars t2)
    TyList t1       -> TyList (groundTyVars t1)
    TyCtxt _ _      -> t
    _		    -> t
 where
  groundTyVar ty
    | isNonUniqTyVar ty = tyUnit
    | otherwise         = 
    	case ty of
--	  TyApply (TyCon tc) args 
--	    | qName tc == "Maybe" -> TyApply (TyCon tc) (map groundTyVars args)
	  TyApply tc args -> TyApply tc (map groundTyVars args)
	  TyFun t1 t2 -> TyFun (groundTyVars t1) (groundTyVars t2)
	  _ -> ty

renameTyVar :: String -> Type -> Type
renameTyVar new_nm (TyVar x _) = TyVar x (mkTyVar new_nm)
renameTyVar _ t = t

replaceTyVar :: Type -> Type -> Type
replaceTyVar newTy ty = 
  case ty of
    TyVar _ _ -> newTy
    TyApply f args -> TyApply f (map (replaceTyVar newTy) args)
    TyTuple ts -> TyTuple (map (replaceTyVar newTy) ts)
    TyFun a b -> TyFun (replaceTyVar newTy a) (replaceTyVar newTy b)
    _ -> ty

{-
 generaliseTys lifts out embedded contexts and renames
 type variables so as to make them unique.
-}
generaliseTys :: [Type] -> ([Type], Maybe Context)
generaliseTys tys = 
  case (go nm_supply [] tys) of
    (ts, []) -> (ts, Nothing)
    (ts, ls) -> (ts, Just (CtxtTuple (reverse ls)))
  where
    nm_supply = map (\ x -> 'a':show x) [(0::Int)..]
    
    substCtxt s x (CtxtTuple ls)   = CtxtTuple (map (substCtxt s x) ls)
    substCtxt s x (CtxtClass c ts) = CtxtClass c (map (substTyVar s x) ts)

    substTyVar o_t x t =
      case t of
        TyVar fixed n | not fixed && n == x -> o_t
	TyApply t1 ty_args -> TyApply t' ty_args'
	  where
	   (t':ty_args') = map (substTyVar o_t x) (t1:ty_args)
        TyList t1  -> TyList  (substTyVar o_t x t1)    
	TyTuple ts -> TyTuple (map (substTyVar o_t x) ts)
	TyFun a b  -> TyFun   (substTyVar o_t x a)
			      (substTyVar o_t x b)
	TyCtxt c t1 -> TyCtxt (substCtxt o_t x c)
			      (substTyVar o_t x t1)
	_ -> t			      

    go _ acc [] = ([], acc)
    go supply@(s:ss) acc_ctxt (x:xs) =
      case x of
        TyVar fixed _ | not fixed ->
	  let
	    x'        = renameTyVar s x
	    (xs',acc) = go ss acc_ctxt xs
	  in
	  (x' : xs', acc)
	TyCtxt ctxt tv@(TyVar fixed n) | not fixed -> 
	  let
	    tv'   = renameTyVar s tv
	    ctxt' = substCtxt tv' n ctxt
	    (xs',acc) = go ss (ctxt' : acc_ctxt) xs
	  in
	  (tv' : xs', acc)
	TyApply t ty_args ->
	  let
	   (ts, acc) = go supply acc_ctxt (t:ty_args++xs)
	   (t':ty_args', rs) = splitAt (length ty_args + 1) ts
          in
	  (TyApply t' ty_args' : rs, acc)
	TyList t   -> 
	  let
	   (t':xs', acc) = go supply acc_ctxt (t:xs)
	  in
	  (TyList t' : xs' , acc)
        TyTuple tuple_tys ->
	  let
	   (ts, acc)  = go supply acc_ctxt (tuple_tys++xs)
	   (tys', rs) = splitAt (length tuple_tys) ts
          in
	  (TyTuple tys' : rs, acc)

	TyFun t1 t2 ->
	  let
	   (t1' : t2' : xs', acc) = go supply acc_ctxt (t1:t2:xs)
          in
	  (TyFun t1' t2' :  xs', acc)
	_ -> 
          let
	   (xs', acc) = go supply acc_ctxt xs
	  in
	  (x:xs', acc)
    go _ _ _ = error "generaliseTys"

--
-- [Foo a, Foo b, Foo a] ==> [Foo a0, Foo b, Foo a0]
--
{- I suspect this is no longer needed - leaving it out for now.
relabelTypes :: [Type] -> [Type]
relabelTypes ts = 
  case (go supply [] ts) of 
    (ts,_,_) -> ts
  where
    supply = map (\ x -> 'a':show x) [0..]

    go s acc [] = ([],s,acc)
    go supply@(s:ss) acc (x:xs) =
      case x of
        TyVar fixed v ->
	  case lookup v acc of
	    Nothing -> 
		let (xs',s',acc') = go ss ((v,s):acc) xs in
		((TyVar fixed (mkTyVar s)):xs',s',acc')
	    Just tv -> 
	        let (xs',s',acc') = go supply acc xs in
		(TyVar fixed (mkTyVar tv) : xs',s',acc')
        TyApply t tvs ->
	    let
	     ([t'], supply', acc')      = go supply acc [t]
	     (tvs',supply'',  acc'')  = go supply' acc' tvs
	     (xs', supply''', acc''') = go supply'' acc'' xs
	    in
	    (TyApply t' tvs' : xs', supply''', acc''')
        TyTuple ts ->
	    let
	     (ts', supply', acc')   = go supply acc ts
	     (xs', supply'', acc'') = go supply' acc' xs
	    in
	    (TyTuple ts' : xs', supply'', acc'')
        TyList ts ->
	    let
	     ([ts'], supply', acc')   = go supply acc [ts]
	     (xs', supply'', acc'') = go supply' acc' xs
	    in
	    (TyList ts' : xs', supply'', acc'')
	TyFun f a ->
	    let
	     ([f'], supply', acc') = go supply acc [f]
	     ([a'], supply'', acc'') = go supply' acc' [a]
	     (xs', supply''', acc''') = go supply'' acc'' xs
	    in
	    ((TyFun f' a') : xs', supply''', acc''')
	TyCtxt c t ->
	    let
	     ([t'], supply', acc') = go supply acc [t]
	    in
	    ([TyCtxt c t'], supply', acc')
	_ -> 
	    let
	     (xs', ss , acc') = go supply acc xs
	    in
	    (x : xs', ss, acc')

    go _ _ _ = error "relabelTypes"
-}

tyList :: Type -> Type
tyList t = TyList t

tyMaybe :: Type -> Type
tyMaybe t = TyApply (TyCon maybeName) [t]

tyVariant :: Type
tyVariant = TyCon variantType

tuple :: [Type] -> Type
tuple []  = tyUnit
tuple [t] = t
tuple ts  = TyTuple ts

tyInt8Name, tyInt16Name, tyInt32Name, tyInt64Name, tyIntName :: QualName
(tyInt8Name, tyInt16Name, tyInt32Name, tyInt64Name)
  | optIntsEverywhere = (tyIntName, tyIntName, tyIntName, tyIntName)
  | otherwise         = 
     ( libTyQName intLib hdirectLib "Int8"
     , libTyQName intLib hdirectLib "Int16"
     , libTyQName intLib hdirectLib "Int32"
     , libTyQName intLib hdirectLib "Int64"
     )

tyInt8, tyInt16, tyInt32, tyInt64, tyInt :: Type
tyInt8  = mkTyConst tyInt8Name
tyInt16 = mkTyConst tyInt16Name
tyInt32 = mkTyConst tyInt32Name
tyInt64 = mkTyConst tyInt64Name

tyIntName = libTyQName prelude hdirectLib "Int"
tyInt     = mkTyConst tyIntName

tyIntegerName :: QualName
tyInteger :: Type
tyIntegerName = libTyQName prelude hdirectLib "Integer"
tyInteger = mkTyConst tyIntegerName

tyAddr :: Type
--tyAddr = libTyQConst addrLib hdirectLib "Addr"
tyAddr = tyPtr tyUnit

tyPtr :: Type -> Type
tyPtr t = TyApply (libTyQConst ptrLib hdirectLib ptrName) [t]

anyTyPtr :: Type
anyTyPtr = tyPtr (uniqueTyVar "a")

tyStable :: Type
tyStable = mkTyCon (libTyQName foreignLib hdirectLib "StablePtr") [uniqueTyVar "a"]

tyForeignObj :: Type
tyForeignObj = tyForeignPtr tyUnit

tyForeignPtr :: Type -> Type
tyForeignPtr t = TyApply (libTyQConst foreignPtrLib hdirectLib foreignPtrName) [t]

tyFunPtr :: Type -> Type
tyFunPtr t = TyApply (libTyQConst ptrLib hdirectLib funPtrName) [t]

isFOTy :: Type -> Bool
isFOTy (TyApply (TyCon tc) _) = qName tc == foreignPtrName
isFOTy _ = False

isPtrTy :: Type -> Bool
isPtrTy (TyApply (TyCon tc) _) = nm == ptrName || nm == foreignPtrName
 where
  nm = qName tc
isPtrTy _ = False

toPtrTy :: Type -> Type
toPtrTy ty@(TyApply (TyCon tc) [t]) 
 | qName tc == foreignPtrName = tyPtr (toPtrTy t)
 | otherwise = ty
toPtrTy (TyApply tc ts) = TyApply tc (map toPtrTy ts)
toPtrTy t = t

isVARIANTTy :: Type -> Bool
isVARIANTTy (TyCon tc) = qName tc == "VARIANT"
isVARIANTTy _ = False

tyString :: Type
tyString = tyQConst prelude stringName

tyWString :: Type
tyWString 
  | optNoWideStrings = tyString
  | otherwise        = tyQConst wStringLib "WideString"

tyByte, tyBool, tyChar :: Type
tyChar = libTyQConst prelude hdirectLib "Char"
tyBool = libTyQConst prelude hdirectLib "Bool"
tyByte = tyWord8

tyWordName :: QualName
tyWordName = mkQualName wordLib "Word"

tyWord :: Type
tyWord = mkTyConst (tyWordName{qModule=hdirectLib})

tyWord8Name, tyWord16Name, tyWord32Name, tyWord64Name :: QualName
(tyWord8Name, tyWord16Name, tyWord32Name, tyWord64Name)
  | optIntsEverywhere && optIntAsWord = (tyIntName, tyIntName, tyIntName, tyIntName)
  | otherwise =
     ( libTyQName wordLib hdirectLib "Word8"
     , libTyQName wordLib hdirectLib "Word16"
     , libTyQName wordLib hdirectLib "Word32"
     , libTyQName wordLib hdirectLib "Word64"
     )

tyWord8, tyWord16, tyWord32, tyWord64 :: Type
tyWord8  = mkTyConst tyWord8Name
tyWord16 = mkTyConst tyWord16Name
tyWord32 = mkTyConst tyWord32Name
tyWord64 = mkTyConst tyWord64Name

tyWChar :: Type
tyWChar = tyWord16

tyFloat, tyDouble, tyLongDouble :: Type
tyFloat      = libTyQConst prelude hdirectLib "Float"
tyDouble     = libTyQConst prelude hdirectLib "Double"
tyLongDouble = libTyQConst prelude hdirectLib "Double" -- best we can do at the mo'.

\end{code}


\begin{code}
funTy :: Type -> Type -> Type
funTy a b = TyFun a b

funTys :: [Type] -> Type -> Type
funTys ls res = foldr TyFun res ls

io :: Type -> Type
io x = tyQCon prelude "IO" [x]

isIOTy :: Type -> Bool
isIOTy (TyApply (TyCon q) _)
   | qName q == "IO" && qModule q == Just "Prelude" = True
isIOTy _					    = False

purifyType :: Type -> Type
purifyType (TyFun x y@TyFun{}) = TyFun x (purifyType y)
purifyType t@(TyFun x y) 
 | isIOTy y = case y of { (TyApply _ [arg]) -> TyFun x arg ; _ -> t}
purifyType t = t

io_unit :: Type
io_unit = io tyUnit

tyUnit :: Type
tyUnit = tyConst "()"
\end{code}

Constructor decls:

\begin{code}
recCon :: Name -> [(Name, Type)] -> ConDecl
recCon nm fields = RecDecl nm (map (\ (x,t) -> (x,Unbanged t)) fields)

recConBanged :: Name -> [(Name, Type)] -> ConDecl
recConBanged nm fields = RecDecl nm (map (\ (x,t) -> (x,Banged t)) fields)

conDecl :: Name -> [Type] -> ConDecl
conDecl nm ls = ConDecl nm (map Unbanged ls)

recToConDecl :: ConDecl -> ConDecl
recToConDecl (RecDecl nm fs) = ConDecl nm (map snd fs)
recToConDecl c = c

dataTy :: Name -> [Name] -> [ConDecl] -> HDecl
dataTy dname tvs constrs = TyD (TyDecl Data dname tvs constrs [])

newTy :: Name -> [Name] -> ConDecl -> [QualName] -> HDecl
newTy dname tvs constr ls = TyD (TyDecl Newtype dname tvs [constr] ls)

hInstance :: Maybe [(ClassName,[TyVar])] -> ClassName -> Type -> [HDecl] -> HDecl
hInstance Nothing cname t decls   = Instance (CtxtTuple []) cname t decls
hInstance (Just ls) cname t decls = 
   Instance (CtxtTuple (map (uncurry (\ x y -> CtxtClass x (map (TyVar False) y))) ls)) cname t decls

hClass :: Context -> ClassName -> [TyVar] -> [HDecl] -> HDecl
hClass ctxt nm tvs ds = Class ctxt nm tvs ds 

--unparameterised type synonym.
tySyn :: Name -> [Name] -> Type  -> HDecl
tySyn dname tvs ty = TyD (TypeSyn dname tvs ty)


-- (Foo T1 T2 T3) ==> (Foo a1 a2 a3)
-- (Foo {f1::T1,f2::T2}) ==> (Foo f1 f2)
conDeclToCon :: ConDecl -> Expr
conDeclToCon (ConDecl nm args)   = 
  dataCon (mkConName nm) (zipWith (\ _ a -> var ('a':show a)) args [(1::Int)..])
conDeclToCon (RecDecl nm fields) = 
  dataCon (mkConName nm) (map (\ (f,_) -> var f) fields)

conDeclToPat :: ConDecl -> Pat
conDeclToPat (ConDecl nm args)   = 
  conPat (mkConName nm) (zipWith (\ _ a -> patVar ('a':show a)) args [(1::Int)..])
conDeclToPat (RecDecl nm fields) = 
  conPat (mkConName nm) (map (\ (f,_) -> patVar f) fields)

-- prelude/Type.lhs rip-off
splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
 where
  split args _       (TyFun arg res) = split (arg:args) res res
  split args orig_ty _               = (reverse args, orig_ty)
\end{code}

\begin{code}
andDecl :: HDecl -> HDecl -> HDecl
andDecl = AndDecl

andDecls :: [HDecl] -> HDecl
andDecls = foldr (andDecl) emptyDecl

isEmptyDecl :: HDecl -> Bool
isEmptyDecl EmptyDecl = True
isEmptyDecl _	      = False

emptyDecl :: HDecl
emptyDecl = EmptyDecl

comment :: String -> HDecl
comment str = Haskell ('-':'-':' ':str)

cCode :: String -> HDecl
cCode s = CCode s
\end{code}

\begin{code}
typeSig :: String -> Type -> HDecl
typeSig nm ty = TypeSig nm Nothing ty

genTypeSig :: String -> Maybe Context -> Type -> HDecl
genTypeSig nm mb_ctxt ty = TypeSig nm mb_ctxt ty

-- hoists out the contexts before constructing the tysig.
mkTypeSig :: String -> [Type] -> Type -> HDecl
mkTypeSig nm pts rty = genTypeSig nm ctxt (foldr funTy rty' pts')
 where
   (rty':pts', ctxt) = generaliseTys (rty:pts)

funDef :: String -> [Pat] -> Expr -> HDecl 
funDef nm pats rhs = ValDecl (mkVarName nm) pats [GExpr [] rhs]

valDef :: String -> Expr -> HDecl
valDef nm rhs = ValDecl (mkVarName nm) [] [GExpr [] rhs]

methodDef :: QualName -> [Pat] -> Expr -> HDecl 
methodDef qnm pats rhs = ValDecl qnm pats [GExpr [] rhs]

guardedFunDef :: String -> [Pat] -> [(Expr,Expr)] -> HDecl
guardedFunDef nm pats grhs = ValDecl (mkVarName nm) pats (map (\ (g,e) -> GExpr [g] e) grhs)

prim :: CallConv -> LocSpec -> Name -> Type -> Bool -> [(Bool,String)] -> (Bool,String) -> HDecl
prim cc ls nm ty need_wrapper c_args c_res 
  = Primitive True cc ls nm ty need_wrapper c_args c_res

extLabel :: Name -> Name -> Type -> HDecl
extLabel cname hname t = ExtLabel cname hname t

primcst :: CallConv -> Name -> Type -> Bool -> [(Bool,String)] -> (Bool,String) -> HDecl
primcst cc nm ty need_wrapper c_args c_res 
  = PrimCast cc nm ty need_wrapper c_args c_res

fexport :: CallConv -> Maybe Name -> Name -> Type -> HDecl
fexport cc Nothing     h_nm ty = Callback cc h_nm ty
fexport cc (Just c_nm) h_nm ty = Entry cc c_nm h_nm ty
\end{code}

\begin{code}
conPat :: ConName -> [Pat] -> Pat
conPat dc a = PatCon dc a

patVar :: Name -> Pat
patVar v = PatVar (mkVarName v)

isVarPat :: Pat -> Bool
isVarPat (PatVar _) = True
isVarPat _	    = False

wildPat :: Pat
wildPat = PatWildCard

patRec :: VarName -> [(VarName, Pat)] -> Pat
patRec nm pats = PatRecord nm pats

qpatVar :: Maybe String -> Name -> Pat
qpatVar qmod v = PatVar (mkQVarName qmod v)

varPat :: Expr -> Pat
varPat (Var v) = PatVar v
varPat _       = error "varPat: no can do - wasn't passed a Var, guv."

litPat :: Literal -> Pat
litPat l = PatLit l

tuplePat :: [Pat] -> Pat
tuplePat [p] = p
tuplePat ps  = PatTuple ps

exprToPat :: Expr -> Maybe Pat
exprToPat (Var v)   = Just (PatVar v)
exprToPat (Con c)   = Just (PatCon c [])
exprToPat (Apply (Con c) ls) = Just (PatCon c (map ((fromMaybe PatWildCard).exprToPat) ls))
exprToPat (Lit l)   = Just (PatLit l)
exprToPat (List ls) = Just (PatList  (map ((fromMaybe PatWildCard).exprToPat) ls))
exprToPat (Tup ls)  = Just (tuplePat (map ((fromMaybe PatWildCard).exprToPat) ls))
exprToPat _	    = Nothing
\end{code}

Expressions:

\begin{code}
ret :: Expr -> Expr
ret e = Return e

bind :: Expr -> Expr -> Expr -> Expr
bind m v n = Bind m (varPat v) n

genBind :: Expr -> Pat -> Expr -> Expr
genBind m p n = Bind m p n

bind_ :: Expr -> Expr -> Expr
bind_ m n = Bind_ m n

var :: Name -> Expr
var v = Var (mkVarName v)

varName :: VarName -> Expr
varName v = Var v

qvar :: Maybe String -> Name -> Expr
qvar qmod v = Var (mkQVarName qmod v)

lam :: [Pat] -> Expr -> Expr
lam pats e = Lam pats e

lit :: Literal -> Expr
lit l = Lit l

integerLit :: IntegerLit -> Expr
integerLit l = Lit (IntegerLit l)

dataConst :: ConName -> Expr
dataConst nm = Con nm

dataCon :: ConName -> [Expr] -> Expr
dataCon dc args = Apply (Con dc) args

funApp :: VarName -> [Expr] -> Expr
funApp f args = Apply (Var f) args

-- right-assoc function application.
contApply :: Expr -> Expr -> Expr
contApply e1 e2 = RApply e1 e2

funApply :: Expr -> [Expr] -> Expr
funApply f args = Apply f args

binOp :: BinaryOp -> Expr -> Expr -> Expr
binOp bop e1 e2 = BinOp bop e1 e2

infixOp :: Expr -> VarName -> Expr -> Expr
infixOp e1 op e2 = InfixOp e1 op e2

unaryOp :: UnaryOp -> Expr -> Expr
unaryOp uop e1 = UnOp uop e1

tup :: [Expr] -> Expr
tup [e] = e
tup es = Tup es

hList :: [Expr] -> Expr
hList es = List es

hCase :: Expr -> [CaseAlt] -> Expr
hCase scrut alts = Case scrut alts

hIf :: Expr -> Expr -> Expr -> Expr
hIf c e1 e2 = If c e1 e2

alt :: Pat -> Expr -> CaseAlt
alt p e = Alt p [GExpr [] e]

genAlt :: Pat -> Expr -> Expr -> CaseAlt
genAlt p g e = Alt p [GExpr [g] e]

defaultAlt :: (Maybe VarName) -> Expr -> CaseAlt
defaultAlt b e = Default b e

equals :: Expr -> Expr -> Binding
equals (Var v) e = Binder (qName v) e
equals _       _ = error "equals: no can do - wasn't passed a Var, guv."

hLet :: Expr{-a Var-} -> Expr -> Expr -> Expr
hLet v x y = Let [(equals v x)] y

hLets :: [(Expr,Expr)] -> Expr -> Expr
hLets bs e = Let (map (uncurry equals) bs) e

intLit :: Integral a => a -> Expr
intLit v = Lit (IntegerLit (ILit 10 (toInteger v)))

stringLit :: String -> Expr
stringLit v = Lit (StringLit v)

addPtr :: Expr -> Expr -> Expr
addPtr ptr off = funApp (mkQVarName hdirectLib "addNCastPtr") [ptr, off]

castPtr :: Expr -> Expr
castPtr ptr = funApp castPtrName [ptr]

nothing :: Expr
nothing = dataConst nothingName

just :: Expr -> Expr
just v = dataCon justName [v]

unit :: Expr
unit = tup [] --dataConst (mkConName "()")

prefix :: String -> TyCon -> VarName
prefix = prefixQName

prefixApp :: String -> TyCon -> VarName
prefixApp = prefixAppQName

appendStr :: String -> TyCon -> VarName
appendStr v tname = tname{qName=qName tname ++ v, qDefModule=Nothing}

isVarsEq :: Expr -> Expr -> Bool
isVarsEq (Var a) (Var b) = qName a == qName b
isVarsEq _       _       = error "isVarsEq"

\end{code}

Misc toplevel decls

\begin{code}
hModule :: Name -> Bool -> [HExport] -> [HImport] -> HDecl -> HTopDecl
hModule nm flg exps imps d = HMod (HModule nm flg exps imps d)

hMeta   :: String -> HTopDecl
hMeta str = HLit str

cMeta   :: String -> HTopDecl
cMeta str = CLit str

hInclude   :: String -> HTopDecl
hInclude str = HInclude str

hExport  :: HIEEntity -> Maybe String -> HExport
hExport ent comment_ = HExport ent comment_

hImport  :: Name -> Bool -> [HIEEntity] -> HImport
hImport nm is_qualed ls =
 HImport is_qualed Nothing nm $
 case ls of
   [] -> Nothing
   _  -> Just ls

hQImport :: Name -> Maybe Name -> [HIEEntity] -> HImport
hQImport nm maybeAs stuff = HImport True maybeAs nm (Just stuff)

ieModule, ieValue, ieClass :: Name -> HIEEntity
ieModule nm = IEModule nm
ieValue  nm = IEVal    nm
ieClass  nm = IEClass  nm

ieType :: Name -> Bool -> HIEEntity
ieType nm abstractly = IEType nm abstractly
\end{code}


\begin{code}
subst :: Name -> Expr -> Expr -> Expr
subst nm e1 e2 = go e2
  where
   go e@(Var v)
      | qName v == nm = e1
      | otherwise     = e
   go e@(Con _)    = e
   go e@(Lit _)    = e
    -- don't worry about capture..yet.
   go (Lam pats e) = Lam pats (go e)
   go (Apply f args) = Apply (go f) (map go args)
   go (RApply f x) = RApply (go f) (go x)
   go (Tup es) = Tup (map go es)
   go (BinOp op e_1 e_2) = BinOp op (go e_1) (go e_2)
   go (UnOp op e) = UnOp op (go e)
   go (Bind e_1 p e_2) = Bind (go e_1) p (go e_2)
   go (Bind_ e_1 e_2) = Bind_ (go e_1) (go e_2)
   go (List es) = List (map go es)
   go (InfixOp op qnm e) = InfixOp op qnm (go e)
   go (Return e) = Return (go e)
   go (Case e alts) = Case (go e) (map substAlt alts)
   go (If e_1 e_2 e_3) = If (go e_1) (go e_2) (go e_3)
   go (Let binds e) = Let binds (go e)
   go (WithTy e ty) = WithTy (go e) ty

   substAlt (Alt p gs)     = Alt p (map (\ (GExpr ls e) -> GExpr (map go ls) (go e)) gs)
   substAlt (Default mb e) = Default mb (go e)

\end{code}

\begin{code}
mkQVarName :: Maybe String -> String -> VarName
mkQVarName qmod nm = mkQualName qmod nm

mkVarName :: String -> VarName
mkVarName nm = mkQualName Nothing nm

mkConName :: String -> ConName
mkConName nm = mkQualName Nothing nm

mkQConName :: Maybe String -> String -> ConName
mkQConName qmod nm = mkQualName qmod nm

mkTyVar :: String -> TyVar
mkTyVar nm = mkQualName Nothing nm

mkQTyVar :: Maybe String -> String -> TyVar
mkQTyVar qmod nm = mkQualName qmod nm

mkQTyCon :: Maybe String -> String -> TyCon
mkQTyCon qmod nm = mkQualName qmod nm
\end{code}



Generating a corresponding int type. Slightly awkward expressed, so
that we can easily retarget the mapping (and home) for the various
numeric types in AbsHUtils.

\begin{code}
type Signed = Bool

mkIntTy :: Size -> Signed -> Type
mkIntTy sz isSigned
  | isSigned =
      case sz of
         Short    -> tyInt16
	 Long     -> tyInt32
	 Natural
	  | optIntIsInt -> tyInt
	  | otherwise   -> tyInt32

	 LongLong 
	    | optLongLongIsInteger -> tyInteger
	    | otherwise            -> tyInt64
  | otherwise =
      case sz of 
         Short    -> tyWord16
	 Long     -> tyWord32
	 Natural
	  | optIntIsInt -> tyWord
	  | otherwise   -> tyWord32
	 LongLong 
	  | optLongLongIsInteger -> tyInteger
	  | otherwise		 -> tyWord64
\end{code}


Mapping for floats and chars       

\begin{code}
mkFloatTy :: Size -> Type
mkFloatTy sz =
  case sz of 
    Short    -> tyFloat
    Long     -> tyDouble
    LongLong -> tyLongDouble
    Natural  -> tyFloat

mkCharTy :: Signed -> Type
mkCharTy isSigned
 | isSigned  = tyInt8
 | otherwise = tyChar
\end{code}

\begin{code}
findIncludes :: HDecl -> [String]
findIncludes d = whizz d []
 where
  whizz (AndDecl h1 h2) rs = whizz h1 (whizz h2 rs)
  whizz (Include x)     rs = x:rs
  whizz _		rs = rs

\end{code}

\begin{code}
mkTySig :: [Type] -> Type -> String
mkTySig ps res = concat (intersperse "-" ls)
 where
   ls = map toSig (ps ++ [res])
   toSig (TyCon tc) = 
      case qName tc of
        'I':'n':'t':xs -> 'I':xs 
        'W':'o':'r':'d':xs -> 'W':xs 
	v -> v
   toSig (TyVar _ tv) = qName tv
   toSig (TyApply tc@TyCon{} ts) = concatMap toSig (tc:ts)
    -- weaken once debugged.
   toSig _ = error "mkTySig: not supposed to happen"

\end{code}
