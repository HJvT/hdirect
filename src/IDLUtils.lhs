%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Aug. 21th 2003  08:42  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Misc utilities to help out the desugarer.

\begin{code}
module IDLUtils 

       ( iName
       , noAttrs
       , removeIdAttrs
       , idAttrs

       , isUnpointedId
       , isConstructedTy
       , isCompleteTy
       , isReferenceTy
       , isMIDLishTy
       , isMIDLishId
       , isFunTy
       , isEmptyStructTy
       , isAnonTy

       , tyTag
       , withTyTag
       , getTyQual
       , mkReferenceTy
       
       , getTyTag
       
       , isVoidTyDef

       , reduceExpr

       , isLeafDefn
       
       , tyShort
       , tyWord16
       , tyInt16
       , tyWord32
       , tyInt32
       , tyGUID
       , tyIDispatch
       , tyVARIANT
       , tySafeArray
       , tyCURRENCY
       , tyDATE
       , tyFILETIME
       , tyAddr
       , tyVoid
       , tyIStream
       , tyIStorage
       , tyChar
       , tyWord64
       , tyInt64
       , tyByte
       , tyVariantBool
       , tyIUnknown
       , tyHRESULT
       , tyBSTR
       , tyString
       , tyWString
       , tyFloat
       , tyDouble
       , tyInt

       , retValAttribute
       , defaultAttribute
       , lcidAttribute
       , optionalAttribute
       , controlAttribute
       , restrictedAttribute
       , hiddenAttribute
       , versionAttr
       , helpStringAttr
       , helpContextAttr
       , helpFileAttr
       , helpStringDllAttr
       , helpStringCtxtAttr
       , lcidvalAttribute
       
       , mkFunId
       , mkMethodId
       , massageId
       
       , sortDefns
       , winnowDefns
       
       , exprToName
       , exprType
       , transferPointedness
       
       , toCConvAttrib
       , toPackedAttrib
       , mkGNUAttrib
       
       , handlePackPragma
       
       , childAttributes

       ) where

import IDLSyn
import qualified CoreIDL as Core (Expr(..),Type(..))
import DsMonad
import BasicTypes
import Literal
import Data.Int
import Data.Word   ( Word16 )
import Data.Bits
import Utils       ( notNull )
import Data.List   ( isPrefixOf, find )
import Data.Char   ( isDigit, isAlpha, isSpace )
import Opts        ( optUnwrapSingletonStructs, optOnlyRemoveDefns )
import Digraph
import Data.Maybe
import Control.Monad
import PpIDLSyn
import Env

{-
Only used by idlToCoreTy, idlToCoreExpr
import CoreUtils ( mkId, iUnknownTy, iDispatchTy, bstrTy )
import Utils     ( mapFromMb )
-}

\end{code}

\begin{code}
mkFunId :: Id -> [Param] -> Id
mkFunId f ps = 
  case findCC f of
--    (mb_cc, FunId i' _ _ ) -> FunId i' mb_cc ps
    (mb_cc, i')            -> FunId i' mb_cc ps
 where
  findCC i@Id{}        = (Nothing, i)
  findCC (AttrId as i) = 
     case findCC i of
       (mb_cc, i') -> (mb_cc, AttrId as i')
  findCC i@ArrayId{} = (Nothing, i)
  findCC i@FunId{}   = (Nothing, i)
  findCC (BitFieldId _ i) = findCC i
  findCC (Pointed qs i)  = 
     case findCC i of
       (mb_cc, i') -> (mb_cc, Pointed qs i')
  findCC (CConvId cc i) = (Just cc, i')
    where
      i' = ripOffCC i
      
  ripOffCC (CConvId _ ci) = ripOffCC ci
  ripOffCC i	          = snd (findCC i)

-- push pointedness and callconvs inwards.
massageId :: Id -> Id
massageId = mkMethodId
      
\end{code}

@mkMethodId@ pushes the @FunId@ outwards and @PointedId@s
inwards - need to do this in order to desugar method signatures
correctly.

\begin{code}
mkMethodId :: Id -> Id
mkMethodId m_id = go m_id Nothing []
 where
   go (Pointed qs i) cacc qacc     = go i cacc (qs:qacc)
   go (CConvId c i)  cacc qacc     = go i (cacc `mplus` Just c) qacc
   go (AttrId _  i)  cacc qacc     = go i cacc qacc
   go (BitFieldId _ i) cacc qacc   = go i cacc qacc
   go (FunId i mb_cc ps) cacc qacc = 
   	  FunId (foldr Pointed i (reverse qacc))
		(cacc `mplus` mb_cc)
		ps

   go _ _ _ = m_id -- in vain, just return orig id.

\end{code}

When parsing, attributes such as pointer indirection, calling convention etc.
gets associated with the Id of a method/field rather than the (result) type.
@transferPointedness@ shifts it over to the type.

\begin{code}
transferPointedness :: Id -> Type -> Type
transferPointedness pid ty =
  case pid of
    Pointed _ i   -> transferPointedness i (TyPointer ty)
    ArrayId i e   -> transferPointedness i (TyArray ty e)
    FunId i cc ps -> transferPointedness i (TyFun cc ty ps)
    AttrId _ i    -> transferPointedness i ty
    _		  -> ty

\end{code}


\begin{code}
iName :: Id -> Name
iName (Id s)        = s
iName (ArrayId i _) = iName i
iName (Pointed _ i) = iName i
iName (CConvId _ i) = iName i
iName (AttrId _ i)  = iName i
iName (BitFieldId _ i) = iName i
iName (FunId i _ _) = iName i

removeIdAttrs :: Id -> Id
removeIdAttrs (AttrId _ i) = removeIdAttrs i
removeIdAttrs i = i

idAttrs :: Id -> [Attribute]
idAttrs (AttrId as i) = as ++ idAttrs i
idAttrs _ = []

noAttrs :: [Attribute]
noAttrs = []
\end{code}

Various @IDLSyn.Type@ predicates:

\begin{code}
isUnpointedId :: Id -> Bool
isUnpointedId Pointed{} = False
isUnpointedId ArrayId{} = False
isUnpointedId (AttrId _ i) = isUnpointedId i
isUnpointedId _         = True

isConstructedTy :: Type -> Bool
isConstructedTy (TyStruct _ [(_,_,[_])] _) = not optUnwrapSingletonStructs
isConstructedTy TyStruct{}   = True
isConstructedTy TyEnum{}     = True
isConstructedTy TyUnion{}    = True
isConstructedTy TyUnionNon{} = True
isConstructedTy TyCUnion{}   = True
isConstructedTy _            = False

isEmptyStructTy :: Type -> Bool
isEmptyStructTy (TyStruct _ [] _) = True
isEmptyStructTy _		  = False

isAnonTy :: Type -> Bool
isAnonTy (TyName nm _) = "__IHC_TAG" `isPrefixOf` nm
isAnonTy _	  = False

isFunTy :: Type -> Bool
isFunTy TyFun{} = True
isFunTy _	= False

isCompleteTy :: Type -> Bool
isCompleteTy ty = 
   case ty of
    TyStruct _ ls _    -> notNull ls
    TyEnum _ ls        -> notNull ls
    TyUnion _ _ _ _ ls -> notNull ls
    TyUnionNon _ ls    -> notNull ls
    TyCUnion _ ls _    -> notNull ls
    _		       -> error "isCompleteTy"

isReferenceTy :: Type -> Bool
isReferenceTy = not.isCompleteTy

mkReferenceTy :: Type -> Type
mkReferenceTy ty = 
   case ty of
    TyStruct tg _ _     -> TyStruct tg [] Nothing
    TyEnum tg _         -> TyEnum tg []
    TyUnion tg t i _ _  -> TyUnion tg t i Nothing []
    TyUnionNon tg _     -> TyUnionNon tg []
    TyCUnion tg _ _     -> TyCUnion tg [] Nothing
    _                   -> error "IDLUtils.mkReferenceTy: expected a constructed ty"
                     
getTyTag :: String -> Type -> String
getTyTag def (TyEnum mb_tag _)     = fromMaybe def (fmap iName mb_tag)
getTyTag def (TyStruct mb_tag _ _) = fromMaybe def (fmap iName mb_tag)
getTyTag def (TyUnion mb_tag _ _ _ _) = fromMaybe def (fmap iName mb_tag)
getTyTag def (TyUnionNon mb_tag _)    = fromMaybe def (fmap iName mb_tag)
getTyTag def (TyCUnion mb_tag _ _)    = fromMaybe def (fmap iName mb_tag)
getTyTag _   (TyName n  _)  = n
getTyTag def (TyPointer t)  = getTyTag def t
getTyTag _   t               
  = error ("IDLUtils.getTyTag: unexpected type: " ++ showIDL (ppType t))

-- When generating type libraries, MIDL likes
-- to flatten typedefs to constructed types, i.e.,
--  typedef enum { a } b; 
-- is turned into
--  typedef enum { a } __MIDL___MIDL__aaaa;
--  typedef [public] __MIDL___MIDL__aaaa b;
-- 
-- We try to shorten out these silly names in the
-- desugarer.
isMIDLishTy :: Type -> Bool
isMIDLishTy ty =
  case ty of
    TyName nm _ -> isMIDLishNm nm
    _	        -> False

isMIDLishId :: Id -> Bool
isMIDLishId (Id s) = isMIDLishNm s
isMIDLishId _      = False

isMIDLishNm :: String -> Bool
isMIDLishNm nm = "MIDL___MIDL__" `isPrefixOf` nm'
 where
   -- Note: we have deleted the leading underscores by now
   --  (the desugarer does this.)
   nm' =
    case nm of
     '_':'_':xs -> xs
     _ -> nm
                     
\end{code}

\begin{code}
tyTag :: Type -> String
tyTag (TyStruct   (Just i) _ _)     = iName i
tyTag (TyEnum     (Just i) _)       = iName i
tyTag (TyUnion    (Just i) _ _ _ _) = iName i
tyTag (TyUnionNon (Just i) _)       = iName i
tyTag (TyCUnion   (Just i) _ _)     = iName i
tyTag (TyName nm _)		    = nm
tyTag _				    = ""

withTyTag :: String -> Type -> Type
withTyTag tg ty = 
  case ty of
    TyStruct Nothing a b -> TyStruct tag a b
    TyEnum   Nothing a   -> TyEnum   tag a
    TyUnion  Nothing a b c d -> TyUnion tag a b c d
    TyUnionNon Nothing a  -> TyUnionNon tag a
    TyCUnion Nothing a b  -> TyCUnion tag a b
    _ -> ty
 where
  tag = Just (Id tg)

-- left most type qualifier 'wins'.
getTyQual :: Type -> ([Qualifier], Type)
getTyQual (TyApply (TyQualifier q) t) = ([q], t') 
  where
   -- continue spinning, dropping the type quals we're
   -- going to ignore.
   (_, t') = getTyQual t
getTyQual t = ([],t)
\end{code}

\begin{code}
isVoidTyDef :: Type -> [Id] -> Bool
isVoidTyDef TyVoid [Id _] = True
isVoidTyDef _      _      = False
\end{code}

When filling in values for the enum tags, we have to reduce the expressions
for the tags that do have a value attached to them.

The name to value mapping passed is the mapping for constants.

\begin{code}
reduceExpr :: (Type -> DsM Core.Type) -> Expr -> DsM (Either Int32 Core.Expr)
reduceExpr redType expr =
 case expr of
   Binary op e1 e2 -> do
     i1 <- reduceExpr redType e1
     i2 <- reduceExpr redType e2
     return (binop_m op i1 i2)
   Cond e1 e2 e3 -> do
     i1 <- reduceExpr redType e1
     i2 <- reduceExpr redType e2
     i3 <- reduceExpr redType e3
     return (cond_m i1 i2 i3)
   Unary op e -> do
     i <- reduceExpr redType e
     return (unop_m op i)
   Var nm -> do
     res <- lookupConst nm
     case res of
       Nothing -> return (Right (Core.Var nm))
       Just v  -> return v
   Lit l  -> return (Left (reduceLit l))
   Cast t e -> do
      res <- reduceExpr redType e
      t'  <- redType t
      case res of
        Left  i  -> return (Left i)  -- ToDo: perform cast operation on the result.
	Right e' -> return (Right (Core.Cast t' e'))
   Sizeof t -> do
      t' <- redType t
      return (Right (Core.Sizeof t'))

reduceLit :: Literal -> Int32
reduceLit l =
 case l of
   IntegerLit (ILit _ i) -> fromInteger i
   _ -> error ("reduceLit(" ++ show l ++ "): no can do.")

binop_m :: BinaryOp 
        -> Either Int32 Core.Expr
	-> Either Int32 Core.Expr
	-> Either Int32 Core.Expr
binop_m op (Left i1) (Left i2)   = Left (binop op i1 i2)
binop_m op e1 e2  = Right (Core.Binary op (toExpr e1) (toExpr e2))

binop :: BinaryOp -> Int32 -> Int32 -> Int32
binop op i1 i2 =
 case op of
   Add -> i1 + i2
   Sub -> i1 - i2
   Div -> i1 `div` i2
   Mod -> i1 `mod` i2
   Mul -> i1 * i2
   And -> i1 .&. i2
   Or  -> i1 .|. i2
   Xor -> i1 `xor` i2
   Shift L -> shiftL i1 (fromIntegral i2)
   Shift R -> shiftR i1 (fromIntegral i2)
   _       -> error ("binop: unexpected " ++ show op)

cond_m :: Either Int32 Core.Expr -> Either Int32 Core.Expr 
       -> Either Int32 Core.Expr -> Either Int32 Core.Expr 
cond_m (Left i1) (Left i2) (Left i3) = Left (cond i1 i2 i3)
cond_m e1 e2 e3 = Right (Core.Cond (toExpr e1) (toExpr e2) (toExpr e3))

toExpr :: Either Int32 Core.Expr -> Core.Expr
toExpr (Right e) = e
toExpr (Left  i) = Core.Lit (IntegerLit (ILit 10 (toInteger i)))

cond :: Int32 -> Int32 -> Int32 -> Int32
cond 0 x _ = x
cond _ _ y = y

unop_m :: UnaryOp -> Either Int32 Core.Expr -> Either Int32 Core.Expr
unop_m op (Left i1)  = Left  (unop op i1)
unop_m op (Right e1) = Right (Core.Unary op e1)

unop :: UnaryOp -> Int32 -> Int32
unop Minus i = negate i
unop Plus  i = i
unop Not   i = complement i
unop o     _ = error ("unop: unexpected " ++ show o)
\end{code}


\begin{code}
isLeafDefn :: Defn -> Bool
isLeafDefn TypeDecl{}  = True
isLeafDefn Typedef{}   = True
isLeafDefn Operation{} = True
isLeafDefn Constant{}  = True
isLeafDefn _	       = False

\end{code}

begin{code}
idlToCoreTy :: Type -> Core.Type
idlToCoreTy ty = 
 case ty of
  TyInteger sz        -> Core.Integer sz True
  TyFloat sz          -> Core.Float sz
  TyChar              -> Core.Char False
  TyWChar             -> Core.WChar
  TyBool              -> Core.Bool
  TyOctet             -> Core.Octet
  TyAny               -> Core.Any
  TyObject            -> Core.Object
  TyBString           -> bstrTy
  TyVoid              -> Core.Void
  TyName nm Nothing   -> Core.Name nm nm Nothing Nothing Nothing Nothing
  TyName nm (Just t)  -> Core.Name nm nm Nothing Nothing (Just (idlToCoreTy t)) Nothing
  TyIface "IUnknown"  -> iUnknownTy
  TyIface "IDispatch" -> iDispatchTy
  TyIface nm          -> Core.Iface nm Nothing nm [] False []
  TyPointer t         -> Core.Pointer Ptr False (idlToCoreTy t)
--  TyFixed mb e i    -> Core.Fixed (idlToCoreExpr e) i
  TyArray t es        -> Core.Array (idlToCoreTy t) (map idlToCoreExpr es)
  TyApply (TySigned s) (TyInteger sz) -> Core.Integer sz s
  TyApply (TySigned s) TyChar -> Core.Char s
  TyApply (TySigned s) _ -> Core.Integer Long s
  TySigned s ->  Core.Integer Long s
  TyApply (TyQualifier _) t -> idlToCoreTy t
  TyApply t (TyQualifier _) -> idlToCoreTy t
  TyString mb_expr     ->
     let core_expr = mapFromMb Nothing
			       (Just . idlToCoreExpr) 
			       mb_expr
     in
     Core.String (Core.Char False) False core_expr
  TyWString mb_expr    ->
     let 
       core_expr = mapFromMb Nothing
			     (Just . idlToCoreExpr) 
			     mb_expr
     in
     Core.WString False core_expr
  TySequence t mb_expr ->
     let
       core_ty   = idlToCoreTy t
       core_expr = mapFromMb Nothing
       			     (Just . idlToCoreExpr)
			     mb_expr
     in
     Core.Sequence core_ty core_expr Nothing
  TyApply (TyQualifier _) t -> idlToCoreTy t

  TyEnum (Just (Id nm)) _     -> Core.Enum (mkId nm nm Nothing []) Unclassified []
  TyStruct (Just (Id nm)) _ mb_pack -> Core.Struct (mkId nm nm Nothing []) [] mb_pack
  TyUnion (Just (Id nm1)) t 
              (Id nm2) (Just (Id nm3)) _-> 
	Core.Union (mkId nm1 nm1 Nothing []) (idlToCoreTy t)
		   (mkId nm2 nm2 Nothing []) (mkId nm3 nm3 Nothing []) []
  TyUnionNon (Just (Id nm1)) _ -> 
        Core.UnionNon (mkId nm1 nm1 Nothing []) []
  TyCUnion (Just (Id nm)) _ mb_pack ->
	Core.CUnion (mkId nm nm Nothing []) [] mb_pack

end{code}

Conversion an IDLSyn expression tree into a CoreIDL one - 
a candidate for polytypic treatment.

begin{code}
idlToCoreExpr :: Expr -> Core.Expr
idlToCoreExpr e =
 case e of
  Binary bop e1 e2 -> Core.Binary bop (idlToCoreExpr e1)
  				      (idlToCoreExpr e2)
  Cond e1 e2 e3 -> Core.Cond (idlToCoreExpr e1)
  			     (idlToCoreExpr e2)
			     (idlToCoreExpr e3)
  Unary op e1 -> Core.Unary op (idlToCoreExpr e1)
  Var nm      -> Core.Var nm
  Lit l       -> Core.Lit l
  Cast t e1   -> Core.Cast (idlToCoreTy t) (idlToCoreExpr e1)
  Sizeof t    -> Core.Sizeof (idlToCoreTy t)
end{code}

Common attributes:

\begin{code}
simpleAttr :: String -> Attribute
simpleAttr nm = Attrib (Id nm) []

retValAttribute, lcidAttribute, optionalAttribute :: Attribute
retValAttribute		= simpleAttr "retval"
lcidAttribute	        = simpleAttr "lcid"
optionalAttribute       = simpleAttr "optional"

controlAttribute, restrictedAttribute, hiddenAttribute :: Attribute
controlAttribute	= simpleAttr "control"
restrictedAttribute	= simpleAttr "restricted"
hiddenAttribute	        = simpleAttr "hidden"

defaultAttribute :: Maybe Literal -> Maybe Attribute
defaultAttribute Nothing  = Nothing
defaultAttribute (Just x) = Just (Attrib (Id "defaultvalue") [AttrLit x])

versionAttr :: Word16 -> Word16 -> Maybe Attribute
versionAttr maj mino    = toMaybe (\ _ -> maj /=0 || mino /=0)
				  (Attrib (Id "version") [AttrLit (LitLit ((show maj) ++'.':show mino))])
				  undefined

helpStringAttr :: String -> Maybe Attribute
helpStringAttr	s
  = toMaybe notNull (Attrib (Id "helpstring") [AttrLit (StringLit s)]) s

helpContextAttr :: Integer -> Maybe Attribute
helpContextAttr c 
  = toMaybe (/=0)   (Attrib (Id "helpcontext") [AttrLit (IntegerLit (ILit 16 c))]) c
helpFileAttr :: String -> Maybe Attribute
helpFileAttr hfile
  = toMaybe notNull (Attrib (Id "helpfile") [AttrLit (StringLit hfile)]) hfile

helpStringDllAttr :: String -> Maybe Attribute
helpStringDllAttr dll
  = toMaybe notNull (Attrib (Id "helpstringdll")
  			    [AttrLit (StringLit dll)]) dll

helpStringCtxtAttr :: Integer -> Maybe Attribute
helpStringCtxtAttr hc   = 
   toMaybe (/=0) (Attrib (Id "helpstringcontext")
   			 [AttrLit (IntegerLit (ILit 16 hc))]) hc

lcidvalAttribute :: Integer -> Maybe Attribute
lcidvalAttribute lc     = 
   toMaybe (/=0) (Attrib (Id "lcid") [AttrLit (IntegerLit (ILit 10 lc))]) lc

toMaybe :: (a -> Bool) -> b -> a -> Maybe b
toMaybe predic res mb_val
  | predic mb_val = Just res
  | otherwise	  = Nothing

\end{code}

The type-library reader needs to map TLB types to IDL types - 
here they are:

\begin{code}
tyWord16, tyWord32, tyWord64 :: Type
tyWord16  = TyApply (TySigned False) (TyInteger Short)
tyWord32  = TyApply (TySigned False) (TyInteger Long)
tyWord64      = TyApply (TySigned False) (TyInteger LongLong)

tyInt16, tyInt32, tyInt64, tyShort, tyInt :: Type
tyInt16   = tyShort
tyInt32   = TyApply (TySigned True) (TyInteger Long)
tyInt64   = TyApply (TySigned True) (TyInteger LongLong)
tyShort   = TyApply (TySigned True) (TyInteger Short)
tyInt	      = tyInt32

tyChar, tyByte :: Type
tyChar	      = TyChar
tyByte	      = TyOctet

tyAddr, tyVoid :: Type
tyAddr        = TyPointer TyVoid
tyVoid        = TyVoid

tyGUID :: Type
tyGUID       = TyName "GUID" Nothing

tyIUnknown, tyIDispatch :: Type
tyIDispatch  = TyIface "IDispatch"
tyIUnknown    = TyIface "IUnknown"

tyVARIANT :: Type
tyVARIANT    = TyName "VARIANT" Nothing

tySafeArray :: Type -> Type
tySafeArray t = TySafeArray t

tyCURRENCY :: Type
tyCURRENCY    = TyName "CURRENCY" Nothing
tyDATE :: Type
tyDATE        = TyName "DATE" Nothing
tyFILETIME :: Type
tyFILETIME    = TyName "FILETIME" Nothing

tyIStorage, tyIStream :: Type
tyIStream     = TyIface "IStream"
tyIStorage    = TyIface "IStorage"

tyVariantBool :: Type
tyVariantBool = TyName "VARIANT_BOOL" Nothing
tyHRESULT :: Type
tyHRESULT     = TyName "HRESULT" Nothing

tyBSTR, tyString, tyWString :: Type
tyString      = TyString Nothing
tyWString     = TyWString Nothing
tyBSTR	      = TyBString

tyFloat, tyDouble :: Type
tyFloat       = TyFloat Short
tyDouble      = TyFloat Long
\end{code}

Order sorting a sequence of definitions.

\begin{code}
sortDefns :: [Defn] -> [Defn]
sortDefns ds = map sortDefn ds_sorted
  where
     -- compute def & use of the individual decls.
   ds_depped  = map mkDefnDep ds

     -- compute scc's
   ds_groups  = stronglyConnComp ds_depped
   
   ds_i = filter isImport ds

   isImport (Import _)    = True
   isImport (ImportLib _) = True
   isImport (CInclude _)  = True
   isImport _	          = False

     -- expand the cyclic groups, taking care to leave
     -- the import statements up front.
   ds_sorted  = ds_i ++ filter (not.isImport) (concatMap expandGroup ds_groups)

sortDefn :: Defn -> Defn
sortDefn (Library i ds) = Library i (sortDefns ds)
sortDefn (Module i ds)  = Module i (sortDefns ds)
--sortDefn (Import ls)    = Import (map (\ (x,ds) -> (x, sortDefns ds)) ls)
sortDefn (Attributed x d) = Attributed x (sortDefn d)
sortDefn x		= x

mkDefnDep :: Defn -> (Defn, String, [String])
mkDefnDep d = let (def,uses) = getDefUses d in (d,def,uses)

getDefUses :: Defn -> (String,[String])
getDefUses d = (def, uses)
  where
   uses = getUses d
   def  = getDef d

getDef :: Defn -> String
getDef d =
 case d of
   Typedef _ _ (i:_)        -> iName i
   Attributed _ d1	    -> getDef d1
   ExternDecl _ [i]         -> iName i
   Operation i _ _ _        -> iName i
   Interface (Id i) _ _     -> i
   Module (Id i) _          -> i
   DispInterface (Id i) _ _ -> i
   CoClass (Id i) _	    -> i
   Library (Id i) _	    -> i
   TypeDecl t		    -> tyTag t
   _			    -> ""

getUses :: Defn -> [String]
getUses d = 
  case d of 
    Typedef ty _ _	  -> getTyUses ty
    Constant _ _ ty _	  -> getTyUses ty  -- expressions will never, 
					   -- ever have free variables (in fact, const is a foreign concept
					   -- to typelibs.)
    Interface _ is ds	  -> is ++ concatMap getUses ds
    Module _ ds	          -> concatMap getUses ds
    DispInterface _ ps ds -> concatMap (\ (_,t, _) -> getTyUses t) ps ++ concatMap getUses ds
    CoClass _ cs	  -> map (\ (_,Id i,_) -> i) cs
    Library _ ds	  -> concatMap getUses ds
    Attributed _ d1	  -> getUses d1
    TypeDecl t		  -> getTyUses t
    ExternDecl t _        -> getTyUses t
    Operation (FunId _ _ ps) r _ _ -> getTyUses r ++ concatMap (\ (Param _ t _) -> getTyUses t) ps
    _			  -> []

-- Since the types were constructed from type libraries, we can make
-- a number of simplifying assumptions.
getTyUses :: Type -> [String]
getTyUses ty =
  case ty of
    TyName  n _     -> [n]
    TyIface n       -> [n]
    TySafeArray t   -> getTyUses t
    TyArray t _     -> getTyUses t
    TyPointer t     -> getTyUses t
    TyCUnion _ fs _ -> concatMap (\ (t,_,_) -> getTyUses t) fs
    TyStruct (Just (Id n)) [] _ -> [n]
    TyStruct _ fs _ -> concatMap (\ (t,_,_) -> getTyUses t) fs
    TyEnum (Just (Id n)) []   -> [n]
    TyApply t1 t2	      -> getTyUses t1 ++ getTyUses t2
    _			      -> []

expandGroup :: SCC Defn -> [Defn]
expandGroup (AcyclicSCC d) = [d]
expandGroup (CyclicSCC ds) = ds'
  where
    -- record who was in our group, so that
    -- we can fight loops later.

   ds_uses = map getDef ds
   ds'     = forwardDecls ds (go [] ds_uses ds)

   forwardDecls []     cont = cont
   forwardDecls (a:as) cont = mkForwardDecl a (forwardDecls as cont)

   mkForwardDecl (Attributed _ d) cont = mkForwardDecl d cont
   mkForwardDecl (Interface i _ _) cont = Forward i : cont
   mkForwardDecl (DispInterface i _ _) cont = Forward i : cont
   mkForwardDecl _ cont = cont

   go _   _           [] = []
   go _   []          _  = []
   go bef (a:aft) (x:xs) = Attributed as x : go (a:bef) aft xs
    where
      as = map (\ ll -> Attrib (Id "depender") [AttrLit (LitLit ll)]) (bef ++ aft)

\end{code}

Sigh - copy of the routine you'll find in CoreUtils, but this
time over IDLSyn attributes.

\begin{code}
childAttributes :: [Attribute] -> [Attribute]
childAttributes as = filter (not.notAggregatableAttribute) as

notAggregatableAttribute :: Attribute -> Bool
notAggregatableAttribute (Attrib (Id nm) _) = nm `elem` junk_list
  where
   junk_list =
     [ "helpstring"
     , "helpcontext"
--     , "pointer_default"
     , "dllname"
     , "lcid"
     , "odl"
     , "restricted"
     , "ole"
     , "uuid"
     , "object"
     , "oleautomation"
     , "hidden"
     , "version"
     , "local"
     , "custom"
     , "public"
     , "dual"
     , "switch_type"
     , "switch_is"
     , "depender"
     , "ty_params"
     , "jni_interface"
     , "jni_iface_ty"
     , "jni_class"
     , "hs_name"
     , "hs_import"
     , "hs_newtype"
     ]
notAggregatableAttribute _ = False
\end{code}

The user can off-line specify which of the defns are (or aren't) of interest.
@winnowDefn@ is responsible from picking the chaff from the wheat, as it where.

\begin{code}
winnowDefns :: Env String (Bool,[Attribute]) 
	    -> [Defn]
	    -> [Defn]
winnowDefns wenv ws = reverse $ fst (go wenv "" (reverse ws))
 where
   go env _          [] = ([], env)
   go env prefix (d:ds) =
     case getDef d of 
       "" -> let (ds', e) = go env prefix ds in (d:ds', e)
       nm -> let
	       res  = lookupEnv env nm `mplus` lookupEnv env (prefix ++ '.':nm)
	       uses = getUses d
	       
		-- for a names that are to be retained, arrange for
		-- any of its uses to be retained also.
	       inh_value = (True, [Mode In])

                -- keep the decl iff:
		--   * it didn't have remove-me flag 
		--   * it was found in the environment nontheless.
		--   * we're in remove-only mode.
	       keep_it = not remove_it && (optOnlyRemoveDefns || isJust res)

	       remove_it =
	         case res of
		   Just (flg,ls) -> not flg && null ls 
		   		-- an asf entry is a 'remove-me' entry iff
				-- it is of the form "Name=<empty>"
		   _		 -> False

	       env'
		| isJust res    =
		   if remove_it || optOnlyRemoveDefns then
		      env
		   else
		      addListToEnv env (map (\ x -> (x, inh_value)) uses)
		| otherwise     = env
		
	       newPrefix i
	        | null prefix = iName i
		| otherwise   = prefix ++ '.':iName i
	      in
	      case d of
	        Attributed as d1 ->
		    case go env' prefix [d1] of
		      ([],_)     -> go env' prefix ds
		      ((x:_), e) -> let 
		                     (ds', e1) = go e prefix ds
				    in 
				    (Attributed as x : ds', e1)
	        Interface i inhs ms ->
		    case go env' (newPrefix i) (reverse ms) of
		      (ms', e) -> let (ds', e1) = go e prefix ds in
		                  if remove_it || (null ms' && (notNull ms || not keep_it)) then
				     (ds', e1)
				  else
				     (Interface i inhs (reverse ms') : ds', e1)
	        DispInterface i props ms ->
		    case go env' (newPrefix i) (reverse ms) of
		      (ms', e) -> let (ds', e1) = go e prefix ds in
		                  if remove_it || (null ms' && (notNull ms || not keep_it)) then
				     (ds', e1)
				  else
				     (DispInterface i props (reverse ms') : ds', e1)
		Library i ls ->
		    case go env' (newPrefix i) (reverse ls) of
		      (ls', e) -> let (ds', e1) = go e prefix ds in
		                  if remove_it || (null ls' && (notNull ls || not keep_it)) then
				     (ds', e1)
				  else
		                     (Library i (reverse ls') : ds', e1)
		Module i ms ->
		    case go env' (newPrefix i) (reverse ms) of
		      (ms', e) -> let (ds', e1) = go e prefix ds in
		                  if remove_it || (null ms' && (notNull ms || not keep_it)) then
				     (ds', e1)
				  else
		                     (Module i (reverse ms') : ds', e1)
	        _ -> let (ds', e) = go env' prefix ds in 
		     if keep_it then
		        (d:ds', e)
	             else
		        (ds', e)
    
\end{code}


\begin{code}
exprToName :: Expr -> String
exprToName e = map (\ x -> if (isAlpha x || isDigit x) then x else '_') 
                   (showIDL (ppExpr e))
\end{code}

Gather the type of an expression - unknown 

\begin{code}
exprType :: Type -> Expr -> Type
exprType defTy ex = 
  case ex of 
     Lit l         -> litType l
     Cast t _      -> t
     Sizeof{}      -> TyInteger Natural
     Var{}         -> defTy
     Cond _ e1 _   -> exprType defTy e1
     Binary _ e1 _ -> exprType defTy e1
     Unary uop e -> 
       case uop of
          Deref -> TyPointer (exprType defTy e)
	  _     -> exprType defTy e

litType :: Literal -> Type
litType l = 
  case l of
    IntegerLit{}  -> TyInteger Natural
    StringLit{}   -> TyString Nothing
    TypeConst s   -> TyName s Nothing
    WStringLit{}  -> TyWString Nothing
    CharLit{}     -> TyChar
    WCharLit{}    -> TyWChar
    FixedPtLit{}  -> TyFixed Nothing
    FloatingLit{} -> TyFloat Long
    BooleanLit{}  -> TyBool
    NullLit{}     -> TyPointer TyVoid
    GuidLit{}     -> TyName "GUID" Nothing
    LitLit{}      -> error "litType{LitLit}: can't determine type"

\end{code}

\begin{code}
toPackedAttrib :: [GNUAttrib] -> Maybe Int
toPackedAttrib [] = Nothing
toPackedAttrib ls = 
   case find (==Packed) ls of
     Nothing -> Nothing
     Just _  -> Just 1

toCConvAttrib :: [GNUAttrib] -> (Id -> Id)
toCConvAttrib [] = id
toCConvAttrib ls = 
   case find isCConv ls of
     Just (CConv cc) -> CConvId cc
     _               -> id
 where
  isCConv CConv{} = True
  isCConv _	  = False

mkGNUAttrib :: String -> [Expr] -> GNUAttrib
mkGNUAttrib "packed" _ = Packed
mkGNUAttrib x        _ = Unsupported x -- record just the name.
\end{code}

Hidden here because it is pig ugly.

\begin{code}
handlePackPragma :: String -> DsM ()
handlePackPragma ('p':'a':'c':'k':xs) = 
    -- For now, cheap & nasty.
   case dropWhile isSpace xs of
     ')':_                     -> pushPack Nothing
     '(':'p':'u':'s':'h':')':_ -> pushPack (Just Nothing)
     '(':'p':'o':'p':')':_     -> popPack Nothing
     '(':'p':'u':'s':'h':',':ys@(y:_) | isAlpha y -> 
  	let
	 (nm, rs) = break (\x -> x == ',' || x == ')') ys
	in
	case rs of
	  ')':_ -> pushPack (Just (Just (nm, Nothing)))
	  ',':rs2 -> 
	     let
	      (val, rs3) = break (== ')') rs2
	     in
	     case rs3 of
	       ')':_ -> 
  	         case reads val of
	           ((v,_):_) -> pushPack (Just (Just (nm, Just v)))
		   _ -> return ()
	       _ -> return ()
          _ -> return ()
     '(':'p':'u':'s':'h':',':ys@(y:_) | isDigit y -> 
	let
	 (val, rs) = break (== ')') ys
	in
	case rs of
	  ')':_ -> do
	     case reads val of
	       ((v,_):_) -> pushPack (Just (Just ("", Just v)))
	       _         -> return ()
          _ -> return ()
     '(':'p':'o':'p':',':ys@(y:_) | isAlpha y -> 
  	let
	 (nm, rs) = break (\x -> x == ',' || x == ')') ys
	in
	case rs of
	  ')':_   -> popPack (Just (nm, Nothing))
	  ',':rs2 -> 
	     let
	      (val, rs3) = break (== ')') rs2
	     in
	     case rs3 of
	       ')':_ -> 
  	         case reads val of
	           ((v,_):_) -> popPack (Just (nm, Just v))
		   _ -> return ()
	       _ -> return ()
          _ -> return ()
     '(':'p':'o':'p':',':ys@(y:_) | isDigit y -> 
	let
	 (val, rs) = break (== ')') ys
	in
	case rs of
	  ')':_ -> do
	     case reads val of
	       ((v,_):_) -> popPack (Just ("", Just v))
	       _         -> return ()
          _ -> return ()
     _ -> return ()

handlePackPragma _ = return ()

\end{code}
