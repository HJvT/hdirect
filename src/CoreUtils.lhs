%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Feb. 9th 2003  15:15  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

A hodgepodge of helper functions over the CoreIDL data types.

\begin{code}
module CoreUtils 
        (
          mkId
        , setIdModule
        , mkParam
        
        , flattenDecls
        , reallyFlattenDecls
        , inSeparateHaskellModule
        , findFieldTy
        , findFieldOrigTy
        , findParam
        , findParamTy
        
        , localiseTypes
        
        , getTypeAttributes        -- :: Type -> [Attribute]
        , getHsImports             -- :: Id   -> [QualName]

        , keepValueAsPointer

        , isStructTy
        , isEnumTy
        , isPointerTy
        , isVoidPointerTy
        , isArrayTy
        , isSafeArrayTy
        , isOpenArrayTy
        , isFunTy
        , isBoolTy
        , isVoidTy
        , isPointerOrArrayTy
        , isPtrPointerTy
        , isRefPointerTy
        , isUniquePointerTy
        , isStringTy
        , isSeqTy
        , isAnyTy
        , isObjectTy
        , isConstructedTy
        , isCompleteTy
        , isReferenceTy
        , isSimpleTy
        , isIntegerTy
        , isSynTy
        , isAbstractTy
        , isAbstractFinalTy
        , isNonEncUnionTy
        , getNonEncUnionTy
        , isUnionTy
        , isIfaceTy
        , isIUnknownTy
        , isIfacePtr
        , isVariantTy
        , getIfaceTy

        , tyFun
        , stringTy
        , wStringTy
        , bstrTy
        , intTy
        , addrTy
        , boolTy
        , variantBoolTy
        , variantTy
        , charTy
        , wCharTy
        , int32Ty
        , int64Ty
        , word64Ty
        , shortTy
        , floatTy
        , doubleTy
        , byteTy
        , word32Ty
        , int16Ty
        , word16Ty
        , voidTy
        , currencyTy
        , dateTy
        , fileTimeTy
        , safeArrayTy
        
        , iUnknownTy
        , iDispatchTy
        , hresultTy
        , guidTy
        , isHRESULTTy   -- :: Type   -> Bool
        , isHRESULT     -- :: Result -> Bool

        , mkPtrPointer
        , removePtr
        , removePtrAndArray
        , removePtrAll
        , removePtrs
        , removeNames
        , nukeNames
        , pushPointerType
        , hasIgnoreAttribute
        , findPtrType
        , mkRefPointer
        , rawPointerToIP
        , notAggregatableAttribute
        , childAttributes
        
        , getTyTag
        , findFreeVars
        , solve
        , complementOp
        , isCommutative
        , contains
        , evalExpr
        , simplifyExpr
        , simpRedExpr

        , plusOne
        , minusOne
        , add
        
        , sizeofType
        , sizeAndAlignModulus
        , computeStructSizeOffsets
        , align

        , Dependent(..)
        , DepVal(..)
        , DependInfo
        , findDependents
        , attrToDependent
        , computeArrayConstraints

        , isLengthIs
        , isSizeIs
        , isMaxIs
        , isMinIs
        , isFirstIs
        , isLastIs
        , sizeOrLength
        , minOrFirst
        , maxOrLast
        , isSwitchIs
        , lookupDepender
        , isDepender
        , isDependee
        , isSwitchDependee
        , isSwitchDepender
        , isNotSwitchDependee
        , hasNonConstantExprs

        , mkHaskellVarName
        , mkHaskellTyConName

        , toCType
        
        , mkIfaceTypeName
        , getInterfaceIds      -- :: Decl -> [Id]
        
        , idHaskellModule
        
        , isMethod
        , isConst
        , isMethodOrProp
        , isProperty
        , isCoClass
        
        , dummyMethod
        
        , unionToStruct
        
        , binParams
        , objParam
        , resultParam
        , iPointerParam
        
        , derivesFromIDispatch
        , toDispInterfaceMethod

        , sortDecls
        
        , isFinalisedType
        
        ) where

import CoreIDL
import BasicTypes ( Name, Size(..), BinaryOp(..), ShiftDir(..)
                  , PointerType(..), UnaryOp(..), ParamDir(..)
                  , CallConv, qName, qDefModule, qModule, QualName
                  , toQualName, qOrigName
                  )
import Attribute
import Literal
import LibUtils
import Opts   ( optOneModulePerInterface, optNoDependentArgs,
                optDeepMarshall, optHaskellToC, optClassicNameMangling,
                optLongLongIsInteger, optPointerDefault
              )
import PpCore ( ppType, showCore, ppExpr )
import Digraph
import Utils

import Data.Maybe  ( mapMaybe, fromMaybe, mapMaybe, isJust )
import Data.List   ( partition )
import Data.Char   ( toLower, toUpper, isLower, isUpper, isAlpha, isDigit )
import TypeInfo
import NativeInfo
import Env
import Data.Int
import Data.List
import Data.Bits
{- BEGIN_GHC_ONLY
import GlaExts
   END_GHC_ONLY -}
        
\end{code}

\begin{code}
mkId :: Name -> Name -> Maybe Name -> [Attribute] -> Id
mkId nm orig_nm md attrs = Id nm orig_nm md attrs

setIdModule :: Maybe Name -> Id -> Id
setIdModule md i = i{idModule=md}
\end{code}

\begin{code}
mkParam :: Name -> ParamDir -> Type -> Param
mkParam nm mode ty = Param (mkId nm nm Nothing noAttrs) mode ty ty False
\end{code}

\begin{code}
isHRESULT :: Result -> Bool
isHRESULT res = isHRESULTTy (resultOrigType res)
\end{code}


Lifting contents of modules and libraries to the top.

\begin{code}
flattenDecls :: [Decl] -> [Decl]
flattenDecls ls = flatDecls True inSeparateHaskellModule ls

flatDecls :: Bool
          -> (Decl -> Bool)
          -> [Decl]
          -> [Decl]
flatDecls _ _ [] = []
flatDecls isTopLev predic (Module i ds : rs) = 
   ( lift ++ (Module i don't_lift : flatDecls isTopLev predic rs) )
    where (lift, don't_lift) = partition predic ds 
flatDecls isTopLev predic (Library i ds : rs)
    | not isTopLev = Library i ds : flatDecls isTopLev predic rs
    | otherwise    = (Library i don't_lift) : lift ++ flatDecls False predic rs
    where 
     (lift, don't_lift) = partition (\ x -> predic x && not (isLibrary x)) ds 

     isLibrary (Library _ _) = True
     isLibrary _             = False
flatDecls isTopLev predic (d:ds) = d : flatDecls isTopLev predic ds

reallyFlattenDecls :: [Decl] -> [Decl]
reallyFlattenDecls ls = flatDecls True (\ _ -> True) ls

-- True => put the generated contents of IDL declaration in
--         separate Haskell module.
inSeparateHaskellModule :: Decl -> Bool
inSeparateHaskellModule Module{}        = True
inSeparateHaskellModule Library{}       = True
inSeparateHaskellModule DispInterface{} = optOneModulePerInterface
inSeparateHaskellModule Interface{}     = optOneModulePerInterface
inSeparateHaskellModule CoClass{}       = optOneModulePerInterface
inSeparateHaskellModule _               = False

{-
 If we're splitting up the input into multiple Haskell
 modules, minimise the scope of types/constants. i.e.,
 iff a type is only used within one interface (==module),
 float it into that interface.
-}
localiseTypes :: [Decl] -> [Decl]
localiseTypes ds 
 | not optOneModulePerInterface = ds
 | otherwise = --trace (show (map fst $ envToList moveEnv, envToList uniqueEnv, allTD_Names)) $
               mapMaybe moveTypes ds
  where
    moveTypes d@Typedef{declId=i}
      | isJust (lookupEnv uniqueEnv (idName i)) = Nothing
      | otherwise = Just d
    moveTypes (Module i ds1)  = 
        let ds' = mapMaybe moveTypes ds1 in
        case lookupEnv moveEnv (idName i) of
           Just ts -> Just (Module i (map (adjustMod (idName i) ts) (ts ++ ds')))
           _ -> Just (Module i ds')
    moveTypes (Library i ds1) = 
        let ds' = mapMaybe moveTypes ds1 in
        case lookupEnv moveEnv (idName i) of
           Just ts -> Just (Library i (map (adjustMod (idName i) ts) (ts ++ ds')))
           _ -> Just (Library i ds')
    moveTypes d@(Interface{declId=i}) =
       case lookupEnv moveEnv (idName i) of
          Just ts -> Just d{declDecls=map (adjustMod (idName i) ts) (ts ++ declDecls d)}
          _ -> Just d
    moveTypes d@(DispInterface{declId=i}) =
       case lookupEnv moveEnv (idName i) of
          Just ts -> Just d{declDecls=map (adjustMod (idName i) ts) (ts ++ declDecls d)}
          Nothing -> Just d
    moveTypes d = Just d
    
    adjustMod nm ls d = foldl adj d ls
      where
        adj acc Typedef{declId=i} = adjustModName (idName i) nm acc

    moveEnv :: Env String{-type name-}
                   [Decl] {- _declarations_ to be added-}
    moveEnv = addListToEnv_C (++)
                             newEnv
                             (mapMaybe findDecl (envToList uniqueEnv))
     where
       findDecl (nm,use) = 
         case find (withName nm) typesMoving of
           Just d  -> Just (use,[d])
           Nothing -> Nothing

       withName nm (Typedef{declId=i}) = idName i == nm || idOrigName i == nm
       withName _ _ = False

    typesMoving = filter isMoving allTDs
      where
       isMoving Typedef{declId=i} = isJust (lookupEnv uniqueEnv (idName i))
       isMoving _                 = False

    allTD_Names = map (idName.declId) allTDs

    allTDs = concatMap allTypedefs ds
     where
       allTypedefs d = 
          case d of
           Typedef{}     -> [d]
           Module _ ds1  -> concatMap allTypedefs ds1
           Library _ ds1 -> concatMap allTypedefs ds1
           Interface{declDecls=ds1}     -> concatMap allTypedefs ds1
           DispInterface{declDecls=ds1} -> concatMap allTypedefs ds1
           _            -> []


     -- environment mapping type names to the name of the _only_ 
     -- declaration that uses it.
    uniqueEnv :: Env String{-used type name-}
                     String{-where it is used-}
    uniqueEnv = mapMaybeEnv isOfInterest useEnv
      where
       isOfInterest _ ls  =
          case nub ls of
             [x] | x `notElem` allTD_Names -> Just x -- only interested if it was used at a
                                                     -- a non typedef-site (=> in an interface).
             _ -> Nothing
       isOfInterest _ _   = Nothing

    useEnv :: Env String   {- used type name -}
                  [String] {- names of decls that uses type -}
    useEnv = foldl addIt newEnv use_info
      where
        addIt env (_decl,d,us) = foldl (addUse d) env us
        addUse nm env use     = addToEnv_C (++) env use [nm]
           
    use_info = concatMap mkDeps ds
     where
       mkDeps d = 
         case d of 
           Module  _ ds1 -> concatMap mkDeps ds1
           Library _ ds1 -> concatMap mkDeps ds1
           _             -> [mkDeclDep d]


adjustModName :: String -> String -> Decl -> Decl
adjustModName nm newMod d = 
  case d of
    Typedef i ty oty  -> Typedef i (adjustType ty) (adjustType oty)
    Constant i ty oty e -> Constant i (adjustType ty) (adjustType oty) e
    Interface i flg inh ds -> Interface i flg (map adjustInherit inh) 
                                              (map (adjustModName nm newMod) ds)
    DispInterface i expF ps ds -> DispInterface i (fmap (adjustModName nm newMod) expF)
                                                  (map  (adjustModName nm newMod) ps)
                                                  (map  (adjustModName nm newMod) ds)
    Module i ds  -> Module i (map (adjustModName nm newMod) ds)
    Library i ds -> Module i (map (adjustModName nm newMod) ds)
    Method i cc res ps off -> Method i cc (adjustResult res) (map adjustParam ps) off
    Property i ty off si gi -> Property i (adjustType ty) off si gi
    _ -> d
 where
   adjustResult (Result ty oty) = Result (adjustType ty) (adjustType oty)
   adjustParam p@Param{paramType=ty,paramOrigType=oty} =
        p{paramType=adjustType ty,paramOrigType=adjustType oty}

   adjustInherit (qnm,i) = (adjustQName qnm, i)
   
   adjustQName qnm 
     | qName qnm == nm ||
       qOrigName qnm == nm = qnm{qModule=Just newMod,qDefModule=Just newMod}
     | otherwise = qnm

   adjustType ty = 
      case ty of
        Integer{} -> ty
        StablePtr{} -> ty
        FunTy cc res ps -> FunTy cc (adjustResult res) (map adjustParam ps)
        Float{} -> ty
        Char{}  -> ty
        WChar{} -> ty
        Bool{}  -> ty
        Octet{}  -> ty
        Any{}  -> ty
        Object{}  -> ty
        String t flg mb -> String (adjustType t) flg mb
        WString{} -> ty
        Fixed{}   -> ty
        Name n onm mbMod attrs mbTy mbTi 
          | n == nm   -> Name n onm (Just newMod) attrs (fmap adjustType mbTy) mbTi
          | otherwise -> Name n onm mbMod attrs (fmap adjustType mbTy) mbTi
        Struct i fs p -> Struct i (map adjustField fs) p
        Enum{}        -> ty
        Union i ty1 a b sws -> Union i (adjustType ty1) a b (map adjustSwitch sws)
        UnionNon i sws -> UnionNon i (map adjustSwitch sws)
        CUnion i fs p -> CUnion i (map adjustField fs) p
        Pointer p flg t -> Pointer p flg (adjustType t)
        Array t e     -> Array (adjustType t) e
        Void{}        -> ty
        Iface n md onm attrs flg ih 
          | n == nm || onm == nm -> Iface n (Just newMod) onm attrs flg (map adjustInherit ih)
          | otherwise -> Iface n md onm attrs flg (map adjustInherit ih)
        SafeArray t   -> SafeArray (adjustType t)

   adjustField f@Field{fieldType=ty,fieldOrigType=oty} = f{fieldType=adjustType ty,
                                                           fieldOrigType=adjustType oty
                                                           }
   adjustSwitch s@SwitchEmpty{} = s
   adjustSwitch s@Switch{switchType=ty,switchOrigType=oty} = s{switchType=adjustType ty,
                                                               switchOrigType=adjustType oty}

isMethod :: Decl -> Bool
isMethod Method{} = True
isMethod _        = False

isConst :: Decl -> Bool
isConst Constant{} = True
isConst _          = False

isMethodOrProp :: Decl -> Bool
isMethodOrProp Method{}   = True
isMethodOrProp Property{} = True
isMethodOrProp _          = False

isProperty :: Decl -> Bool
isProperty Property{}       = True
isProperty Method{declId=i} = 
  attrs `hasAttributeWithNames` ["propget", "propput", "propputref"]

 where
  attrs = idAttributes i
isProperty _ = False

isCoClass :: Decl -> Bool
isCoClass CoClass{} = True
isCoClass _         = False

\end{code}

\begin{code}
dummyMethod :: Decl
dummyMethod = Method (mkId "dummy" "dummy" Nothing [])
                     defaultCConv
                     (Result hresultTy hresultTy)
                     [{-no params-}]
                     Nothing
\end{code}

\begin{code}
getInterfaceIds :: Decl -> [Id]
getInterfaceIds decl = reverse (go [] decl)
 where
   go acc d = 
     case d of
        Interface{} -> foldl go ((declId d):acc) (declDecls d)
        Module _ ds -> foldl go acc ds
        DispInterface{} -> foldl go ((declId d):acc) (declDecls d)
        Library _ ds    -> foldl go acc ds
        _ -> acc

\end{code}

\begin{code}
findFieldTy :: [Field] -> Name -> Type
findFieldTy [] nm = error ("findFieldTy: " ++ nm) -- not supposed to happen.
findFieldTy (f : xs) nm
 | nm == idName (fieldId f) = fieldType f
 | otherwise                = findFieldTy xs nm

findFieldOrigTy :: [Field] -> Name -> Type
findFieldOrigTy [] nm = error ("findFieldOrigTy: " ++ nm) -- not supposed to happen.
findFieldOrigTy (f : xs) nm
 | nm == idName (fieldId f) = fieldOrigType f
 | otherwise                = findFieldOrigTy xs nm

findParam :: [Param] -> Name -> Param
findParam []       nm       = error ("findParam: " ++ nm) -- not supposed to happen.
findParam (p : xs) nm
 | nm == idName (paramId p) = p
 | otherwise                = findParam xs nm

findParamTy :: [Param] -> Name -> Type
findParamTy ps nm = paramType (findParam ps nm)

\end{code}

Fish out attributes attached to type:

\begin{code}
getTypeAttributes :: Type -> [Attribute]
getTypeAttributes ty = 
  case ty of 
    Name _ _ _ mb_as mb_ty _  -> fromMaybe [] mb_as ++
                                 fromMaybe [] (fmap getTypeAttributes mb_ty)
    Struct i _ _ -> idAttributes i
    Enum i _ _   -> idAttributes i
    Union i _ _ _ _ -> idAttributes i
    UnionNon i _    -> idAttributes i
    CUnion i _ _    -> idAttributes i
    Pointer _ _ t  -> getTypeAttributes t
    Iface _ _ _ as _ _ -> as
    _ -> []

\end{code}

The [hs_import(qualName)] is used on IDL module declarations to
have 'qualName' be imported in the corresponding Haskell module.
Normally only used in conjunction with hs_quote(..) declarations &
you don't want to write all the import declarations yourself.

\begin{code}
getHsImports :: Id -> [QualName]
getHsImports i = imp_attrs
 where
  imp_attrs = mapMaybe toHsImport attrs

  attrs = filterAttributes (idAttributes i) ["hs_import"]
  
  toHsImport :: Attribute -> Maybe QualName
  toHsImport a =
    case a of
      Attribute _ [ParamLit (StringLit s)] ->
         let qNm = toQualName s in
         case qModule qNm of 
           Nothing -> Nothing
           Just _  -> Just (qNm{qDefModule=qModule qNm})
      _ -> Nothing

\end{code}

\begin{code}
isStructTy :: Type -> Bool
isStructTy Struct{} = True
isStructTy (Name _ _ _ _ (Just t) _) = isStructTy t
isStructTy _        = False

isEnumTy   :: Type -> Bool
isEnumTy Enum{} = True
isEnumTy (Name _ _ _ _ (Just t) _) = isEnumTy t
isEnumTy _      = False

isPointerTy :: Type -> Bool
isPointerTy Pointer{} = True
isPointerTy _         = False

isVoidPointerTy :: Type -> Bool
isVoidPointerTy (Pointer _ _ Void) = True
isVoidPointerTy (Name _ _ _ _ (Just t) _) = isVoidPointerTy t
isVoidPointerTy _                = False

{-
 keepValueAsPointer is used to determine whether
 we should unmarshall a parameter/result coming
 back from a method.

 The policy is as follows:
   - pointer to a struct or union is held back
     as an external value.
   - pointers to basic types, strings,
     arrays are unmarshalled into the Haskell heap.
-}
keepValueAsPointer :: Type -> Bool
keepValueAsPointer ty
  | optDeepMarshall = False
  | otherwise       =
     case ty of
        Pointer _ _ (Name _ _ _ _ Nothing  _) -> True
        Pointer _ _ (Name _ _ _ _ (Just t) _) -> keepValueAsPointer t
        Pointer _ _ Struct{}       -> True
        Pointer _ _ Union{}        -> True
        Pointer _ _ UnionNon{}     -> True
        Pointer _ _ CUnion{}       -> True
        _                          -> False

isArrayTy :: Type -> Bool
isArrayTy Array{}     = True
isArrayTy (Name _ _ _ _ (Just t) _) = isArrayTy t
isArrayTy _           = False

isSafeArrayTy :: Type -> Bool
isSafeArrayTy SafeArray{}  = True
isSafeArrayTy (Name _ _ _ _ (Just t) _) = isSafeArrayTy t
isSafeArrayTy _           = False

isOpenArrayTy :: Type -> Bool 
isOpenArrayTy (Array _ []) = True
isOpenArrayTy _            = False

isBoolTy :: Type -> Bool
isBoolTy Bool  = True
isBoolTy (Name _ _ _ _ (Just t) _) = isBoolTy t
isBoolTy _     = False

isFunTy :: Type -> Bool
isFunTy FunTy{} = True
isFunTy (Name _ _ _ _ (Just t) _) = isFunTy t
isFunTy _              = False

isVoidTy :: Type -> Bool
isVoidTy Void  = True
isVoidTy (Name _ _ _ _ (Just t) _) = isVoidTy t
isVoidTy _     = False

isPointerOrArrayTy :: Type -> Bool
isPointerOrArrayTy ty = isPointerTy ty || isArrayTy ty

isPtrPointerTy  :: Type -> Bool
isPtrPointerTy (Pointer Ptr _ _) = True
isPtrPointerTy _                 = False

isRefPointerTy :: Type -> Bool
isRefPointerTy (Pointer Ref _ _) = True
isRefPointerTy _                 = False

mkRefPointer :: Type -> Type
mkRefPointer (Pointer _ expl t) = Pointer Ref expl t
mkRefPointer (Name nm onm md a (Just ty) mb_ti) = Name nm onm md a (Just (mkRefPointer ty)) mb_ti
mkRefPointer t = t

-- convert the (innermost) void* into an interface pointer.
-- (used when a [iid_is()] is in effect.
rawPointerToIP :: Type -> Type
rawPointerToIP (Pointer _ _ Void)  = Pointer Ref True iUnknownTy
rawPointerToIP (Pointer pt expl t) = Pointer pt expl (rawPointerToIP t)
rawPointerToIP (Name nm onm md a (Just ty) mb_ti) =
   Name nm onm md a (Just (rawPointerToIP ty)) mb_ti
rawPointerToIP t = t

isUniquePointerTy :: Type -> Bool
isUniquePointerTy (Pointer Unique _ _) = True
isUniquePointerTy _                    = False

isStringTy :: Type -> Bool
isStringTy String{}  = True
isStringTy WString{} = True
isStringTy (Name _ _ _ _ _ (Just ti)) = qName (haskell_type ti) == stringName
isStringTy _         = False

isSeqTy :: Type -> Bool
isSeqTy Sequence{} = True
isSeqTy (Name _ _ _ _ (Just t) _) = isSeqTy t
isSeqTy _          = False

isAnyTy :: Type -> Bool
isAnyTy Any = True
isAnyTy _   = False

isObjectTy :: Type -> Bool
isObjectTy Object                  = True
isObjectTy (Name _ _ _ _ (Just t) _) = isObjectTy t
isObjectTy _                       = False

intTy :: Type
intTy = Integer Natural True{-Signed-}

addrTy :: Type
addrTy = Pointer Ptr True Void

charTy :: Type
charTy = Char False{-Unsigned-}

wCharTy :: Type
wCharTy = WChar

boolTy :: Type
boolTy = Bool

-- built-in Automation type
variantBoolTy :: Type
variantBoolTy = Name "VARIANT_BOOL" "VARIANT_BOOL" Nothing Nothing Nothing Nothing

variantTy :: Type
variantTy = Name "VARIANT" "VARIANT" autoLib Nothing Nothing (Just variant_ti)

int32Ty :: Type
int32Ty = Integer Long True{-Signed-}

int64Ty :: Type
int64Ty = Integer LongLong True{-Signed-}

word64Ty :: Type
word64Ty = Integer LongLong False{-unsigned-}

word32Ty :: Type
word32Ty = Integer Long False{-Unsigned-}

word16Ty :: Type
word16Ty = Integer Short False{-Unsigned-}

int16Ty :: Type
int16Ty = Integer Short True{-Signed-}

voidTy :: Type
voidTy = Void

currencyTy :: Type
currencyTy = Name  "CURRENCY" "CURRENCY" autoLib Nothing Nothing mb_currency_ti

dateTy :: Type
dateTy = Name "DATE" "DATE" autoLib Nothing (Just int64Ty) mb_date_ti

fileTimeTy :: Type
fileTimeTy = Name "FILETIME" "FILETIME" Nothing Nothing Nothing Nothing

safeArrayTy :: Type -> Type
safeArrayTy t = SafeArray t

shortTy :: Type
shortTy = Integer Short True{-signed-}

byteTy :: Type
byteTy = Char False

floatTy :: Type
floatTy = Float Short

doubleTy :: Type
doubleTy = Float Long

stringTy :: Type
stringTy = String (Char False) False Nothing

wStringTy :: Type
wStringTy = WString False Nothing

bstrTy :: Type
bstrTy = Name "BSTR" "BSTR" comLib Nothing Nothing (Just bstr_ti)

iUnknownTy :: Type
iUnknownTy = Iface "IUnknown" comLib "IUnknown" [] False []

iDispatchTy :: Type
iDispatchTy = Iface "IDispatch" autoLib "IDispatch" [] True [(iUnknown,3)]

hresultTy :: Type
hresultTy = Name "HRESULT" "HRESULT" comLib Nothing (Just int32Ty) Nothing

isHRESULTTy :: Type -> Bool
isHRESULTTy (Name "HRESULT" _ _ _ _ _) = True
isHRESULTTy _                          = False

guidTy :: Type
guidTy = Name "GUID" "GUID" comLib Nothing Nothing Nothing

-- weird name, funTy is already taken, I'm afraid.
tyFun :: CallConv -> Result -> [Param] -> Type
tyFun = FunTy

\end{code}

\begin{code}
mkPtrPointer :: Type -> Type
mkPtrPointer (Pointer _ _ t) = Pointer Ptr True t
mkPtrPointer (Array t [])    = Pointer Ptr True t
mkPtrPointer t               = t

removePtr :: Type -> Type
removePtr t@(Pointer _ _ Void) = t
removePtr (Pointer _ _ t)      = t
removePtr t                    = t

removePtrAndArray :: Type -> Type
removePtrAndArray t@(Pointer _ _ Void) = t
removePtrAndArray (Pointer _ _ t)    = t
removePtrAndArray (Array t _)        = t
removePtrAndArray t                  = t

removePtrAll :: Type -> Type
removePtrAll t@(Pointer _ _ Void) = t
removePtrAll (Pointer _ _ t)      = t
removePtrAll (Array t _)          = t
removePtrAll (String t _ _)       = t
removePtrAll WString{}            = WChar
removePtrAll t                    = t

removePtrs :: Type -> Type
removePtrs (Pointer _ _ t) = removePtrs t
removePtrs t               = t

removeNames :: Type -> Type
removeNames t@(Name _ _ _ _ _ (Just _)) = t
removeNames (Name _ _ _ _ (Just t) _) | not (isConstructedTy t) = removeNames t
removeNames t = t

nukeNames :: Type -> Type
nukeNames t@(Name _ _ _ _ _ (Just _)) = t
nukeNames (Name _ _ _ _ (Just t) _)  = nukeNames t
nukeNames t = t

pushPointerType :: PointerType -> Type -> Type
pushPointerType pt (Pointer _ expl ty) = Pointer pt expl ty
pushPointerType _  ty        = ty

\end{code}

\begin{code}
hasIgnoreAttribute :: Id -> Bool
hasIgnoreAttribute i = idAttributes i `hasAttributeWithName` "ignore"

childAttributes :: [Attribute] -> [Attribute]
childAttributes as = filter (not.notAggregatableAttribute) as

notAggregatableAttribute :: Attribute -> Bool
notAggregatableAttribute (AttrMode _) = False
notAggregatableAttribute (AttrDependent _ _) = False
notAggregatableAttribute (Attribute nm _) = nm `elem` junk_list
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

\end{code}

\begin{code}
isConstructedTy :: Type -> Bool
isConstructedTy Struct{}   = True
isConstructedTy Enum{}     = True
isConstructedTy Union{}    = True
isConstructedTy UnionNon{} = True
isConstructedTy CUnion{}   = True
isConstructedTy FunTy{}    = True
isConstructedTy _          = False

{-
 only used on constructed types.
-}
isCompleteTy :: Type -> Bool
isCompleteTy (Struct _ ls _)    = notNull ls
isCompleteTy (Enum _ _ ls)      = notNull ls
isCompleteTy (Union _ _ _ _ ls) = notNull ls
isCompleteTy (UnionNon _ ls )   = notNull ls
isCompleteTy (CUnion _ ls _)    = notNull ls
isCompleteTy (Name _ _ _ _ (Just t) _) = isCompleteTy t
isCompleteTy FunTy{}            = True
isCompleteTy _                  = False

isReferenceTy :: Type -> Bool
isReferenceTy = not.isCompleteTy
\end{code}

What is a simple IDL type? This predicate is currently
only used by the code that implements the translation
of type(def) declarations, determining whether to use
a type synonym or data declaration to represent the
Haskell repr of the IDL type.

\begin{code}
isSimpleTy :: Type -> Bool
isSimpleTy ty =
 case ty of
   Sequence{}        -> False -- for now.
   Fixed{}           -> False
   Struct{}          -> False
   Enum{}            -> False
   Union{}           -> False
   UnionNon{}        -> False
   CUnion{}          -> False
   Array{}           -> False
   SafeArray{}       -> False
   Pointer{}         -> False
   String {}         -> False
   WString{}         -> False
   Bool              -> False
   FunTy{}           -> False
   Name _ _ _ _ _ (Just _) -> False -- bit of a sweeping statement.
   Name _ _ _ _ Nothing  _ -> False
   Name _ _ _ _ (Just t) _ -> isSimpleTy t
   Iface{}           -> not optHaskellToC
   Integer LongLong _ -> not optLongLongIsInteger
   _                 -> True

isIntegerTy :: Type -> Bool
isIntegerTy (Integer LongLong _) = optLongLongIsInteger
isIntegerTy _                    = False

isSynTy :: Type -> Bool
isSynTy Name{} = True
isSynTy _      = False

isAbstractTy :: Type -> Bool
isAbstractTy Iface{} = optHaskellToC 
isAbstractTy (Pointer _ _ Iface{}) = optHaskellToC
isAbstractTy _           = False

isAbstractFinalTy :: Type -> Bool
isAbstractFinalTy (Iface _ _ _ attrs _ _)
  = optHaskellToC && attrs `hasAttributeWithName` "finaliser"
isAbstractFinalTy (Pointer _ _ (Iface _ _ _ attrs _ _)) 
  = optHaskellToC && attrs `hasAttributeWithName` "finaliser"
isAbstractFinalTy _
  = False

isNonEncUnionTy :: Type -> Bool
isNonEncUnionTy t = have_a_look t
  where
   have_a_look ty =
     case ty of
       UnionNon{}      -> True
       CUnion{}        -> True
       Name _ _ _ _ (Just tt) _ -> have_a_look tt
       _                      -> False

{-
 Invariant: always called on a ty for which 'isNonEncUnionTy'
            returned True.
-}
getNonEncUnionTy :: Type -> Type
getNonEncUnionTy t = look_around t
 where
   look_around ty =
     case ty of
       UnionNon{}      -> ty
       CUnion{}        -> ty
       Name _ _ _ _ (Just tt) _ -> look_around tt
       _                      ->
          error "getNonEncUnionTy: you've reached unreachable code (..oops!)"

isUnionTy :: Type -> Bool
isUnionTy t = have_a_look t
  where
   have_a_look ty =
     case ty of
       Union{}      -> True
       UnionNon{}   -> True
       CUnion{}     -> True
       Name _ _ _ _ (Just tt) _ -> have_a_look tt
       _                      -> False


-- peer through names and pointers to see if there's an interface hiding here somewhere.
isIfaceTy :: Type -> Bool
isIfaceTy (Name _ _ _ _ (Just t) _) = isIfaceTy t
isIfaceTy (Pointer _ _ t)         = isIfaceTy t
isIfaceTy Iface{}                 = True
isIfaceTy _                       = False

isIUnknownTy :: Type -> Bool
isIUnknownTy (Name _ _ _ _ (Just t) _) = isIUnknownTy t
isIUnknownTy (Pointer _ _ t)         = isIUnknownTy t
isIUnknownTy (Iface "IUnknown" _ _ _ _ _) = True
isIUnknownTy (Iface _ _ _ _ flg _) = not flg
isIUnknownTy _                     = False

isIfacePtr :: Type -> Bool
isIfacePtr (Name _ _ _ _ (Just t) _) = isIfacePtr t
isIfacePtr (Pointer _ _ (Iface{})) = True
isIfacePtr (Pointer _ _ t) = 
  case (removeNames t) of
    Iface{} -> True
    _       -> False
isIfacePtr _                       = False


getIfaceTy :: Type -> Type
getIfaceTy (Name _ _ _ _ (Just t) _) = getIfaceTy t
getIfaceTy (Pointer _ _ t)         = getIfaceTy t
getIfaceTy t@Iface{}               = t
getIfaceTy _                       = error "getIfaceTy: should never happen"

isVariantTy :: Type -> Bool
isVariantTy (Name "VARIANT" _ _ _ _ _) = True
isVariantTy (Name _ _ _ _ (Just t) _)  = isVariantTy t
isVariantTy _ = False

\end{code}

\begin{code}
getTyTag :: Type -> Id
getTyTag (Enum i _ _)       = i
getTyTag (Struct i _ _)     = i
getTyTag (Union i _ _ _ _)  = i
getTyTag (UnionNon i _)     = i
getTyTag (CUnion i _ _)     = i
getTyTag (Name n onm md attrs _ _) = mkId n onm md (fromMaybe [] attrs)
getTyTag (Pointer _ _ t)    = getTyTag t
getTyTag t                  = error ("getTyTag: not supposed to be given this type as arg!" ++ showCore (ppType t))
\end{code}

\begin{code}
findFreeVars :: Expr -> [Name]
findFreeVars (Var v)          = [v]
findFreeVars (Lit _)          = []
findFreeVars (Sizeof _)       = []
findFreeVars (Cast _ e)       = findFreeVars e
findFreeVars (Unary _ e)      = findFreeVars e
findFreeVars (Binary _ e1 e2) = findFreeVars e1 ++ findFreeVars e2
findFreeVars (Cond e1 e2 e3)  = findFreeVars e1 ++ findFreeVars e2 ++ findFreeVars e3
\end{code}

A very simplistic expression solver, the variable we're solving for
is given as first argument.

\begin{code}
solve :: Name -> Expr -> Expr -> Expr
solve nm lhs (Cast _ e)     = solve nm lhs e
solve nm lhs (Unary op rhs) = solve nm (Unary op lhs) rhs
solve nm lhs (Binary op e1 e2) 
 | contains nm e1 = solve nm (Binary op' lhs e2) e1
 | isCommutative op && contains nm e2 = solve nm (Binary op' lhs e1) e2
 | contains nm e2 = solve nm (Binary op e1 lhs) e2
   where op' = complementOp op
solve _ lhs _  = lhs

complementOp :: BinaryOp -> BinaryOp
complementOp Add = Sub
complementOp Sub = Add
complementOp Mul = Div
complementOp Mod = Div
complementOp Div = Mul
complementOp Eq  = Ne
complementOp Ne  = Eq
complementOp And = Or
complementOp Or  = And
complementOp (Shift L) = Shift R
complementOp (Shift R) = Shift L
complementOp Gt  = Lt
complementOp Ge  = Le
complementOp Le  = Ge
complementOp Lt  = Gt
complementOp LogOr  = LogAnd
complementOp LogAnd = LogOr
complementOp Xor = Xor 

isCommutative :: BinaryOp -> Bool
isCommutative Add = True
isCommutative Mul = True
isCommutative _   = False

contains :: Name -> Expr -> Bool
contains nm (Var v) = v == nm
contains nm (Cast _ e) = contains nm e
contains nm (Unary _ e) = contains nm e
contains nm (Binary _ e1 e2) = contains nm e1 || contains nm e2
contains _  _ = False

plusOne :: Expr -> Expr
plusOne e = Binary Add e (Lit (iLit (1::Int)))

minusOne :: Expr -> Expr
minusOne e = Binary Sub e (Lit (iLit (1::Int)))

add :: Expr -> Expr -> Expr
add e1 e2 = Binary Add e1 e2
\end{code}

\begin{code}

evalExpr :: Expr -> Integer
evalExpr e = 
 case e of
    {-
     In order to be correct, evaluation needs to 
     be type driven
    -}
   Binary bop e1 e2 -> 
      let 
        i1 = evalExpr e1
        i2 = evalExpr e2
      in
      case bop of
        Add -> i1 + i2
        Sub -> i1 - i2
        Div -> i1 `div` i2
        Mod -> i1 `mod` i2
        Mul -> i1 * i2
        Xor -> toInteger (fromInteger i1 `xor` ((fromInteger i2)::Int32)) -- an Bits instance for Integers, anyone?
        Or  -> toInteger (fromInteger i1 .|.   ((fromInteger i2)::Int32))
        And -> toInteger (fromInteger i1 .&.   ((fromInteger i2)::Int32))
        Shift L -> toInteger (shiftL ((fromInteger i1)::Int32) (fromIntegral i2))
        Shift R -> toInteger (shiftR ((fromInteger i1)::Int32) (fromIntegral i2))
        LogAnd  -> if i1 /= 0 && i2 /=0 then 1 else 0
        LogOr   -> if i1 /= 0 || i2 /=0 then 1 else 0
        Gt  -> if i1 > i2 then 1 else 0
        Ge  -> if i1 >= i2 then 1 else 0
        Eq  -> if i1 == i2 then 1 else 0
        Le  -> if i1 <= i2 then 1 else 0
        Lt  -> if i1 <  i2 then 1 else 0
        Ne  -> if i1 /= i2 then 1 else 0
        
   Cond e1 e2 e3 ->
      let
       i1 = evalExpr e1
       i2 = evalExpr e2
       i3 = evalExpr e3
      in
      if i1 == 0 then
         i2
      else
         i3
   Unary  uop e1 -> 
      let
       i1 = evalExpr e1
      in
      case uop of
        Minus  -> -i1
        Plus   -> i1
        Not    -> if i1==0 then 1 else 0
        Negate -> toInteger ((complement (fromInteger i1)) :: Int32)
        Deref  -> i1
   Var nm       -> error ("evalExpr: cannot handle free variable " ++ show nm)
   Lit (IntegerLit (ILit _ i))  -> i
   Cast _ e1     -> evalExpr e1
   Sizeof t     -> fromIntegral (sizeofType t)
   _ -> error ("CoreUtils.evalExpr: Unmatched case for: " ++ showCore (ppExpr e))
 
\end{code}

Expand out occurrences of @(Var x)@ and @(Sizeof t)@:

\begin{code}

simpRedExpr  :: Env String (Either Int32 Expr)
             -> Type
             -> Expr
             -> Expr
simpRedExpr env ty ex = 
   case (simplifyExpr env ex) of
     e@(Lit _) -> e
     e         -> 
         case ty of
           Integer _ _ -> Lit (iLit (evalExpr e))  -- reduce 'int'y things.
           _           -> e

simplifyExpr :: Env String (Either Int32 Expr)
             -> Expr
             -> Expr
simplifyExpr val_env ex = 
  case ex of
    Binary bop e1 e2 -> Binary bop (simplifyExpr val_env e1)
                                   (simplifyExpr val_env e2)
    Cond e1 e2 e3 -> Cond (simplifyExpr val_env e1)
                          (simplifyExpr val_env e2)
                          (simplifyExpr val_env e3)
    Unary op e -> Unary op (simplifyExpr val_env e)
    Var nm     -> 
      case lookupEnv val_env nm of
        Nothing        -> Var nm -- good luck!
        Just (Left x)  -> Lit (iLit (toInteger x))
        Just (Right e) -> e
    Lit l      -> Lit l
      -- notice that casting would have been trickier
      -- to deal with if the expression language permitted
      -- sizeof(x), where x is an expression, since 
      -- sizeof((t)x) == sizeof(t) 
    Cast t e   -> Cast t (simplifyExpr val_env e)
    Sizeof t   -> Lit (iLit (toInteger (sizeofType t)))

\end{code}

@findDependents@ computes the attribute dependencies an identifier has
on others, i.e., in DCE IDL, it is possible to express dependencies
between field members and parameters. The dependencies encode what/how
much of an array/pointer value should be marshalled between client
and server. The attributes are:

 first_is(params)   -- non-neg (array) index(es) of first element
 last_is(params)    -- (array) index(es) of last element to be transmitted/received.
 length_is(params)  -- number of elements of array that are to be transmitted/received.
 max_is(params)     -- specifies upper bound for valid array indexes.
 min_is(params)     -- lower bound (normally zero.)
 size_is(params)    -- the allocation size of the array.

where params is a list of expressions (arrays can be multi-dimensional.), some
of which might be empty.

@findDependents@ returns a list of identifiers, with each id paired with a list
containing its dependencies.

\begin{code}

type DependInfo = [(Id,[Dependent])]

data DepVal 
 = DepNone  -- empty/unspecified (for this dimension.)
 | DepVal (Maybe Name) -- a dependent value might contain at most one
                       -- free variable which refers to another field/param.
          Expr         -- 
-- for debugging purps. only
   deriving (Show)

data Dependent = Dep DepReason [DepVal] -- a list, since the id might be multi-dimensional.
                  deriving ( Show )

{- BEGIN_GHC_ONLY
-- For Hugs users, this instance will conflict with the one in CoreIDL.
instance Show Expr where
  show x = showCore (ppExpr x)
  END_GHC_ONLY -}
\end{code}

\begin{code}
findDependents :: [Id] -> DependInfo
findDependents ls
  | optNoDependentArgs = []
  | otherwise          = map (\ i -> (i, findDep i)) ls
  where
   findDep i = mapMaybe ((mapMb attrToDependent).isDependentAttribute)
                        (idAttributes i)

attrToDependent :: Attribute -> Dependent
attrToDependent (AttrDependent reason args) = Dep reason (map toDepVal args)
  where
   toDepVal (ParamLit  l@(IntegerLit _)) = DepVal Nothing (Lit l)  -- the only legal lit
   toDepVal (ParamVar v)                 = DepVal (Just v) (Var v)
   toDepVal (ParamExpr (Var v))          = DepVal (Just v) (Var v)
   toDepVal ParamVoid                    = DepNone
   toDepVal (ParamExpr e)                =
        -- Assume the expression has been checked
        -- as having at most one free variable.
        case (findFreeVars e) of
         []    -> DepVal Nothing  e
         (f:_) -> DepVal (Just f) e
   toDepVal (ParamPtr p)                 =
       case (toDepVal p) of
         DepVal fv e  -> DepVal fv (Unary Deref e)
         d            -> d
   toDepVal _ = DepNone

attrToDependent _ = error "attrToDependent"
\end{code}

\begin{code}
computeArrayConstraints :: Bool
                        -> [Dependent]
                        -> ([DepVal], [DepVal], [DepVal])
computeArrayConstraints unmarshalling deps 
 | unmarshalling = (trans_start_posns, trans_end_posns, trans_lengths)
 | otherwise     = (trans_start_posns, trans_end_posns, alloc_sizes)
 where

    -- multiple traversals of the dependencies list here, but lists
    -- are likely to be very short, so merging the passes into
    -- one is likely to cost more than it saves.

--not supported, and of little use 
--when we're representing lists as arrays:
-- mins    = filter isMinIs deps
   maxs    = mapHead (\ (Dep _ ls) -> ls) $ filter isMaxIs deps
   firsts  = mapHead (\ (Dep _ ls) -> ls) $ filter isFirstIs deps
   lasts   = mapHead (\ (Dep _ ls) -> ls) $ filter isLastIs deps
   lengths = mapHead (\ (Dep _ ls) -> ls) $ filter isLengthIs deps
   sizes   = mapHead (\ (Dep _ ls) -> ls) $ filter isSizeIs deps

   dimensions          = maximum (map length [maxs,firsts,lasts,lengths,sizes])

   alloc_sizes         = zipWith  genUpperBound (fillInDims maxs) (fillInDims sizes)
   trans_start_posns   = map      genLowerBound (fillInDims firsts)
   trans_end_posns     = zipWith3 genEnd        (fillInDims lasts)   trans_lengths trans_start_posns
   trans_lengths       = zipWith  genLength     (fillInDims lengths) alloc_sizes

    -- generate the array indexes from which to start transmitting elements.
    -- If none given for a dimension, start from zero.
   genLowerBound DepNone = DepVal Nothing (Lit (iLit (0::Int)))
   genLowerBound d       = d

    -- generate expressions holding the lengths of transmittable ranges. The second
    -- argument will not have any DepNone's in it.
   genLength DepNone  d = d
   genLength d        _ = d

   {- The upper bound of what is to be transmitted is determined by
      preferably looking at the last_is() attribute. If not present, we
      derive its value as follows:   last_is = length + first - 1
   -}
   genEnd DepNone  (DepVal fv l) (DepVal _ f)
     = DepVal fv (minusOne (add l f))  -- BUG: we're dropping a free variable here!
   genEnd d        _             _ = d

   {- The upper allocation boundary is determined by looking at either the max_is()
      or size_is(). max_is 
   -}
   genUpperBound DepNone      DepNone
     = DepNone --error "genUpperBound: max_is nor size_is value not specified (need one of them.)"
   genUpperBound (DepVal _ _) (DepVal _ _)
     = error "genUpperBound: size_is and max_is both given for a dimension (not legal.)"
   genUpperBound DepNone       d       = d
   genUpperBound (DepVal fv e) DepNone = DepVal fv (plusOne e)

   --mapHead :: (a -> [b]) -> [a] -> [b]
   mapHead _ []    = []
   mapHead f (x:_) = f x

   --fillInDims :: [a] -> [a]
   fillInDims ls = go dimensions ls
     where
      go 0 _      = []
      go n (x:xs) = x:go (n-1) xs
      go n []     = DepNone:go (n-1) []

\end{code}


Predicates over dependency items and lists.

\begin{code}
isLengthIs, isSizeIs :: Dependent -> Bool
isSizeIs   (Dep SizeIs   _) = True
isSizeIs   _                = False
isLengthIs (Dep LengthIs _) = True
isLengthIs _                = False

isMaxIs, isMinIs :: Dependent -> Bool
isMinIs    (Dep MinIs _)    = True
isMinIs    _                = False
isMaxIs    (Dep MaxIs _)    = True
isMaxIs    _                = False

isFirstIs, isLastIs :: Dependent -> Bool
isFirstIs  (Dep FirstIs _)  = True
isFirstIs  _                = False
isLastIs   (Dep LastIs  _)  = True
isLastIs   _                = False

sizeOrLength :: Dependent -> Bool
sizeOrLength d = isSizeIs d || isLengthIs d

minOrFirst :: Dependent -> Bool
minOrFirst d = isMinIs d || isFirstIs d

maxOrLast :: Dependent -> Bool
maxOrLast d = isMaxIs d || isLastIs d

isSwitchIs :: Dependent -> Bool
isSwitchIs (Dep SwitchIs _) = True
isSwitchIs _                = False

lookupDepender :: DependInfo -> Id -> Maybe [Dependent]
lookupDepender ls i = 
 case (filter (\ (i1, _) -> idName i1 == nm) ls) of
   ((_,ls2):_) -> Just ls2
   _           -> Nothing
 where
  nm = idName i

isDepender :: DependInfo -> Id -> Bool
isDepender ls i = any isDependerElem ls
  where
   nm = idName i
 
   isDependerElem (x,deps) = idName x == nm  && 
                             notNull deps

isSwitchDepender :: DependInfo -> Id -> Bool
isSwitchDepender ls i = any isElem ls
  where
   nm = idName i
 
   isElem (x,deps) = idName x == nm  && any (isSwitchIs) deps

isDependee :: DependInfo -> Id -> Bool
isDependee ls i = any (isDependeeElem nm) dvals
 where
   -- join up all the DepVals
  dvals = concatMap ((concatMap (\ (Dep _ ls1) -> ls1)).snd) ls

  nm  = idName i

isDependeeElem :: String -> DepVal -> Bool
isDependeeElem _  DepNone            = False
isDependeeElem _  (DepVal Nothing  _) = False
isDependeeElem nm (DepVal (Just x) _) = x == nm

isSwitchDependee :: DependInfo -> Id -> Bool
isSwitchDependee lss i = any isSwitchDependeeElem ls
 where
  ls = concatMap (snd) lss

  nm  = idName i

  isSwitchDependeeElem (Dep SwitchIs ls1) = any (isDependeeElem nm) ls1
  isSwitchDependeeElem _                  = False

isNotSwitchDependee :: DependInfo -> Id -> Bool
isNotSwitchDependee lss i = any isNotSwitchDependeeElem ls
 where
  ls = concatMap (snd) lss

  nm  = idName i

  isNotSwitchDependeeElem (Dep r ls1) | r /= SwitchIs = any (isDependeeElem nm) ls1
  isNotSwitchDependeeElem _                           = False

hasNonConstantExprs :: Dependent -> Bool
hasNonConstantExprs (Dep _ ls) = any isNon ls
  where
   isNon (DepVal (Just _) _) = True
   isNon _                   = False
\end{code}

Translating an IDL type name into a valid Haskell variable name.

\begin{code}
mkHaskellVarName :: Name -> Name
mkHaskellVarName nm 
  | optClassicNameMangling = toHask (casifyName nm)
  | otherwise              = toHask nm
 where
  toHask [] = "anon" -- shouldn't happen!
--  toHask ('_':xs) = toHask xs -- drop leading underscores.
  toHask (x:xs) | not (isAlpha x) = toHask xs  -- anything non-alphabetic, really.
  toHask ls@(x:xs) 
      | isUpper x = toLower x : map (subst '$' '_') xs
      | otherwise = map (subst '$' '_') ls

  subst x y ch | x == ch   = y
               | otherwise = ch

\end{code}

Translating an IDL type name into a valid Haskell type
constructor name.

\begin{code}
mkHaskellTyConName :: Name -> Name
mkHaskellTyConName nm 
  | optClassicNameMangling = casifyName nm
  | otherwise              = toHask nm
 where
  toHask [] = "Anon" -- shouldn't happen!
  toHask (x:xs) | not (isAlpha x) = toHask xs  -- it's a non-starter with anything 
                                               -- non-alphabetic at the front (i.e.,
                                               -- don't want '_' there).
  toHask ls@(x:xs) 
        | isLower x = toUpper x : map (subst '$' '_') xs
        | otherwise = map (subst '$' '_') ls

  subst x y ch | x == ch   = y
               | otherwise = ch

-- From: THIS_IS_A_SILLY_ID, ThisIs_ANOTHER_Silly_ID
-- to: ThisIsASillyId, ThisIsAnotherSillyId
casifyName :: String -> String
casifyName nm = concatMap caseWord (split '_' nm)

caseWord :: String -> String
caseWord []     = []
caseWord (c:cs) = toUpper c : cs'
  where
   cs'
    | all (\ ch -> isUpper ch || isDigit ch) cs = map toLower cs
    | otherwise      = cs

\end{code}

Until GHC supports the new FFI declarations, the IDL compiler will
emit _casm_s that performs the actual invocation of COM methods.
(but, more importantly, this is also used by the Hugs backend.)

\begin{code}
toCType :: Type -> Either String -- (primitive / FFI supported) C type
                          String -- other; need C impedance matching code.
toCType ty = 
 case ty of
   Char signed
     | signed    -> Left "signed char"
     | otherwise -> Left "unsigned char"
   WChar -> Left "wchar_t"
   Bool  -> Left "int" -- contentious
   Octet    -> Left "unsigned char"
   Integer LongLong signed
     | signed     -> Left "int64"
     | otherwise  -> Left "uint64"
   Integer Natural True -> Left "int"
   Integer sz signed
     | signed    -> Left (sizeToString sz)
     | otherwise -> Left ("unsigned " ++ sizeToString sz)
   StablePtr -> Left "unsigned long"
   Float sz -> 
      case sz of
       Short    -> Left "float"
       Long     -> Left "double"
       LongLong -> Left "long double"
       Natural  -> Left "float"

   String{}       -> Left "char*"
   WString{}      -> Left "void*"
   Sequence{}     -> Left "void*"
   Enum{}         -> Left "int"

   Struct _ [f] _ | isSimpleTy (fieldType f) -> toCType (fieldType f)
   Struct i _ _    -> Right ("struct " ++ idName i)
   Union i _ _ _ _ -> Right ("struct " ++ idName i) -- an encapsulated union is a struct in C.
   UnionNon i _    ->  Right ("union "  ++ idName i)
   CUnion i _ _    -> Right ("union " ++ idName i)

   Name _ onm _ _ _  (Just ti) | not (is_pointed ti) -> Left (c_type ti)
                               | otherwise -> Right onm
   Name _ onm _ _ Nothing  _   -> Right onm
   Name _ onm _ _ (Just t) _  
     | isConstructedTy t   -> if isEnumTy ty || isFunTy ty then toCType t else Right onm
     | otherwise           -> toCType t
   Pointer _ _ (Name _ _ _ (Just as) _ _)   -> 
       case findAttribute "ctype" as of
         Just (Attribute _ [ParamLit (StringLit t)]) -> Left t
         _ -> Left "void*"
   Pointer _ _ (t@Iface{})   ->
         case toCType t of
             Left l 
                 -- confusingly, IA and IA* are the same thing when in 'C mode',
                 -- so qualify the addition of an indirection accordingly.
                 -- Ditto on the 'COM' side - if we end up with just 'IA' (which
                 -- we shouldn't), treat this as IA*.
                 -- 
                     -> Left l
             Right x -> Right x
   Pointer{}    -> Left "void*"
   Array{}      -> Left "void*"
   Void         -> Left "void"
   SafeArray{}  -> Left "void*"
   Iface _ _ _ as _ _ ->
       case findAttribute "ctype" as of
         Just (Attribute _ [ParamLit (StringLit t)]) -> Left t
         _ -> Left "void*"
   FunTy{}      -> Left "void*"
   Object       -> Left "void*"
   Any          -> Left "void*"
   _            -> error ("toCType: unhandled " ++ showCore (ppType ty))
 where
   sizeToString Short    = "short"
   sizeToString Long     = "long"
   sizeToString Natural  = "long"
   sizeToString LongLong = "int64"
\end{code}


\begin{code}
mkIfaceTypeName :: Name -> Name
mkIfaceTypeName ('_':nm) = mkIfaceTypeName nm
mkIfaceTypeName nm       = map (subst '$' '_') nm
 where
   subst x y ch | x == ch   = y
                | otherwise = ch

\end{code}

From a set of parameter/result attributes, figure out the pointer
type. Boolean flag determine whether the pointer is embedded or not.

\begin{code}
findPtrType :: Bool -> [Attribute] -> (Type -> Type) --PointerType
findPtrType isTop ls =
   -- a specific pointer type takes priority over
   -- any setting of a pointer default.
  case (filter isPtrAttr ptr_ls) of
    ((Attribute kind []):_) -> Pointer (stringToPointerType kind) True
    [] 
     | isTop     -> Pointer Ref False
     | otherwise -> 
         case (filter isPtrDefault ptr_ls) of
            ((Attribute _ [ParamVar v]):_) -> Pointer (stringToPointerType v) False
            [] -> 
                case optPointerDefault of
                  Nothing -> Pointer Unique False
                  Just x  -> Pointer (stringToPointerType x) False
  where
   ptr_ls = filter isPtrAttrib ls
   
   isPtrAttrib a = isPtrDefault a || isPtrAttr a
 
   isPtrDefault (Attribute "pointer_default" [ParamVar _]) = True
   isPtrDefault _ = False

   isPtrAttr (Attribute "ptr" [])    = True
   isPtrAttr (Attribute "ref" [])    = True
   isPtrAttr (Attribute "unique" []) = True
   isPtrAttr (Attribute "any" [])    = True
   isPtrAttr _                       = False

stringToPointerType :: String -> PointerType
stringToPointerType "ref"     = Ref
stringToPointerType "unique"  = Unique
stringToPointerType "ptr"     = Ptr
\end{code}

\begin{code}
idHaskellModule :: Id -> Maybe Name
idHaskellModule i = mapMb (mkHaskellTyConName.dropSuffix) (idModule i)
\end{code}


Group the parameters according to their attributes
(preserving the ordering of the given parameter list.)

@binParams ls@ returns @(ps, is, os, ios, rs)@

where

<itemize>
<item> @ps@ is the list of parameters the Haskell function takes
   as arguments (this is not the final list, as the processing
   of dependent arguments will remove the dependees.)
<item> @is@ is the [in] parameters (preserved left-to-right ordering
   of original parameter list.)
<item> @os@ is the [out] params.
<item> @ios@ is the [in,out] params.
<item> @rs@ is the parameters that should be returned as results
   from the Haskell programmer.
</itemize>

\begin{code}
binParams :: [Param] -> ([Param], [Param], [Param], [Param], [Param])
binParams ps = foldr binParam ([],[],[],[],[]) ps
 where
  binParam p (params, ins, outs, inouts, results) =
      case (paramMode p) of
        InOut ->
          case (paramType p) of
            Pointer Ptr _ _ -> (p:params, p:ins,outs, inouts, results)
            _               -> (p:params, ins, outs, p:inouts, p:results)
        In    -> (p:params, p:ins,outs, inouts, results)
        Out   -> (params, ins, p:outs, inouts, p:results)
\end{code}


\begin{code}
iPointerParam :: Name -> Param
iPointerParam nm = 
  mkParam "iptr" In
          (Pointer Ptr True (Name (mkHaskellTyConName nm) nm Nothing Nothing Nothing Nothing))

objParam :: Name -> Param
objParam nm = 
  mkParam "obj" In
          (Name (mkHaskellTyConName nm) nm Nothing Nothing Nothing Nothing)

resultParam :: Type -> Param
resultParam ty = mkParam "result" Out ty
\end{code}
     
Reduce fancy unions down to C-style unions and structs.

\begin{code}
unionToStruct :: Type -> (Maybe (Id, Type), Type)
unionToStruct t =
  case t of
    UnionNon un_tag sws                   -> (Nothing, CUnion un_tag fields Nothing)
         where
          fields = map switchToField sws
    Union enc_struct_tag tag_ty tg un_tag sws -> 
        ( Just (un_tag{idName=un_ty_nm, idOrigName=un_ty_nm}, c_union)
        , Struct enc_struct_tag
               [ Field un_tag nm_ty nm_ty Nothing Nothing
               , Field tg     tag_ty  tag_ty  Nothing Nothing
               ]
               Nothing
        )
        where
          un_ty_nm  = "__IHC__" ++ idOrigName un_tag
          nm_ty     = Name un_ty_nm un_ty_nm Nothing Nothing (Just c_union) Nothing
          c_union = CUnion un_tag fields Nothing
          fields  = map switchToField sws
    _ -> (Nothing, t)
 where
  switchToField (Switch i _ ty o_ty) = Field i ty o_ty Nothing Nothing
  switchToField _                    = error "switchToField"

\end{code}

Check whether an interface derives from IDispatch or not.

\begin{code}
derivesFromIDispatch :: CoClassDecl -> Bool
derivesFromIDispatch (CoClassInterface _ mb_decl) = 
  case mb_decl of
    Nothing -> False -- worth a warning?
    Just d  -> 
      case d of
        Interface{declId=i,declInherit=inherits} ->
           idOrigName i == "IDispatch" || 
           any (\ (x,_) -> qName x == "IDispatch") inherits
        DispInterface{} -> True
derivesFromIDispatch _  = True
\end{code}

\begin{code}
toDispInterfaceMethod :: Decl -> Decl
toDispInterfaceMethod (Method i cc _ ps off) =
     case break ((\ x -> hasAttributeWithName x "retval").idAttributes.paramId) ps of
       (bef,p:aft) -> 
          let
           ty   = removePtr (paramType p)
           o_ty = removePtr (paramOrigType p)
          in
          Method i cc (Result ty o_ty) (bef++aft) off
       _ -> Method i cc (Result voidTy voidTy) ps off
   
toDispInterfaceMethod d = d

\end{code}


Order-sort declarations - by now, there should be no cyclic dependencies..

\begin{code}
sortDecls :: [Decl] -> [Decl]
sortDecls ds = ds_sorted
  where
     -- compute def & use of the individual decls.
   ds_depped  = map mkDeclDep ds

     -- compute scc's
   ds_groups  = stronglyConnComp ds_depped
   
     -- expand the cyclic groups
   ds_sorted  = concatMap expandGroup ds_groups


mkDeclDep :: Decl -> (Decl, String, [String])
mkDeclDep d = let (def,uses) = getDeclUses d in (d,def,uses)

getDeclUses :: Decl -> (String,[String])
getDeclUses d = (def, uses)
  where
   uses = getUses d
   def  = getDef d

   getDef defn =
    case defn of
      Typedef i _ _         -> idName i
      Constant i _ _ _      -> idName i
      Interface i _ _ _     -> idName i
      DispInterface i _ _ _ -> idName i
      CoClass i _           -> idName i
      Library i _           -> idName i
      Method i _ _ _ _      -> idName i
      Property i _ _ _ _    -> idName i
      _                     -> ""

getUses :: Decl -> [String]
getUses d = 
  case d of 
    Typedef _ ty _          -> getTyUses ty
    Constant _ ty _ _       -> getTyUses ty
    Interface _ _ is ds     -> map (qName.fst) is ++ concatMap getUses ds
    Module _ ds             -> concatMap getUses ds
    DispInterface _ _ ps ds -> concatMap getUses ps ++ concatMap getUses ds
    CoClass _ cs            -> map (idName.coClassId) cs
    Library _ ds            -> concatMap getUses ds
    Method _ _ r ps _       -> getTyUses (resultType r) ++ concatMap (getTyUses.paramType) ps
    Property _ t _ _ _      -> getTyUses t
    _                       -> []

-- Since the types were constructed from type libraries, we can make
-- a number of simplifying assumptions.
getTyUses :: Type -> [String]
getTyUses ty =
  case ty of
    FunTy _ r ps    -> getTyUses (resultType r) ++ concatMap (getTyUses.paramType) ps
    String t _ _    -> getTyUses t
    Sequence t _ _  -> getTyUses t
    Name  n _ _ _ _ _ -> [n]
    Struct _ fs _   -> concatMap (getTyUses.fieldType)  fs
    Union _ _ _ _ ss -> concatMap (getTyUses.switchType) ss
    UnionNon _ ss   -> concatMap (getTyUses.switchType) ss
    CUnion _ fs _   -> concatMap (getTyUses.fieldType)  fs
    Pointer _ _ t   -> getTyUses t
    Array t _       -> getTyUses t
    Iface n _ _ _ _ _ -> [n]
    SafeArray t       -> getTyUses t
    _                 -> []

expandGroup :: SCC a -> [a]
expandGroup (AcyclicSCC d) = [d]
expandGroup (CyclicSCC ds) = ds -- for now..

\end{code}

\begin{code}

sizeofType :: Type -> Int
sizeofType t = fst (sizeAndAlignModulus Nothing t)

--tedious function that maps a type to its size and alignment modulus.
--The constants it returns are platform dependent.
sizeAndAlignModulus :: Maybe Int -> Type -> (Int, Int)
sizeAndAlignModulus mb_pack ty =
  case ty of
    Float sz ->
       case sz of
         Short    -> (fLOAT_SIZE,  fLOAT_ALIGN_MODULUS)
         Long     -> (dOUBLE_SIZE, dOUBLE_ALIGN_MODULUS)
         LongLong -> (dOUBLE_SIZE, dOUBLE_ALIGN_MODULUS) -- no support for (long double)/quads yet (TODO)
         Natural  -> (dOUBLE_SIZE, dOUBLE_ALIGN_MODULUS)
    Integer sz signed
       | signed -> 
         case sz of
           Short     -> (sHORT_SIZE,    sHORT_ALIGN_MODULUS)
           Long      -> (lONG_SIZE,     lONG_ALIGN_MODULUS)
           Natural   -> (lONG_SIZE,     lONG_ALIGN_MODULUS)
           LongLong  -> (lONGLONG_SIZE, lONGLONG_ALIGN_MODULUS)
       | otherwise ->
         case sz of
           Short     -> (uSHORT_SIZE,    uSHORT_ALIGN_MODULUS)
           Long      -> (uLONG_SIZE,     uLONG_ALIGN_MODULUS)
           Natural   -> (uLONG_SIZE,     uLONG_ALIGN_MODULUS)
           LongLong  -> (uLONGLONG_SIZE, uLONGLONG_ALIGN_MODULUS)
    Char signed
       | signed       -> (sCHAR_SIZE, sCHAR_ALIGN_MODULUS)
       | otherwise    -> (uCHAR_SIZE, uCHAR_ALIGN_MODULUS)
    WChar             -> (uCHAR_SIZE, uCHAR_ALIGN_MODULUS)
    Bool              -> (uLONG_SIZE, uLONG_ALIGN_MODULUS)
    Octet             -> (uCHAR_SIZE, uCHAR_ALIGN_MODULUS)
    String{}          -> (dATA_PTR_SIZE, dATA_PTR_ALIGN_MODULUS)
    WString{}         -> (dATA_PTR_SIZE, dATA_PTR_ALIGN_MODULUS)
    Struct _ fields mb_pack2 -> (real_sz, real_ale)
          where
            mb_pack_to_use = combinePackLevels mb_pack mb_pack2
            (sz, al) = fst (computeStructSizeOffsets mb_pack_to_use fields)
            real_ale = realModulus mb_pack_to_use al
            real_sz  = align sz real_ale

    Enum{}             -> (lONG_SIZE, lONG_ALIGN_MODULUS) -- TODO: this is [v1_enum]
    Union _ tty _ _ sw -> (align (uni_off + uni_sz) uni_align, uni_align)
            where
             sw_no_empties             = filter (not.isEmptySwitch) sw
             
             isEmptySwitch (SwitchEmpty _) = True
             isEmptySwitch _               = False

             (sw_sizes, sw_aligns)     = unzip (map ((sizeAndAlignModulus mb_pack).switchType) sw_no_empties)
             (tag_sz, {-tag_align-} _) = sizeAndAlignModulus mb_pack tty
             uni_sz     = maximum sw_sizes
             uni_align  = realModulus mb_pack (maximum sw_aligns)

              -- compute the offset of union (=> size of tag + pad.)
             uni_off    = align tag_sz uni_align

    UnionNon _ sw    -> (uni_sz, uni_align) 
            where
             sw_no_empties         = filter (not.isEmptySwitch) sw

             isEmptySwitch (SwitchEmpty _) = True
             isEmptySwitch _               = False

             (sw_sizes, sw_aligns) = unzip (map ((sizeAndAlignModulus mb_pack).switchType) sw_no_empties)
             uni_sz     = maximum sw_sizes
             uni_align  = realModulus mb_pack (maximum sw_aligns)

    CUnion _ fields mb_pack2 -> (uni_sz, uni_align)
            where
             mb_pack_to_use = combinePackLevels mb_pack mb_pack2

             (sw_sizes, sw_aligns) = 
                unzip (map ((sizeAndAlignModulus mb_pack_to_use).fieldType) fields)
             uni_sz     = maximum sw_sizes
             uni_align  = realModulus mb_pack_to_use (maximum sw_aligns)

    Pointer{}        -> (dATA_PTR_SIZE, dATA_PTR_ALIGN_MODULUS)
    Object           -> (dATA_PTR_SIZE, dATA_PTR_ALIGN_MODULUS)
    Any              -> (dATA_PTR_SIZE, dATA_PTR_ALIGN_MODULUS)
    FunTy{}          -> (dATA_PTR_SIZE, dATA_PTR_ALIGN_MODULUS)
    Array aty [e]    -> let (el_sz, al) = sizeAndAlignModulus mb_pack aty in
                        (el_sz * fromIntegral (evalExpr e), al)
     -- catch the case of conformant/open arrays.
    Array aty []     -> let (el_sz, al) = sizeAndAlignModulus mb_pack aty in
                        (el_sz, al)
    Array _ _        -> (dATA_PTR_SIZE, dATA_PTR_ALIGN_MODULUS)
    Void             -> (0, dATA_PTR_ALIGN_MODULUS)
    SafeArray{}      -> (sAFEARRAY_SIZE, sAFEARRAY_ALIGN_MODULUS)
    Name _ _ _ _ _ (Just ti) -> (prim_sizeof ti, prim_align ti)
     -- need next one for when we're not doing magic overloading of variants.
    Name "VARIANT" _ _ _ _ _ -> (16, 8) -- Vanilla fudge.
--    Name "GUID" _ _ _ _ _    -> (16, 4) -- Vanilla fudge.
    Name _ _ _ _ (Just t) _  -> sizeAndAlignModulus mb_pack t
    Name nm _ _ _ Nothing _  -> let msg = error ("sizeAndAlignModulus: "++nm) in (msg, msg)
    Fixed{}           -> (undefined, undefined)
    Sequence{}        -> (dATA_PTR_SIZE, dATA_PTR_ALIGN_MODULUS)
    Any               -> (undefined, undefined)
    Object            -> (dATA_PTR_SIZE, dATA_PTR_ALIGN_MODULUS)
    Iface{}           -> (dATA_PTR_SIZE, dATA_PTR_ALIGN_MODULUS)
    StablePtr{}       -> (dATA_PTR_SIZE, dATA_PTR_ALIGN_MODULUS)

\end{code}

\begin{code}
computeStructSizeOffsets :: Maybe Int -> [Field] -> ((Int, Int), [Int])
computeStructSizeOffsets mb_pack fields =
 case (mapAccumL (place mb_pack) (0::Int,1::Int) fields) of
   ((size, al), offsets) ->
     let real_al = max structAlign al in
     ( (align size real_al, real_al)
     , offsets
     )
 where
  structAlign = sTRUCT_ALIGN_MODULUS

place :: Maybe Int
      -> ( Int       -- tentative offset for field 
         , Int       -- current alignment modulus for struct
         )  
      -> Field
      -> ( (Int,Int) -- updated state
         , Int       -- offset at which to store/access field.
         )
place mb_pack (off,struct_align) f =
 case sizeAndAlignModulus mb_pack (fieldType f) of
   (sz, al) -> 
        let 
         real_ale  = realModulus mb_pack al
         field_off = align off real_ale
        in
        ((field_off+sz, max real_ale struct_align), field_off)

realModulus :: Maybe Int -> Int -> Int
realModulus Nothing  n = n
realModulus (Just v) n = min v n

-- align off al = off' such that (off' `mod` align = 0)
align :: Int -> Int -> Int
align off al 
 | off `mod` al == 0 = off -- perfect, no padding.
 | otherwise         = off + (al - off `mod` al)

combinePackLevels :: Maybe Int -> Maybe Int -> Maybe Int
combinePackLevels Nothing x = x
combinePackLevels x       Nothing = x
combinePackLevels (Just _) (Just y) = Just y -- the inner one 'wins'.
\end{code}

When deciding whether to generate (or use) a marshaller for a 
type, we need to know whether it is (or has components) that are
finalised. The reason we need to know this detail of the type is
that finalisation is handled differently depending on whether the
unmarshaller is called from a stub or a proxy.

\begin{code}
isFinalisedType :: Bool -> Type -> Bool
isFinalisedType ifaceOnly t =
  case t of
    Integer{}   -> False
    StablePtr{} -> False
    Float{}     -> False
    Char{}    -> False
    WChar{}   -> False
    FunTy _ r ps -> any (isFinalisedType ifaceOnly) (resultType r : map paramType ps)
    Bool{}    -> False
    Octet{}   -> False
    Any{}     -> False
    Object{}  -> False
    String{}  -> False
    WString{} -> False
    Sequence ty _ _ -> isFinalisedType ifaceOnly ty
    Fixed{}   -> False
    Void{}    -> False
    SafeArray _ -> True
    Iface _ _ _ as _ _   -> not optHaskellToC || as `hasAttributeWithName` "finaliser"
    Array ty   _ -> isFinalisedType ifaceOnly ty
    Pointer Ptr _ _ -> False
    Pointer _ _ ty  -> isFinalisedType ifaceOnly ty
    Struct _ fs _ -> any (isFinalisedType ifaceOnly) (map fieldType fs)
    CUnion _ fs _ -> any (isFinalisedType ifaceOnly) (map fieldType fs)
    UnionNon _ ss -> any (isFinalisedType ifaceOnly) (map switchType (filter isNonDef ss))
    Enum{}    -> False
    Union _ _ _ _ ss -> any (isFinalisedType ifaceOnly) (map switchType (filter isNonDef ss))
    Name _ _ _ _ _ (Just ti) -> not ifaceOnly && finalised ti
    Name _ _ _ _ (Just ty) _ -> isFinalisedType ifaceOnly ty
    Name{}      -> False
 where
  isNonDef Switch{} = True
  isNonDef _        = False

\end{code}
