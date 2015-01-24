%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Feb. 9th 2003  15:53  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

The generated code makes use of a number of library functions. This
module abstracts away from their names and locations.

\begin{code}
module LibUtils
        (
          hdirectLib
        , bitsLib
        , comLib
        , comServLib
        , prelude
        , maybe_module
        , autoLib
        , ioExts
        , intLib
        , wordLib
--      , addrLib
        , foreignLib
        , arrayLib
        , jniLib
        , wStringLib
        , safeArrayLib
        , ptrLib
        , foreignPtrLib
        
        , iDispatch
        , iUnknown
        , iUnknownFO
        , iID
        , cLSID
        , gUID
        , lIBID
        , mkIID
        , mkCLSID
        , mkLIBID
        , primIP
        , currency
        , variantClass
        , variantType
        , groundInterface

        , vARIANT
        , sAFEARRAY

        , derefPtr
        , indexPtr
        , interfacePtrToAddr
        , intToAddr
        , mkWString
        , lengthWString
        
        , castPtrName
        , castFPtrName
        , withForeignPtrName

        , checkHR
        , check2HR
        , invokeAndCheck
        , returnHR
        , invokeIt
        , primInvokeIt
        
        , getIfaceState
        , createComVTable
        , createDispVTable
        , comVTableTy
        , createComInst
        , mkComInterface
        , mkDispInterface
        , mkDualInterface
        , comInterfaceTy
        
        , componentInfo
        , mkComponentInfo
        , hasTypeLib
        
        , mkForeignObj

        , trivialFree
        , allocOutPointer
        , allocBytes
        , allocWords

        , list
        , blist
        , bstring
        , ref
        , unique
--      , ptr
        , fptr
        , iptr
        , wstring
        , wstring2

        , enum32
        , enum16

        , stringName
        , bool
        , integer
        , bstr
        , safearray
        , ptrName
        , funPtrName
        , foreignPtrName
        
        , free
        , freeRef
        , doThenFree
        , nullPtr
        , nullFO
        , nullFinaliser
        , nullIPointer
        , prelError
        , raiseIOException
        , prelUserError
        , prelReturn
        , bindName
        , bind_Name
        
        , xorName
        , orName
        , andName
        , shiftLName
        , shiftRName
        , bitsClass
        , complementName
        , shiftName
        , rotateName
        , bitSizeName
        , isSignedName

        , addName
        , subName
        , divName
        , modName
        , mulName
        , logAndName
        , logOrName
        , gtName
        , geName
        , eqName
        , leName
        , ltName
        , neName
        , negateName
        , notName

        , marshStruct
        , unmarshStruct
        , marshUnion
        , unmarshUnion
        
        , dollarName
        , inArgName
        , inIUnknownArgName
        , outArgName
        , inoutArgName
        , retValName
        , applyName
        , mkDispMethod
        
        , enumClass
        , eqClass
        , showClass
        , numClass

        , fromEnumName
        , toEnumName
        
        , enumToInt
        , enumToFlag
        , unboxInt
        , flagToIntTag
        , tagToEnum
        , toIntFlag
        , pow2Series
        , orListName
        , orFlagsName
        , orFlagName
        , flagsClass
        
        , inVariantName
        , resVariantName
        , defaultVariantName
        , vtEltTypeName

        , fromMaybeName
        , maybeName
        , justName
        , nothingName
        , mapName
        , mapListName
        , concatName
        , concatMapName
        , intersectName
        , mapMaybeName
        , sumName
        , fromIntegralName
        , lengthName
        
        , true
        , false
        
        , uPerformIO

        , marshallPrefix
        , marshallRefPrefix
        , unmarshallPrefix
        , unmarshallRefPrefix
        , sizeofPrefix
        , sizeOfName
        , outPrefix
        , allocPrefix
        , freePrefix
        , copyPrefix
        
        , marshallMaybe
        , writeMaybe
        , readMaybe

        , mkPrimitiveName
        , mkWrapperName
        , mkPrimExportName
        , mkVtblOffsetName
        , mkCLSIDName
        , mkLIBIDName
        
        , defaultCConv
        
        , invokeMethod
        , invokeStaticMethod
        , invokeInterfaceMethod
        , getField
        , getStaticField
        , setField
        , setStaticField
        , inArg
        , jvalueClass
        , jObject
        , jArray
        , jniEnv
        , fPointer

        , newObj
        , className
        , makeClassName
        , mkClassName
        , newFPointer
        
        , orbLib
        , cObject
        
       ) where

import BasicTypes
import Opts ( optHaskellToC, optH1_4, 
              optCorba
            )

\end{code}

Where it's at - the different modules we may end up
generating imports from:

\begin{code}
hdirectLib, bitsLib, comLib, comServLib, listLib, ptrLib, foreignPtrLib :: Maybe String
hdirectLib = Just "HDirect"
bitsLib = Just "Bits"
comLib  = Just "Com"
comServLib  = Just "ComServ"
listLib = Just "List"
ptrLib  = Just "Foreign.Ptr"
foreignPtrLib = Just "Foreign.ForeignPtr"

comDll, prelude, prelGHC, maybe_module, autoLib, ioExts :: Maybe String
comDll  = Just "ComDll"
prelude = Just "Prelude"
prelGHC = Just "PrelGHC"
maybe_module = Just "Maybe"
autoLib = Just "Automation"
ioExts  = Just "System.IO"

intLib, wordLib, foreignLib, arrayLib :: Maybe String
intLib  = Just "Int"
wordLib = Just "Word"
--addrLib = Just "Addr"
foreignLib = Just "Foreign"
arrayLib   = Just "Array"

stdDispatchLib, wStringLib, jniLib, orbLib, safeArrayLib :: Maybe String
stdDispatchLib = Just "StdDispatch"
wStringLib = Just "WideString"
safeArrayLib = Just "SafeArray"

jniLib = Just "JNI"
orbLib = Just "Corba"

\end{code}

Some standard COM types/functions :

\begin{code}
iDispatch, iUnknown, iUnknownFO, primIP, iID, cLSID, gUID, lIBID, mkIID, mkCLSID, mkLIBID :: QualName
iDispatch  = mkQualName autoLib "IDispatch"
iUnknown   = mkQualName comLib "IUnknown"
iUnknownFO = mkQualName comLib "IUnknownFO"
primIP     = mkQualName comLib "PrimIP"
iID        = mkQualName comLib "IID"
cLSID      = mkQualName comLib "CLSID"
gUID       = mkQualName comLib "GUID"
lIBID      = mkQualName comLib "LIBID"
mkIID      = mkQualName comLib "mkIID"
mkCLSID    = mkQualName comLib "mkCLSID"
mkLIBID    = mkQualName comLib "mkLIBID"

mkForeignObj, nullIPointer, groundInterface :: QualName

-- since ghc-4.08.1's Foreign.makeForeignObj doesn't quite work,
-- we rely on the FO creator in the HDirect support libs instead.
--mkForeignObj = mkQualName foreignLib "makeForeignObj"
mkForeignObj = mkQualName (Just "Pointer") "makeFO"

nullIPointer = mkQualName comLib "interfaceNULL"

groundInterface = mkQualName Nothing "()"

currency, vARIANT, sAFEARRAY :: QualName
currency     = mkQualName autoLib "Currency"
vARIANT      = mkQualName autoLib "VARIANT"
sAFEARRAY    = mkQualName safeArrayLib "SAFEARRAY"

dollarName :: QualName
dollarName   = mkQualName prelude "$"

derefPtr, interfacePtrToAddr, intToAddr, indexPtr :: QualName
derefPtr      = mkQualName hdirectLib "derefPtr"
interfacePtrToAddr = mkQualName comLib "interfacePtrToAddr"
intToAddr      = mkQualName hdirectLib "intToAddr"
indexPtr       = mkQualName hdirectLib "indexPtr"

castPtrName, castFPtrName, withForeignPtrName :: QualName
castPtrName    = mkQualName ptrLib     "castPtr"
castFPtrName   = mkQualName foreignPtrLib "castForeignPtr"
withForeignPtrName = mkQualName foreignPtrLib "withForeignPtr"

checkHR, check2HR, invokeAndCheck, returnHR, invokeIt, primInvokeIt :: QualName
checkHR        = mkQualName comLib "checkHR"
check2HR       = mkQualName comLib "check2HR"
invokeAndCheck = mkQualName comLib "invokeAndCheck"
returnHR       = mkQualName comLib "returnHR"
invokeIt       = mkQualName comLib "invokeIt"
primInvokeIt   = mkQualName hdirectLib "primInvokeIt"

variantClass, variantType, inVariantName, resVariantName, defaultVariantName, vtEltTypeName :: QualName
variantClass   = mkQualName autoLib "Variant"
variantType    = mkQualName autoLib "VARIANT"
inVariantName  = mkQualName autoLib "inVariant"
resVariantName = mkQualName autoLib "resVariant"
defaultVariantName = mkQualName autoLib "defaultVariant"
vtEltTypeName  = mkQualName autoLib "vtEltType"

getIfaceState, createComVTable, comVTableTy, createComInst :: QualName
getIfaceState    = mkQualName comServLib "getObjState"
createComVTable  = mkQualName comServLib "createComVTable"
comVTableTy      = mkQualName comServLib "ComVTable"
createComInst    = mkQualName comServLib "createComInstance"

mkComInterface, mkDispInterface, mkDualInterface, comInterfaceTy :: QualName
mkComInterface   = mkQualName comServLib "mkIface"
mkDispInterface  = mkQualName comServLib "mkDispIface"
mkDualInterface  = mkQualName comServLib "mkDualIface"
comInterfaceTy   = mkQualName comServLib "ComInterface"

componentInfo, mkComponentInfo, hasTypeLib :: QualName
componentInfo    = mkQualName comDll "ComponentInfo"
mkComponentInfo  = mkQualName comDll "mkComponentInfo"
hasTypeLib       = mkQualName comDll "hasTypeLib"

inArgName, inIUnknownArgName, outArgName, inoutArgName, retValName, applyName :: QualName
inArgName         = mkQualName stdDispatchLib "inArg"
inIUnknownArgName = mkQualName stdDispatchLib "inIUnknownArg"
outArgName        = mkQualName stdDispatchLib "outArg"
inoutArgName      = mkQualName stdDispatchLib "inoutArg"
retValName        = mkQualName stdDispatchLib "retVal"
applyName         = mkQualName stdDispatchLib "apply_"

createDispVTable, mkDispMethod :: QualName
createDispVTable = mkQualName stdDispatchLib "createStdDispatchVTBL2"
mkDispMethod     = mkQualName stdDispatchLib "mkDispMethod"

trivialFree, free, freeRef, doThenFree :: QualName
trivialFree   = mkQualName hdirectLib "trivialFree"
free          = mkQualName hdirectLib "free"
freeRef       = mkQualName hdirectLib "freeref"
doThenFree    = mkQualName hdirectLib "doThenFree"

nullPtr, nullFO, nullFinaliser, prelError, prelFail, prelIOError :: QualName
nullPtr       = mkQualName ptrLib     "nullPtr"
nullFO        = mkQualName hdirectLib "nullFO"
nullFinaliser = mkQualName hdirectLib "nullFinaliser"
prelError     = mkQualName prelude    "error"
prelFail      = mkQualName prelude    "fail"
prelIOError   = mkQualName prelude    "ioError"

prelUserError, prelReturn, bindName, bind_Name :: QualName
prelUserError = mkQualName prelude    "userError"
prelReturn    = mkQualName prelude    "return"
bindName      = mkQualName prelude    ">>="
bind_Name     = mkQualName prelude    ">>"

xorName, orName, andName, shiftLName, shiftRName :: QualName
xorName       = mkQualName bitsLib    "xor"
orName        = mkQualName bitsLib    ".|."
andName       = mkQualName bitsLib    ".&."
shiftLName    = mkQualName bitsLib    "shiftL"
shiftRName    = mkQualName bitsLib    "shiftR"

complementName, shiftName, rotateName, bitSizeName, isSignedName :: QualName
complementName = mkQualName bitsLib    "complement"
shiftName      = mkQualName bitsLib    "shift"
rotateName     = mkQualName bitsLib    "rotate"
bitSizeName    = mkQualName bitsLib    "bitSize"
isSignedName   = mkQualName bitsLib    "isSigned"

bitsClass :: QualName
bitsClass     = mkQualName bitsLib    "Bits"

addName, subName, divName, modName, mulName :: QualName
addName       = mkQualName prelude    "+"
subName       = mkQualName prelude    "-"
divName       = mkQualName prelude    "div"
modName       = mkQualName prelude    "mod"
mulName       = mkQualName prelude    "*"

logAndName, logOrName :: QualName
logAndName    = mkQualName prelude    "&&"
logOrName     = mkQualName prelude    "||"
gtName, geName, eqName, leName, ltName, neName :: QualName
gtName        = mkQualName prelude    ">"
geName        = mkQualName prelude    ">="
eqName        = mkQualName prelude    "=="
leName        = mkQualName prelude    "<="
ltName        = mkQualName prelude    "<"
neName        = mkQualName prelude    "/="

negateName, notName :: QualName
negateName    = mkQualName prelude    "negate"
notName       = mkQualName prelude    "not"

marshStruct, unmarshStruct, marshUnion, unmarshUnion :: QualName
marshStruct   = mkQualName hdirectLib "marshallStruct"
unmarshStruct = mkQualName hdirectLib "unmarshallStruct"
marshUnion    = mkQualName hdirectLib "marshallUnion"
unmarshUnion  = mkQualName hdirectLib "unmarshallUnion"

marshallMaybe, writeMaybe, readMaybe :: QualName
marshallMaybe = mkQualName hdirectLib "marshallMaybe"
writeMaybe    = mkQualName hdirectLib "writeMaybe"
readMaybe     = mkQualName hdirectLib "readMaybe"

eqClass, numClass, showClass, enumClass, fromEnumName, toEnumName :: QualName
eqClass       = mkQualName prelude "Eq"
showClass     = mkQualName prelude "Show"
numClass      = mkQualName prelude "Num"
enumClass     = mkQualName prelude "Enum"
fromEnumName  = mkQualName prelude "fromEnum"
toEnumName    = mkQualName prelude "toEnum"

enumToInt, enumToFlag, flagToIntTag, pow2Series :: QualName
enumToInt     = mkQualName hdirectLib "enumToInt"
enumToFlag    = mkQualName hdirectLib "enumToFlag"
flagToIntTag  = mkQualName hdirectLib "flagToIntTag"
pow2Series    = mkQualName hdirectLib "pow2Series"

orListName :: QualName
orListName    = mkQualName hdirectLib "orList"

orFlagsName :: QualName
orFlagsName    = mkQualName hdirectLib "orFlags"

flagsClass :: QualName
flagsClass = mkQualName hdirectLib "Flags"

orFlagName :: QualName
orFlagName = mkQualName hdirectLib ".+."

unboxInt, tagToEnum, toIntFlag :: QualName
unboxInt      = mkQualName hdirectLib "unboxInt"
tagToEnum     = mkQualName prelGHC    "tagToEnum#"
toIntFlag     = mkQualName hdirectLib "toIntFlag"

fromMaybeName, maybeName :: QualName
fromMaybeName = mkQualName maybe_module "fromMaybe"
maybeName     = mkQualName prelude "Maybe"
justName, nothingName :: QualName
justName      = mkQualName prelude "Just"
nothingName   = mkQualName prelude "Nothing"

lengthName :: QualName
lengthName = mkQualName prelude "length"

mapName :: QualName
mapName
 | optH1_4    = mkQualName prelude "map"
 | otherwise  = mkQualName prelude "fmap"

mapListName :: QualName
mapListName   = mkQualName prelude "map"

concatName :: QualName
concatName   = mkQualName prelude "concat"

concatMapName :: QualName
concatMapName   = mkQualName listLib "concatMap"

intersectName :: QualName
intersectName   = mkQualName listLib "intersect"

mapMaybeName :: QualName
mapMaybeName   = mkQualName maybe_module "mapMaybe"

sumName :: QualName
sumName   = mkQualName prelude "sum"

fromIntegralName :: QualName
fromIntegralName = mkQualName prelude "fromIntegral"

true, false :: QualName
true          = mkQualName prelude "True"
false         = mkQualName prelude "False"

uPerformIO :: QualName
uPerformIO    = mkQualName ioExts  "unsafePerformIO"

mkWString :: QualName
mkWString     = mkQualName wStringLib "mkWideString"

lengthWString :: QualName
lengthWString     = mkQualName wStringLib "lengthWideString"

allocOutPointer, allocBytes, allocWords :: QualName
allocOutPointer = mkQualName hdirectLib "allocOutPtr"
allocBytes      = mkQualName hdirectLib "allocBytes"
allocWords      = mkQualName hdirectLib "allocWords"
\end{code}

\begin{code}
list, blist, bstring, fptr, iptr :: String
list    = "list"
blist   = "blist"
bstring = "BString"
--ptr     = "ptr"
fptr    = "fptr"
iptr    = "iptr"

ref, unique :: String
ref     = "ref"
unique  = "unique"

stringName , wstring, wstring2 :: String
stringName = "String"
wstring    = "WideString"
wstring2   = "WideString2"

ptrName, funPtrName, foreignPtrName :: String
ptrName  = "Ptr"
funPtrName = "FunPtr"
foreignPtrName = "ForeignPtr"

bool, integer, bstr, safearray :: String
bool    = "Bool"
integer = "Integer"
bstr    = "BSTR"
safearray = "SafeArray"

enum32, enum16 :: String
enum32  = "Enum32"
enum16  = "Enum16"
\end{code}


The Haskell names for the different functions that we generate
during the translation from IDL to Haskell are formed by adding
a prefix to the (Haskell) type of the value we're marshalling
to/from:

\begin{code}
outPrefix, unmarshallPrefix, marshallPrefix :: String
marshallPrefix      = "marshall"
outPrefix           = "o_"
unmarshallPrefix    = "unmarshall"

marshallRefPrefix, unmarshallRefPrefix, allocPrefix, sizeofPrefix :: String
marshallRefPrefix   = "write"
unmarshallRefPrefix = "read"
allocPrefix         = "alloc"
sizeofPrefix        = "sizeof"

freePrefix, copyPrefix :: String
freePrefix          = "free"
copyPrefix          = "copy"

-- given the name of a method/function, produce the
-- name of the primitive Haskell function that represent it.
mkPrimitiveName :: String -> String
mkPrimitiveName nm = "prim_"++nm

-- wrappers are Haskell functions that convert between
-- the expected signature of an external function type or method
-- and the Haskell implementation - i.e., arguments are marshalled
-- prior to calling the Haskell function, followed by unmarshalling
-- it's result.
mkWrapperName :: String -> String
mkWrapperName nm = "wrap_"++nm

mkPrimExportName :: String -> String
mkPrimExportName nm = "export_" ++ nm

mkVtblOffsetName :: String -> String -> String
mkVtblOffsetName iface meth = "off_" ++ iface ++ '_':meth


mkCLSIDName :: String -> String
mkCLSIDName cls_nm = "clsid" ++ cls_nm

mkLIBIDName :: String -> String
mkLIBIDName cls_nm = "libid" ++ cls_nm

mkClassName :: String -> String
mkClassName nm = nm ++ "ClassName"
\end{code}

\begin{code}
invokeMethod :: QualName
invokeMethod          = mkQualName jniLib "callMethodPrim"

invokeStaticMethod :: QualName
invokeStaticMethod    = mkQualName jniLib "invokeStaticMethod"

invokeInterfaceMethod :: QualName
invokeInterfaceMethod = mkQualName jniLib "callMethodPrim"

getField :: QualName
getField              = mkQualName jniLib "getFieldPrim"

getStaticField :: QualName
getStaticField        = mkQualName jniLib "get_StaticField"

setField :: QualName
setField              = mkQualName jniLib "setFieldPrim"

setStaticField :: QualName
setStaticField        = mkQualName jniLib "set_StaticField"

inArg, jvalueClass, jObject, jArray, jniEnv :: QualName
inArg         = mkQualName jniLib "inVal"
jvalueClass   = mkQualName jniLib "JValue"
jObject       = setOrigQName "java.lang.Object" (mkQualName jniLib "JObject")
jArray        = mkQualName jniLib "JArray"
jniEnv        = mkQualName jniLib "JNIEnv"

fPointer, newObj, className, makeClassName, newFPointer :: QualName
fPointer      = mkQualName jniLib "FunctionPtr"
newObj        = mkQualName jniLib "new"
className     = mkQualName jniLib "ClassName"
makeClassName = mkQualName jniLib "mkClassName"
newFPointer   = mkQualName jniLib "new_FunctionPtr"
\end{code}

\begin{code}
cObject :: QualName
cObject  = mkQualName orbLib "Object"

\end{code}

The method calling convention used if none specified.

\begin{code}
defaultCConv :: CallConv
defaultCConv 
 | optHaskellToC || optCorba = Cdecl
 | otherwise     = Stdcall
\end{code}

\begin{code}
raiseIOException :: QualName
raiseIOException 
  | optH1_4   = prelFail
  | otherwise = prelIOError
\end{code}

FFI names

\begin{code}
sizeOfName :: String
sizeOfName = "sizeOf"

\end{code}
