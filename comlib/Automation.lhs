%
% Copyright (c) 1998-99,  Daan Leijen,    leijen@@fwi.uva.nl
%                         Sigbjorn Finne  sof@@dcs.gla.ac.uk
%

\begin{code}
{-# OPTIONS -#include "autoPrim.h" #-}
module Automation (
      module Com,

      IDispatch_, IDispatch, iidIDispatch, 

      queryIUnknown, queryIDispatch,

      createObject, getObject, getActiveObject, getFileObject,

      Member, DISPID, getMemberID, VARIANT, sizeofVARIANT,
      marshallVARIANT, unmarshallVARIANT, readVARIANT, writeVARIANT,
      copyVARIANT, allocVARIANT,

      VarIn, VarRes, ArgIn, ArgInOut, ArgOut, ArgRes,

      Variant(..),

      defaultVariant, inVariant, resVariant, inoutVariant, outVariant,
      defaultEmpty, inEmpty, resEmpty, inoutEmpty, outEmpty, inNoArg,
      defaultInt, inInt, resInt, inoutInt, outInt,
      defaultInt8, inInt8, resInt8, inoutInt8, outInt8,
      defaultInt16, inInt16, resInt16, inoutInt16, outInt16,
      defaultInt32, inInt32, resInt32, inoutInt32, outInt32,
      defaultInt64, inInt64, resInt64, inoutInt64, outInt64,
      defaultInteger, inInteger, resInteger, inoutInteger, outInteger,
      defaultHRESULT, inHRESULT, resHRESULT, inoutHRESULT, outHRESULT,
      defaultWord, inWord, resWord, inoutWord, outWord,
      defaultWord8, inWord8, resWord8, inoutWord8, outWord8,
      defaultWord16, inWord16, resWord16, inoutWord16, outWord16,
      defaultWord32, inWord32, resWord32, inoutWord32, outWord32,
      defaultWord64, inWord64, resWord64, inoutWord64, outWord64,
      defaultBool, inBool, resBool, inoutBool, outBool,
      defaultByte, inByte, resByte, inoutByte, outByte,
      defaultChar, inChar, resChar, inoutChar, outChar,
      defaultFloat, inFloat, resFloat, inoutFloat, outFloat,
      defaultDouble, inDouble, resDouble, inoutDouble, outDouble,
      defaultString, inString, resString, inoutString, outString,
      defaultIUnknown, inIUnknown, resIUnknown, inoutIUnknown, outIUnknown,
      defaultIDispatch, inIDispatch, resIDispatch, inoutIDispatch, outIDispatch,
      defaultDate, inDate, resDate, inoutDate, outDate, Date,
      defaultError, inError, resError, inoutError, outError,
      defaultMaybe, inMaybe, resMaybe, inoutMaybe, outMaybe, inOptional,
      defaultCurrency, inCurrency, resCurrency, inoutCurrency, outCurrency, Currency,
      defaultSafeArray, inSafeArray, resSafeArray, inoutSafeArray, outSafeArray, SafeArray, mkSafeArray,
      defaultEnum, inEnum, resEnum, inoutEnum, outEnum, vtTypeEnum,
      inHaskellValue, unsafeResHaskellValue, unsafeOutHaskellValue,
      defaultSqlNull, inSqlNull, resSqlNull, inoutSqlNull, outSqlNull,
      SqlNull(..),
     
      inGUID, outGUID,

      inDefaultValue, noInArg, 

      propertyGet, propertySet, propertySetGet,
      propertyGet2, propertyGet3, propertyGet4,
      propertyGetID, propertySetID, propertySetGetID,
      propertyGet2ID, propertyGet3ID, propertyGet4ID,

      function1, function2, function3, function4, function5, function6, 
      functionID1, functionID2, functionID3, functionID4, functionID5, functionID6,

      method0, method1, method2, method3, method4, method5, method6, method7, method8,
      methodID0, methodID1, methodID2, methodID3, methodID4,
      methodID5, methodID6, methodID7, methodID8,

      unmarshallVariants0, unmarshallVariants1,
      unmarshallVariants2, unmarshallVariants3,
      unmarshallVariants4, unmarshallVariants5,
      unmarshallVariants6, unmarshallVariants7,
      unmarshallVariants8,

      readVariants0, readVariants1,
      readVariants2, readVariants3,
      readVariants4, readVariants5,
      readVariants6, readVariants7,
      readVariants8,

      method_0_0, method_1_0, method_2_0, method_3_0, method_4_0,
      method_0_1, method_1_1, method_2_1, method_3_1, method_4_1,
      method_0_2, method_1_2, method_2_2, method_3_2, method_4_2,

      function_0_1, function_1_1, function_2_1, function_3_1, function_4_1,
      function_0_2, function_1_2, function_2_2, function_3_2, function_4_2,

      propertyGet_0, propertyGet_1, propertyGet_2, propertyGet_3, propertyGet_4,
      propertySet_1, propertySet_2, propertySet_3, propertySet_4,

      invokePropertyGet, invokePropertySet,
      invokeMethod, invokeFunction,

      enumVariants,
      
{- BEGIN_GHC_ONLY
      marshallCurrency, unmarshallCurrency,
      readCurrency, writeCurrency,
   END_GHC_ONLY -}
      sizeofCurrency,
      
      VARENUM(..),
      marshallVARENUM, unmarshallVARENUM,
      readVARENUM, writeVARENUM,
      sizeofVARENUM, 
      
      sizeofVARIANT_BOOL,
      marshallVARIANT_BOOL, unmarshallVARIANT_BOOL,
      readVARIANT_BOOL, writeVARIANT_BOOL,
      vARIANT_TRUE, vARIANT_FALSE,
      
      marshallVariant, unmarshallVariant,
      readVariant, writeVariant,
      
      readVarEnum, 
      readVarInt, 
      readVarFloat,
      readVarDouble,
      readVarString,
      readVarBool

      , marshallSafeArray
      , unmarshallSafeArray
      , writeSafeArray
      , readSafeArray
      , freeSafeArray
      , readSA
      
      , clockTimeToDate    -- :: Time.ClockTime  -> IO Date
    ) where

import HDirect
import IO   ( ioeGetErrorString )
import Char ( chr, ord )
import System.Time ( ClockTime(..) )

import Word ( Word8, Word16, Word32 )
import Int  ( Int32, Int16, Int8, Int64 )
import Foreign.StablePtr

import Com
import ComPrim  ( stringToBSTR )
import ComException ( dISP_E_UNKNOWNNAME, dISP_E_EXCEPTION )
import AutoPrim
import SafeArray ( addrToSAFEARRAY, marshallSAFEARRAY, readSAFEARRAY
		 , writeSAFEARRAY, unmarshallSAFEARRAY, SAFEARRAY
		 )
import WideString
import Pointer ( writeSeqAtDec, stackFrame, allocMemory, freeMemory )
import ComException ( e_FAIL )
import Foreign.Ptr
import Foreign.ForeignPtr
import IOExts	    ( unsafePerformIO )
import Bits

\end{code}

The following creation functions are the VB equivalents in Haskell,
notice the `unsafe' interface pointer return types used here. The
interface pointers returned are compatible with the stubs for
*any* IDispatch-derived interface. This makes it more convenient
(saves the extra QI / type cast), but means that it is now
possible to get run-time errors of the sort:
'method X called but not supported'.

\begin{code}
createObject  :: ProgID -> IO (IDispatch a)
createObject progid
      = coCreateObject progid iidIDispatch_unsafe

iidIDispatch_unsafe  = mkIID "{00020400-0000-0000-C000-000000000046}"

getFileObject :: String -> ProgID -> IO (IDispatch a)
getFileObject fname progid = coGetFileObject fname progid iidIDispatch_unsafe

getActiveObject :: ProgID -> IO (IDispatch a)
getActiveObject progid
      = coGetActiveObject progid iidIDispatch_unsafe

getObject :: String -> IO (IDispatch a)
getObject fname = coGetObject fname iidIDispatch_unsafe
\end{code}

The following functions are overloaded versions of the basic functions.
The postfix "_n_m" means n input arguments and m results.

\begin{code}
method_0_0 name               = method0 name []
method_1_0 name a1            = method0 name [inVariant a1] 
method_2_0 name a1 a2         = method0 name [inVariant a1, inVariant a2]
method_3_0 name a1 a2 a3      = method0 name [inVariant a1, inVariant a2, inVariant a3]
method_4_0 name a1 a2 a3 a4   = method0 name [inVariant a1, inVariant a2, 
                                              inVariant a3, inVariant a4]

method_0_1 name               = method1 name [] outVariant
method_1_1 name a1            = method1 name [inVariant a1] outVariant
method_2_1 name a1 a2         = method1 name [inVariant a1, inVariant a2] outVariant
method_3_1 name a1 a2 a3      = method1 name [inVariant a1, inVariant a2, 
                                              inVariant a3] outVariant
method_4_1 name a1 a2 a3 a4   = method1 name [inVariant a1, inVariant a2, 
                                              inVariant a3, inVariant a4] outVariant

method_0_2 name               = method2 name [] outVariant outVariant
method_1_2 name a1            = method2 name [inVariant a1] outVariant outVariant
method_2_2 name a1 a2         = method2 name [inVariant a1, inVariant a2] outVariant outVariant
method_3_2 name a1 a2 a3      = method2 name [inVariant a1, inVariant a2, 
                                              inVariant a3] outVariant outVariant
method_4_2 name a1 a2 a3 a4   = method2 name [inVariant a1, inVariant a2, 
                                              inVariant a3, inVariant a4] outVariant outVariant

function_0_1 name               = function1 name [] outVariant
function_1_1 name a1            = function1 name [inVariant a1] outVariant
function_2_1 name a1 a2         = function1 name [inVariant a1, inVariant a2] outVariant
function_3_1 name a1 a2 a3      = function1 name [inVariant a1, inVariant a2, 
                                              inVariant a3] outVariant
function_4_1 name a1 a2 a3 a4   = function1 name [inVariant a1, inVariant a2, 
                                              inVariant a3, inVariant a4] outVariant

function_0_2 name               = function2 name [] outVariant outVariant
function_1_2 name a1            = function2 name [inVariant a1] outVariant outVariant
function_2_2 name a1 a2         = function2 name [inVariant a1, inVariant a2] 
                                                  outVariant outVariant
function_3_2 name a1 a2 a3      = function2 name [inVariant a1, inVariant a2, 
                                                  inVariant a3] outVariant outVariant
function_4_2 name a1 a2 a3 a4   = function2 name [inVariant a1, inVariant a2, 
                                                  inVariant a3, inVariant a4] 
                                                  outVariant outVariant

propertyGet_0 name              = propertyGet name [] outVariant
propertyGet_1 name a1           = propertyGet name [inVariant a1] outVariant
propertyGet_2 name a1 a2        = propertyGet name [inVariant a1, inVariant a2] outVariant
propertyGet_3 name a1 a2 a3     = propertyGet name [inVariant a1, inVariant a2,
                                                    inVariant a3] outVariant
propertyGet_4 name a1 a2 a3 a4  = propertyGet name [inVariant a1, inVariant a2,
                                                    inVariant a3, inVariant a4] outVariant

propertySet_1 name a1           = propertySet name [inVariant a1]
propertySet_2 name a1 a2        = propertySet name [inVariant a1, inVariant a2]
propertySet_3 name a1 a2 a3     = propertySet name [inVariant a1, inVariant a2, inVariant a3]
propertySet_4 name a1 a2 a3 a4  = propertySet name [inVariant a1, inVariant a2,
                                                    inVariant a3, inVariant a4]
\end{code}

Automation member functions or properties are identified
by name or DISPID.

\begin{code}
type Member        = String
--type DISPID        = Int
sizeDISPID         = 4

getMemberID :: Member -> IDispatch a -> IO DISPID
getMemberID name obj = do 
   bstr        <- allocBSTR name
   (dispid,hr) <- dispatchGetMemberID (castIface obj) bstr lcidNeutral
                       `always` freeBSTR bstr
   checkHR hr  `catch` (handleErr hr)
   return dispid
 where
  handleErr hr err
    | hr == dISP_E_UNKNOWNNAME =
       coFail ("method '" ++ name ++ "' called but not supported by object")
    | otherwise		      = errorMember name err

\end{code}

Type definitions for marshalling functions. Variants are represented
as functions that can read or write a value from or to a variant structure.

\begin{code}
type VarIn            = VARIANT -> IO ()
type VarRes a         = VARIANT -> IO a

type ArgIn a          = a -> VarIn
type ArgRes a         = VarRes a
type ArgOut a         = (VarIn,ArgRes a)
type ArgInOut a b     = a -> ArgOut b
\end{code}

For each type we define 5 functions; @defaultT@, @inT@,
@resT@, @inoutT@, @outT@ where the last
two functions are defined in terms of the first three.
The @Variant@ class overloads these functions; if an
argument can be more than one type, it will be overloaded
and Haskell takes care of resolving the marshall function to use.
We enable overlapping instance by providing explicit constructor functions
in the class definition.

Input variants.

\begin{code}
class Variant a where
  inVariant :: ArgIn a
  inVarList :: ArgIn [a]
  inVarIUnknown :: ArgIn (IUnknown a)

  vtEltType :: a -> VARENUM

  resVariant :: ArgRes a
  defaultVariant :: a

  resVarList :: ArgRes [a]
  resVarIUnknown :: ArgRes (IUnknown a)
  resVarIDispatch :: ArgRes (IDispatch a)
\end{code}

Overlapping instance for strings.

\begin{code}
instance Variant a => Variant [a] where
  inVariant       = inVarList
  resVariant      = resVarList
  defaultVariant  = []


instance Variant Char where
  inVariant   = inChar
  resVariant  = resChar
  inVarList   = inString
  resVarList  = resString
  
  vtEltType _ = VT_UI1
\end{code}

Overlapping instance for @IDispatch a@ and @IUnknown ()@
variants.

\begin{code}
instance Variant a => Variant (IUnknown_ a) where
  inVariant   = inVarIUnknown
  resVariant  = resVarIUnknown
  defaultVariant = defaultIUnknown

  vtEltType _ = VT_UNKNOWN

instance Variant (IDispatch_ a) where
  inVarIUnknown  = inIDispatch
  resVarIUnknown = resIDispatch

  vtEltType _ = VT_DISPATCH

instance Variant () where
  inVarIUnknown   = inIUnknown
  resVarIUnknown  = resIUnknown
  resVarIDispatch = resIDispatch

  inVariant      = inNoArg
  resVariant     = resEmpty
  defaultVariant = defaultEmpty

  vtEltType _ = VT_ERROR
\end{code}

Normal instances.

\begin{code}
instance Variant Bool where
  inVariant   = inBool
  resVariant  = resBool
  defaultVariant = defaultBool

  vtEltType _ = VT_UI4

instance Variant Int where
  inVariant   = inInt
  resVariant  = resInt
  defaultVariant = defaultInt

  vtEltType _ = VT_I4

instance Variant Int32 where
  inVariant   = inHRESULT
  resVariant  = resHRESULT
  defaultVariant = defaultHRESULT

  vtEltType _ = VT_I4

instance Variant Int16 where
  inVariant   = inInt16
  resVariant  = resInt16
  defaultVariant = defaultInt16

  vtEltType _ = VT_I2

instance Variant Int8 where
  inVariant   = inInt8
  resVariant  = resInt8
  defaultVariant = defaultInt8

  vtEltType _ = VT_I1

{- BEGIN_GHC_ONLY
instance Variant Int64 where
   END_GHC_ONLY -}
{- BEGIN_NOT_FOR_GHC -}
instance Variant Integer where
{- END_NOT_FOR_GHC -}
  inVariant   = inInt64
  resVariant  = resInt64
  defaultVariant = defaultInt64

  vtEltType _ = VT_CY -- since VT_I8 isn't supported in VARIANTs.

instance Variant Word8 where
  inVariant   = inWord8
  resVariant  = resWord8
  defaultVariant = defaultWord8

  vtEltType _ = VT_UI1

instance Variant Word16 where
  inVariant   = inWord16
  resVariant  = resWord16
  defaultVariant = defaultWord16

  vtEltType _ = VT_UI2

instance Variant Word32 where
  inVariant   = inWord32
  resVariant  = resWord32
  defaultVariant = defaultWord32

  vtEltType _ = VT_UI4

{- BEGIN_GHC_ONLY
instance Variant Word64 where
  inVariant   = inWord64
  resVariant  = resWord64
  defaultVariant = defaultWord64

  vtEltType _ = VT_DECIMAL  -- since VT_UI8 isn't supported in VARIANTs.
   END_GHC_ONLY -}

instance Variant Float where
  inVariant   = inFloat
  resVariant  = resFloat
  defaultVariant = defaultFloat
  vtEltType _ = VT_R4

instance Variant Double where
  inVariant   = inDouble
  resVariant  = resDouble
  defaultVariant = defaultDouble
  vtEltType _ = VT_R8

instance (Variant a) => Variant (Maybe a) where
  inVariant      = inMaybe
  resVariant     = resMaybe
  defaultVariant = defaultMaybe
  vtEltType mbx  = vtEltType (f mbx)
		    where
		      f :: Maybe a -> a
		      f = undefined

instance Variant (Ptr a) where
  inVariant      = \ p y -> copyVARIANT y (castPtr p)
  resVariant     = \ p -> return (castPtr p)
  defaultVariant = nullPtr
  
\end{code}

Marshallers derived from instance methods:

\begin{code}
inoutVariant :: (Variant a, Variant b) => ArgInOut a b
inoutVariant x        = (inVariant x,resVariant)

inoutVariant' :: (Variant a) => ArgInOut a a
inoutVariant' = inoutVariant

outVariant :: (Variant a) => ArgOut a
outVariant            = (inoutVariant' defaultVariant)
\end{code}

\begin{code}
inDefaultValue :: VarIn -> ArgIn a -> ArgIn a
inDefaultValue varin_def argin = \ val var -> do
  argin val var
  vt <- readVarEnum var
  case vt of -- to avoid having to define Eq..
    VT_ERROR -> do
      primVARIANTClear var
      varin_def var
    _ -> return ()

defaultMaybe :: Variant a => Maybe a
defaultMaybe = Nothing

inOptional :: VarIn -> ArgIn a -> ArgIn (Maybe a)
inOptional varin_def argin = \val var -> do
   case val of
     Nothing -> varin_def var
     Just v  -> argin v var

inMaybe :: Variant a => ArgIn (Maybe a)
inMaybe Nothing  = inEmpty ()
inMaybe (Just x) = inVariant x

resMaybe :: Variant a => ArgRes (Maybe a)
resMaybe p = 
  catch
    (readVarError p >> return Nothing)
    (\ _ -> fmap Just (resVariant p))

inoutMaybe :: Variant a => ArgInOut (Maybe a) (Maybe a)
inoutMaybe o       = (inMaybe o,resMaybe)

outMaybe :: Variant a => (VarIn,ArgRes (Maybe a))
outMaybe  = inoutMaybe defaultMaybe
\end{code}

\begin{code}
data SqlNull     = SqlNull

defaultSqlNull  :: SqlNull
defaultSqlNull   = SqlNull

inSqlNull :: ArgIn SqlNull
inSqlNull SqlNull p   = writeVarNull p

resSqlNull :: ArgRes SqlNull
resSqlNull p          = readVarNull p >> return SqlNull

inoutSqlNull SqlNull  = (inSqlNull SqlNull,resSqlNull)
outSqlNull            = inoutSqlNull defaultSqlNull
\end{code}

Haskell values (stable ptr's). 

\begin{code}
inHaskellValue :: ArgIn a
inHaskellValue x p = do
   stable <- newStablePtr x
   writeVarStablePtr stable (castPtr p)

unsafeResHaskellValue :: ArgRes a
unsafeResHaskellValue p = do
   stable <- readVarStablePtr p
   deRefStablePtr stable

unsafeOutHaskellValue  =
  ( \ p -> writeVarStablePtr undefinedStablePtr p
  , unsafeResHaskellValue
  )

undefinedStablePtr :: StablePtr a
undefinedStablePtr = unsafePerformIO (newStablePtr undefined)
\end{code}


Convenience QIs - are they really used?

\begin{code}
queryIUnknown :: IID (IUnknown a) -> IUnknown () -> IO (IUnknown a)
queryIUnknown          = queryInterface

queryIDispatch :: IID (IUnknown a) -> IDispatch () -> IO (IUnknown a)
queryIDispatch         = queryInterface
\end{code}

The basic marshalling functions for automation types.

\begin{code}
defaultEmpty :: ()
defaultEmpty            = ()

inNoArg :: ArgIn ()
inNoArg i               = writeVarOptional

inEmpty :: ArgIn ()
inEmpty i               = writeVarEmpty

noInArg :: VarIn
noInArg = inEmpty ()

resEmpty :: ArgRes ()
resEmpty p              = return ()

inoutEmpty e            = (inEmpty e,resEmpty)
outEmpty                = inoutEmpty defaultEmpty

inGUID :: ArgIn GUID
inGUID g = inString (show g)

inoutGUID i           = (inGUID i,resGUID)
outGUID                = inoutGUID nullGUID

resGUID :: ArgRes GUID
resGUID p  = resString p >>= stringToGUID

\end{code}

Integers.

\begin{code}
defaultInt :: Int
defaultInt            = 0

inInt :: ArgIn Int
inInt i               = writeVarInt (fromIntegral i)

resInt :: ArgRes Int
resInt p              = readVarInt p >>= return.fromIntegral

inoutInt i            = (inInt i,resInt)
outInt                = inoutInt defaultInt

defaultInt8 :: Int8
defaultInt8        = 0

inInt8 :: ArgIn Int8
inInt8 i           = writeVarInt (fromIntegral i)

resInt8 :: ArgRes Int8
resInt8 p          = readVarInt p >>= return.fromIntegral

inoutInt8 i        = (inInt8 i,resInt8)
outInt8            = inoutInt8 defaultInt8

defaultInt16 :: Int16
defaultInt16        = 0

inInt16 :: ArgIn Int16
inInt16 i           = writeVarInt (fromIntegral i)

resInt16 :: ArgRes Int16
resInt16 p          = readVarInt p >>= return.fromIntegral

inoutInt16 i        = (inInt16 i,resInt16)
outInt16            = inoutInt16 defaultInt16

defaultInt32 :: Int32
defaultInt32        = 0

inInt32 :: ArgIn Int32
inInt32 i           = writeVarInt i

resInt32 :: ArgRes Int32
resInt32 p          = readVarInt p >>= return

inoutInt32 i        = (inInt32 i,resInt32)
outInt32            = inoutInt32 defaultInt32

defaultHRESULT = defaultInt32
inHRESULT = inInt32
resHRESULT = resInt32
inoutHRESULT = inoutInt32
outHRESULT   = outInt32

{- BEGIN_GHC_ONLY
defaultInt64 :: Int64
   END_GHC_ONLY -}
{- BEGIN_NOT_FOR_GHC -}
defaultInt64 :: Integer
{- END_NOT_FOR_GHC -}
defaultInt64        = 0

inInt64 :: ArgIn Currency
inInt64 = inCurrency

resInt64 :: ArgRes Currency
resInt64 = resCurrency

inoutInt64 i = (inInt64 i,resInt64)
outInt64     = inoutInt64 defaultInt64

defaultInteger = defaultInt64
inInteger      = inInt64
resInteger     = resInt64
inoutInteger   = inoutInt64
outInteger     = outInt64

\end{code}

Words

\begin{code}
defaultWord :: Int
defaultWord            = 0

inWord :: ArgIn Int
inWord i               = writeVarInt (fromIntegral i)

resWord :: ArgRes Int
resWord p              = readVarInt p >>= return.fromIntegral

inoutWord i            = (inInt i,resInt)
outWord                = inoutInt defaultInt

defaultWord8 :: Word8
defaultWord8            = 0

inWord8 :: ArgIn Word8
inWord8 i               = writeVarWord (fromIntegral i)

resWord8 :: ArgRes Word8
resWord8 p              = readVarWord p >>= return.fromIntegral

inoutWord8 i            = (inWord8 i,resWord8)
outWord8                = inoutWord8 defaultWord8

defaultWord16 :: Word16
defaultWord16            = 0

inWord16 :: ArgIn Word16
inWord16 i               = writeVarWord (fromIntegral i)

resWord16 :: ArgRes Word16
resWord16 p              = readVarWord p >>= return.fromIntegral

inoutWord16 i            = (inWord16 i,resWord16)
outWord16                = inoutWord16 defaultWord16

defaultWord32 :: Word32
defaultWord32            = 0

inWord32 :: ArgIn Word32
inWord32 i               = writeVarWord i

resWord32 :: ArgRes Word32
resWord32 p              = readVarWord p

inoutWord32 i            = (inWord32 i,resWord32)
outWord32                = inoutWord32 defaultWord32

{- BEGIN_GHC_ONLY
defaultWord64 :: Word64
   END_GHC_ONLY -}
{- BEGIN_NOT_FOR_GHC -}
defaultWord64 :: Integer
{- END_NOT_FOR_GHC -}
defaultWord64            = 0

{- BEGIN_GHC_ONLY
inWord64 :: ArgIn Word64
   END_GHC_ONLY -}
{- BEGIN_NOT_FOR_GHC -}
inWord64 :: ArgIn Integer
{- END_NOT_FOR_GHC -}
inWord64 f  = 
   let
    (hi,lo) = toInteger f `divMod` (toInteger (maxBound :: Int) + 1)
   in
   writeVarWord64 (fromInteger hi) (fromInteger lo)

{- BEGIN_GHC_ONLY
resWord64 :: ArgRes Word64
   END_GHC_ONLY -}
{- BEGIN_NOT_FOR_GHC -}
resWord64 :: ArgRes Integer
{- END_NOT_FOR_GHC -}
resWord64           = 
   let
    coerceW = fromIntegral
    coerceI = fromIntegral
    
    readWord v = do
      (hi,lo) <- readVarWord64 v
      return (coerceW hi * (coerceI (maxBound :: Int) + 1) + coerceW lo)
   in
   readWord

inoutWord64 i            = (inWord64 i,resWord64)
outWord64                = inoutWord64 defaultWord64

\end{code}

Bytes (yeah, I know, the name of the type was a bit of a give-away :-)

\begin{code}
--type Byte = Char

defaultByte :: Byte
defaultByte            = 0

inByte :: ArgIn Byte
inByte i               = writeVarByte i

resByte :: ArgRes Byte
resByte                = readVarByte

inoutByte i            = (inByte i,resByte)
outByte                = inoutByte defaultByte

defaultChar :: Char
defaultChar            = '\0'

inChar :: ArgIn Char
inChar i               = writeVarByte (fromIntegral (ord i))

resChar :: ArgRes Char
resChar  p             = readVarByte p >>= \ x -> return (chr (fromIntegral x))

inoutChar i            = (inChar i,resChar)
outChar                = inoutChar defaultChar
\end{code}

Booleans.

\begin{code}
defaultBool :: Bool
defaultBool            = False

inBool :: ArgIn Bool
inBool b               = writeVarBool b

resBool :: ArgRes Bool
resBool                = readVarBool

inoutBool b            = (inBool b,resBool)
outBool                = inoutBool defaultBool
\end{code}


Floats.

\begin{code}
defaultFloat :: Float
defaultFloat           = 0.0

inFloat :: ArgIn Float
inFloat f              = writeVarFloat f

resFloat :: ArgRes Float
resFloat               = readVarFloat

inoutFloat b           = (inFloat b,resFloat)
outFloat               = inoutFloat defaultFloat
\end{code}

Doubles.

\begin{code}
defaultDouble :: Double
defaultDouble           = 0.0

inDouble :: ArgIn Double
inDouble f              = writeVarDouble f

resDouble :: ArgRes Double
resDouble               = readVarDouble

inoutDouble b           = (inDouble b,resDouble)
outDouble               = inoutDouble defaultDouble
\end{code}

Dates.

\begin{code}
type Date = Double

defaultDate :: Date
defaultDate           = 0.0

inDate :: ArgIn Date
inDate f              = writeVarDouble f

resDate :: ArgRes Date
resDate               = readVarDouble

inoutDate b           = (inDate b,resDate)
outDate               = inoutDate defaultDate

--
-- clockTimeToDate relies on a non-standard implementation of Time,
-- i.e., one which exports ClockTime non-abstractly.
-- 
clockTimeToDate :: ClockTime -> IO Date
clockTimeToDate (TOD secs _) 
  | secs > fromIntegral (maxBound :: Int) ||
    secs < fromIntegral (minBound :: Int) = 
    ioError (userError "Automation.clockTimeToDate: ClockTime out of range")
  | otherwise = primClockToDate (fromIntegral secs)
\end{code}

Currency:

\begin{code}
type Currency 
{- BEGIN_GHC_ONLY
   = Int64
   END_GHC_ONLY -}
{- BEGIN_NOT_FOR_GHC -}
   = Integer
{- END_NOT_FOR_GHC -}

defaultCurrency :: Currency
defaultCurrency  = 0

-- Note: the Int64/Integer is interpreted literally here,
-- and no account of the implicit scaling that CURRENCY 
-- does is taken into account. ToDo: fix.
inCurrency :: ArgIn Currency
inCurrency f  = 
   let
{- BEGIN_NOT_FOR_GHC -}
    (hi,lo) = f `divMod` (toInteger (maxBound :: Int) + 1)
{- END_NOT_FOR_GHC -}
{- BEGIN_GHC_ONLY
    (hi,lo) = f `divMod` (fromIntegral (maxBound :: Int) + 1)
   END_GHC_ONLY -}
   in
   writeVarCurrency (fromIntegral (fromIntegral hi)) (fromIntegral (fromIntegral lo))


resCurrency :: ArgRes Currency
resCurrency           = 
   let
{- BEGIN_GHC_ONLY
    coerceI = fromIntegral
   END_GHC_ONLY -}
{- BEGIN_NOT_FOR_GHC -}
    coerceI = toInteger
{- END_NOT_FOR_GHC -}
    
    readCur v = do
      (hi,lo) <- readVarCurrency v
      return (coerceI (fromIntegral hi) * (coerceI (maxBound :: Int) + 1) +
	      coerceI (fromIntegral lo))
   in
   readCur

inoutCurrency b       = (inCurrency b, resCurrency)
outCurrency           = inoutCurrency defaultCurrency
\end{code}


Strings.

\begin{code}
defaultString :: String
defaultString            = ""

inString :: ArgIn String
inString s p           = do 
                           pbstr  <- nofreeAllocBSTR s
                           writeVarString (castPtr pbstr) p

resString :: ArgRes String
resString p            = readTempVar readVarString p (\ p -> unmarshallBSTR (castPtr p))

inoutString i            = (inString i,resString)
outString                = inoutString defaultString
\end{code}

Unknown objects.

\begin{code}
defaultIUnknown :: IUnknown a
defaultIUnknown          = interfaceNULL

inIUnknown :: ArgIn (IUnknown a)
inIUnknown u p
  | isNullInterface u = return ()
  | otherwise         = do 
                             u # addRef
                             writeVarUnknown (castIface u) p


resIUnknown :: ArgRes (IUnknown a)
resIUnknown p = 
  readTempVar readVarUnknown p (unmarshallIUnknown True{-finalise-})

inoutIUnknown d          = (inIUnknown d,resIUnknown)
outIUnknown              = inoutIUnknown defaultIUnknown
\end{code}

Dispatch objects.

\begin{code}
defaultIDispatch :: IDispatch a
defaultIDispatch          = interfaceNULL

inIDispatch :: ArgIn (IDispatch a)
inIDispatch d p
  | isNullInterface d = return ()
  | otherwise         = do 
                             d # addRef
                             writeVarDispatch (castIface d) p

resIDispatch :: ArgRes (IDispatch a)
resIDispatch p    = 
   readTempVar readVarDispatch p (unmarshallIUnknown True{-finalise-})

inoutIDispatch d  = (inIDispatch d,resIDispatch)
outIDispatch      = inoutIDispatch defaultIDispatch
\end{code}

Error objects.

\begin{code}
defaultError :: Int32
defaultError         = 0

inError :: ArgIn Int32
inError d p          = writeVarError d p

resError :: ArgRes Int32
resError p           = readVarError p

inoutError d          = (inError d,resError)
outError              = inoutError defaultError
\end{code}

Generic wrappers for Enum instances

\begin{code}
inEnum :: Enum a => ArgIn a
inEnum e = inInt (fromEnum e)

defaultEnum  :: Enum a => a
defaultEnum  = toEnum 0

resEnum      :: Enum a => ArgRes a
resEnum p    = readVarInt p >>= return.toEnum.fromIntegral
inoutEnum    :: Enum a => ArgInOut a a
inoutEnum i  = (inEnum i,resEnum)
outEnum      :: Enum a => ArgOut a
outEnum      = inoutEnum defaultEnum
vtTypeEnum   :: Enum a => a -> VARENUM
vtTypeEnum _ = VT_I4

{- Support for overlapping instances required
   to compile this one - let's not demand that 
   being supported for now.
   
   If you do uncomment this one, you probably
   also want to invoke the IDL compiler with
   -fno-variant-enum-instances.

instance Enum a => Variant a where
  inVariant   = inEnum
  resVariant  = resEnum
  defaultVariant = defaultEnum

  vtEltType _ = VT_I4
-}
\end{code}

Setting and Getting properties: @getVisible = propertyGet "Visible" [] outBool@.

\begin{code}
propertyGet :: Member -> [VarIn] -> ArgOut a -> IDispatch d -> IO a
propertyGet member argsin argout obj
      = do dispid <- obj # getMemberID member
           propertyGetID dispid argsin argout obj

propertyGet2 :: Member -> [VarIn] -> ArgOut a1 -> ArgOut a2 -> IDispatch d -> IO (a1,a2)
propertyGet2 member argsin argout1 argout2 obj
      = do dispid <- obj # getMemberID member
           propertyGet2ID dispid argsin argout1 argout2 obj

propertyGet3 :: Member -> [VarIn] -> ArgOut a1 -> ArgOut a2 -> ArgOut a3 -> IDispatch d -> IO (a1,a2,a3)
propertyGet3 member argsin argout1 argout2 argout3 obj
      = do dispid <- obj # getMemberID member
           propertyGet3ID dispid argsin argout1 argout2 argout3 obj

propertyGet4 :: Member -> [VarIn] -> ArgOut a1 -> ArgOut a2 -> ArgOut a3 -> ArgOut a4 -> IDispatch d -> IO (a1,a2,a3,a4)
propertyGet4 member argsin argout1 argout2 argout3 argout4 obj
      = do dispid <- obj # getMemberID member
           propertyGet4ID dispid argsin argout1 argout2 argout3 argout4 obj

propertySet :: Member -> [VarIn] -> IDispatch d -> IO ()
propertySet member argsin obj
      = do dispid <- obj # getMemberID member
           propertySetID dispid argsin obj

propertySetGet :: Member -> [VarIn] -> ArgOut a -> IDispatch d -> IO a
propertySetGet member argsin argout obj
      = do dispid <- obj # getMemberID member
           propertySetGetID dispid argsin argout obj

propertyGetID :: DISPID -> [VarIn] -> ArgOut a -> IDispatch d -> IO a
propertyGetID dispid argsin (varin,argres) obj
      = do p <- obj # invokePropertyGet dispid argsin [varin]
           unmarshallVariants1 argres p

propertyGet2ID :: DISPID -> [VarIn] -> ArgOut a1 -> ArgOut a2 -> IDispatch d -> IO (a1,a2)
propertyGet2ID dispid argsin (varin1,argres1) (varin2,argres2) obj
      = do p <- obj # invokePropertyGet dispid argsin [varin1,varin2]
           unmarshallVariants2 argres1 argres2 p

propertyGet3ID :: DISPID -> [VarIn] -> ArgOut a1 -> ArgOut a2 -> ArgOut a3 -> IDispatch d -> IO (a1,a2,a3)
propertyGet3ID dispid argsin (varin1,argres1) (varin2,argres2) (varin3,argres3) obj
      = do p <- obj # invokePropertyGet dispid argsin [varin1,varin2,varin3]
           unmarshallVariants3 argres1 argres2 argres3 p

propertyGet4ID :: DISPID -> [VarIn] -> ArgOut a1 -> ArgOut a2 -> ArgOut a3 -> ArgOut a4 -> IDispatch d -> IO (a1,a2,a3,a4)
propertyGet4ID dispid argsin (varin1,argres1) (varin2,argres2) (varin3,argres3) (varin4,argres4) obj
      = do p <- obj # invokePropertyGet dispid argsin [varin1,varin2,varin3,varin4]
           unmarshallVariants4 argres1 argres2 argres3 argres4 p

propertySetID :: DISPID -> [VarIn] -> IDispatch d -> IO ()
propertySetID dispid argsin obj
      = do p <- obj # invokePropertySet dispid argsin []
           unmarshallVariants0 p

propertySetGetID :: DISPID -> [VarIn] -> ArgOut a -> IDispatch d -> IO a
propertySetGetID dispid argsin (varin,argres) obj
      = do p <- obj # invokePropertySet dispid argsin [varin]
           unmarshallVariants1 argres p
\end{code}

Methods and functions are defined using @method@/@funtion@. The digit
appended to the name gives the number of results.
For example: @confirm msg  = function1 "Confirm" [inString msg] outBool@.

\begin{code}
method0 :: Member
        -> [VarIn]
	-> IDispatch i
	-> IO ()
method0 member args obj = do
   dispid <- obj # getMemberID member
   catchMethError member (methodID0 dispid args obj)

method1 :: Member
        -> [VarIn]
	-> ArgOut a1
	-> IDispatch i
	-> IO a1
method1 member args argout obj = do
   dispid <- obj # getMemberID member
   catchMethError member (methodID1 dispid args argout obj)

method2 :: Member
        -> [VarIn]
	-> ArgOut a1
	-> ArgOut a2
	-> IDispatch i
	-> IO (a1,a2)
method2 member args argout1 argout2 obj = do
   dispid <- obj # getMemberID member
   catchMethError member (methodID2 dispid args argout1 argout2 obj)

method3 :: Member
        -> [VarIn]
	-> ArgOut a1 -> ArgOut a2 -> ArgOut a3
        -> IDispatch i -> IO (a1,a2,a3)
method3 member args argout1 argout2 argout3 obj = do 
    dispid <- obj # getMemberID member
    catchMethError member (methodID3 dispid args argout1 argout2 argout3 obj)

method4 :: Member
        -> [VarIn]
	-> ArgOut a1 -> ArgOut a2 
	-> ArgOut a3 -> ArgOut a4
        -> IDispatch i -> IO (a1,a2,a3,a4)
method4 member args argout1 argout2 argout3 argout4 obj = do
    dispid <- obj # getMemberID member
    catchMethError member $
      methodID4 dispid args argout1 argout2 argout3 argout4 obj

method5 :: Member 
        -> [VarIn]
	-> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	-> ArgOut a4 -> ArgOut a5
        -> IDispatch i -> IO (a1,a2,a3,a4,a5)
method5 member args argout1 argout2 argout3 argout4 argout5 obj = do
    dispid <- obj # getMemberID member
    catchMethError member $
      methodID5 dispid args argout1 argout2 argout3 argout4 argout5 obj

method6 :: Member 
        -> [VarIn]
	-> ArgOut a1 -> ArgOut a2 -> ArgOut a3 
	-> ArgOut a4 -> ArgOut a5 -> ArgOut a6
        -> IDispatch i
	-> IO (a1,a2,a3,a4,a5,a6)
method6 member args argout1 argout2 argout3 argout4 argout5 argout6 obj = do
    dispid <- obj # getMemberID member
    catchMethError member $
      methodID6 dispid args argout1 argout2 argout3
                            argout4 argout5 argout6 obj

method7 :: Member 
        -> [VarIn]
	-> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	-> ArgOut a4 -> ArgOut a5 -> ArgOut a6 -> ArgOut a7
        -> IDispatch i -> IO (a1, a2, a3, a4, a5, a6, a7)
method7 member args argout1 argout2 argout3 argout4 argout5 argout6 argout7 obj = do
    dispid <- obj # getMemberID member
    catchMethError member $
          methodID7 dispid args argout1 argout2 argout3
	                        argout4 argout5 argout6 argout7 obj

method8 :: Member
        -> [VarIn]
	-> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	-> ArgOut a4 -> ArgOut a5 -> ArgOut a6
	-> ArgOut a7 -> ArgOut a8
        -> IDispatch i 
	-> IO (a1, a2, a3, a4, a5, a6, a7, a8)
method8 member args argout1 argout2 argout3 
                    argout4 argout5 argout6 
		    argout7 argout8 obj = do
    dispid <- obj # getMemberID member
    catchMethError member $
      methodID8 dispid args argout1 argout2 argout3 
                            argout4 argout5 argout6 
			    argout7 argout8 obj

\end{code}

Methods invoked on DISPID.

\begin{code}
methodID0 :: DISPID
          -> [VarIn]
	  -> IDispatch i
	  -> IO ()
methodID0 dispid args obj = do
   p <- obj # invokeMethod dispid args []
   unmarshallVariants0 p

methodID1 :: DISPID
          -> [VarIn]
	  -> ArgOut a1
	  -> IDispatch i
	  -> IO a1
methodID1 dispid args (varin,argres) obj = do
   p <- obj # invokeMethod dispid args [varin]
   unmarshallVariants1 argres p

methodID2 :: DISPID
          -> [VarIn]
          -> ArgOut a1 -> ArgOut a2
	  -> IDispatch i
	  -> IO (a1,a2)
methodID2 dispid args (varin1,argres1) (varin2,argres2) obj = do
   p <- obj # invokeMethod dispid args [varin1,varin2]
   unmarshallVariants2 argres1 argres2 p

methodID3 :: DISPID
          -> [VarIn]
          -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	  -> IDispatch i
	  -> IO (a1,a2,a3)
methodID3 dispid args (varin1,argres1) (varin2,argres2) (varin3,argres3) obj = do
   p <- obj # invokeMethod dispid args [varin1,varin2,varin3]
   unmarshallVariants3 argres1 argres2 argres3 p

methodID4 :: DISPID
          -> [VarIn]
          -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	  -> ArgOut a4
          -> IDispatch i
	  -> IO (a1,a2,a3,a4)
methodID4 dispid args (varin1,argres1) (varin2,argres2)
                      (varin3,argres3) (varin4,argres4) obj = do
   p <- obj # invokeMethod dispid args [varin1,varin2,varin3,varin4]
   unmarshallVariants4 argres1 argres2 argres3 argres4 p

methodID5 :: DISPID
          -> [VarIn]
          -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	  -> ArgOut a4 -> ArgOut a5
          -> IDispatch i 
	  -> IO (a1,a2,a3,a4,a5)
methodID5 dispid args (varin1,argres1) (varin2,argres2)
                      (varin3,argres3) (varin4,argres4)
		      (varin5,argres5) obj = do
   p <- obj # invokeMethod dispid args [varin1,varin2,varin3,varin4,varin5]
   unmarshallVariants5 argres1 argres2 argres3 argres4 argres5 p


methodID6 :: DISPID
          -> [VarIn]
          -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	  -> ArgOut a4 -> ArgOut a5 -> ArgOut a6
          -> IDispatch i
	  -> IO (a1,a2,a3,a4,a5,a6)
methodID6 dispid args (varin1,argres1) (varin2,argres2)
                      (varin3,argres3) (varin4,argres4)
		      (varin5,argres5) (varin6,argres6) obj = do
   p <- obj # invokeMethod dispid args [varin1,varin2,varin3,varin4,varin5,varin6]
   unmarshallVariants6 argres1 argres2 argres3 argres4 argres5 argres6 p

methodID7 :: DISPID
          -> [VarIn]
          -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	  -> ArgOut a4 -> ArgOut a5 -> ArgOut a6
	  -> ArgOut a7
          -> IDispatch i 
	  -> IO (a1,a2,a3,a4,a5,a6,a7)
methodID7 dispid args (varin1,argres1) (varin2,argres2)
                      (varin3,argres3) (varin4,argres4)
		      (varin5,argres5) (varin6,argres6)
		      (varin7,argres7) obj = do
   p <- obj # invokeMethod dispid args [varin1,varin2,varin3,varin4,varin5,varin6,varin7]
   unmarshallVariants7 argres1 argres2 argres3 argres4 argres5 argres6 argres7 p

methodID8 :: DISPID
          -> [VarIn]
          -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	  -> ArgOut a4 -> ArgOut a5 -> ArgOut a6
	  -> ArgOut a7 -> ArgOut a8
	  -> IDispatch i
	  -> IO (a1,a2,a3,a4,a5,a6,a7,a8)
methodID8 dispid args (varin1,argres1) (varin2,argres2)
                      (varin3,argres3) (varin4,argres4)
		      (varin5,argres5) (varin6,argres6)
		      (varin7,argres7) (varin8,argres8) obj = do
   p <- obj # invokeMethod dispid args [varin1,varin2,varin3,varin4,varin5,varin6,varin7,varin8]
   unmarshallVariants8 argres1 argres2 argres3 argres4 argres5 argres6 argres7 argres8 p

\end{code}

Functions. Of course @function0@ is missing. The difference with
methods is that functions expect the last @out@ argument to be
a result (@retval@) instead of a real @out@ argument.

\begin{code}
function1 :: Member
          -> [VarIn]
	  -> ArgOut a1
	  -> IDispatch i
	  -> IO a1
function1 member args argout obj = do
   dispid <- obj # getMemberID member
   catchMethError member (functionID1 dispid args argout obj)

function2 :: Member
          -> [VarIn]
	  -> ArgOut a1 -> ArgOut a2
	  -> IDispatch i
	  -> IO (a1,a2)
function2 member args argout1 argout2 obj = do
   dispid <- obj # getMemberID member
   catchMethError member (functionID2 dispid args argout1 argout2 obj)

function3 :: Member
          -> [VarIn]
	  -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	  -> IDispatch i
	  -> IO (a1,a2,a3)
function3 member args argout1 argout2 argout3 obj = do
   dispid <- obj # getMemberID member
   catchMethError member (functionID3 dispid args argout1 argout2 argout3 obj)

function4 :: Member
          -> [VarIn]
	  -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	  -> ArgOut a4
	  -> IDispatch i
	  -> IO (a1,a2,a3,a4)
function4 member args argout1 argout2 argout3 argout4 obj = do
   dispid <- obj # getMemberID member
   catchMethError member (functionID4 dispid args argout1 argout2 argout3 argout4 obj)

function5 :: Member
          -> [VarIn]
	  -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	  -> ArgOut a4 -> ArgOut a5
	  -> IDispatch i
	  -> IO (a1,a2,a3,a4,a5)
function5 member args argout1 argout2 argout3 
		      argout4 argout5 obj = do
   dispid <- obj # getMemberID member
   catchMethError member $
     functionID5 dispid args argout1 argout2 argout3 argout4 argout5 obj

function6 :: Member
          -> [VarIn]
	  -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	  -> ArgOut a4 -> ArgOut a5 -> ArgOut a6
	  -> IDispatch i
	  -> IO (a1,a2,a3,a4,a5,a6)
function6 member args argout1 argout2 argout3 
		      argout4 argout5 argout6 obj = do
   dispid <- obj # getMemberID member
   catchMethError member $
     functionID6 dispid args argout1 argout2 argout3 argout4 argout5 argout6 obj

functionID1 :: DISPID
            -> [VarIn]
	    -> ArgOut a1
	    -> IDispatch i
	    -> IO a1
functionID1 dispid args (varin,argres) obj = do
   p <- obj # invokeFunction dispid args [varin]
   unmarshallVariants1 argres p

functionID2 :: DISPID
            -> [VarIn]
            -> ArgOut a1 -> ArgOut a2
	    -> IDispatch i
	    -> IO (a1,a2)
functionID2 dispid args (varin1,argres1) (varin2,argres2) obj = do
   p <- obj # invokeFunction dispid args [varin1,varin2]
   unmarshallVariants2 argres1 argres2 p

functionID3 :: DISPID
            -> [VarIn]
            -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	    -> IDispatch i
	    -> IO (a1,a2,a3)
functionID3 dispid args (varin1,argres1) (varin2,argres2)
                        (varin3,argres3) obj = do
   p <- obj # invokeFunction dispid args [varin1,varin2,varin3]
   unmarshallVariants3 argres1 argres2 argres3 p

functionID4 :: DISPID
            -> [VarIn]
            -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	    -> ArgOut a4 
	    -> IDispatch i
	    -> IO (a1,a2,a3,a4)
functionID4 dispid args (varin1,argres1) (varin2,argres2)
                        (varin3,argres3) (varin4,argres4) obj = do
   p <- obj # invokeFunction dispid args [varin1,varin2,varin3,varin4]
   unmarshallVariants4 argres1 argres2 argres3 argres4 p

functionID5 :: DISPID
            -> [VarIn]
            -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	    -> ArgOut a4 -> ArgOut a5
            -> IDispatch i
	    -> IO (a1,a2,a3,a4,a5)
functionID5 dispid args (varin1,argres1) (varin2,argres2)
                        (varin3,argres3) (varin4,argres4)
			(varin5,argres5) obj = do
   p <- obj # invokeFunction dispid args [varin1,varin2,varin3,varin4,varin5]
   unmarshallVariants5 argres1 argres2 argres3 argres4 argres5 p

functionID6 :: DISPID
            -> [VarIn]
            -> ArgOut a1 -> ArgOut a2 -> ArgOut a3
	    -> ArgOut a4 -> ArgOut a5 -> ArgOut a6
            -> IDispatch i
	    -> IO (a1,a2,a3,a4,a5,a6)
functionID6 dispid args (varin1,argres1) (varin2,argres2)
                        (varin3,argres3) (varin4,argres4)
			(varin5,argres5) (varin6,argres6) obj = do
   p <- obj # invokeFunction dispid args [varin1,varin2,varin3,varin4,varin5,varin6]
   unmarshallVariants6 argres1 argres2 argres3 argres4 argres5 argres6 p

\end{code}

Error reporting:

\begin{code}
errorMember :: String -> IOError -> IO a
errorMember member err
      = coFail ("method '" ++ member ++ "': " ++ (coGetErrorString err))

catchMethError :: Member -> IO a -> IO a
catchMethError member act = catch act (errorMember member)

\end{code}

Unmarshall the @out@ arguments.

\begin{code}
unmarshallVariants0 p 
      = readVariants0 p `always` freeMemVariants 0 p

unmarshallVariants1 a p
      = readVariants1 a p `always` freeMemVariants 1 p

unmarshallVariants2   a b p
      = readVariants2 a b p `always` freeMemVariants 2 p

unmarshallVariants3   a b c p
      = readVariants3 a b c p `always` freeMemVariants 3 p

unmarshallVariants4   a b c d p
      = readVariants4 a b c d p `always` freeMemVariants 4 p

unmarshallVariants5   a b c d e p
      = readVariants5 a b c d e p `always` freeMemVariants 5 p

unmarshallVariants6   a b c d e f p
      = readVariants6 a b c d e f p `always` freeMemVariants 6 p

unmarshallVariants7   a b c d e f g p
      = readVariants7 a b c d e f g p `always` freeMemVariants 7 p

unmarshallVariants8   a b c d e f g h p
      = readVariants8 a b c d e f g h p `always` freeMemVariants 8 p


readVariants0 :: VARIANT -> IO ()
readVariants0 p = return ()

readVariants1 :: ArgRes a -> VARIANT  -> IO a
readVariants1 f p = f p

readVariants2 :: ArgRes a -> ArgRes b -> VARIANT  -> IO (a,b)
readVariants2 f g p
      = do y <- g p
           x <- f (p `addNCastPtr` sizeofVARIANT)
           return (x,y)

readVariants3 :: ArgRes a -> ArgRes b -> ArgRes c
                      -> VARIANT  -> IO (a,b,c)
readVariants3 f g h p
      = do z <- h p
           y <- g (p `addNCastPtr` sizeofVARIANT)
           x <- f (p `addNCastPtr` (2*sizeofVARIANT))
           return (x,y,z)

readVariants4 :: ArgRes a -> ArgRes b -> ArgRes c -> ArgRes d
                      -> VARIANT  -> IO (a,b,c,d)
readVariants4 f g h i p
      = do z <- i p
           y <- h (p `addNCastPtr` sizeofVARIANT)
           x <- g (p `addNCastPtr` (2*sizeofVARIANT))
           w <- f (p `addNCastPtr` (3*sizeofVARIANT))
           return (w,x,y,z)

readVariants5 :: ArgRes a -> ArgRes b -> ArgRes c -> ArgRes d -> ArgRes e
                      -> VARIANT  -> IO (a,b,c,d,e)
readVariants5 f g h i j p
      = do z <- j p
           y <- i (p `addNCastPtr` sizeofVARIANT)
           x <- h (p `addNCastPtr` (2*sizeofVARIANT))
           w <- g (p `addNCastPtr` (3*sizeofVARIANT))
           v <- f (p `addNCastPtr` (4*sizeofVARIANT))
           return (v,w,x,y,z)

readVariants6 :: ArgRes a1 
	      -> ArgRes a2
	      -> ArgRes a3
	      -> ArgRes a4
	      -> ArgRes a5
	      -> ArgRes a6
              -> VARIANT
	      -> IO (a1,a2,a3,a4,a5,a6)
readVariants6 f1 f2 f3 f4 f5 f6 p
      = do v6 <- f6 p
           v5 <- f5 (p `addNCastPtr` sizeofVARIANT)
           v4 <- f4 (p `addNCastPtr` (2*sizeofVARIANT))
           v3 <- f3 (p `addNCastPtr` (3*sizeofVARIANT))
           v2 <- f2 (p `addNCastPtr` (4*sizeofVARIANT))
           v1 <- f1 (p `addNCastPtr` (5*sizeofVARIANT))
           return (v1,v2,v3,v4,v5,v6)

readVariants7 :: ArgRes a1 -> ArgRes a2 -> ArgRes a3 -> ArgRes a4 -> ArgRes a5 
                      -> ArgRes a6 -> ArgRes a7
                      -> VARIANT  -> IO (a1,a2,a3,a4,a5,a6,a7)
readVariants7 f1 f2 f3 f4 f5 f6 f7 p
      = do v7 <- f7 p
           v6 <- f6 (p `addNCastPtr` sizeofVARIANT)
           v5 <- f5 (p `addNCastPtr` (2*sizeofVARIANT))
           v4 <- f4 (p `addNCastPtr` (3*sizeofVARIANT))
           v3 <- f3 (p `addNCastPtr` (4*sizeofVARIANT))
           v2 <- f2 (p `addNCastPtr` (5*sizeofVARIANT))
           v1 <- f1 (p `addNCastPtr` (6*sizeofVARIANT))
           return (v1,v2,v3,v4,v5,v6,v7)

readVariants8:: ArgRes a1 -> ArgRes a2 -> ArgRes a3 -> ArgRes a4 -> ArgRes a5 
                      -> ArgRes a6 -> ArgRes a7 -> ArgRes a8
                      -> VARIANT  -> IO (a1,a2,a3,a4,a5,a6,a7,a8)
readVariants8 f1 f2 f3 f4 f5 f6 f7 f8 p
      = do v8 <- f8 p
           v7 <- f7 (p `addNCastPtr` sizeofVARIANT)
           v6 <- f6 (p `addNCastPtr` (2*sizeofVARIANT))
           v5 <- f5 (p `addNCastPtr` (3*sizeofVARIANT))
           v4 <- f4 (p `addNCastPtr` (4*sizeofVARIANT))
           v3 <- f3 (p `addNCastPtr` (5*sizeofVARIANT))
           v2 <- f2 (p `addNCastPtr` (6*sizeofVARIANT))
           v1 <- f1 (p `addNCastPtr` (7*sizeofVARIANT))
           return (v1,v2,v3,v4,v5,v6,v7,v8)

{- UNUSED:
unmarshallVariantList :: [ArgRes a] -> VARIANT  -> IO [a]
unmarshallVariantList fls p = 
   (go p fls []) `always` freeMemVariants len p
  where
    len = length fls

    go p [] acc     = return acc
    go p (f:fs) acc = do
        v <- f p
        go (p `addNCastPtr` sizeofVARIANT) fs (v:acc)
-}
\end{code}

@invokeMethod/Function@ and @propertyGet/Set@ all use the primitive
@primInvokeMethod@.

\begin{code}
invokePropertyGet     = primInvokeMethod dispPROPERTYGET True
invokePropertySet     = primInvokeMethod dispPROPERTYSET False
invokeMethod          = primInvokeMethod dispMETHOD False
invokeFunction        = primInvokeMethod dispMETHOD True
\end{code}

Some constants used with the invoke functions.

\begin{code}
type DispAction         = Word32

dispMETHOD :: Word32
dispMETHOD              = 1
dispPROPERTYGET :: Word32
dispPROPERTYGET         = 2
dispPROPERTYSET :: Word32
dispPROPERTYSET         = 4
dispPROPERTYSETREF :: Word32
dispPROPERTYSETREF      = 8

lcidNeutral :: Word32
lcidNeutral             = 0
\end{code}

The primitive invokation mechanism. Exceptions are directed to the normal
@coFail@ function.

\begin{code}
primInvokeMethod :: DispAction 
	         -> Bool
		 -> DISPID
                 -> [VarIn] -> [VarIn]
                 -> IDispatch d 
		 -> IO (VARIANT)
primInvokeMethod action isfunction dispid argin argout iptr
      = let cargsout = fromIntegral (length argout)
            cargs    = cargsout + fromIntegral (length argin)
        in
           stackFrame (fromIntegral (sizeofVARIANT * fromIntegral cargs)) $ \ pargs ->
        do
           pargout      <- allocMemory (fromIntegral $ sizeofVARIANT * fromIntegral cargsout)
           let pargin   = pargs `addNCastPtr` (sizeofVARIANT * fromIntegral cargsout)

           writeSeqAtDec (fromIntegral sizeofVARIANT) argin  pargin
           writeSeqAtDec (fromIntegral sizeofVARIANT) argout pargout

           (pinfo,hr) <- dispatchInvoke (castIface iptr)
                              dispid lcidNeutral isfunction
                              action (fromIntegral cargs) 
			      cargsout
                              pargs pargout

           if (succeeded hr)
            then return pargout
            else if hr == dISP_E_EXCEPTION
                  then do 
                          pstr <- getExcepInfoMessage pinfo
			  str  <- unmarshallString (castPtr pstr)
			  coFree pstr
                          freeExcepInfo pinfo
                          freeMemory pinfo
                          freeMemVariants cargsout pargout
                          coFail str
                  else do 
                          putMessage "invoke failed"
                          freeMemVariants cargsout pargout
                          coFailHR hr
\end{code}

Some helper functions for @Variants@.

\begin{code}
readTempVar :: (VARIANT -> IO (Ptr (Ptr b), Ptr (VARIANT)))
	    -> VARIANT
	    -> (Ptr b -> IO d)
	    -> IO d
readTempVar io p f
      = do (x,v) <- io p
           x <- readPtr x  -- we always get a ty* back, so dereference it before using.
           f x  `always` (freeVariants 1 (castPtr v) >> free v) 
	       -- _don't_ use freeMemVariants, as it ends up
	       -- calling freeMemory (==CoTaskMemFree()),
	       -- which isn't right ('v' is allocated by malloc()).

freeMemVariants count p = do
      freeVariants count p
      freeMemory p

\end{code}

Marshall BSTR values. @allocBSTR@ is called @primAllocBSTR@
since it doesn't take care of freeing the string. (If we
just had true foreign objects: @mkPointer xbstr freeBSTR@.)

\begin{code}
allocBSTR :: String -> IO (Ptr String)
allocBSTR s             = stackString s $ \ _ pstr -> do
                            ptr <- stringToBSTR (castPtr pstr)
			    readPtr ptr

nofreeAllocBSTR :: String -> IO (Ptr String)
nofreeAllocBSTR s       = stackString s $ \ _ pstr -> do
                            ptr <- nofreeBstrFromString (castPtr pstr)
			    return ptr
--			    makePointer finalFreeBSTR ptr

nofreeBstrFromString :: Ptr String -> IO (Ptr String)
nofreeBstrFromString str = do
   ptr <- stringToBSTR str
   readPtr ptr

data EnumVARIANT a      = EnumVARIANT
type IEnumVARIANT a     = IUnknown (EnumVARIANT a)
iidIEnumVARIANT :: IID (IEnumVARIANT ())
iidIEnumVARIANT = mkIID "{00020404-0000-0000-C000-000000000046}"

newEnum :: IDispatch a -> IO (Int, IEnumVARIANT b)
newEnum ip = do
  iunk  <- ip   # function1 "_NewEnum" [] outIUnknown
  ienum <- iunk # queryInterface iidIEnumVARIANT
  len   <- ip   # propertyGet "length" [] outInt
  return (len, castIface ienum)
  

enumVariants :: Variant a => IDispatch b -> IO [a]
enumVariants ip = do
     (len, ienum) <- newEnum ip
     enumNext (fromIntegral sizeofVARIANT) resVariant (fromIntegral len) ienum

\end{code}

Helpers

\begin{code}
always :: IO a -> IO () -> IO a
always io action = do
  x <- io `catch` (\ e -> action >> ioError e)
  action
  return x

\end{code}

\begin{code}
{- BEGIN_GHC_ONLY
marshallCurrency = marshallInt64
unmarshallCurrency = unmarshallInt64
readCurrency = readInt64
writeCurrency = writeInt64
   END_GHC_ONLY -}
sizeofCurrency = sizeofInt64

\end{code}

\begin{code}
data VARENUM
 = VT_EMPTY
 | VT_NULL
 | VT_I2
 | VT_I4
 | VT_R4
 | VT_R8
 | VT_CY
 | VT_DATE
 | VT_BSTR
 | VT_DISPATCH
 | VT_ERROR
 | VT_BOOL
 | VT_VARIANT
 | VT_UNKNOWN
 | VT_DECIMAL
 | VT_I1
 | VT_UI1
 | VT_UI2
 | VT_UI4
 | VT_I8
 | VT_UI8
 | VT_INT
 | VT_UINT
 | VT_VOID
 | VT_HRESULT
 | VT_PTR
 | VT_SAFEARRAY
 | VT_CARRAY
 | VT_USERDEFINED
 | VT_LPSTR
 | VT_LPWSTR
 | VT_FILETIME
 | VT_BLOB
 | VT_STREAM
 | VT_STORAGE
 | VT_STREAMED_OBJECT
 | VT_STORED_OBJECT
 | VT_BLOB_OBJECT
 | VT_CF
 | VT_CLSID
 | VT_BSTR_BLOB
 | VT_VECTOR
 | VT_ARRAY
 | VT_BYREF
 | VT_RESERVED
 | VT_ILLEGAL
 | VT_ILLEGALMASKED
 | VT_TYPEMASK
   deriving ( Eq, Show )
 
instance Enum VARENUM where
  fromEnum vt = 
   case vt of 
     VT_EMPTY ->  0
     VT_NULL ->  1
     VT_I2 ->  2
     VT_I4 ->  3
     VT_R4 ->  4
     VT_R8 ->  5
     VT_CY ->  6
     VT_DATE ->  7
     VT_BSTR ->  8
     VT_DISPATCH ->  9
     VT_ERROR ->  10
     VT_BOOL ->  11
     VT_VARIANT ->  12
     VT_UNKNOWN ->  13
     VT_DECIMAL ->  14
     VT_I1 ->  16
     VT_UI1 ->  17
     VT_UI2 ->  18
     VT_UI4 ->  19
     VT_I8 ->  20
     VT_UI8 ->  21
     VT_INT ->  22
     VT_UINT ->  23
     VT_VOID ->  24
     VT_HRESULT ->  25
     VT_PTR ->  26
     VT_SAFEARRAY ->  27
     VT_CARRAY ->  28
     VT_USERDEFINED ->  29
     VT_LPSTR ->  30
     VT_LPWSTR ->  31
     VT_FILETIME ->  64
     VT_BLOB ->  65
     VT_STREAM ->  66
     VT_STORAGE ->  67
     VT_STREAMED_OBJECT ->  68
     VT_STORED_OBJECT ->  69
     VT_BLOB_OBJECT ->  70
     VT_CF ->  71
     VT_CLSID ->  72
     VT_BSTR_BLOB ->  4095
     VT_VECTOR ->  4096
     VT_ARRAY ->  8192
     VT_BYREF ->  16384
     VT_RESERVED ->  32768
     VT_ILLEGAL ->  65535
     VT_ILLEGALMASKED ->  4095
     VT_TYPEMASK ->  4095

  toEnum v =
   case v of
     0 -> VT_EMPTY
     1 -> VT_NULL
     2 -> VT_I2
     3 -> VT_I4
     4 -> VT_R4
     5 -> VT_R8
     6 -> VT_CY
     7 -> VT_DATE
     8 -> VT_BSTR
     9 -> VT_DISPATCH
     10 -> VT_ERROR
     11 -> VT_BOOL
     12 -> VT_VARIANT
     13 -> VT_UNKNOWN
     14 -> VT_DECIMAL
     16 -> VT_I1
     17 -> VT_UI1
     18 -> VT_UI2
     19 -> VT_UI4
     20 -> VT_I8
     21 -> VT_UI8
     22 -> VT_INT
     23 -> VT_UINT
     24 -> VT_VOID
     25 -> VT_HRESULT
     26 -> VT_PTR
     27 -> VT_SAFEARRAY
     28 -> VT_CARRAY
     29 -> VT_USERDEFINED
     30 -> VT_LPSTR
     31 -> VT_LPWSTR
     64 -> VT_FILETIME
     65 -> VT_BLOB
     66 -> VT_STREAM
     67 -> VT_STORAGE
     68 -> VT_STREAMED_OBJECT
     69 -> VT_STORED_OBJECT
     70 -> VT_BLOB_OBJECT
     71 -> VT_CF
     72 -> VT_CLSID
     4095 -> VT_BSTR_BLOB
     4096 -> VT_VECTOR
     8192 -> VT_ARRAY
     16384 -> VT_BYREF
     32768 -> VT_RESERVED
     65535 -> VT_ILLEGAL
     4095 -> VT_ILLEGALMASKED
     4095 -> VT_TYPEMASK
     _   
       | v' .&. 26    == 26    -> VT_PTR   -- ho-hum.
       | v' .&. 8192  == 8192  -> VT_ARRAY -- ho-hum.
       | v' .&. 16384 == 16384 -> toEnum (v-16384) -- drop the VT_BYREF flag.
       | otherwise -> error ("unmarshallVARENUM: illegal enum value " ++ show v)
   where
     v' = (fromIntegral v :: Int32)

unmarshallVARENUM :: Int16 -> IO VARENUM
unmarshallVARENUM v = return (toEnum (fromIntegral v))

marshallVARENUM :: VARENUM -> IO Int16
marshallVARENUM v = return (fromIntegral (fromEnum v))

writeVARENUM :: Ptr Int16 -> VARENUM -> IO ()
writeVARENUM = HDirect.writeenum16 marshallVARENUM

readVARENUM :: Ptr Int16 -> IO VARENUM
readVARENUM = HDirect.readenum16 unmarshallVARENUM

sizeofVARENUM :: Word32
sizeofVARENUM = sizeofInt16

sizeofVARIANT_BOOL :: Word32
sizeofVARIANT_BOOL = sizeofInt16

marshallVARIANT_BOOL :: Bool -> IO Int16
marshallVARIANT_BOOL True  = return minBound
marshallVARIANT_BOOL False = return 0

unmarshallVARIANT_BOOL :: Int16 -> IO Bool
unmarshallVARIANT_BOOL 0  = return False
unmarshallVARIANT_BOOL _  = return True

writeVARIANT_BOOL :: Ptr Int16 -> Bool -> IO ()
writeVARIANT_BOOL ptr v = marshallVARIANT_BOOL v >>= writeInt16 ptr

readVARIANT_BOOL :: Ptr Int16 -> IO Bool
readVARIANT_BOOL ptr = do
  x <- readInt16 ptr
  unmarshallVARIANT_BOOL x

vARIANT_TRUE :: Int
vARIANT_TRUE = -1

vARIANT_FALSE :: Int
vARIANT_FALSE = 0

readVarEnum :: VARIANT -> IO VARENUM
readVarEnum v = do
  vt <- readVariantTag v
  return (toEnum (fromIntegral vt))

\end{code}

\begin{code}
data SafeArray a = SA SAFEARRAY

mkSafeArray :: (Variant a) => SAFEARRAY -> SafeArray a
mkSafeArray s = SA s

defaultSafeArray :: Variant a => SafeArray a
defaultSafeArray = SA (addrToSAFEARRAY nullPtr)

inSafeArray :: Variant a => ArgIn (SafeArray a)
inSafeArray s  = inSafe' undefined s

-- type hack.
inSafe' :: Variant a => a -> ArgIn (SafeArray a)
inSafe' b (SA s)  p = writeVarSAFEARRAY p s (fromIntegral (fromEnum (vtEltType b)))

inSAFEARRAY :: ArgIn SAFEARRAY
inSAFEARRAY s p = writeVarSAFEARRAY p s (fromIntegral (fromEnum VT_VARIANT))


resSafeArray :: Variant a => ArgRes (SafeArray a)
resSafeArray p       = resSafe' undefined p 

resSafe' :: Variant a => a -> ArgRes (SafeArray a)
resSafe' vt p = do
	    x <- readVarSAFEARRAY (castPtr p) (fromIntegral (fromEnum (vtEltType vt)))
	    s <- doThenFree free (readSAFEARRAY True) (castPtr x)
	    return (SA s)

resSAFEARRAY :: ArgRes SAFEARRAY
resSAFEARRAY p       = do
	    x <- readVarSAFEARRAY (castPtr p) (fromIntegral (fromEnum VT_VARIANT))
	    doThenFree free (readSAFEARRAY True) (castPtr x)

inoutSafeArray  :: (Variant a) => ArgInOut (SafeArray a) (SafeArray a)
inoutSafeArray d          = (inSafeArray d,resSafeArray)
outSafeArray :: Variant a => ArgOut (SafeArray a)
outSafeArray              = inoutSafeArray defaultSafeArray

freeSafeArray :: SafeArray a -> IO ()
freeSafeArray (SA s) = return () -- it's a foreignObj..

marshallSafeArray :: SafeArray a -> IO (ForeignPtr SAFEARRAY)
marshallSafeArray (SA s) = marshallSAFEARRAY s

unmarshallSafeArray :: Ptr a -> IO (SafeArray a)
unmarshallSafeArray x = do
  s <- unmarshallSAFEARRAY True (castPtr x)
  return (SA s)

writeSafeArray :: Ptr (SafeArray a) -> SafeArray a -> IO ()
writeSafeArray ptr (SA s) = writeSAFEARRAY (castPtr ptr) s

readSafeArray :: Variant a => Bool -> Ptr (SafeArray a) -> IO (SafeArray a)
readSafeArray finaliseMe ptr = readSafeArray' finaliseMe ptr undefined

readSafeArray' :: Variant a => Bool -> Ptr (SafeArray a) -> a -> IO (SafeArray a)
readSafeArray' finaliseMe ptr x = do
  xx <- readSA finaliseMe ptr (vtEltType x)
  return (SA xx)

readSA :: Bool -> Ptr (SafeArray a) -> VARENUM -> IO SAFEARRAY
readSA finaliseMe ptr vt = do
  x <- readVarSAFEARRAY (castPtr ptr) (fromIntegral (fromEnum vt))
  doThenFree free (readSAFEARRAY finaliseMe) (castPtr x)

instance Variant a => Variant (SafeArray a) where
    inVariant  = inSafeArray
    resVariant = resSafeArray   

instance Variant SAFEARRAY where
    inVariant  = inSAFEARRAY
    resVariant = resSAFEARRAY

\end{code}

\begin{code}
marshallVariant :: Variant a => a -> IO VARIANT
marshallVariant v = do
  x <- allocMemory (fromIntegral sizeofVARIANT)
  inVariant v (castPtr x)
  return x

writeVariant :: Variant a => VARIANT -> a -> IO ()
writeVariant ptr v = inVariant v ptr

readVariant :: Variant a => VARIANT -> IO a
readVariant ptr = do
  ptr' <- readPtr ptr
  resVariant ptr'

unmarshallVariant :: Variant a => VARIANT -> IO a
unmarshallVariant ptr = resVariant ptr

\end{code}

\end{document}
