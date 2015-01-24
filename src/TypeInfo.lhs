%
%
%

Specifying type / marshalling information

\begin{code}
module TypeInfo 

       ( TypeInfo(..)
       , typeInfos

       , v_bool_ti
       , variant_ti
       , mb_currency_ti
       , mb_date_ti
       , guid_ti
       , iid_ti
       , clsid_ti
       , bstr_ti

       ) where

import BasicTypes
import NativeInfo
import Opts
import Maybe
import AbsHUtils
import AbstractH ( Type )
import LibUtils  ( comLib )
{- BEGIN_SUPPORT_TYPELIBS
import
       Automation ( VARENUM(..) )
   END_SUPPORT_TYPELIBS -}

\end{code}

A @TypeInfo@ record contains all the info needed by the
backend(s) to convert the use of a type into appropriate
Haskell code. 

\begin{code}
data TypeInfo 
 = TypeInfo {
     type_name        :: String,
     haskell_type     :: QualName,
     marshaller       :: QualName,
     copy_marshaller  :: QualName,
     unmarshaller     :: QualName,
     ref_marshaller   :: QualName,
     ref_unmarshaller :: QualName,
     alloc_type       :: Maybe QualName,
     free_type        :: Maybe QualName,
     prim_type	      :: Type,
     c_type           :: String,
     prim_size        :: QualName,
     prim_sizeof      :: Int,
     prim_align       :: Int,
     auto_type        :: QualName,
{- BEGIN_SUPPORT_TYPELIBS
     auto_vt          :: Maybe VARENUM,
   END_SUPPORT_TYPELIBS -}
     is_pointed	      :: Bool,
     finalised	      :: Bool,
     attributes       :: Maybe String
   }
   deriving ( Show, Eq )

\end{code}

\begin{code}
typeInfos :: [TypeInfo]
typeInfos = 
  [ variant_ti
  , v_bool_ti
  , currency_ti
  , iid_ti
  , clsid_ti
  , guid_ti
  ]

iid_ti :: TypeInfo
iid_ti =
    TypeInfo 
        { type_name        = "IID"
	, haskell_type     = toQualName "Com.IID a"
	, marshaller       = toQualName "Com.marshallIID"
	, copy_marshaller  = toQualName "Com.copyIID"
	, unmarshaller     = toQualName "Com.unmarshallIID"
	, ref_marshaller   = toQualName "Com.writeIID"
	, ref_unmarshaller = toQualName "Com.readIID"
	, alloc_type       = Nothing
	, free_type        = Nothing
	, prim_type	   = tyForeignPtr (tyQCon comLib "IID" [uniqueTyVar "a"])
	, c_type           = "IID*"
	, auto_type	   = toQualName "Com.IID a"
	, prim_size        = toQualName "Com.sizeofIID"
	, prim_sizeof      = 16
	, prim_align       = 4
{- BEGIN_SUPPORT_TYPELIBS
	, auto_vt	   = Nothing
   END_SUPPORT_TYPELIBS -}
	, is_pointed	   = True
	, finalised	   = True
	, attributes	   = Nothing
	}

clsid_ti :: TypeInfo
clsid_ti = 
  TypeInfo 
        { type_name        = "CLSID"
	, haskell_type     = toQualName "Com.CLSID"
	, marshaller       = toQualName "Com.marshallCLSID"
	, copy_marshaller  = toQualName "Com.copyCLSID"
	, unmarshaller     = toQualName "Com.unmarshallCLSID"
	, ref_marshaller   = toQualName "Com.writeCLSID"
	, ref_unmarshaller = toQualName "Com.readCLSID"
	, alloc_type       = Nothing
	, free_type        = Nothing
	, prim_type	   = tyForeignPtr (tyQConst comLib "CLSID")
	, c_type           = "CLSID*"
	, auto_type	   = toQualName "Com.CLSID"
	, prim_size        = toQualName "Com.sizeofCLSID"
	, prim_sizeof      = 16
	, prim_align       = 4
{- BEGIN_SUPPORT_TYPELIBS
	, auto_vt	   = Nothing
   END_SUPPORT_TYPELIBS -}
	, is_pointed	   = True
	, finalised	   = True
	, attributes	   = Nothing
	}

guid_ti :: TypeInfo
guid_ti = 
  TypeInfo 
        { type_name        = "GUID"
	, haskell_type     = toQualName "Com.GUID"
	, marshaller       = toQualName "Com.marshallGUID"
	, copy_marshaller  = toQualName "Com.copyGUID"
	, unmarshaller     = toQualName "Com.unmarshallGUID"
	, ref_marshaller   = toQualName "Com.writeGUID"
	, ref_unmarshaller = toQualName "Com.readGUID"
	, alloc_type       = Nothing
	, free_type        = Nothing
	, prim_type	   = tyForeignPtr (tyQConst comLib "GUID")
	, c_type           = "GUID*"
	, auto_type	   = toQualName "Com.GUID"
	, prim_size        = toQualName "Com.sizeofGUID"
	, prim_sizeof      = 16
	, prim_align       = 4
{- BEGIN_SUPPORT_TYPELIBS
	, auto_vt	   = Nothing
   END_SUPPORT_TYPELIBS -}
	, is_pointed	   = True
	, finalised	   = True
	, attributes	   = Nothing
	}

mb_currency_ti :: Maybe TypeInfo
mb_currency_ti = Just currency_ti

currency_ti :: TypeInfo
currency_ti = 
  TypeInfo 
        { type_name        = "CURRENCY"
	, haskell_type     = toQualName "Automation.Currency"
	, marshaller       = toQualName "Automation.marshallCurrency"
	, copy_marshaller  = toQualName "Automation.marshallCurrency"
	, unmarshaller     = toQualName "Automation.unmarshallCurrency"
	, ref_marshaller   = toQualName "Automation.writeCurrency"
	, ref_unmarshaller = toQualName "Automation.readCurrency"
	, alloc_type       = Nothing
	, free_type        = Nothing
	, prim_type	   = tyInt64
	, c_type           = "CURRENCY"
	, auto_type	   = toQualName "Automation.Currency"
	, prim_size        = toQualName "HDirect.sizeofInt64"
	, prim_sizeof      = lONGLONG_SIZE
	, prim_align       = lONGLONG_ALIGN_MODULUS
{- BEGIN_SUPPORT_TYPELIBS
	, auto_vt	   = Just VT_CY
   END_SUPPORT_TYPELIBS -}
	, is_pointed	   = False
	, finalised	   = False
	, attributes	   = Nothing
	}

mb_date_ti :: Maybe TypeInfo
mb_date_ti = Just date_ti

date_ti :: TypeInfo
date_ti = 
  TypeInfo 
        { type_name        = "DATE"
	, haskell_type     = toQualName "Automation.Date"
	, marshaller       = toQualName "HDirect.marshallDouble"
	, copy_marshaller  = toQualName "HDirect.marshallDouble"
	, unmarshaller     = toQualName "HDirect.unmarshallDouble"
	, ref_marshaller   = toQualName "HDirect.writeDouble"
	, ref_unmarshaller = toQualName "HDirect.readDouble"
	, alloc_type       = Nothing
	, free_type        = Nothing
	, prim_type	   = tyDouble
	, c_type           = "double"
	, auto_type	   = toQualName "Automation.Date"
	, prim_size        = toQualName "HDirect.sizeofDouble"
	, prim_sizeof      = dOUBLE_SIZE
	, prim_align       = dOUBLE_ALIGN_MODULUS
{- BEGIN_SUPPORT_TYPELIBS
	, auto_vt	   = Just VT_DATE
   END_SUPPORT_TYPELIBS -}
	, is_pointed	   = False
	, finalised	   = False
	, attributes	   = Nothing
	}

variant_ti :: TypeInfo
variant_ti 
  | optNoOverloadVariant || optServer =
    TypeInfo 
        { type_name        = "VARIANT"
	, haskell_type     = toQualName "Automation.VARIANT"
	, marshaller       = toQualName "Automation.marshallVARIANT"
	, copy_marshaller  = toQualName "Automation.copyVARIANT"
	, unmarshaller     = toQualName "Automation.unmarshallVARIANT"
	, ref_marshaller   = toQualName "Automation.writeVARIANT"
	, ref_unmarshaller = toQualName "Automation.readVARIANT"
	, alloc_type       = Just (toQualName "Automation.allocVARIANT")
	, free_type        = Nothing
	, prim_type	   = {-tyPtr-} (mkTyConst $ toQualName "Automation.VARIANT")
	, c_type           = "VARIANT"
	, auto_type	   = toQualName "a"
	, prim_size        = toQualName "Automation.sizeofVARIANT"
	, prim_sizeof      = 16
	, prim_align       = 8
{- BEGIN_SUPPORT_TYPELIBS
	, auto_vt	   = Just VT_VARIANT
   END_SUPPORT_TYPELIBS -}
	, is_pointed	   = True
	, finalised	   = False
	, attributes	   = Nothing
	}
  | otherwise =
    TypeInfo 
        { type_name        = "VARIANT"
	, haskell_type     = toQualName "a"  -- magic.
	, marshaller       = toQualName "Automation.marshallVariant"
	, copy_marshaller  = toQualName "Automation.marshallVariant"
	, unmarshaller     = toQualName "Automation.unmarshallVariant"
	    -- Note: when we're marshalling Variants by reference, this
	    --       is only done for constructed types, so we want to use
	    --       the non-overloaded VARIANT marshallers rather than
	    --       the overloaded (since VARIANTs embedded inside a
	    --       constructed type is represented by VARIANT.)
	, ref_marshaller   = toQualName "Automation.writeVARIANT"
	, ref_unmarshaller = toQualName "Automation.readVARIANT"
	, alloc_type       = Just (toQualName "Automation.allocVARIANT")
	, free_type        = Nothing
	, prim_type	   = {-tyPtr-} (mkTyConst $ toQualName "Automation.VARIANT")
	, c_type           = "VARIANT"
	, auto_type	   = toQualName "a"
	, prim_size        = toQualName "Automation.sizeofVARIANT"
	, prim_sizeof      = 16
	, prim_align       = 8
{- BEGIN_SUPPORT_TYPELIBS
	, auto_vt	   = Just VT_VARIANT
   END_SUPPORT_TYPELIBS -}
	, is_pointed	   = True
	, finalised	   = False
	, attributes	   = Nothing
	}

v_bool_ti :: TypeInfo
v_bool_ti = 
  TypeInfo 
        { type_name        = "VARIANT_BOOL"
	, haskell_type     = toQualName "Prelude.Bool"
	, marshaller       = toQualName "Automation.marshallVARIANT_BOOL"
	, copy_marshaller  = toQualName "Automation.marshallVARIANT_BOOL"
	, unmarshaller     = toQualName "Automation.unmarshallVARIANT_BOOL"
	, ref_marshaller   = toQualName "Automation.writeVARIANT_BOOL"
	, ref_unmarshaller = toQualName "Automation.readVARIANT_BOOL"
	, alloc_type       = Nothing
	, free_type        = Nothing
	, prim_type	   = tyInt16
	, c_type           = "VARIANT_BOOL"
	, auto_type        = toQualName "Prelude.Bool"
	, prim_size        = toQualName "HDirect.sizeofInt16"
	, prim_sizeof      = sHORT_SIZE
	, prim_align       = sHORT_ALIGN_MODULUS
{- BEGIN_SUPPORT_TYPELIBS
	, auto_vt	   = Nothing
   END_SUPPORT_TYPELIBS -}
	, is_pointed	   = False
	, finalised	   = False
	, attributes	   = Nothing
	}

bstr_ti :: TypeInfo
bstr_ti = 
  TypeInfo 
        { type_name        = "BSTR"
	, haskell_type     = toQualName "Prelude.String"
	, marshaller       = toQualName "Com.marshallBSTR"
	, copy_marshaller  = toQualName "Com.marshallBSTR"
	, unmarshaller     = toQualName "Com.unmarshallBSTR"
	, ref_marshaller   = toQualName "Com.writeBSTR"
	, ref_unmarshaller = toQualName "Com.readBSTR"
	, alloc_type       = Nothing
	, free_type        = Just (toQualName "Com.freeBSTR")
	, prim_type	   = tyPtr tyString
	, c_type           = "void*"
	, auto_type        = toQualName "Prelude.String"
	, prim_size        = toQualName "HDirect.sizeofPtr"
	, prim_sizeof      = dATA_PTR_SIZE
	, prim_align       = dATA_PTR_ALIGN_MODULUS
{- BEGIN_SUPPORT_TYPELIBS
	, auto_vt	   = Just VT_BSTR
   END_SUPPORT_TYPELIBS -}
	, is_pointed	   = False
	, finalised	   = False
	, attributes	   = Nothing
	}

\end{code}
