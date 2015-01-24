%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Mar. 31th 2003  08:37  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Support for generating client stubs for Automation interfaces.

\begin{code}
module MarshallAuto
       (
         marshallVariantParam
       , unmarshallVariantParam
       , marshallVariant
       , classifyCall
       , permissibleAutoSig
       ) where

import qualified AbstractH as Haskell
import AbsHUtils
import MarshallType ( coreToHaskellExpr )
import MarshallCore ( toBaseTy, autoTypeToQName, mbAutoTypeToHaskellTy )
import CoreIDL
import CoreUtils ( int32Ty, doubleTy, mkHaskellVarName, 
		   isHRESULTTy, isVoidTy, boolTy
		 )

import BasicTypes
import Attribute ( hasAttributeWithName, findAttribute )
import LibUtils ( autoLib )
import Opts  ( optOptionalAsMaybe
	     )
import Maybe ( isJust )
import Literal

\end{code}


\begin{code}
marshallVariantParam :: Param -> Haskell.Expr
marshallVariantParam p = funApply real_marshaller [ var (idName (paramId p)) ]
 where
  m = paramMode p

  attrs = idAttributes (paramId p)

   -- taking [defaultvalue()]s into account (if any).
   -- Use 'marshallerMeth' if you don't care for this stuff.
  real_marshaller
   | m == In && attrs `hasAttributeWithName` "defaultvalue" = 
      case findAttribute "defaultvalue" attrs of
        Just (Attribute _ (ap:_))
	     | okLooking ap -> 
		    let
		      pt      = paramType p
		      base_ty = toBaseTy pt
			{-
			  Stuff like
			    [in,optional,defaultvalue(0)]IUnknown* ip
			    [in,optional,defaultvalue(0)]char ip
			  
			  won't work if we use the arg type to drive
			  marshalling, so catch this sep.
			-}
		      the_base_ty
		       | isIntLit expr    = int32Ty
		       | isBoolLit expr   = boolTy
		       | isDoubleLit expr = doubleTy
		       | otherwise        = base_ty
		       
		      def_val_marshaller
		       | not optOptionalAsMaybe = qvar autoLib "inDefaultValue"
		       | otherwise		= qvar autoLib "inMaybe"
                    in		       
		       funApply def_val_marshaller
			        [ funApply (marshallVariant kind the_base_ty)
					   [coreToHaskellExpr expr]
				, marshallerMeth
				]
	    where
	     okLooking (ParamLit  _) = True
	     okLooking (ParamExpr _) = True
	     okLooking _	     = False
	     
	     isIntLit e = 
	        case e of
		  Lit (IntegerLit{}) -> True
		  _		     -> False

	     isDoubleLit e = 
	        case e of
		  Lit (FloatingLit{}) -> True
		  _		      -> False

	     isBoolLit e = 
	        case e of
		  Lit (BooleanLit{}) -> True
		  _		     -> False

	     expr = 
	       case ap of
	         ParamLit  l -> Lit l
		 ParamExpr e -> e
		 _           -> error "MarshallAuto.marshallVariantParam.expr: unexpected parameter kind"

        _ 
	  | optOptionalAsMaybe ->
		       funApply (qvar autoLib "inMaybe")
			        [ qvar autoLib "noInArg"
				, marshallerMeth
				]
	      
	  | otherwise  -> marshallerMeth

   | has_optional && optOptionalAsMaybe =
       funApply (qvar autoLib "inMaybe")
	        [ qvar autoLib "noInArg"
		, marshallerMeth
		]
   | otherwise = marshallerMeth

  marshallerMeth
   | m == In && not optOptionalAsMaybe && has_optional = qvar autoLib "inVariant"
   | otherwise = marshallVariant kind (paramOrigType p)
     
  has_optional = attrs `hasAttributeWithName` "optional"

  kind =
   case m of
     In    -> "in"
     Out   -> "out"
     InOut -> "inout"

unmarshallVariantParam :: Param -> Haskell.Expr
unmarshallVariantParam p = 
  case m of
    InOut -> funApply expr [var (mkHaskellVarName (idName (paramId p)))]
    _     -> expr
 where
  expr = marshallVariant kind (paramType p)
  m    = paramMode p
  kind =
   case m of
     In    -> "in"
     Out   -> "out"
     InOut -> "inout"

marshallVariant :: String -> Type -> Haskell.Expr
marshallVariant pre ty = 
   let 
    qn = autoTypeToQName ty
    qv = prefix pre qn
   in
   qvar autoLib (qName qv)

\end{code}
 
determine what kind of Automation library stub to call.
 
\begin{code}
classifyCall :: Id -> Bool -> [Param] -> Result -> Haskell.VarName
classifyCall f useDISPID ps res
  | isPropGet      = mkQVarName autoLib ("propertyGet" ++ prop_arity ++ dispid)
  | isPropPutWeird = mkQVarName autoLib ("propertySetGet" ++ dispid)
  | isPropPut      = mkQVarName autoLib ("propertySet" ++ dispid)
  | otherwise      = mkQVarName autoLib (kind ++ dispid ++ arity_str)
  where
   kind
    | any hasRetValAttr ps ||
      (not (isHRESULTTy res_ty) && not (isVoidTy r_ty) &&
       all isInParam ps ) = "function"
    | otherwise = "method"

   dispid
    | useDISPID = "ID"
    | otherwise = ""

   r_ty   = resultType res
   res_ty = resultOrigType res

   hasRetValAttr p =
     (idAttributes (paramId p)) `hasAttributeWithName` "retval"

   attrs     = idAttributes f

   isPropPutWeird = isPropPut &&
		    any (\ p -> paramMode p == InOut) ps

   isInParam p = paramMode p == In

   isPropGet = attrs `hasAttributeWithName` "propget"
   isPropPut = attrs `hasAttributeWithName` "propput"    ||
               attrs `hasAttributeWithName` "propputref"

   prop_arity
     | arity <= (1::Int) = ""
     | otherwise	 = arity_str

   arity_str = show arity

   arity =
    case res_ty of
      Name "HRESULT" _ _ _ _ _ -> arity'
      Void -> arity'
      _    -> arity' + 1
    where
     arity' = length (filter isOutParam ps)

   isOutParam p = pm == Out || pm == InOut
     where pm = paramMode p

\end{code}

\begin{code}
permissibleAutoSig :: Result -> [Param] -> Bool
permissibleAutoSig res ps = 
  (isVoidTy r_ty || isHRESULTTy r_ty || isJust (mbAutoTypeToHaskellTy In r_ty)) &&
  all isJust (map ((mbAutoTypeToHaskellTy In).paramType) ps)
 where
  r_ty = resultType res

\end{code}
