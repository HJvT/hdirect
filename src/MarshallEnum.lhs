%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Jan. 30th 2003  14:19  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Generating code for going between IDL enums and
their Haskell equivalent.

\begin{code}
module MarshallEnum 
	   ( marshallEnum
	   , genDerivedEnumInstanceFor
	   ) where

import MarshallType

import BasicTypes ( BinaryOp(..),QualName, qName, EnumKind(..)  )
import Literal    ( iLit  )
import AbstractH  ( HDecl )
import qualified AbstractH as Haskell ( CaseAlt, Expr )
import CgMonad

import AbsHUtils
import LibUtils hiding ( enumToInt )
import qualified LibUtils ( enumToInt )

import CoreIDL
import CoreUtils  ( mkHaskellTyConName )
import Attribute  ( hasAttributeWithName, filterAttributes )
import Literal    ( Literal(..) )
import Opts ( optSmartEnums, optNoVariantInstance, 
	      optVariantInstance, optEnumsAsFlags,
	      optGenBitsInstance, optGenNumInstance
	    )
import PpCore ( ppEnumValue, showCore )

\end{code}


\begin{code}
marshallEnum :: Id -> EnumKind -> Bool -> [EnumValue] -> CgM HDecl
marshallEnum tdef_id kind noEnumInstance enums
 | noEnumInstance && genVariantInstance = return variantInst
 | noEnumInstance     = return emptyDecl
 | genVariantInstance = return (enumInst `andDecl` variantInst)
 | otherwise	      = return enumInst
 where   
  genVariantInstance = optVariantInstance && not optNoVariantInstance

  attrs        = idAttributes tdef_id
  tdef_name    = idName tdef_id
  name         = mkVarName tdef_name

  v            = var "v"
  x1           = var "x1"
  x2           = var "x2"
  t_ty         = tyConst (qName name)

    -- The size of the integer value an enum tags gets mapped
    -- to is determined as follows:
    -- 
    --   - OMG IDL => Int32
    --   - MIDL with v1_enum attr => Int32
    --   - MIDL    => Int16
    -- 
    -- However, [v1_enum] (or not) only affects what's transmitted
    -- across the network, so we can uniformly represent a marshalled
    -- enum via an Int32.

  enumInst = 
    addBitsInstance $     
    hInstance Nothing enumClass t_ty 
	[ methodDef fromEnumName [varPat v] m_rhs 
	, methodDef toEnumName   [varPat v] u_rhs 
	]

  variantInst = 
    hInstance Nothing variantClass t_ty 
	[ methodDef inVariantName      [] inv_rhs 
	, methodDef resVariantName     [] resv_rhs 
	, methodDef defaultVariantName [] def_rhs 
	, methodDef vtEltTypeName      [] vt_elt_rhs
	]

  addBitsInstance d 
   | withListCon = 
   	if optGenBitsInstance || optGenNumInstance then
	   (if optGenNumInstance then
	       numInstance
	    else
	       numInstance `andDecl` bitsInstance) 
          `andDecl` d
	else
	   flagsInstance `andDecl` d
   | otherwise   = d

  withListCon = 
    case kind of
      EnumFlags{} -> True
      _           -> forceFlag

  forceFlag = optEnumsAsFlags || attrs `hasAttributeWithName` "hs_flag"

  numInstance  =
    hInstance Nothing numClass t_ty
        [ methodDef (mkQVarName prelude "+") [varPat x1, varPat x2] or_rhs
	]

  bitsInstance =
    hInstance Nothing bitsClass t_ty
	[ methodDef andName [varPat x1, varPat x2] and_rhs 
	, methodDef orName  [varPat x1, varPat x2] or_rhs 
	, methodDef xorName   []     xor_rhs
	, methodDef complementName[] comp_rhs
	, methodDef shiftName []     shift_rhs
	, methodDef rotateName []    rot_rhs
	, methodDef bitSizeName []   bit_rhs
	, methodDef isSignedName []  isS_rhs
	]

  flagsInstance =
    hInstance Nothing flagsClass t_ty
	[ methodDef orFlagName  [varPat x1, varPat x2] or_rhs 
	]

  inv_rhs    = qvar autoLib "inEnum"
  resv_rhs   = qvar autoLib "resEnum"
  def_rhs    = qvar autoLib "defaultEnum"
  vt_elt_rhs = qvar autoLib "vtTypeEnum"
  
  or_rhs    = funApp toEnumName [ binOp Add (funApp fromEnumName [x1])
  					     (funApp fromEnumName [x2])]

  and_rhs     = 
    hLet (var "flatten")
         (lam [patVar "x"] 
    	      (hCase (var "x")
	       	     [ alt (conPat (mkConName (tdef_name ++ "List__")) [patVar "xs"])
		     	   (funApp concatMapName [var "flatten", var "xs"])
		     , defaultAlt (Just (mkVarName "x")) (hList [var "x"])
		     ]))
         (dataCon (mkConName (tdef_name ++ "List__"))
	          [funApp intersectName [ funApp (mkVarName "flatten") [x1]
		  			, funApp (mkVarName "flatten") [x2]
					]])

  xor_rhs = funApp prelError
  		   [ stringLit ("Bits.xor{"++qName name++"}: unimplemented") ]
  comp_rhs = funApp prelError
  		   [ stringLit ("Bits.complement{"++qName name++"}: unimplemented") ]
  shift_rhs = funApp prelError
  		   [ stringLit ("Bits.shift{"++qName name++"}: unimplemented") ]
  rot_rhs = funApp prelError
  		   [ stringLit ("Bits.rotate{"++qName name++"}: unimplemented") ]
  bit_rhs = funApp prelError
  		   [ stringLit ("Bits.bitSize{"++qName name++"}: unimplemented") ]
  isS_rhs = funApp prelError
  		   [ stringLit ("Bits.isSigned{"++qName name++"}: unimplemented") ]

    -- Marshalling --
  m_rhs
    | not optSmartEnums || kind == Unclassified = hCase v (add_m_list (map (enumToInt True) enums))
    | otherwise = 
        case kind of
	  EnumProgression st 1 -> binOp Add (intLit st)
	  				    (funApp LibUtils.enumToInt [v])
	  EnumProgression st step -> binOp Add (intLit st)
	  				       (binOp Mul (intLit step)
					       		  (funApp LibUtils.enumToInt [v]))
	  EnumFlags 0 -> funApp enumToFlag [v]
	  EnumFlags k -> funApp toIntFlag [intLit k, funApp LibUtils.enumToInt [v]]
	  Unclassified -> error "MarshallEnum.marshallEnum.m_rhs: the impossible happened"

  add_m_list 
    | withListCon = ((alt (conPat (mkConName (tdef_name ++ "List__")) [patVar "xs"]) rhs) :)
    | otherwise   = id
   where
    rhs = funApp orListName [funApp mapListName [varName fromEnumName, var "xs"]]

  u_name   = qName (prefix unmarshallPrefix name)
  u_rhs  
    | not optSmartEnums || kind == Unclassified = normal_u_rhs
    | otherwise            =
        case kind of
		-- solve y = step * x + st
	  EnumProgression st 1 -> 
	       funApp tagToEnum
	       	   [funApp unboxInt
		   	   [binOp Sub v (intLit st)]]
	  EnumProgression st step -> 
	       funApp tagToEnum
	       	   [funApp unboxInt
		   	   [binOp Div (binOp Sub v (intLit st))
		   	     	      (intLit step)]]
	  EnumFlags k ->
		funApp tagToEnum
		   [funApp unboxInt
		   	   [funApp toIntFlag [intLit k, funApp flagToIntTag [v]]]]
	  Unclassified -> error "MarshallEnum.marshallEnum.u_rhs: the impossible happened"
	  	
  normal_u_rhs = hCase v (add_u_list (map intToEnum enums) ++ 
                         [defaultAlt Nothing
			   (giveUp (u_name ++ ": illegal enum value "))])

  giveUp msg =funApp prelError [stringLit msg]

  add_u_list ls =
    case kind of
      EnumFlags start -> (ls ++ [alt (patVar "x") (rhsFlg start)])
      _ | forceFlag   -> (ls ++ [alt (patVar "x") (rhsGen)])
        | otherwise   -> ls
   where
    rhsFlg st= 
          dataCon (mkConName (tdef_name ++ "List__"))
    		  [ funApp mapMaybeName
		  	   [ lam [patVar "val"]
			   	 (hIf (binOp Eq (binOp And (var "val") (funApp fromIntegralName [var "x"]))
					       (var "val"))
				      (just (funApp toEnumName [funApp fromIntegralName [var "val"]]))
				      nothing)
			   , funApp pow2Series [intLit (length enums), intLit st]
			   ]
		  ]

    rhsGen = 
          dataCon (mkConName (tdef_name ++ "List__"))
    		  [ funApp mapMaybeName
		  	   [ lam [patVar "val"]
			   	 (hIf (binOp Eq (binOp And (var "val") (var "x"))
					       (var "val"))
				      (just (funApp toEnumName [var "val"]))
				      nothing)
			   , hList (map (enumToIntExpr fromIntegralName) enums)
			   ]
		  ]

\end{code}

Helpers:

\begin{code}
{- UNUSED
mkGuard :: Bool -> Haskell.Expr -> EnumValue -> (Haskell.Expr, Haskell.Expr)
mkGuard long_enum_tags v (EnumValue i val) =
  (binOp Eq v val', ret (dataConst (mkConName (mkHaskellTyConName nm))))
  where
   nm   = idName i
   val' =
    case val of
      Left  il -> intLit il
      Right e  -> funApp (mkQVarName hdirectLib (if long_enum_tags then "toInt32" else "toInt16")) 
		         [coreToHaskellExpr e]
-}

enumToInt :: Bool -> EnumValue -> Haskell.CaseAlt
enumToInt long_enum_tags ev@(EnumValue i _) = 
   alt (patKind (mkConName (mkHaskellTyConName nm))) val
 where
  patKind
    | has_args  = \ x -> patRec x []
    | otherwise = \ x -> conPat x []

  has_args = (idAttributes i) `hasAttributeWithName` "hs_tyarg"

  nm  = idName i
  val = enumToIntExpr coerce ev
  coerce = mkQVarName hdirectLib (if long_enum_tags then "toInt32" else "toInt16")

enumToIntExpr :: QualName -> EnumValue -> Haskell.Expr
enumToIntExpr coerce (EnumValue _ v) =
   case v of
     Left il -> intLit il
     Right e -> funApp coerce [coreToHaskellExpr e]

-- Assume: always called with (Left i)
intToEnum :: EnumValue -> Haskell.CaseAlt
intToEnum (EnumValue i (Left v)) = alt (litPat (iLit v)) tag
 where
  nm                = idName i
  tag
    | has_args      = dataCon tag_nm def_vals
    | otherwise     = dataConst tag_nm
  tag_nm            = mkConName (mkHaskellTyConName nm)

  def_vals          = 
     	(\ x -> 
	  case x of 
	   [] -> []
	   xs -> [lit (LitLit xs)]) $
     	   unwords $
	   map (\ xs -> '(':xs ++ ")") $
	   map getStr $
           filterAttributes (idAttributes i)
     			    ["hs_default"]

  getStr (Attribute _ [ParamLit (StringLit s)]) = s
  getStr _ = ""
  has_args          = (idAttributes i) `hasAttributeWithName` "hs_tyarg"

intToEnum eVal = error ("intToEnum: unhandled enum RHS -- " ++ showCore (ppEnumValue eVal))
\end{code}

\begin{code}
genDerivedEnumInstanceFor :: EnumKind -> [EnumValue] -> Bool
genDerivedEnumInstanceFor (EnumProgression 0 1) vs 
  = not (any (\ x -> idAttributes (enumName x) `hasAttributeWithName` "hs_tyarg") vs)
genDerivedEnumInstanceFor _ _	= False
\end{code}
