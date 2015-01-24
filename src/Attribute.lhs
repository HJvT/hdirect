%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Feb. 9th 2003  14:48  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Operations on attributes, the <tt/Attribute/ type itself
is defined in <tt/CoreIDL/ 

\begin{code}
module Attribute where

import BasicTypes
import Literal
import CoreIDL
import Data.List  ( find )
import Utils      ( elemBy, mapMb, notNull )
import Data.Maybe ( mapMaybe )

\end{code}

\begin{code}
noAttrs :: [Attribute]
noAttrs = []

hasAttribute :: [Attribute] -> Attribute -> Bool
hasAttribute attrs attr = attr `elem` attrs

hasAttributeWithName :: [Attribute] -> Name -> Bool
hasAttributeWithName attrs nm = elemBy lup attrs
 where
  lup (Attribute n _) = n == nm
  lup _		      = False

hasAttributeWithNames :: [Attribute] -> [Name] -> Bool
hasAttributeWithNames attrs nms = elemBy lup attrs
 where
  lup (Attribute n _) = n `elem` nms
  lup _		      = False

-- return list of attributes that haven't got any of the names given in the list.
filterOutAttributes :: [Attribute] -> [Name] -> [Attribute]
filterOutAttributes attrs nms = filter out attrs
  where
   out (Attribute n _) = not (n `elem` nms)
   out _	       = True

-- return list of attributes that have got one of the names given in the list.
filterAttributes :: [Attribute] -> [Name] -> [Attribute]
filterAttributes attrs nms = filter predic attrs
  where
   predic (Attribute n _) = n `elem` nms
   predic _	          = False

findAttribute :: Name -> [Attribute] -> Maybe Attribute
findAttribute nm attrs = find lup attrs
 where
  lup (Attribute n _) = n == nm
  lup _		      = False

findStringAttributes :: Name -> [Attribute] -> [String]
findStringAttributes nm = mapMaybe toString . filter lup
 where
  lup (Attribute n _) = n == nm
  lup _		      = False

  toString (Attribute _ [ParamLit (StringLit s)]) = Just s
  toString _ = Nothing

\end{code}

\begin{code}
isDependentAttribute :: Attribute -> Maybe Attribute
isDependentAttribute a@(AttrDependent _ _) = Just a
isDependentAttribute _			   = Nothing

isConstantAttribute :: Attribute -> Bool
isConstantAttribute (AttrMode _) = True
isConstantAttribute a		 = null (atParams a)

stringToDepReason :: String -> Maybe DepReason
stringToDepReason "size_is"   = Just SizeIs
stringToDepReason "first_is"  = Just FirstIs
stringToDepReason "last_is"   = Just LastIs
stringToDepReason "length_is" = Just LengthIs
stringToDepReason "max_is"    = Just MaxIs
stringToDepReason "min_is"    = Just MinIs
stringToDepReason "switch_is" = Just SwitchIs
stringToDepReason _	      = Nothing
\end{code}

Special purpose ones:

\begin{code}
hasStringAttribute :: [Attribute] -> Bool
hasStringAttribute attrs = hasAttributeWithName attrs "string"

hasSeqAttribute :: [Attribute] -> Bool
hasSeqAttribute attrs = hasAttributeWithName attrs "sequence"

hasSourceAttribute :: [Attribute] -> Bool
hasSourceAttribute attrs = hasAttributeWithName attrs "source"

hasUniqueAttribute :: [Attribute] -> Bool
hasUniqueAttribute attrs = hasAttributeWithName attrs "unique"

getLengthAttribute :: [Attribute] -> Maybe AttributeParam
getLengthAttribute attrs = 
   case (filter withDep attrs) of
     (AttrDependent _ (x:_) : _) -> Just x
     _ -> Nothing
  where
   withDep (AttrDependent LengthIs _) = True
   withDep _		 = False


hasModeAttribute :: ParamDir -> [Attribute] -> Bool
hasModeAttribute dir attrs = any withMode attrs
  where
   withMode (AttrMode d) = d == dir
   withMode _		 = False

getSwitchIsAttribute :: [Attribute] -> Maybe Expr
getSwitchIsAttribute as = 
  case mapMb atParams (findAttribute "switch_is" as) of
    Just [ParamExpr e]          -> Just e
    Just [ParamVar v]		-> Just (Var v)
    _ ->
      case filter (isSwitchIs) as of
        ((AttrDependent _ [ParamVar v]): _ )  -> Just (Var v)
        ((AttrDependent _ [ParamExpr e]): _ ) -> Just e
	_				      -> Nothing
 where
  isSwitchIs (AttrDependent SwitchIs _) = True
  isSwitchIs _				= False

getUuidAttribute :: [Attribute] -> Maybe [String]
getUuidAttribute as =
 case mapMb atParams (findAttribute "uuid" as) of
   Just [ParamLit (GuidLit guid)] -> Just guid
   Just [ParamLit (LitLit  guid)] -> Just [guid]
   _ -> Nothing

getDispIdAttribute :: [Attribute] -> Maybe IntegerLit
getDispIdAttribute as =
 case mapMb atParams (findAttribute "id" as) of
   Just [ParamLit (IntegerLit i)] -> Just i
   _ -> Nothing

hasDependentAttrs :: [Attribute] -> Bool
hasDependentAttrs ls = notNull (mapMaybe isDependentAttribute ls)

sourceAttribute :: Attribute
sourceAttribute = Attribute "source" []

getDefaultCConv :: [Attribute] -> Maybe CallConv
getDefaultCConv as = 
  case mapMb atParams (findAttribute "cconv" as) of
   Just [ParamLit (StringLit cc)] -> strToCallConv cc
   _			          -> Nothing

\end{code}

