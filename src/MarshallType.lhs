%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Jun. 10th 2003  12:17  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

MarshallType provides the following:

 - converting IDL types to Haskell ones.
 - functions for generating code that (un)marshalls a value
   of an IDL type.
 - marshalling dependent parameter types

\begin{code}
module MarshallType 
       (
         marshallType
       , unmarshallType
       , refMarshallType
       , refUnmarshallType
       , szType
       , allocPointerTo
       , freeType
       , mbFreeType
       , needsFreeing

       , coreToHaskellExpr

       , coerceTy
       , coerceToInt          -- :: Expr -> Haskell.Expr
       
       ) where

import Prelude hiding ( mod )
import BasicTypes
import Attribute
import Literal
import LibUtils
import Utils   ( mapFromMb, trace, notNull )

import qualified AbstractH as Haskell ( Expr(..) )
--import qualified PpAbstractH as PPHaskell ( showAbstractH )
import AbsHUtils

import CoreIDL
import CoreUtils
import PpCore ( showCore, ppType )

import Data.Maybe ( fromMaybe, isJust, fromJust )

import MarshallCore
import MarshallMonad
import Opts  ( optHaskellToC, optLongLongIsInteger,
               optCorba, optCom, optNoWideStrings,
               optNoDerefRefs
             )
import TypeInfo ( TypeInfo(..) )

\end{code}

%*
%
<sect>Marshalling types
%
%*

From a given IDL type t we may need to generate calls to functions
that implement various marshalling operations over type t:

  - marshall,   by-value and by-reference
  - unmarshall, by-value and by-reference
  - size of
  - allocate
  - release/free

The following functions can be used to do this:

\begin{verbatim}
marshallType      :: MarshallInfo -> Type -> Haskell.Expr
refMarshallType   :: MarshallInfo -> Type -> Haskell.Expr
unmarshallType    :: MarshallInfo -> Type -> Haskell.Expr
refUnmarshallType :: MarshallInfo -> Type -> Haskell.Expr

szType    :: MarshallInfo -> Type -> Haskell.Expr
 -- alloc storage to hold a (fixed size) value of a particular type,
 -- returning a pointer to it.
allocPointerTo    :: Type -> Haskell.Expr
\end{verbatim}

\begin{code}
marshallType :: MarshallInfo -> Type -> Haskell.Expr
marshallType mInfo t =
  case t of
    Array at [] 
       | forRef mInfo -> funApp w_list  [ varName false, szType at, refMarshallType mInfo at ]
       | otherwise    -> funApp m_list  [ szType at, refMarshallType mInfo{forRef=True} at ]
    Array (Char _) [e] 
       | forRef mInfo -> funApp w_bstring [ varName false, coreToHaskellExpr e ]
    Array at [e] 
       | forRef mInfo -> funApp w_blist [ szType at
                                        , coreToHaskellExpr e
                                        , refMarshallType mInfo{forRef=True} at
                                        ]
       | otherwise       -> funApp m_blist
                                   [ szType at
                                   , coreToHaskellExpr e
                                   , refMarshallType mInfo{forRef=True} at
                                   ]
    Array at [mi,ma] -> 
      funApp m_blist [ szType at
                     , binOp Sub (coreToHaskellExpr ma)
                                 (coreToHaskellExpr mi)
                     , refMarshallType mInfo{forRef=True} at
                     ]
    Pointer _ _ ty | isVoidTy ty -> varName prelReturn
    Pointer Ptr _ ty
      | (isConstructedTy (nukeNames ty) && isReferenceTy ty) -> varName prelReturn
    Pointer pt isExp (Iface nm mod _ attrs _ inh)  ->
        case pt of
          Unique | optCom && isExp        -> funApp marshallMaybe [ varName m_iptr, varName nullFO ]
                 | optHaskellToC && isExp -> funApp marshallMaybe 
                                            [ varName m_ip
                                            , varName $
                                               if attrs `hasAttributeWithName` "finaliser" then
                                                 nullFO
                                               else
                                                 nullPtr
                                            ]

          _ ->
           case inh of
             [] | optHaskellToC -> varName m_ip
                | otherwise     -> varName m_iptr
             (x:_) 
                   | optCorba   -> varName (prefix marshallPrefix (fst x))
                   | optCom || 
                     qName (fst x) == "IUnknown" -> varName m_iptr
                   | otherwise                   -> varName (prefix marshallPrefix (fst x))
      where
       m_ip = prefix marshallPrefix (mkQVarName mod nm)
          
    Pointer pt _ (Name _ _ _ _ _ (Just ti))
      | pt /= Ptr && is_pointed ti -> 
          let
           mshaller 
            | forInOut mInfo && finalised ti = varName (copy_marshaller ti)
            | otherwise                      = varName (marshaller ti)

           null_elt
            | forInOut mInfo && finalised ti = varName nullPtr
            | finalised ti                   = varName nullFO
            | otherwise                      = varName nullPtr
          in
          case pt of
            Unique -> funApp marshallMaybe [ mshaller, null_elt ]
            _      -> mshaller

    Pointer pt _ ty 
           | isFunTy ty && pt /= Ptr          -> marshallType mInfo ty
           | forInOut mInfo && forProxy mInfo -> refMarshallType mInfo t
           | otherwise ->
               case pt of
                Ptr     -> varName prelReturn
                Ref     -> funApp m_ref    [ allocPointerTo ty, refMarshallType mInfo{forRef=False} ty ]
                Unique  -> funApp m_unique [ allocPointerTo ty, refMarshallType mInfo{forRef=False} ty ]
    Void         -> varName prelReturn  -- this shouldn't really happen.
    String _ isUnique _ 
      | isUnique  -> funApp marshallMaybe [varName m_string, varName nullPtr]
      | otherwise -> varName m_string
    Sequence ty mbSz mbTerm
                  -> funApp m_sequence [ refMarshallType mInfo{forRef=False} ty
                                       , terminatorElt True mbTerm ty
                                       , szType ty
                                       , case mbSz of 
                                           Nothing -> nothing
                                           Just x  -> just (coreToHaskellExpr x)
                                       ]
    SafeArray{}  -> varName m_safearray
    WString isUnique _
      | isUnique  -> funApp marshallMaybe [varName m_wstring, varName nullPtr]
--      | isUnique  -> funApp marshallMaybe [varName m_wstring, varName nullFO]
      | otherwise -> varName m_wstring
    Bool         -> varName m_bool
    Enum{}       -> varName m_enum32

    Name _ _ _ _ _ (Just ti)   
       | forInOut mInfo && finalised ti  -> varName (copy_marshaller ti)
       | otherwise                       -> varName (marshaller ti)
    Name _ _ _ _ (Just ty@Enum{}) _ -> marshallType mInfo ty
    Name _ _ _ _ (Just ty@(Name{})) _ -> marshallType mInfo ty
    Name nm _ mod _ (Just (Struct i [f] _)) _ 
       | isSimpleTy (fieldType f) ||
         ((idAttributes i) `hasAttributeWithName` "hs_newtype")
       -> qvar mod (marshallPrefix  ++ nm)

    Name nm _ mod _ (Just ty) _ 
      | isConstructedTy (nukeNames ty) && not (isFunTy ty) -> funApp m_ref [ allocPointerTo t, refMarshallType mInfo{forRef=False} t ]
      | isFunTy ty                     -> qvar mod (marshallPrefix ++ nm)
      | otherwise                      -> marshallType mInfo ty

    Struct i fs _
       | length fs == 1 && isSimpleTy (fieldType (head fs)) &&
         not ((idAttributes i) `hasAttributeWithName` "hs_newtype")
       -> qvar (idModule i) (marshallPrefix  ++ idName i)
       | otherwise  -> funApp m_ref    [ allocPointerTo t, refMarshallType mInfo{forRef=False} t ]
    Union{}     -> funApp m_ref    [ allocPointerTo t, refMarshallType mInfo{forRef=False} t ]
    UnionNon{}  -> funApp m_ref    [ allocPointerTo t, refMarshallType mInfo{forRef=False} t ]
    CUnion{}    -> funApp m_ref    [ allocPointerTo t, refMarshallType mInfo{forRef=False} t ]

    Integer LongLong isSigned
      | optLongLongIsInteger -> varName (m_integer isSigned)
    Iface{} -> marshallType mInfo (Pointer Ref True t)
    _    -> let nm = mkMarshaller marshallPrefix t in
            varName nm

  where
   m_bool    = libImport bool
   m_string  = libImport stringName
   m_list    = libImport list
   m_blist   = libImport blist
   m_ref     = libImport ref
   m_unique  = libImport unique

   m_sequence  = libImport "Sequence"

   m_safearray
     | forProxy mInfo = prefix marshallPrefix sAFEARRAY
     | otherwise       = prefix marshallPrefix (mkQVarName autoLib safearray)
   
   w_list    = prefix marshallRefPrefix (mkQVarName hdirectLib list)
   w_bstring = prefix marshallRefPrefix (mkQVarName hdirectLib bstring)
   w_blist   = prefix marshallRefPrefix (mkQVarName hdirectLib blist)

   m_integer isSigned
     | isSigned  = libImport integer
     | otherwise = libImport ('U':integer)

   m_enum32  = libImport enum32

   m_iptr        = prefix marshallPrefix iUnknown
   m_wstring
    | optNoWideStrings  = wideImport wstring2
    | otherwise         = wideImport wstring

   libImport nm     = prefix marshallPrefix (mkQVarName hdirectLib nm)
   wideImport nm    = prefix marshallPrefix (mkQVarName wStringLib nm)


terminatorElt :: Bool -> Maybe Expr -> Type -> Haskell.Expr
terminatorElt forMarshalling mbTerm ty
 | forMarshalling = lam [patVar "x"] $
                        funApp (prefix marshallRefPrefix termTyName)
                               [ var "x"
                               , termVal
                               ] 
 | otherwise      = lam [patVar "ptr"] $
                        bind (funApp (prefix unmarshallRefPrefix termTyName) [var "ptr"])
                             (var "dx")
                             (ret (infixOp (var "dx") eqName termVal))
 where
  (termVal, termTyName) = 
    case ty of
     Char{}    -> (Haskell.Lit (CharLit '\0'), mkQVarName hdirectLib "Char")
     Integer sz signed ->
        case mbTerm of
          Just e -> (coreToHaskellExpr e, deTyCon $ mkIntTy sz signed)
          _      -> (coreToHaskellExpr (Lit (iLit (0::Int))), deTyCon $ mkIntTy sz signed)
     _         -> (varName nullPtr, mkQVarName hdirectLib "Ptr")
\end{code}


\begin{code}
unmarshallType :: MarshallInfo -> Type -> Haskell.Expr
unmarshallType mInfo ty =
  case ty of
    Array (Char _) [] -> varName u_string
        {-
          Handling open arrays when unmarshalling is tricky
          if you haven't got any information on the size of the
          thing. Nontheless, we represent open arrays as lists,
          since there's no trouble marshalling a list into an
          (open) array - it's this other way which is troublesome.
          So..we emit code which tries to marshall *one* element
          from the array here. Elsewhere we emit a warning message
          that the unmarshaller for a struct containing open arrays
          isn't 100% cool. The fix is either to use [size_is()] or
          manually tweak the gen'ed code.
          
          ToDo: add support for the notion of zero terminated vectors?
        -}
    Array t [] -> funApp u_single [ refUnmarshallType mInfo{forRef=True} t ]
    Array (Char _) [e] -> 
          funApp u_bstring [ coreToHaskellExpr e ]
    Array t [e]
      | doFree mInfo -> 
          funApp doThenFree
             [ freeType ty
             , funApp u_list [ szType t
                             , var "0"
                             , coreToHaskellExpr e
                             , refUnmarshallType mInfo{forRef=True} t
                             ]
             ]      
      | otherwise ->
          funApp u_list [ szType t
                        , var "0"
                        , coreToHaskellExpr e
                        , refUnmarshallType mInfo{forRef=True} t
                        ]
    Array t [mi,ma]
       | doFree mInfo ->
            funApp doThenFree
                   [ freeType ty
                   , funApp u_list [ szType t
                                   , coreToHaskellExpr mi
                                   , coreToHaskellExpr ma
                                   , refUnmarshallType mInfo{forRef=True} t
                                   ]
             ]
       | otherwise ->
             funApp u_list [ szType t
                           , coreToHaskellExpr mi
                           , coreToHaskellExpr ma
                           , refUnmarshallType mInfo{forRef=True} t
                           ]
    String _ isUnique _ 
        | doFree mInfo   -> funApp doThenFree [ freeType ty, uString ]
        | otherwise      -> uString
       where
         uString 
           | isUnique  = funApp readMaybe [ varName u_string ]
           | otherwise = varName u_string

    Sequence t mbSz mbTerm -> 
                    funApp u_sequence [ refUnmarshallType mInfo{forRef=False} t
                                      , terminatorElt False mbTerm t
                                      , szType t
                                      , case mbSz of 
                                           Nothing -> nothing
                                           Just x  -> just (coreToHaskellExpr x)
                                      ]

    WString isUnique _ 
        | doFree mInfo   -> funApp doThenFree [ freeType ty, uString ]
        | otherwise      -> uString
       where
        uString 
          | isUnique  = funApp readMaybe [ varName u_wstring ]
          | otherwise = varName u_wstring

    Pointer pt isExp t@(Iface nm mod _ attrs _ inh) ->
         case pt of   -- see marshallType comment re: raw interface ptrs.
           Ptr    | optCom -> u_ipointer
           Unique | (optCom || optHaskellToC) && isExp ->
                if forRef mInfo then
                   funApp u_unique [ refUnmarshallType mInfo{forRef=False} t ]
                else
                   funApp u_unique [ unmarshallType mInfo t ]
           _   -> 
            case inh of
              [] | optCom        -> u_ipointer
                 | optHaskellToC && attrs `hasAttributeWithName` "finaliser" ->
                                    funApp u_ip [ lit (BooleanLit (not (forProxy mInfo))) ]
                 | otherwise -> varName u_ip
              (x:_) 
                    | optCorba       -> varName (prefix unmarshallPrefix (fst x))
                    | optCom || qName (fst x) == "IUnknown" -> 
                        if forRef mInfo then
                           refUnmarshallType mInfo{forRef=False} ty
                        else
                           u_ipointer
                    | optHaskellToC && attrs `hasAttributeWithName` "finaliser" ->
                                    funApply (varName (prefix unmarshallPrefix (fst x)))
                                             [ lit (BooleanLit (not (forProxy mInfo))) ]
                    | otherwise                   -> varName (prefix unmarshallPrefix (fst x))
      where
       u_ip = prefix unmarshallPrefix (mkQVarName mod nm)

    Pointer _ _ (Name _ _ _ _ _ (Just ti))
      | is_pointed ti -> 
                if finalised ti then
                   funApply (varName (unmarshaller ti)) [lit (BooleanLit (not (forProxy mInfo))) ]
                else
                   varName (unmarshaller ti)
    Pointer _ _ Void -> varName prelReturn
    Pointer pt _ t -> 
      case pt of
        Ptr
         | isIfacePtr t  -> 
             if forRef mInfo then
                refUnmarshallType mInfo{forRef=False} t
             else
                u_ipointer
         | isPointerTy t && doFree mInfo -> funApp doThenFree 
                                                   [ freeType ty
                                                   , refUnmarshallType mInfo{forRef=False} t
                                                   ]
         | isPointerTy t -> refUnmarshallType mInfo{forRef=False} t
         | otherwise     -> varName prelReturn

        Ref
          | doFree mInfo -> funApp doThenFree [ freeType ty
                                              , refUnmarshallType mInfo{forRef=False} t
                                              ]
          | otherwise    -> refUnmarshallType mInfo{forRef=False} t
        Unique
          | isIfacePtr t -> 
                if forRef mInfo then
                   funApp u_unique [ refUnmarshallType mInfo{forRef=False} (getIfaceTy ty) ]
                else
                   funApp u_unique [ unmarshallType mInfo (getIfaceTy ty) ]

          | doFree mInfo -> funApp doThenFree [ freeType ty
                                              , funApp u_unique
                                                       [ refUnmarshallType mInfo{forRef=False} t ]
                                              ]
          | otherwise    -> funApp u_unique [ refUnmarshallType mInfo{forRef=False} t ]

    Void -> varName prelReturn
    Bool -> varName u_bool
    Name _ _ _ _ _ (Just ti) {-  not (is_pointed ti) -} -> 
                if finalised ti then
                   funApply (varName (unmarshaller ti)) [lit (BooleanLit (not (forProxy mInfo))) ]
                else
                   varName (unmarshaller ti)
    Name _ _ _ _ (Just t@Enum{}) _ -> unmarshallType mInfo t
    Name _ _ _ _ (Just t@(Name{})) _ -> unmarshallType mInfo t
    Name nm _ mod _ (Just (Struct i [f] _)) _ 
       | isSimpleTy (fieldType f) || 
         ((idAttributes i) `hasAttributeWithName` "hs_newtype")
       -> qvar mod (unmarshallPrefix  ++ nm)
    Name _ _ _ _ (Just t) _ 
      | isConstructedTy (nukeNames t) -> refUnmarshallType mInfo{forRef=False} (Pointer Ref True ty)
    Struct i fs _
       | (length fs == 1 && isSimpleTy (fieldType (head fs))) ||
         ((idAttributes i) `hasAttributeWithName` "hs_newtype")
       -> qvar (idModule i) (unmarshallPrefix  ++ idName i)
       | otherwise -> refUnmarshallType mInfo{forRef=False} (Pointer Ref True ty)
    Union{}     -> refUnmarshallType mInfo{forRef=False} (Pointer Ref True ty)
    UnionNon{}  -> refUnmarshallType mInfo{forRef=False} (Pointer Ref True ty)
    CUnion{}    -> refUnmarshallType mInfo{forRef=False} (Pointer Ref True ty)
    Enum{}      -> varName u_enum32
    Iface{}     -> unmarshallType mInfo (Pointer Ptr True ty)
    SafeArray{} -> u_safearray

    Integer LongLong isSigned
      | optLongLongIsInteger -> varName (u_integer isSigned)
    _       -> let nm = mkMarshaller unmarshallPrefix ty in
               varName nm
  where
   u_bool    = libImport bool
   u_string  = libImport stringName
   u_list    = libImport list
   u_bstring = libImport bstring
   u_unique  = libImport unique
   u_single  = libImport "Single"

   u_sequence  = libImport "Sequence"

   u_safearray
     | forProxy mInfo  = funApply (varName (prefix unmarshallPrefix sAFEARRAY)) [ lit (BooleanLit False) ]
     | forStruct mInfo = funApply (varName (prefix unmarshallPrefix sAFEARRAY)) [ lit (BooleanLit False) ]
     | otherwise       = varName (prefix marshallPrefix (mkQVarName autoLib safearray))

   u_integer isSigned
     | isSigned  = libImport integer
     | otherwise = libImport ('U':integer)

   u_enum32 = libImport enum32

   u_iptr       = prefix unmarshallPrefix iUnknown
   u_ipointer
    | forInOut mInfo = varName (prefix unmarshallPrefix iUnknownFO)
    | otherwise      = funApp u_iptr [ lit (BooleanLit (forProxy mInfo)) ]

   u_wstring
     | optNoWideStrings = wStringImport wstring2
     | otherwise        = wStringImport wstring

   libImport nm     = prefix unmarshallPrefix (mkQVarName hdirectLib nm)
   wStringImport nm = prefix unmarshallPrefix (mkQVarName wStringLib nm)

\end{code}

\begin{code}
refMarshallType :: MarshallInfo -> Type -> Haskell.Expr
refMarshallType mInfo t = 
  case t of
    Array (Char _) [e] -> funApp w_bstring [ varName true,  coreToHaskellExpr e ]
    Array at [e] -> 
      funApp w_blist
             [ szType at
             , coreToHaskellExpr e
             , refMarshallType mInfo{forRef=False} at
             ]
    Array at [mi, ma] -> 
      funApp w_blist
             [ szType at
             , binOp Sub (coreToHaskellExpr ma)
                         (coreToHaskellExpr mi)
             , refMarshallType mInfo{forRef=False} at
             ]
    Array at [] -> funApp w_list [ varName false
                                 , szType at
                                 , refMarshallType mInfo{forRef=False} at
                                 ]

    SafeArray{} -> varName w_safearray
    String _ isUnique _ 
      | isUnique  -> funApp writeMaybe [ varName w_string ]
      | otherwise -> funApp  w_string  [ varName true ]
    WString isUnique _ 
      | isUnique  -> funApp writeMaybe [ varName w_string ]
      | otherwise -> varName w_wstring

    Sequence ty mbSz mbTerm -> 
                     funApp w_sequence [ varName true
                                       , refMarshallType mInfo{forRef=False} ty
                                       , terminatorElt True mbTerm ty
                                       , szType ty
                                       , case mbSz of 
                                           Nothing -> nothing
                                           Just x  -> just (coreToHaskellExpr x)
                                       ]

{- Hmm..I'm not sure what is the right thing here - include the
   Maybe or not. If the Maybe type is used, then MarshallCore.toHaskellTy
   will need to change accordingly. -}
    Pointer Unique isExp ty
      | forRef mInfo              &&
        (optCom || optHaskellToC) &&
        isExp                     &&
        isIfaceTy ty    -> funApp writeMaybe [ w_ip ]
      where
       (Iface nm mod _ _ _ _) = getIfaceTy ty
       w_ip 
        | optHaskellToC = varName (prefix marshallRefPrefix (mkQVarName mod nm))
        | otherwise     = funApp w_iptr [w_iptr_arg]
      
    Pointer _ _ (Iface nm mod _ _ _ inh) ->
       case inh of
         [] | not optHaskellToC -> funApp w_iptr [ w_iptr_arg ]
            | otherwise         -> varName w_ip
         (x:_) 
               | optCorba          -> varName (prefix marshallRefPrefix (fst x))
               | not optHaskellToC -> funApp w_iptr [ w_iptr_arg ]
               | otherwise         -> varName (prefix marshallRefPrefix (fst x))
      where
       w_ip = prefix marshallRefPrefix (mkQVarName mod nm)

    Pointer pt _ (Name _ _ _ _ _ (Just ti))
      | pt /= Ptr && is_pointed ti -> 
          case pt of
            Unique -> 
                if finalised ti then
                   funApp writeMaybe [ varName (ref_marshaller ti) ]
                else
                   funApp writeMaybe [ varName (ref_marshaller ti) ]
            _ -> varName (ref_marshaller ti)

    Pointer Ptr _ (Name _ _ _ (Just as) _ _) 
     | as `hasAttributeWithName` "foreign" -> varName w_fptr
    Pointer _ _ ty | isVoidTy ty -> varName w_ptr
    Pointer pt _ ty
      | isIfaceTy ty -> refMarshallType mInfo{forRef=False}
                                        (Pointer Ref True (getIfaceTy ty))
      | otherwise ->
        case pt of
          Ptr     -> varName w_ptr
          Ref  
             | optNoDerefRefs -> funApp w_ref [ allocPointerTo ty, refMarshallType mInfo ty ]
             | otherwise      -> refMarshallType mInfo ty
          Unique  -> funApp w_unique   [ allocPointerTo ty, refMarshallType mInfo ty ]
    Void -> varName w_addr
    Bool -> varName w_bool
    CUnion i _ _ -> 
        let 
          nm  = mkMarshaller marshallRefPrefix t
          sw  = 
            case (getSwitchIsAttribute (idAttributes i)) of
              Just e | notNull fs -> 
                 let v = head fs in var ("write_tag_" ++ v)
                where
                 fs = findFreeVars e
              _ -> var ("where_do_I_put_the_tag")
        in
        funApply (varName nm) [sw]
    Iface{} -> refMarshallType mInfo (Pointer Ref True t)
    Name _ _ _ _ _ (Just ti) | not (is_pointed ti) -> varName (ref_marshaller ti)
    Name _ _ _ _ (Just ty@Enum{}) _ -> refMarshallType mInfo ty
    Name _ _ _ _ (Just ty@(Name{})) _ -> refMarshallType mInfo ty
    Enum{} -> varName w_enum32
    Name nm _ mod _ (Just ty) _ 
       | not is_cons_type -> refMarshallType mInfo ty
       | not (forStruct mInfo) && isNonEncUnionTy (removeNames ty) -> funApply w_ty w_args
       | otherwise        -> 
           if isFinalisedType True ty then
              funApply w_ty [ w_iptr_arg ]
           else
              w_ty
      where
       is_cons_type = isConstructedTy (nukeNames ty)
       w_ty = varName (prefix marshallRefPrefix (mkQVarName mod nm))
      
       w_args
         | isFinalisedType True ty = [ w_iptr_arg, write_tag ]
         | otherwise               = [ write_tag ]

       write_tag = lam [wildPat] (ret unit)

    Integer LongLong isSigned
      | optLongLongIsInteger -> varName (w_integer isSigned)
    _    -> varName (mkMarshaller marshallRefPrefix t)

  where
   w_bool      = libImport bool
   w_string    = libImport stringName
   w_blist     = libImport blist
   w_list      = libImport list
   w_bstring   = libImport bstring
   w_ptr       = libImport ptrName
   w_fptr      = libImport fptr
   w_ref       = libImport ref
   w_unique    = libImport unique
   w_addr      = libImport ptrName
   w_sequence  = libImport "Sequence"

   w_safearray
     | forStruct mInfo = prefix marshallRefPrefix sAFEARRAY
     | otherwise       = prefix marshallRefPrefix (mkQVarName autoLib safearray)

   w_integer isSigned
     | isSigned  = libImport integer
     | otherwise = libImport ('U':integer)

   w_enum32   = libImport enum32

   w_iptr     = prefix marshallRefPrefix iUnknown

   w_iptr_arg
     | forStruct mInfo = var "addRefMe__"
     | otherwise       = lit (BooleanLit (forProxy mInfo))

   w_wstring 
     | optNoWideStrings = wStringImport wstring2
     | otherwise        = wStringImport wstring

   libImport nm = prefix marshallRefPrefix (mkQVarName hdirectLib nm)
   wStringImport nm = prefix marshallRefPrefix (mkQVarName wStringLib nm)


refUnmarshallType :: MarshallInfo -> Type -> Haskell.Expr
refUnmarshallType mInfo t = 
  case t of
    Array (Char _) [e] -> funApp r_bstring  [ coreToHaskellExpr e ]
       -- this is the identity unmarshallers under the assumption
       -- that the array ref. type is used in conjunction with
       -- a 'dependent' attribute (size_is() et. al). This may
       -- not always be the case, so this is may cause breakage.
       --
       -- 3/99: Changed to instead do something a bit saner, but
       --       tell the user of the problem.
    Array ty []   
       -> trace ("warning: unmarshalling incomplete array type decl, " ++ showCore (ppType t) ++ 
                 " , don't know how big.\n Assuming of length 1. " ) $
          funApp r_blist
             [ szType t
             , lit (iLit (1::Int))
             , refUnmarshallType mInfo{forRef=False} ty
             ]
    Array ty [e] -> 
      funApp r_blist
             [ szType ty
             , coreToHaskellExpr e
             , refUnmarshallType mInfo{forRef=False} ty
             ]

    String _ isUnique _ 
      | isUnique  -> funApp readMaybe [ varName r_string ]
      | otherwise -> varName r_string

    WString isUnique _ 
      | isUnique  -> funApp readMaybe [ varName r_wstring ]
      | otherwise -> varName  r_wstring
    SafeArray _ -> r_safearray

    Sequence ty mbSz mbTerm -> 
                    funApp r_sequence [ refUnmarshallType mInfo{forRef=False} ty
                                      , terminatorElt False mbTerm ty
                                      , szType ty
                                      , case mbSz of 
                                          Nothing -> nothing
                                          Just x  -> just (coreToHaskellExpr x)
                                      ]

    Pointer Unique isExp ty
      | forRef mInfo              && 
        (optCom || optHaskellToC) &&
        isExp                     &&
        isIfaceTy ty    -> funApp r_unique [ u_ip ]
      where
       (Iface nm mod _ attrs _ _) = getIfaceTy ty
       u_ip 
        | optHaskellToC  =
                funApp (prefix unmarshallRefPrefix (mkQVarName mod nm))
                       (if attrs `hasAttributeWithName` "finaliser" then
                           [ lit (BooleanLit (not (forProxy mInfo))) ]
                        else
                           [{-empty-}]
                       )
        | otherwise      = funApp u_iptr [ final_arg ]

    Pointer _ _ (Iface nm mod _ attrs _ inh) ->
       case inh of
         [] | optCom     -> funApp r_iptr [ final_arg ]
            | optHaskellToC && attrs `hasAttributeWithName` "finaliser" ->
                                    funApply r_ip [ lit (BooleanLit (not (forProxy mInfo))) ]

            | otherwise  -> r_ip
         (x:_) 
               | optCorba  -> varName (prefix unmarshallRefPrefix (fst x))
               | optCom    -> funApp r_iptr [ final_arg ]
               | optHaskellToC && attrs `hasAttributeWithName` "finaliser" ->
                                    funApply (varName (prefix unmarshallRefPrefix (fst x)))
                                             [ lit (BooleanLit (not (forProxy mInfo))) ]
               | otherwise -> varName (prefix unmarshallRefPrefix (fst x))
      where
       r_ip = varName (prefix unmarshallRefPrefix (mkQVarName mod nm))

{-
    Pointer Unique isExp ty
      | forRef mInfo && 
        optCom       &&
        isIfaceTy ty    -> funApp r_unique [ funApp u_iptr [ final_arg ]]
-}

    Pointer pt _ (Name _ _ _ _ _ (Just ti))
      | pt /= Ptr && is_pointed ti -> 
          case pt of
            Unique -> funApp readMaybe [ e ]
            _      -> e
         where
          e 
           | finalised ti = funApply (varName (ref_unmarshaller ti)) [ final_arg ]
           | otherwise    = varName (ref_unmarshaller ti)

    Pointer Ref _ t1@(Name _ _ _ _ (Just ty) _)
      | isConstructedTy (nukeNames ty) -> 
              if isFinalisedType False ty then
                 funApply r_ty [ final_arg ]
              else
                 r_ty
          where
           r_ty = varName (mkMarshaller unmarshallRefPrefix t1)

    Pointer Ptr _ (Name _ _ _ (Just as) _ _)
      | as `hasAttributeWithName` "foreign" -> varName r_fptr
    Pointer _ _ Void    -> varName r_ptr
    Pointer pt _ ty
      | isIfaceTy t -> refUnmarshallType mInfo (Pointer Ref True (getIfaceTy t))
      | otherwise   -> 
      case pt of
        Ptr     -> varName r_ptr -- we stop unmarshalling once we encounter a [ptr] pointer.
        Ref     -> 
            -- I don't understand the motivation for ignoring the [ref] here any longer!
            -- Conseq, -fopt-no-deref-refs can be used to turn off this 'feature' until
            -- the ramifications of doing it one way vs. the other can be more carefully
            -- assessed (same option applies to ref-marshalling of [ref]s.)
           if (forRef mInfo || not (isPointerTy ty)) && not optNoDerefRefs then 
              refUnmarshallType mInfo ty
           else
                  funApp r_ref     [ refUnmarshallType mInfo{forRef=False} ty ]
        Unique -> funApp r_unique  [ refUnmarshallType mInfo{forRef=False} ty ]
    Void  -> varName r_addr
    Bool  -> varName r_bool
    CUnion i _ _ -> 
        let 
          nm  = mkMarshaller unmarshallRefPrefix t
          sw  = 
            case (getSwitchIsAttribute (idAttributes i)) of
              Just e | notNull fs -> 
                let v = head fs in var ("read_tag_" ++ v)
               where
                 fs = findFreeVars e
                
              _            -> var ("where_do_I_get_the_tag_from")
        in
        funApply (varName nm) [sw]
    Iface{} -> refUnmarshallType mInfo (Pointer Ref True t)
    Name _ _ _ _ _ (Just ti) {-  not (is_pointed ti) -} -> 
                if finalised ti then
                  funApply (varName (ref_unmarshaller ti)) [ final_arg ]
                else
                   varName (ref_unmarshaller ti)
    Name _ _ _ _ (Just ty@Enum{}) _ -> refUnmarshallType mInfo ty
    Name _ _ _ _ (Just ty@(Name{})) _ -> refUnmarshallType mInfo ty
    Enum{} -> varName r_enum32
    Integer LongLong isSigned
      | optLongLongIsInteger -> varName (r_integer isSigned)
    Name nm _ mod mb_attrs (Just ty) _
      | not (forRef mInfo) && isNonEncUnionTy (removeNames ty) ->
         funApply (varName (mkMarshaller unmarshallRefPrefix t))
                  r_args
      | not is_cons_type -> refUnmarshallType mInfo ty
      | is_cons_type     -> 
           if isFinalisedType False ty then
              funApply r_ty [ final_arg ]
           else
              r_ty
       where
          

          r_ty = varName (prefix unmarshallRefPrefix (mkQVarName mod nm))

          is_cons_type = isConstructedTy (nukeNames ty)
          attrs    = 
                fromMaybe [] mb_attrs ++
                idAttributes (getTyTag (getNonEncUnionTy ty))

          r_args
           | isFinalisedType False ty = [ final_arg, read_tag ]
           | otherwise                = [ read_tag ]

          read_tag = 
               case getSwitchIsAttribute attrs of
                 Nothing -> ret (lit (iLit ((-1)::Int)))
                 Just v  -> ret (coreToHaskellExpr v)
    _     -> varName (mkMarshaller unmarshallRefPrefix t)
  where
   r_bool   = libImport bool
   r_string = libImport stringName
   r_blist  = libImport blist
   r_bstring = libImport bstring
   r_ptr    = libImport ptrName
   r_fptr   = libImport fptr
   r_ref    = libImport ref
   r_unique = libImport unique
   r_addr   = libImport ptrName
   r_sequence  = libImport "Sequence"

   r_safearray 
     | forStruct mInfo = funApply (varName (prefix unmarshallRefPrefix sAFEARRAY))  [ lit (BooleanLit False) ]
     | otherwise       = funApply (varName (prefix unmarshallRefPrefix (mkQVarName autoLib safearray)))
                                  [ lit (BooleanLit (not (forProxy mInfo))) ]

   r_integer isSigned
     | isSigned  = libImport integer
     | otherwise = libImport ('U':integer)

   r_enum32 = libImport enum32

   r_iptr        = prefix unmarshallRefPrefix iUnknown

   final_arg
     | forStruct mInfo = var "finaliseMe__"
     | otherwise       = lit (BooleanLit (forProxy mInfo))

   u_iptr        = prefix unmarshallPrefix iUnknown
   r_wstring
    | optNoWideStrings = wStringImport wstring
    | otherwise        = wStringImport wstring

   libImport nm     = prefix unmarshallRefPrefix (mkQVarName hdirectLib nm)
   wStringImport nm = prefix unmarshallRefPrefix (mkQVarName wStringLib nm)


\end{code}

\begin{code}
coreToHaskellExpr :: Expr -> Haskell.Expr
coreToHaskellExpr e =
 case e of
   Binary bop e1 e2 -> binOp bop (coreToHaskellExpr e1) 
                                 (coreToHaskellExpr e2)
   Cond e1 e2 e3 ->
     hCase (coreToHaskellExpr e1) 
        [ alt (conPat (mkQConName prelude "True") [])  (coreToHaskellExpr e2)
        , alt (conPat (mkQConName prelude "False") []) (coreToHaskellExpr e3)
        ]
   Unary  uop e1 -> unaryOp uop (coreToHaskellExpr e1)
   Var nm        -> var (mkHaskellVarName nm)
   Lit l         -> lit l
   Cast _ e1     -> coreToHaskellExpr e1
--   Sizeof t      -> funApp (mkQualName Nothing sizeOfName) [szType t]
   Sizeof t      -> varName (mkMarshaller sizeofPrefix t)
\end{code}

The library functions does not rely on overloading 
of their numeric arguments. Instead, the generated code
will insert coercions that map arguments to the expected
types.

\begin{code}
coerceTy :: Type -> Type -> Haskell.Expr -> Haskell.Expr
coerceTy from_ty to_ty e 
 | to_ty == from_ty = e
 | otherwise = Haskell.WithTy
                   (funApp (mkQualName prelude (mkHaskellVarName "fromIntegral"))
                          [ e ])
                   (toHaskellBaseTy False to_ty)
\end{code}

Allocation and a couple of other functions in the support library
uses Ints as arguments; make sure that arguments passed to them
are properly coerced.

\begin{code}
coerceToInt :: Expr -> Haskell.Expr
coerceToInt e = 
  case e of
    Lit IntegerLit{} -> c_expr
    _ -> funApp fromIntegralName [c_expr]
 where
  c_expr = coreToHaskellExpr e
\end{code}


\begin{code}
szType :: Type -> Haskell.Expr
szType (Array t [e]) = binOp Mul (szType t) (coerceToInt e)
szType t 
  | isEnum        = varName enumSize 
  | isIntegerTy t = varName (prefix sizeofPrefix (mkQualName hdirectLib "Int64"))
  | otherwise = 
      case t of
        Name _ _ _ _ _ (Just ti) -> varName (prim_size ti)
        _                        -> varName (mkMarshaller sizeofPrefix t)
--        _                        -> varName (mkMarshaller sizeOfName t)
 where
  isEnum = isJust enumRes
 
  enumRes = checkIfEnum t

  (Just enumSize) = enumRes

  checkIfEnum Enum{} = Just (mkQVarName hdirectLib "sizeofInt32")
  checkIfEnum (Name _ _ _ _ (Just ty) _) = checkIfEnum ty
  checkIfEnum _                          = Nothing

\end{code}

\begin{code}
allocPointerTo :: Type -> Haskell.Expr
allocPointerTo ty  =
  case ty of
    Name _ _ _ _ _ (Just ti) | isJust (alloc_type ti) -> varName (fromJust (alloc_type ti))
    Pointer _ _ (Name _ _ _ _ _ (Just ti)) | isJust (alloc_type ti) -> varName (fromJust (alloc_type ti))
    _ -> funApp allocBytes [funApp fromIntegralName [szType ty']]
 where
  ty' = 
    case ty of
      Void             -> Pointer Ptr True Void
      Pointer _ _ Void -> ty
      _                -> removePtr ty
\end{code}

Generating a *call* to free a marshalled representation
of type t (if needed.) The modules that deal with the generation
of marshalling code for user-defined types take care of generating
freeing/release functions for such types.

\begin{code}
freeType :: Type -> Haskell.Expr
freeType ty =
 case mbFreeType ty of
   Nothing -> varName trivialFree
   Just e  -> e

mbFreeType :: Type -> Maybe Haskell.Expr
mbFreeType ty =
 case ty of
    {- Pointer to structs decorated with [free(foo)] are freed using 'foo'. -}
  Pointer _ _ (Name _ _ _ _ (Just (Struct tg _ _)) _) 
    | (idAttributes tg) `hasAttributeWithName` "free" ->
      case findAttribute "free" attrs of
        Just (Attribute _ [ParamLit (StringLit freeR)]) -> Just (var freeR)
        _                                               -> mbFreeType' True ty
    | attrs `hasAttributeWithName` "free_method" ->
        -- what an ad-hac hock!
      case findAttribute "free_method" attrs of
        Just (Attribute _ [ParamLit (StringLit freeR)]) -> 
                                Just (lam [varPat (var "x")] $
                                          funApply (var freeR) [var "x", var "iptr"])
        _ -> mbFreeType' True ty
     where
      attrs = idAttributes tg

  _ -> mbFreeType' True ty

mbFreeType' :: Bool -> Type -> Maybe Haskell.Expr
mbFreeType' isTop ty = 
 case ty of
  Pointer _ _ (Name _ _ _ _ _ (Just ti)) | is_pointed ti &&
                                         finalised ti -> Nothing
  Pointer _ _ Void    -> Nothing
  Pointer _ _ Iface{} -> Nothing
   {- The Haskell representation of a unique pointer is as a
      Maybe-valued `thing', where the `thing' is the unmarshalled
      representation of the pointer type. Clearly, the thing we're
      pointing to will have to be freed, but it in turn may have
      to free up some of its components first.
   -}
  Pointer Unique _ t
     | isIfaceTy t       -> Nothing
     | otherwise         ->
       case mbFreeType' False t of
          Just v  -> Just (funApp (prefix freePrefix (mkQVarName hdirectLib unique)) [ v ])
          Nothing | isTop     -> Just (varName free)
                  | otherwise -> Nothing

   -- no marshalling was done on this in the first place, so nothing to free.
  Pointer Ptr _ _ -> Nothing
   -- the type embedded within the reference may have to be freed ...
  Pointer Ref _ t ->
        case (mbFreeType' False t) of
          Just v  -> Just (funApp (prefix freePrefix (mkQVarName hdirectLib ref)) [v])
          Nothing | isTop     -> Just (varName free) -- just free the toplevel pointer.
                  | otherwise -> Nothing

   -- the array may consist of elements that may need to be freed
   -- individually, so we better check by looking at the element type..
   -- [in many cases element-wise freeing isn't right, since the
   --  array was block allocated initially, so let's not do this for
   --  now -- assume block allocation.]
  Array aty _           
   | needsFreeing aty -> Just (varName free)
   | isTop            -> Just (varName free)
   | otherwise        -> Nothing

   -- the String/sequence type contains a null-terminated sequence of unpointed objects,
   -- so no need to do per-elt release/free.
  String{}          -> Just (varName (prefix freePrefix (mkQVarName hdirectLib stringName)))
  WString{}         -> Just (varName (prefix freePrefix (mkQVarName wStringLib wstring)))
  Sequence{}        -> Just (varName (prefix freePrefix (mkQVarName hdirectLib "Sequence")))
   -- nothing to free for marshalled enums (not in an external heap, anyway.)
  Enum{} -> Nothing
   -- Note: a toplevel struct is currently not possible, so we don't
   -- actually free the structure itself, but assume that whoever has a pointer to
   -- this struct value does. 
   -- 
   -- Notice that since we have complete information about what the struct
   -- contains, we could have generated the calls to free up the fields
   -- that needs to be explicitly release here. However, the cost of doing
   -- this inline hardly seems worth it. (Rely on the Haskell compiler
   -- to do the inlining instead.)
   -- 
  Struct tg fields _ 
          | any (needsFreeing.fieldType) fields -> Just (varName (prefix freePrefix (mkVarName (idName tg))))
          | otherwise -> Nothing

   -- See above comment for structs.
   -- The tag type isn't pointed (I hope!), so we only need to worry about
   -- freeing the embedded value.
  Union i _ _ _ switches 
   | swNeedsFreeing switches -> Just (varName (prefix freePrefix (mkVarName (idName i))))
   | otherwise -> Nothing

  UnionNon i switches 
   | swNeedsFreeing switches ->  Just (varName (prefix freePrefix (mkVarName (idName i))))
   | otherwise -> Nothing

  CUnion i fields _
   | any (needsFreeing.fieldType) fields ->  Just (varName (prefix freePrefix (mkVarName (idName i))))
   | otherwise -> Nothing

  Name nm _ mod mb_attrs mb_ty mb_ti
    | don'tFree mb_attrs || 
      (isJust mb_ti && 
        finalised (fromJust mb_ti)) -> Nothing
    | isJust mb_ti && isJust (free_type (fromJust mb_ti)) -> fmap varName (free_type (fromJust mb_ti))
    | otherwise ->
     case mb_ty of
       Nothing -> Nothing -- Hmm..
       Just t 
             -- want the name of the constructed type rather than its tag...if it needs to be released.
         | isConstructedTy (nukeNames t) -> 
                case mbFreeType' False t of
                  Just _ | isTop -> Just (funApply (varName (prefix freePrefix (mkQConName mod (mkHaskellTyConName nm))))
                                                   args)
                  _ -> Nothing
         | otherwise ->
           case mbFreeType' False t of
             Just aty | not (don'tFree mb_attrs) -> Just aty
                      | otherwise                -> Nothing
             Nothing -> Nothing
            where
             args
              | isNonEncUnionTy t = [read_tag]
              | otherwise         = []

             attrs    = fromMaybe [] mb_attrs

             read_tag = 
               case getSwitchIsAttribute attrs of
                 Nothing -> lit (iLit ((-1)::Int))
                 Just v  -> coreToHaskellExpr v
       
  _ -> Nothing

 where
  don'tFree Nothing   = False
  don'tFree (Just ls) = ls `hasAttributeWithName` "nofree"


-- poorly named, but `needsFreeing' returns True
-- if a marshalled representation of an IDL type
-- has to be explicitly released (=> it was allocated
-- externally.)
needsFreeing :: Type -> Bool
needsFreeing t = go True t
 where
  go toplevel ty = 
   case ty of
    Pointer _ _ (Name _ _ _ _ _ (Just ti)) | is_pointed ti -> not (finalised ti)
    Pointer Unique _ pty -> needsFreeing pty
    Pointer Ref _ pty -> needsFreeing pty
    Pointer Ptr _ _ -> False
    Array _ _ -> True
    String{}  -> toplevel
    WString{} -> toplevel
    Name _ _ _ _ _ (Just ti) -> not (finalised ti)
    Name _ _ _ _ mb_ty _ -> mapFromMb False needsFreeing mb_ty
    Struct _ fields _ -> any ((go False).fieldType) fields
    Union _ _ _ _ switches -> any sw_go switches
    UnionNon _ switches    -> any sw_go switches
    CUnion _ fields _ -> any ((go False).fieldType) fields
    _ -> False -- basic numeric/char types + void & enum.

  sw_go (SwitchEmpty _) = False
  sw_go s               = go False (switchType s)

swNeedsFreeing :: [Switch] -> Bool
swNeedsFreeing [] = False
swNeedsFreeing (SwitchEmpty _ : xs) = swNeedsFreeing xs
swNeedsFreeing (s : xs) = (needsFreeing (switchType s)) || swNeedsFreeing xs
\end{code}
