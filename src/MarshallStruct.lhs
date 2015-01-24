%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Feb. 9th 2003  08:01  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Generating code for going between IDL structs and
their Haskell equivalent.

\begin{code}
module MarshallStruct ( marshallStruct ) where

import BasicTypes ( Name, QualName, qName )
import Literal ( iLit )

import Attribute
import AbstractH  ( HDecl )
import qualified AbstractH as Haskell ( Expr, ConDecl )
import AbsHUtils
import LibUtils
import CgMonad

import CoreIDL
import CoreUtils
import MarshallUtils
import MarshallMonad
import MarshallType ( refMarshallType
                    , refUnmarshallType
                    , unmarshallType
                    , marshallType
                    , freeType
                    , needsFreeing
                    )

import MarshallDep ( marshallDependents
                   , unmarshallDependents
                   )
import MarshallCore
import Data.List   ( findIndex, partition )
import Data.Maybe  ( mapMaybe  )
import Utils       ( diff, notNull )
import Opts        ( optCom )

\end{code}

The marshalling of structs is mostly complete, but here's
a list of current shortcomings/ToDos:

 - structure layout is very simplistic and wrong.
 - there's chance of name capture, as the marshalling
   routines introduce the names "ptr" and "pf"[0-9]+
 - [ignore] attributes may just work..(lightly tested.)   

\begin{code}
marshallStruct :: Name 
               -> Id 
               -> Haskell.ConDecl 
               -> [Field] 
               -> Maybe Int
               -> CgM HDecl
marshallStruct tdef_name struct_tag datacon fields mb_pack = do
  ds <- mapM exportDecl decl_list
  return (andDecls ds)
 where
    decl_list =
      (if needToFree then
         ((f_name, f_tysig `andDecl`  f_def):)
       else
         id) $
      (if simplStruct then
         (\ x -> (m_name, m_tysig `andDecl`  m_def):
                 (u_name, u_tysig `andDecl`  u_def):x)
       else
         id)
      [ (w_name, w_tysig `andDecl`  w_def)
      , (r_name, r_tysig `andDecl`  r_def)
      , (s_name, s_tysig `andDecl`  s_def)
      ]

     {-
       To be able to marshal stuff like
       
         typedef struct { uint64 w ; } ULARGE_INTEGER;     
     
       by value, we need to check if this is possible.
     -}
    simplStruct = length (fields) == 1 &&
                  isSimpleTy field_ty

    addRef_fields = isFinalisedType True  (Struct struct_tag fields Nothing)
    final_fields = isFinalisedType  False (Struct struct_tag fields Nothing)

    [field]    = fields
    field_ty   = fieldType field
    field_h_ty = toHaskellBaseTy False field_ty
    v_field    = var (mkHaskellVarName (idName (fieldId field)))

    name       = mkConName tdef_name

    t_ty       = tyConst tdef_name
    b_ty       = tyConst tdef_name

    ptr         = var "ptr"
    pf0         = var "pf0"
    field_names = map (idName.fieldId) fields

    structCon  = conDeclToCon datacon 
    structPat  = conDeclToPat datacon

    {-
      Find out which struct members depend on the value of
      others.
    -}
    dep_list = findFieldDependents fields
    fields_w = mapMaybe (adjustField True  dep_list) fields
    fields_r = mapMaybe (adjustField False dep_list) fields

    -- *** Reference marshalling ***
    w_name     = qName (prefix marshallRefPrefix name)
    w_tysig    = typeSig w_name w_type
    w_type
      | optCom && addRef_fields = funTy tyBool (funTy (tyPtr b_ty) (funTy t_ty io_unit))
      | otherwise               = funTy (tyPtr b_ty) (funTy t_ty io_unit)
    w_def      = funDef w_name w_pats w_rhs
    w_pats
      | optCom && addRef_fields = [patVar "addRefMe__", varPat ptr, structPat]
      | otherwise               = [varPat ptr, structPat]

    w_rhs      = runMm Nothing field_names w_rest w_unpack
    w_rest     = foldr ($) (ret unit) ((hLet pf0 ptr) : w_fields)

    w_fields = zipWith3 (refMarshallField dep_list
                                          (offsetOfName fields offsets)
                                          (findFieldTy fields))
                        rel_offsets
                        [(1::Int)..]
                        (tagLast fields_w)

    w_unpack = marshallDependents True{-inside struct-} False{- for a (client) stub-}
                                  dep_list (findFieldOrigTy fields)
      -- here's a hack for you: when writing the contents of a field such
      --    [size_is(x)]int y[];
      -- 
      -- we cannot do the normal trick of first marshalling the Haskell list
      -- representing 'y' into a chunk of mem. followed by filling in the struct
      -- with a pointer to it, because the assumed layout is for y[] to be inlined
      -- into the struct. 
      -- 
      -- The solution is to remove such fields from the list that's presented to
      -- the 'dependent-arg' marshalling code.
      -- 
     
    -- *** Reference unmarshalling ***
    r_name   = qName (prefix unmarshallRefPrefix name)
    r_tysig  = typeSig r_name r_type
    r_type 
      | final_fields = funTy tyBool (funTy (tyPtr b_ty) (io t_ty))
      | otherwise    = funTy (tyPtr b_ty) (io t_ty)
    r_def    = funDef r_name r_pats r_rhs
    r_pats
      | final_fields = [patVar "finaliseMe__", varPat ptr]
      | otherwise    = [varPat ptr]
    r_rhs    = hLet pf0 ptr r_fields
    r_fields =
      foldr
       ($)
       (runMm Nothing field_names (ret structCon) r_pack)
       (zipWith3 (refUnmarshallField (offsetOfName fields offsets)
                                     (findFieldTy fields))
                 rel_offsets
                 [(1::Int)..]
                 (tagLast fields_r))

    r_pack   = unmarshallDependents True False dep_list (findFieldOrigTy fields)

     -- shouldn't really generate the next two..
    m_name   = qName (prefix marshallPrefix name)
    m_tysig  = typeSig m_name (funTy t_ty (io field_h_ty))
    m_def    = funDef m_name [structPat] m_rhs
    m_rhs    = ret v_field   -- we know it is already in a marshaled form!

    u_name   = qName (prefix unmarshallPrefix name)
    u_tysig  = typeSig u_name (funTy field_h_ty (io t_ty))
    u_def    = funDef u_name [varPat v_field] u_rhs
    u_rhs    = ret structCon -- we know it is already in an unmarshaled form!
         

    s_name   = qName (prefix sizeofPrefix name)
    s_tysig  = typeSig s_name tyWord32
    s_def    = funDef s_name [] s_rhs
    -- Not right, struct alignment/pad not taking into consid.
    s_rhs
      | null fields = var "0"
      | otherwise   = var (show sz)

    rel_offsets = diff offsets

    ((sz, _), offsets) = computeStructSizeOffsets mb_pack fields

    -- Freeing a struct.
    needToFree = needsFreeing (Struct struct_tag fields Nothing)
    
    f_name   = qName (prefix freePrefix name)
    f_tysig  = typeSig f_name (funTy (tyPtr b_ty) (io_unit))
    f_def    = funDef f_name [varPat ptr] f_rhs
    f_rhs    = foldr1 (bind_) (map unmarshalTag field_switches ++
                              (mapMaybe freeField fields_sans_switches))

    (field_switches, fields_sans_switches) =
       partition (\ (f,_) -> isSwitchDependee dep_list (fieldId f))
                 (zip fields offsets)

    struct_ptr__ = var "struct_ptr__"
    field_ptr__  = var "field_ptr__"

    unmarshalTag (f,offset) =
          {- unpack the tag of a union, will be needed later when
             the union is being freed up.
          -}
           let v = idName (fieldId f) in
           bind (funApply (refUnmarshallType structMarshallInfo (toBaseTy (fieldType f)))
                          [addPtr (var "ptr") (lit (iLit offset))])
                (var v)
                (ret unit) -- sigh, shouldn't be forced to do this.

    freeField (f , offset)
      | not (needsFreeing (fieldType f)) = Nothing
      | otherwise                        = 
         let
          ty = fieldOrigType f
          e  = freeType ty
         in
         Just (hLet struct_ptr__ (addPtr ptr (lit (iLit offset)))
                    (if (isPointerTy ty) then
                        (bind (funApp derefPtr [ struct_ptr__ ]) field_ptr__
                              (funApply e [field_ptr__]))
                     else
                        (funApply e [struct_ptr__])))

offsetOfName :: [Field] -> [Int] -> Name -> Int
offsetOfName fields offsets nm =
   case (findIndex (\ f -> idName (fieldId f) == nm) fields) of
     Nothing -> (-1)
     Just v  -> offsets!!v
\end{code}

When marshalling the Haskell representation of a "struct"
into its external representation, @refMarshallField@ takes
care of generating code to marshall a given field plus
set up the offset for the code that will unmarshall the
next "struct" field (if any.)

\begin{code}
refMarshallField :: DependInfo
                 -> (Name -> Int)
                 -> (Name -> Type)
                 -> Int 
                 -> Int 
                 -> (Field, Bool)
                 -> (Haskell.Expr -> Haskell.Expr)
refMarshallField dep_list to_offset lookup_ty offset field_no (field, is_last) = \ hole ->
    hLet pf (addPtr pf_prev (lit (iLit offset))) $
    bind_ (funApply mshaller args) hole
  where
   f_id = fieldId field
   ty   = fieldType field

    {-
      The [switch_is(..)] attribute for a field is pinned on the
      field Id and not the type, so we have to figure the expression
      which computes the dependee argument here rather than
      in MarshallType.refMarshallType.
    -}
   args 
    | isNonEncUnionTy ty = [dependee_arg, pf, fi]
    | otherwise          = [pf, fi]

   dependee_arg =
    case (getSwitchIsAttribute (idAttributes f_id)) of
      Just e | notNull fs -> 
        let v = head fs in
        funApply (refMarshallType stubMarshallInfo (toBaseTy (lookup_ty v)))
                 [addPtr (var "pf0") (lit (iLit (to_offset v)))]
        where
         fs = findFreeVars e

      _      -> (lam [wildPat] (ret unit))

   mshaller
    | isDepender dep_list f_id && 
      not (isSwitchDepender dep_list f_id) &&
      not (isArrayTy ty)
        = refMarshallType structMarshallInfo addrTy
    | is_last && isArrayTy ty = marshallType structMarshallInfo{forInOut=True} ty
        {-
          If we've got a VARIANT, copy the struct in.
          ToDo: add special support for this in TypeInfos.
        -}
    | isVariantTy ty = varName (prefix copyPrefix vARIANT)
    | otherwise      = refMarshallType structMarshallInfo ty
    

   fi
    | hasIgnoreAttribute f_id = varName nullPtr
    | otherwise               = var (mkHaskellVarName (idName f_id))

   pf      = mkFieldPtrName field_no
   pf_prev = mkFieldPtrName (field_no - 1)

mkFieldPtrName :: Int -> Haskell.Expr
mkFieldPtrName field_no = varName (prefix "pf" (mkVarName (show field_no)))

\end{code}

When unmarshalling a "struct" from its external representation
to its Haskell representation, @refUnMarshallField@ takes
care of generating code to unpack a given field.

\begin{code}
refUnmarshallField :: (Name -> Int) 
                   -> (Name -> Type)
                   -> Int
                   -> Int
                   -> (Field, Bool)
                   -> (Haskell.Expr -> Haskell.Expr)
refUnmarshallField to_offset lookup_ty offset field_no (field, is_last) hole =
  hLet pf (addPtr pf_prev (lit (iLit offset))) (binders hole)
  where
   f_id   = fieldId field
   ty     = fieldType field
   o_ty   = removeNames (fieldOrigType field)

   fi      = var (mkHaskellVarName (idName f_id))
   pf      = mkFieldPtrName field_no
   pf_prev = mkFieldPtrName (field_no -1)

   args 
    | isNonEncUnionTy ty = [dependee_arg, pf]
    | otherwise          = [pf]

   dependee_arg =
    case (getSwitchIsAttribute (idAttributes f_id)) of
      Just e | notNull fs -> 
        let v = head fs in
        funApply (refUnmarshallType structMarshallInfo (toBaseTy (lookup_ty v)))
                 [addPtr (var "pf0") (lit (iLit (to_offset v)))]
       where
        fs = findFreeVars e
      _ -> ret (lit (iLit ((-1)::Int)))

   binders 
    | hasIgnoreAttribute f_id = hLet fi (varName nullPtr)
    | otherwise               = bind (funApply un_marshaller args) fi
  
   un_marshaller
        --
        -- A special case is VARIANT types, which are represented
        -- by a pointer to the external VARIANT struct. Hence, we don't
        -- reference-unmarshal these, but value-unmarshal. Similarly
        -- with arrays embedded inside structs; marshall them by value.
    | is_last && isArrayTy o_ty = unmarshallType structMarshallInfo{doFree=True} ty
    | isVariantTy ty   = unmarshallType structMarshallInfo{doFree=True} ty
    | otherwise        = refUnmarshallType structMarshallInfo ty

tagLast :: [a] -> [(a,Bool)]
tagLast []  = []
tagLast [x] = [(x,True)]
tagLast (x:xs) = (x,False) : tagLast xs
\end{code}

