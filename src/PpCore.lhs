% 
% (c) The Foo Project, Universities of Glasgow & Utrecht, 1997-8
%
% @(#) $Docid: Feb. 9th 2003  14:51  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Pretty printing the Core IDL type

\begin{code}
module PpCore where

import CoreIDL
import Literal
import BasicTypes
import PP
import Opts  ( optDebug, optHaskellToC, optShortHeader,
               optCompilingMsIDL
             )
import Data.List  ( partition )
import Utils      ( mapMb, notNull )
import Attribute  ( hasAttributeWithName )

import Data.Maybe ( fromMaybe )
import Data.Char  ( isAlphaNum, toUpper )

\end{code}

The Doc type when pretty printing core IDL carries around
a flag indicating how much info we should print out.

\begin{code}
type CoreDoc = 
  PPDoc ( Bool   -- output debugging information?
        , Bool   -- output as C decls?
        , Bool   -- within a C comment?
        , Bool   -- expand library decls?
        , String -- type of the 'this' pointer.
                 -- "" => not processing an object interface.
        )

showCore :: CoreDoc -> String
showCore cd = showPPDoc cd (optDebug, False, False, True, "")

showHeader :: String -> CoreDoc -> String
showHeader fname cd = 
  showPPDoc (text "#ifndef" <+> text fname' $$
             text "#define" <+> text fname' $$
             cd $$ text "#endif")
            (False, True{-as C-}, False, True, "")
 where
  uu     = "__"
  fname' = uu ++ map canon fname ++ uu

  canon x
   | isAlphaNum x = toUpper x
   | otherwise    = '_'


ppCore :: [Decl] -> CoreDoc
ppCore ls = vsep (map ppDecl ls) $$ text ""

ppHeaderDecl :: [Id] -> Decl -> CoreDoc
ppHeaderDecl is d = vsep (map forwardDecl is) $$
                    ppDecl d $$
                    text ""
 where 
   forwardDecl i
     | is_object =
       let nm = idName i in
       text "#ifndef __" <> text nm <> text "_FWD_DEFINED__" $$
       text "#define __" <> text nm <> text "_FWD_DEFINED__" $$
       text "typedef struct" <+> text nm <+> text nm <> semi $$
       text "#endif"
     | otherwise = empty
    where
     attrs      = idAttributes i
     is_object  = 
        attrs `hasAttributeWithName` "object" ||
        attrs `hasAttributeWithName` "odl"
    

setDebug :: Bool -> CoreDoc -> CoreDoc
setDebug deb d = 
  getPPEnv $ \ (_,as_c,comment,flg,str) ->
  setPPEnv (deb,as_c,comment,flg,str) d

getCommentFlag :: (Bool -> CoreDoc) -> CoreDoc
getCommentFlag cont = getPPEnv $ \ (_,_,comment,_,_) -> cont comment

inComment :: CoreDoc -> CoreDoc
inComment d1 = 
  getPPEnv $ \ (deb,c,_,flg,str) ->
  setPPEnv (deb,c,True,flg,str) d1 

setLibFlag :: Bool -> CoreDoc -> CoreDoc
setLibFlag flg d =
  getPPEnv $ \ (deb,as_c,comment,_,str) ->
  setPPEnv (deb,as_c,comment,flg,str) d

ifTopLevLib :: CoreDoc -> CoreDoc -> CoreDoc
ifTopLevLib if_true if_false = 
  getPPEnv $ \ (_,_,_,flg,_) ->
  if flg then
    if_true
  else
    if_false


ifC :: CoreDoc -> CoreDoc -> CoreDoc
ifC onTrue onFalse = 
  getPPEnv $ \ (_,flg,_,_,_) ->
  if flg then
     onTrue
  else
     onFalse

setThisType :: String -> CoreDoc -> CoreDoc
setThisType this_ty d = 
  getPPEnv $ \ (deb,as_c,comment,flg,_) ->
  setPPEnv (deb,as_c,comment,flg,this_ty) d

getThisType :: (String -> CoreDoc) -> CoreDoc
getThisType cont =
  getPPEnv $ \ (_,_,_,_,this_ty) ->
  cont this_ty

whenNotC :: CoreDoc -> CoreDoc
whenNotC d = ifC empty d

commentOutIfC :: CoreDoc -> CoreDoc
commentOutIfC d = ifC (commentOut d) d

commentOut :: CoreDoc -> CoreDoc
commentOut d = 
   getCommentFlag $ \ flg ->
      start_comment flg <> inComment d <> end_comment flg
 where
  start_comment insideComment
   | insideComment = empty
   | otherwise     = text "/*"

  end_comment insideComment
   | insideComment = empty
   | otherwise     = text "*/"

getIfC :: (Bool -> CoreDoc) -> CoreDoc
getIfC cont = 
  getPPEnv $ \ (_,flg,_,_,_) ->
  cont flg

ifDebug :: CoreDoc -> CoreDoc -> CoreDoc
ifDebug onTrue onFalse =
  getPPEnv $ \ (flg,_,_,_,_) ->
  if flg then
     onTrue
  else
     onFalse
\end{code}

\begin{code}
ppDecl :: Decl -> CoreDoc
ppDecl (Typedef i t orig_ty) =
  (if ignorable then commentOutIfC else id) $
  case orig_ty of
   FunTy cc res ps ->
        text "typedef" <+> ppType (resultType res) <+> 
             parens ( ppCallConv True cc <+> char '*' <> ppId i id) <>
             ppTuple (map ppParam ps) <> semi
   _ ->
    ifC (ppId i (\ x -> text "typedef" <+> x <+> ppType orig_ty))
        (ppId i (\ x -> text "typedef" <+> x <+> ppType t)) <> semi
 where
  ignorable = idAttributes i `hasAttributeWithName` "ignore"

ppDecl (Constant i _ o_t e) =
 ifC (text "#define") (text "const") <+> ppId i (<> ifC empty (ppType o_t)) <+> ifC empty equals <+> ppExpr e <> ifC empty semi

ppDecl (Interface i is_ref inherit decls)
  | is_ref =
    ifC empty
        (ppId i ($+$ (text "interface")) <> semi)
  | otherwise  =
    ifC
      pprIface
      ((hang (ppIdVert i ($+$ (text "interface")) <+> pp_inherit <+> char '{')
         3   (ppCoreDecls decls (map ppDecl decls))) $$
       char '}' <> semi)
 where
  attrs      = idAttributes i
  is_object  = 
        attrs `hasAttributeWithName` "object" ||
        attrs `hasAttributeWithName` "odl"

  pprIface
   | optShortHeader || pure_dispatch = commentOutIfC (text "interface" <+> text (idOrigName i) <+> text "{};")
   | not is_object = text "typedef struct" <+> text (idOrigName i) <+> char '*' <> text (idOrigName i) <> semi
   | otherwise     =
--      text "typedef struct" <+> text (idOrigName i) <+> text (idOrigName i) <> semi $$
      ppCoreDecls non_meth_decls (map ppDecl non_meth_decls) $$
       (hang (text "typedef struct" <+> text (idOrigName i ++ "Vtbl") <+> char '{')
         3   ((if optHaskellToC then id else setThisType (idOrigName i))
                ( ppInhMethodFiller $$ 
                  ppCoreDecls meth_decls (map ppDecl the_meth_decls)))) $$
       char '}' <+> text (idName i ++ "Vtbl") <> semi $$ text "" $$
       hang (text "struct" <+> text (idName i) <+> char '{')
         2  (text "struct" <+> text (idName i ++ "Vtbl") <+> char '*' <> text "lpVtbl" <> semi $$
             char '}' <> semi) $$
       if optHaskellToC then 
          empty 
       else
          text "#ifdef COBJMACROS" $$
          vcat (map (mkObjMacros (idName i)) decls) $$
          text "#endif" <+> commentOut (text "COBJMACROS")

  the_meth_decls = map (removeAttrs) meth_decls

  removeAttrs m  = m{declId=(declId m){idAttributes=[]}}

  (meth_decls, non_meth_decls) = partition isMethod decls

  ppInhMethodFiller
    | is_idispatch || is_iunknown = empty
    | is_dispatch  = ppDecls (map (\ x -> text "void*" <+> text ("reserved"++show x)) [(0::Int)..6])
    | otherwise    = ppDecls (map (\ x -> text "void*" <+> text ("reserved"++show x)) [(0::Int)..2])

  is_dispatch = any (\ x -> qName (fst x) == "IDispatch") inherit

  (is_idispatch, is_iunknown) =
    case (idOrigName i) of
      "IDispatch" -> (True, False)
      "IUnknown"  -> (False, True)
      _           -> (False, False)

  pure_dispatch = not is_idispatch && is_dispatch && not has_dual
    
  has_dual = (idAttributes i) `hasAttributeWithName` "dual"

  isMethod (Method _ _ _ _ _) = True
  isMethod _                  = False

  mkObjMacros if_nm (Method methId _ _ args _) = 
    hang (text "#define" <+> text (if_nm ++ '_':idOrigName methId) <> arg_list <+> char '\\')
      5  (text "(This)->lpVtbl->" <> text (idOrigName methId) <> arg_list)
   where
    arg_list = parens (hcat (punctuate comma (map text ("This" : map (idName.paramId) args))))
  mkObjMacros _ _ = empty

  pp_inherit
   | not optDebug && optCompilingMsIDL =
      case inherit of
          []        -> empty
          ((x,_):_) -> char ':' <+> text (qName x)
   | otherwise =
   case inherit of
     [] -> empty
     ls -> char ':' <+> hsep (punctuate comma (map (\ (x,y) -> ppQualName x <> 
                                                               commentOut (text (show y))) ls))

ppDecl (Module i decls) =
 ifC (ppCoreDecls decls (map ppDecl decls))
     (hang (ppIdVert i ($+$ (text "module")) <+> char '{')
        3   (ppCoreDecls decls (map ppDecl decls)) $$
      text "};")

ppDecl (DispInterface i (Just d) _ _) =
 ifC (commentOutIfC (text "dispinterface" <+> text (idName i) <+> text "{};"))
     (hang (ppIdVert i ($+$ (text "dispinterface")) <+> char '{')
        3  (ppId (declId d) (\ x -> x <+> text "interface") <> semi $$ ifDebug (ppDecl d) empty) $$
      whenNotC (text "};"))

ppDecl (DispInterface i _ props meths) =
 ifC (commentOutIfC (text "dispinterface" <+> text (idName i) <+> text "{};"))
     (hang (ppIdVert i ($+$ (text "dispinterface")) <+> char '{')
       3   (hang (text "properties:")
             2   (ppDecls (map ppDecl props)) $$
            hang (text "methods:")
             2   (ppCoreDecls meths (map ppDecl meths))) $$
      text "};")

ppDecl (Library i [])
 | not optDebug  = text "importlib" <> parens (text (show (idName i))) <> semi
ppDecl (Library i decls) =
 ifC 
   (commentOutIfC (text "library" <+> text (idOrigName i)))
   (ppIdVert i ($+$ (text "library"))) $$
 ifTopLevLib 
   (commentOutIfC (char '{') $$
    setLibFlag False (ppFwdDecls $$
                      ppCoreDecls decls (map ppDecl decls)) $$
    commentOutIfC (text "};"))
   (commentOutIfC (text "{};"))
 where
  ifaces = filter isInterface decls

  ppFwdDecls = vsep (map ppFwdDecl ifaces)

  ppFwdDecl (Interface ifaceId _ inherit _) =
   ifC (if pure_dispatch then 
           empty 
        else 
           text "typedef struct" <+> text (idOrigName ifaceId) <+> text (idOrigName ifaceId) <> semi)
       (text "interface" <+> text (idOrigName ifaceId) <> semi)
    where
     is_idispatch =
      case (idOrigName ifaceId) of
        "IDispatch" -> True
        _           -> False

     pure_dispatch = not is_idispatch && is_dispatch && not has_dual
     is_dispatch = any (\ x -> qName (fst x) == "IDispatch") inherit
     has_dual = (idAttributes ifaceId) `hasAttributeWithName` "dual"

  ppFwdDecl _ = empty

  isInterface Interface{} = True
  isInterface _           = False


ppDecl (CoClass i decls) =
 ifC (commentOutIfC (text "coclass" <+> text (idOrigName i) <+> text "{};"))
     (hang (ppIdVert i ($+$ (text "coclass")) <+> char '{')
       3   (ppDecls (map ppCoClassDecl decls)) $$
      commentOutIfC (text "};"))

ppDecl (Property i ty _ _ _) = ppId i ($$ (ppType ty)) <> semi

ppDecl (Method i cconv res args _) = 
  getThisType $ \ str ->
  (if (null str) then -- not an object method.
     ppId i ($$ (ifC (text "extern") empty <+> ppResult res <+> ppCallConv True cconv))
   else
     ppResult res <+> parens ( ppMethodId i (\ x -> ppCallConv True cconv <+> char '*' <> x)))
       <+>
  pp_param <> semi
 where
  ppMethodId mid cont = ifC (ppId i' cont) (ppId mid  cont)

  attrs = idAttributes i

  i'
   | attrs `hasAttributeWithName` "propget" = i{idOrigName="get"++idOrigName i}
   | attrs `hasAttributeWithName` "propput" = i{idOrigName="put"++idOrigName i}
   | attrs `hasAttributeWithName` "propputref" = i{idOrigName="put"++idOrigName i}
   | otherwise                                 = i

  pp_param =
   getThisType $ \ str ->
   let
    ty = Pointer Ref False (Name str str Nothing Nothing Nothing Nothing)

    args' 
     | null str  = args
     | otherwise = (Param (Id "This" "This" Nothing []) 
                          In ty ty False):args
   in                     
   ppTupleVert (map ppParam args')

ppDecl (HsLiteral str) = whenNotC (text "haskell" <> parens (text str) <> semi)
ppDecl (CInclude  str) = text "include" <+> text str
ppDecl (CLiteral str) = 
  ifC (text str)
      (text "cpp_quote" <> parens (text str))

ppCoreDecls :: [Decl] -> [CoreDoc] -> CoreDoc
ppCoreDecls [] [] = empty
ppCoreDecls (CInclude _ : as) (b:bs) = b $$ ppCoreDecls as bs
ppCoreDecls (CLiteral _ : as) (b:bs) = b $$ ppCoreDecls as bs
ppCoreDecls (_:as) (b:bs)            = b $$ ppCoreDecls as bs
ppCoreDecls [] as = vcat as
ppCoreDecls _ []  = empty

ppCoClassDecl :: CoClassDecl -> CoreDoc
ppCoClassDecl (CoClassInterface i _)     = ppId i (<+> whenNotC (text "interface"))
ppCoClassDecl (CoClassDispInterface i _) = ppId i (<+> whenNotC (text "dispinterface"))
\end{code}

@ppId@ takes the extra function argument to allow the attributes
to be printed not immediately next to the identifier name.
For instance, when printing out an interface decl, the attributes of the
interface id should be prefixed to the "interface" keyword rather
than next to the id.

\begin{code}
ppId :: Id -> (CoreDoc -> CoreDoc) -> CoreDoc
ppId i ty
  | notNull attrs = (ty (commentOutIfC (ppList (map ppAttr attrs)))) <+> ppModule i
  | otherwise     = ty empty <+> ppModule i
  where
    attrs  = idAttributes i

--
-- [ a1
-- , a2
--   ..
-- ]
-- foo

ppIdVert :: Id -> (CoreDoc -> CoreDoc) -> CoreDoc
ppIdVert i ty             
  | notNull attrs' = (ty (commentOutIfC (ppListVert (map ppAttr attrs')))) <+> ppModule i
  | otherwise      = ty empty <+> ppModule i
  where
    attrs  = idAttributes i
    attrs' = filter notIsAny attrs
    
    notIsAny (Attribute "any" _) = False
    notIsAny _                   = True

-- print the name of an Id, and possibly what 
-- file/module it is coming from.
ppModule :: Id -> CoreDoc
ppModule i =
   ifDebug 
      (text $
       case mb_mod of
         Nothing  -> nm
         Just m   -> m++'.':nm)
      (text (idOrigName i))
   where
    mb_mod = idModule i
    nm     = idName i

\end{code}

\begin{code}
ppAttr :: Attribute -> CoreDoc
ppAttr (AttrMode dir)        = ppDirection dir
ppAttr (Attribute nm [])     = text nm
ppAttr (Attribute nm ps)     = text nm <> ppTuple (map ppAttrParam ps)
ppAttr (AttrDependent r ps)  = ppDepReason r <> ppTuple (map ppAttrParam ps)

ppAttrParam :: AttributeParam -> CoreDoc
ppAttrParam (ParamLit  l) = ppLit l
ppAttrParam (ParamType t) = ppType t
ppAttrParam (ParamExpr e) = ppExpr e
ppAttrParam (ParamVar n)  = text n
ppAttrParam ParamVoid     = empty
ppAttrParam (ParamPtr a)  = char '*' <> ppAttrParam a

ppDepReason :: DepReason -> CoreDoc
ppDepReason r = 
  text $
  case r of
    SizeIs   -> "size_is"
    LengthIs -> "length_is"
    LastIs   -> "last_is"
    FirstIs  -> "first_is"
    MaxIs    -> "max_is"
    MinIs    -> "min_is"
    SwitchIs -> "switch_is"
\end{code}

\begin{code}
ppType :: Type -> CoreDoc
ppType (Integer Natural True)   = text "int"
ppType (Integer LongLong True)  = text "int64" 
ppType (Integer LongLong False) = text "uint64" 
ppType (Integer sz signed) = (if signed then empty else text "unsigned") <+> ppSize sz
ppType StablePtr =
  ifC (text "unsigned long")
      (text "stablePtr")

 -- (res (*)(args))
ppType (FunTy cc res params) = ppFunTy cc res params

ppType (Float sz) =
 case sz of
   Short    -> text "float"
   Long     -> text "double"
   LongLong -> text "long double"
   Natural  -> text "float"            

ppType (Char signed)
 | signed    = text "signed char"
 | otherwise = text "char"

ppType WChar  = text "wchar_t"
ppType Bool   = text "boolean"
ppType Octet  = text "char"
ppType Any    = text "any"
ppType Object = text "Object"
ppType (String _ isUnique mb_expr) =
  ifC (text "char*")
      ((if isUnique then text "[unique]" else empty) <> text "string" <> pp_expr)
 where
  pp_expr =
   fromMaybe empty
             (mapMb (\ e -> char '<' <> ppExpr e <> char '>') mb_expr)

ppType (WString isUnique mb_expr) =
 ifC (text "WCHAR*")
     ((if isUnique then text "[unique]" else empty) <> text "wstring" <> pp_expr)
 where
  pp_expr =
   fromMaybe empty
             (mapMb (\ e -> char '<' <> ppExpr e <> char '>') mb_expr)

ppType (Sequence t mb_expr _) =
  text "sequence" <> char '<' <>
  ppType t <> pp_expr
  where
  pp_expr =
   fromMaybe (char '>')
             (mapMb (\ e -> comma <> ppExpr e <> char '>') mb_expr)

ppType (Fixed e i) =
  text "fixed" <> char '<' <>
  ppExpr e <> comma <> ppILit i <> char '>'

--ppType (Name nm onm mod attrs (Just ty) _) = ppModule (Id nm nm mod (fromMaybe [] attrs)) <> parens (ppType ty)

ppType (Name nm onm md attrs _ _) = ppModule (Id nm onm md (fromMaybe [] attrs))
ppType (Struct i [] _) = text "struct" <+> ppId i id
ppType (Struct i fields _) = 
 hang (ppId i (<> text "struct") <+> char '{')
   3  (ppDecls (map ppField fields)) $$
 char '}'

ppType (Enum i _ vals) = 
 (hang (ppId i (<> text "enum") <+> char '{')
   3   (vsep (punctuate comma (map ppEnumValue vals)))) $$
 char '}'

ppType (Union nm switch_ty switch_nm union_nm switches) = 
 hang (ppId nm (<> text "union") <+> commentOutIfC (text "switch" <>
       parens (ppId switch_nm (<> (ppType switch_ty))) <+> ppId union_nm id) <+> char '{')
   3  (ppDecls (map (ppSwitch True) switches)) $$
 char '}'

ppType (UnionNon tag switches) = 
 hang (ppId tag (<> text "union") <+> char '{')
   3  (ppDecls (map (ppSwitch False) switches)) $$
 char '}'

ppType (CUnion i fields _) = 
 hang (ppId i (<> text "union") <+> char '{')
   3  (ppDecls (map ppField fields)) $$
 char '}'

ppType (Pointer pt _ ty)   = ppType ty <> ppPointerType pt <> char '*'
ppType (Array t dims)      = ppArray empty  t dims
ppType Void                = text "void"
ppType (Iface nm md onm _ _ _) = ppModule (Id nm onm md [])

ppType (SafeArray t)      = text "SAFEARRAY" <> ifC (char '*') (parens (ppType t))
                                    -- In .h mode, we run the risk of nested comments here
                                    -- should we have a SAFEARRAY of a SAFEARRAY (yes, it does
                                    -- happen!).

ppEnumValue :: EnumValue -> CoreDoc
ppEnumValue (EnumValue vi (Left val)) = ppId vi id <+> equals <+> text (show val)
ppEnumValue (EnumValue vi (Right e))  = ppId vi id <+> equals <+> ppExpr e


ppArray :: CoreDoc -> Type -> [Expr] -> CoreDoc
ppArray d t dims = ppType t <+> d <> ppArrayDims dims

ppPointerType :: PointerType -> CoreDoc
ppPointerType pt =
  ifDebug (char '{' <> p <> char '}') empty
 where
  p =
   char $
   case pt of
     Ref    -> 'r'
     Ptr    -> 'p'
     Unique -> 'u'

ppFunTy :: CallConv -> Result -> [Param] -> CoreDoc
ppFunTy cc res params = 
  parens (ppResult res <+> 
          parens (ppCallConv True cc <+> char '*') <>
          ppTuple (map ppParam params))

ppArrayDims :: [Expr] -> CoreDoc
ppArrayDims []    = ifC (text "[1]") (text "[]")
ppArrayDims [e]   = char '[' <> ppExpr e <> char ']'
ppArrayDims [l,h] = char '[' <> ppExpr l <+> text ".." <+> ppExpr h <> char ']'
ppArrayDims _     = error "PpCore.ppArrayDims: don't know how to handle an interval with more than elts"
\end{code}

\begin{code}
ppExpr :: Expr -> CoreDoc
ppExpr e =
 case e of
  Binary bop e1 e2 -> ppExpr e1 <+> ppBinaryOp bop <+> ppExpr e2
  Cond e1 e2 e3    -> ppExpr e1 <+> char '?' <+> ppExpr e2 <+> char ':' <+> ppExpr e3
  Unary op e1      -> parens (ppUnaryOp op <+> ppExpr e1)
  Var nm           -> text nm
  Lit l            -> ppLit l
  Cast t e1        -> parens (ppType t) <> parens (ppExpr e1)
  Sizeof t         -> text "sizeof" <> parens (ppType t)
\end{code}

\begin{code}
ppParam :: Param -> CoreDoc
ppParam (Param i _ (Array t e) _ _) = ppArray (ppId i id) t e
ppParam (Param i _ t orig_ty _) = 
   ppId i (<> (ppType t <> ppOrig))
 where 
  ppOrig = ifDebug (parens (ppType orig_ty)) empty

ppSwitch :: Bool -> Switch -> CoreDoc
ppSwitch inEncUn (SwitchEmpty Nothing) 
  | inEncUn   = commentOutIfC (text "default: ")
  | otherwise = commentOutIfC (text "[default] ")
ppSwitch inEncUn (SwitchEmpty (Just ls)) = commentOutIfC (ppCaseLabels inEncUn (map fst ls))
ppSwitch inEncUn (Switch i labs t orig_ty) =
 hang (commentOutIfC $ ppCaseLabels inEncUn labs)
  3   (ppId i (<> (ppType t <> ppOrig)))
 where 
  ppOrig = ifDebug (parens (ppType orig_ty)) empty

ppField :: Field -> CoreDoc
ppField (Field i (Array t es) _ _ _) = ppArray (ppId i id) t es
ppField (Field i t orig_ty mb_sz _) = 
   ppId i (<> (ppType t <> ppOrig)) <> pp_bit_field
 where 
  pp_bit_field = 
    case mb_sz of
      Nothing -> empty
      Just x  -> char ':' <+> text (show x)

  ppOrig = ifDebug (parens (ppType orig_ty)) empty

ppResult :: Result -> CoreDoc
ppResult (Result red_ty orig_ty) =
  ifDebug (ppType red_ty <> parens (ppType orig_ty))
          (ppType orig_ty)

ppCaseLabels :: Bool -> [CaseLabel] -> CoreDoc
ppCaseLabels inEncUn ls 
 | inEncUn    = hsep (punctuate (text ": ") (map ppCaseLabel ls))
 | otherwise  = brackets (hsep (punctuate comma (map ppCaseLabel ls)))

ppCaseLabel :: CaseLabel -> CoreDoc
ppCaseLabel Default  = text "default"
ppCaseLabel (Case e) = text "case" <+> ppExpr e

\end{code}

