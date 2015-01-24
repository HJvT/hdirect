%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Feb. 9th 2003  15:07  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

\begin{code}
module PpIDLSyn where

import IDLSyn
import PP
import Literal
import BasicTypes
import Utils
import Data.Maybe
import Opts ( optDebug, optIncludeAsImport, optExcludeSysIncludes )

type IDLDoc = PPDoc ()

showIDL :: IDLDoc -> String
showIDL i = showPPDoc i ()

ppIDL :: String -> [Defn] -> IDLDoc
ppIDL src ds
 | optIncludeAsImport || optExcludeSysIncludes = vsep (map ppDefn (trundle [Nothing] ds)) $$ text ""
 | otherwise          = vsep (map ppDefn ds) $$ text ""
  where
     -- remove the included bits
    trundle     _          []     = []
    trundle ks@(keepIt:ls) (x:xs) = 
      case x of
        IncludeStart ix | isJust keepIt || is_src -> trundle (Just forKeeps : ks) xs
	   where
	    is_src   = src == ix
	    forKeeps = fromMaybe True keepIt && is_src
	IncludeEnd -> trundle ls xs
	_ | fromMaybe True keepIt -> x : trundle ks xs
	  | otherwise		  -> trundle ks xs
    trundle [] ls = ls
\end{code}

\begin{code}
ppId :: Id -> IDLDoc
ppId iden =
  case iden of
     Id i             -> text i
     AttrId as i      -> ppAttrs False as <> ppId i
     ArrayId i dims   -> ppId i <> ppList (map ppExpr dims)
     FunId i mb_cc ps -> (pp_callconv mb_cc) <+> ppId i <> parens (ppParams ps)
     BitFieldId x i   -> ppId i <+> char ':' <> text (show x)
     CConvId cc i     -> (pp_callconv (Just cc)) <+> ppId i
     Pointed quals i  -> text (replicate len '*') <> ppId i 
       where
        len = length quals
  where
   pp_callconv mb_cc = mapFromMb empty (ppCallConv True) mb_cc
    
\end{code}

%*
%
\subsection{Pretty printing a definition}
%
%*

\begin{code}
ppDefn :: Defn -> IDLDoc

ppDefn (Typedef t attrs ids) 
  = text "typedef"        <+> 
     ppAttrs False attrs  <+> 
     ppType t             <+>
     hsep (punctuate comma (map ppId ids)) <> semi

ppDefn (TypeDecl t)      = ppType t <> semi
ppDefn (ExternDecl t is) = text "extern" <+> ppType t <+> hsep (punctuate comma (map ppId is)) <> semi

ppDefn (Constant i attrs t expr) 
  = ppAttrs False attrs <> text "const" <+> ppType t <+> ppId i <+> equals <+> ppExpr expr <> semi

ppDefn (Attributed attrs d)
  = ppAttrs False attrs $+$ ppDefn d 

ppDefn (Attribute ids read_only t)
  | read_only = text "readonly" <+> ppType t <+> hsep (punctuate comma (map ppId ids)) <> semi
  | otherwise = ppType t <+> hsep (punctuate comma (map ppId ids)) <> semi

ppDefn (Operation i res_ty mb_raises mb_ctxt)
  = ppType res_ty <+> {-pp_callconv <+> -} ppId i
    {- ((ppId i <> lparen) $$
      ppParams params) <> rparen -} <+> ppRaises mb_raises <+> ppContext mb_ctxt <> semi
--    where
--     pp_callconv = mapFromMb empty (ppCallConv True) mb_callconv

ppDefn (Exception i mems)
  = text "exception" <+> ppId i <+> ppMembers mems <> semi

ppDefn (Interface i inherit ds)
  = hang (text "interface" <+> ppId i <+> 
          hsep (punctuate (text ":") 
	                  (text "" : map text inherit)) <+> char '{')
      4 (ppDefns ds) $$
    char '}' <> semi

ppDefn (Forward i)
  = text "interface" <+> ppId i <> semi

ppDefn (Module i ds)
  = text "module" <+> ppId i <+> char '{' $$
     ppDefns ds $$
    char '}' <> semi

ppDefn (DispInterface i props meths)
  = text "dispinterface" <+> ppId i <+> char '{' $$
     hang (text "properties:")
      8   (ppProps props)  $$
     hang (text "methods:")
      8   (ppDefns meths)  $$
    char '}' <> semi

ppDefn (DispInterfaceDecl i iid)
  = text "dispinterface" <+> ppId i <+> char '{' $$
     text "interface" <+> ppId iid <> semi $$
    char '}' <> semi

ppDefn (CoClass i c_mems) -- [(Bool, Id, [Attribute])]
  = text "coclass" <+> ppId i <+> 
    char '{' $$
      ppCoCMembers c_mems $$
    char '}' <> semi

ppDefn (Library i ds)
  = text "library" <+> ppId i <+> char '{' $$
      ppDefns ds $$ 
    char '}' <> semi

ppDefn (CppQuote str)
  = text "cpp_quote" <+> parens (doubleQuotes (text str))

ppDefn (HsQuote str)
  = text "hs_quote" <+> parens (doubleQuotes (text str))

ppDefn (CInclude str)
  = text "include" <+> text str

ppDefn (Import imps) 
 | optDebug = 
   text "import" <+> 
   hsep (punctuate comma (map (\ (v,defs) -> doubleQuotes (text v) $$ vcat (map ppDefn defs)) imps)) <> semi
 | otherwise =
   text "import" <+> 
   hsep (punctuate comma (map (\ (v,_) -> doubleQuotes (text v)) imps)) <> semi

ppDefn (ImportLib imp)
  = text "importlib" <+> parens (doubleQuotes (text imp)) <> semi

ppDefn (Pragma str)
  = text "#pragma" <+> text str 

ppDefn (IncludeStart _) = empty
ppDefn IncludeEnd = empty

\end{code}

%*
%
\subsection{Pretty printing types}
%
%*

\begin{code}
ppType :: Type -> IDLDoc

ppType (TyApply f a)  = ppType f <+> ppType a

ppType (TyInteger sz) = ppSize sz

ppType (TyFloat sz) =
 text $
 case sz of
   Short    -> "float"
   Long     -> "double"
   LongLong -> "long double"
   Natural  -> "float"

ppType (TySigned isSigned)
   | isSigned  = text "signed"
   | otherwise = text "unsigned"

ppType TyChar = text "char"
ppType TyWChar  = text "wchar"
ppType TyBool   = text "boolean" -- or was that bool?
ppType TyOctet  = text "octet"   -- aka byte
ppType TyAny    = text "any"
ppType TyObject = text "Object"
ppType TyStable = text "StablePtr"
ppType TyVoid    = text "void"
ppType TyBString = text "BSTR"
ppType (TyPointer t)  = ppType t <> char '*'
ppType (TyArray t es) = ppType t <> ppList (map ppExpr es)
ppType (TySafeArray t)   = text "SAFEARRAY" <> parens (ppType t)
ppType (TyFun mb_cc t ps) = 
   ppType t <+> parens (pp_callconv <+> char '*') <>
   parens (ppParams ps)
 where
   pp_callconv = mapFromMb empty (ppCallConv True) mb_cc

ppType (TyStruct mb_tag [] _)  = 
  text "struct" <+> tag
  where
   tag = mapFromMb empty ppId mb_tag

ppType (TyStruct mb_tag mems mb_pack) = 
  hang (text "struct" <+> tag <+> char '{')
    4  (ppMembers mems) $$
  char '}' $$
  fromMaybe empty (fmap (\ x -> text "/*" <> text (show x) <> text "*/") mb_pack)
  where
   tag = mapFromMb empty ppId mb_tag

ppType (TyString mblen) = 
  text "string" <>
  (mapFromMb empty 
             (\ l -> char '<' <> ppExpr l <> char '>') mblen)

ppType (TyWString mblen) = 
  text "wstring" <>
  (mapFromMb empty 
             (\ l -> char '<' <> ppExpr l <> char '>') mblen)

ppType (TySequence t mblen) =
  text "sequence" <> 
  char '<' <> ppType t <>
  (mapFromMb empty  (\ l -> comma <> ppExpr l) mblen) <>
  char '>' 

ppType (TyFixed Nothing)      = text "fixed"
ppType (TyFixed (Just (e,i))) =
  text "fixed" <> char '<' <>
    ppExpr e <> comma <+> ppILit i <> 
  char '>'

ppType (TyName nm _) = text nm
ppType (TyIface nm) = text nm

ppType (TyUnion struct_name ty switch_name union_name switches) =
  text "union"     <+>
  (mapFromMb empty ppId struct_name) <+>
  text "switch"    <>
  parens (ppType ty <+> ppId switch_name) <+>
  (mapFromMb empty ppId union_name) <+> char '{' $+$
  ppSwitches switches $+$
  char '}'

ppType (TyCUnion mbid members _) =
  hang (text "union" <+> 
        (mapFromMb empty ppId mbid) <+> char '{')
    3 (ppMembers members) $+$
  char '}'

ppType (TyUnionNon mbid switches) =
  hang (text "union" <+> 
        (mapFromMb empty ppId mbid) <+> char '{')
    3 (ppSwitches switches) $+$
  char '}'

ppType (TyEnum mbid enums) =
  hang (text "enum" <+> pp_id <+> char '{')
   3   (pp_vals) $$
  char '}'
  where
   pp_id = mapFromMb empty ppId mbid
   pp_vals =
     vsep (punctuate comma (map ppVal enums))

   ppVal (i, attrs, Nothing) = ppAttrs False attrs <> ppId i
   ppVal (i, attrs, Just e)  = ppAttrs False attrs <> ppId i <+> equals <+> ppExpr e

ppType (TyQualifier q)     = ppQualifier q

\end{code}

%*
%
\subsection{Misc pretty printing functions}
%
%*

\begin{code}
ppMembers :: [Member] -> IDLDoc
ppMembers mems = ppDecls (map ppMember mems)

ppMember :: Member -> IDLDoc
ppMember (t, attrs, ids) =
  ppAttrs False attrs <+> ppType t <+> hcat (punctuate comma (map ppId ids))

ppDefns :: [Defn] -> IDLDoc
ppDefns ls = ppDecls (map ppDefn ls)

ppAttrs :: Bool -> [Attribute] -> IDLDoc
ppAttrs isParam [] 
  | isParam   = text "[in]"
  | otherwise = empty
ppAttrs _ as = ppList (map ppAttr as)

ppAttr :: Attribute -> IDLDoc
ppAttr (Mode dir)      = ppDirection dir
ppAttr (Attrib f [])   = ppId f
ppAttr (Attrib f args) = 
  ppId f <> parens (hsep (punctuate comma (map ppAttrParam args)))
  where
   ppAttrParam EmptyAttr    = empty
   ppAttrParam (AttrExpr e) = ppExpr e
   ppAttrParam (AttrLit l)  = ppLit l
   ppAttrParam (AttrPtr a)  = char '*' <> ppAttrParam a

ppCoCMembers :: [CoClassMember] -> IDLDoc
ppCoCMembers mems = ppDecls (map ppCoCMember mems)

ppCoCMember :: CoClassMember -> IDLDoc
ppCoCMember (isInterface, i, attrs) =
  ppAttrs False attrs <+> 
  (if isInterface then
      text "interface"
   else
      text "dispinterface") <+>
  ppId i

ppParams :: [Param] -> IDLDoc
ppParams ps = vsep (punctuate comma (map ppParam ps))

ppParam :: Param -> IDLDoc
ppParam (Param nm ty attrs) = ppAttrs True attrs <+> ppType ty <+> ppId nm

ppRaises :: Maybe Raises -> IDLDoc
ppRaises Nothing    = empty
ppRaises (Just ids) = 
  text "raises" <+> 
  ppTuple (map text ids)

ppContext :: Maybe Context -> IDLDoc
ppContext Nothing    = empty
ppContext (Just ids) = 
  text "context" <+> 
  ppTuple (map (doubleQuotes.text) ids)

ppProps :: [([Attribute], Type, Id)] -> IDLDoc
ppProps ls = ppDecls (map ppProp ls)

ppProp :: ([Attribute], Type, Id) -> IDLDoc
ppProp (as, ty, nm) = ppAttrs False as <+> ppType ty <+> ppId nm

ppSwitches :: [Switch] -> IDLDoc
ppSwitches ls = ppDecls (map ppSwitch ls)

ppSwitch :: Switch -> IDLDoc
ppSwitch (Switch labels Nothing) =
  ppCaseLabels labels
ppSwitch (Switch labels (Just param)) =
  ppCaseLabels labels <+> ppParam param

\end{code}


%*
%
\subsection{Pretty printing expressions}
%
%*

\begin{code}
ppExpr :: Expr -> IDLDoc

ppExpr (Binary op e1 e2) = 
  parens (ppExpr e1)  <+>
     ppBinaryOp op    <+>
  parens (ppExpr e2) 

ppExpr (Unary op e) =
  ppUnaryOp op <+> parens (ppExpr e)

ppExpr (Var i)    = text i
ppExpr (Lit l)    = ppLit l
ppExpr (Cast t e) = parens (ppType t) <> parens (ppExpr e)
ppExpr (Sizeof ty) = text "sizeof" <> parens (ppType ty)
ppExpr (Cond a b c) = parens (ppExpr a) <+> char '?' <+> ppExpr b <+> char ':' <+> ppExpr c

ppCaseLabels :: [CaseLabel] -> IDLDoc
ppCaseLabels ls = hsep (punctuate (text ": ") (map ppCaseLabel ls))

ppCaseLabel :: CaseLabel -> IDLDoc
ppCaseLabel Default  = text "default"
ppCaseLabel (Case es) = text "case" <+> ppTuple (map ppExpr es)

\end{code}
