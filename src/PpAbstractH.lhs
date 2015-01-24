%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Mar. 31th 2003  08:36  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Converting the data representing Haskell programs into source
form. 

Apart from the pretty printing of the data constructors of
the various data types representing Haskell constructs (see
@AbstractH.lhs@), this module also performs the following
tasks:

 - if the user requested that names shouldn't be qualified,
   obey this when outputting VarNames.
 - choose the right FFI to do call-outs (and call-ins). The
   options (all controllable from the command line) are one
   of: "new FFI", ghc FFI, or GreenCard stubs.

\begin{code}
module PpAbstractH 
        (
          ppHTopDecls
        , ppType
        , showAbstractH
        , ppExpr
        ) where

import PP hiding ( integer )
import AbstractH
import AbsHUtils ( splitFunTys, isVarPat, tyInt32 )
import Opts      ( optGreenCard, optTargetGhc, optNoQualNames
                 , optNoOutput, optNoModuleHeader, optNoImports
                 , optQualInstanceMethods, optHugs
                 , optUnsafeCalls, optNoDllName
                 , optPatternAsLambda
                 , optLongLongIsInteger
                 )
import Literal
import BasicTypes
import Data.Char  ( isAlpha )
import Utils      ( notNull )
import LibUtils

\end{code}

\begin{code}
type AbsHDoc a = PPDoc a

showAbstractH :: AbsHDoc a -> String
showAbstractH ad = showPPDoc ad undefined
\end{code}


\begin{code}
ppHTopDecls :: [HTopDecl] -> AbsHDoc a
ppHTopDecls ls = vcat (map ppHTopDecl ls)

ppHTopDecl :: HTopDecl -> AbsHDoc a
ppHTopDecl (HMod hm)    = ppHModule hm
ppHTopDecl (HLit s)     = text s
ppHTopDecl (CLit _)     = empty
ppHTopDecl (HInclude s) 
  | optGreenCard = text "%include" <+> text s
     -- Only GHC understands this pragma, but its presence shouldn't
     -- seriously offend anyone else...
  | otherwise    = text "{-# OPTIONS -#include" <+> text str <+> text "#-}"
 where
   -- make sure we escape those double quotes.
  str = 
   case s of
     '<':_ -> s
     '"':_ -> s
     _     -> show s

\end{code}

\begin{code}
ppHModule :: HModule -> AbsHDoc a
ppHModule (HModule nm flg exports imports decls) =
 (if optNoModuleHeader then
     empty
  else
     (case exports of
       [] -> text "module" <+> ppName nm <+> text "where" $$ text ""
       _  -> 
         hang (text "module" <+> ppName nm)
          7   (vsep (zipWith (\ x y -> x <+> ppExport y)
                             (char '(':repeat comma) --)
                             exports)      $$
               text ") where" $$ text ""))) $$
 (if optNoImports then
     empty
  else     
     vsep (map ppImport imports')) $$ 
 text ""            $$
 ppHDecl decls      $$
 if (optHugs && flg) then text "needPrims_hugs 4" else text ""
 where
  generateGreenCard = not optNoOutput && optGreenCard

  imports'
   | not generateGreenCard = imports
   | otherwise             = ((HImport False Nothing ("StdDIS") Nothing):imports)


ppExport :: HExport -> AbsHDoc a
ppExport (HExport expo comment) = ppIEEntity expo <+> ppComment comment
  where
   -- one-line comment
   ppComment Nothing   = empty
   ppComment (Just c)  = text "--" <+> text c

ppIEEntity :: HIEEntity -> AbsHDoc a
ppIEEntity (IEModule nm)     = text "module" <+> ppName nm 
ppIEEntity (IEVal nm)        = ppName nm
ppIEEntity (IEClass c)       = ppName c
ppIEEntity (IEType nm isAbs) = ppName nm <> if isAbs then empty else text "(..)"

ppImport :: HImport -> AbsHDoc a
ppImport (HImport qual as_name nm stuff) =
 text "import" <+>
 (if qual && not optNoQualNames then text "qualified" else empty) <+>
 (case as_name of { Nothing -> empty ; Just a -> text "as" <+> ppName a} ) <+>
 ppName nm <+>
 case stuff of
   Nothing -> empty
   Just ls -> parens (fcat (punctuate (text ", ") (map ppIEEntity ls)))
\end{code}

\begin{code}
ppHDecl :: HDecl -> AbsHDoc a
ppHDecl (AndDecl d1 d2) = ppHDecl d1 $$ ppHDecl d2
ppHDecl (TypeSig i mb_ctxt t) =
  ppName i <+> vcat pp_sig

{- One-line style:
  ppName i <+> text "::" <+> 
  (case mb_ctxt of
    Nothing -> empty
    Just ct -> ppContext True ct) <+>
  ppType t
-}

 where
  (args,res)  = splitFunTys t

  pp_tys = pp_ctxt (map ppFunType (args ++ [res]))
  pp_sig = zipWith (<+>) seps pp_tys

  pp_ctxt rest = 
    case mb_ctxt of
      Nothing -> rest
      Just x  -> ppContext False x : rest

  seps = 
    text "::" : 
    case mb_ctxt of
      Nothing -> arrows
      Just _  -> text "=>" : arrows
    
  arrows = text "->" : arrows

ppHDecl (ValDecl i [p1,p2] ges)
 | isOpName i
 = (ppPat p1 <+> ppValName i <+> ppPat p2) $$
   (nest 2 (ppGuardedExprs ges)) $$
   text ""

ppHDecl (ValDecl i pats [g])
  | isSimple g = sep [ppValName i <+> pp_pats, nest 2 (ppGuardedExpr (char '=') g)] $$
                 text ""
  | otherwise  = hang (ppValName i <+> pp_pats)
                  2   (ppGuardedExpr (char '=') g) $$
                 text ""
  where
    shufflePats = optPatternAsLambda && all isVarPat pats 
    pp_pats
     | shufflePats = equals <+> hsep (map (\ x -> char '\\' <+> ppPat x <+> text "->") pats)
     | otherwise   = hsep (map ppPat pats) <+> equals

    isSimple (GExpr _ (Bind  _ _ _)) = False
    isSimple (GExpr _ (Bind_   _ _)) = False
    isSimple (GExpr _ (Let     _ _)) = False
    isSimple _                       = True

ppHDecl (ValDecl i pats ges) =
  (ppValName i <+> hsep (map ppPat pats)) $$
  (nest 2 (ppGuardedExprs ges)) $$
  text ""

ppHDecl (Primitive safe cconv (dllname,_,fun,_) i t has_structs _ _)
 | optTargetGhc   = -- GHC specific
    (ppName i <+> text "::" <+>  ppType t) $$ 
    ppName i <+> hsep arg_names <+> equals <+> text "_ccall_" <+> hsep (text fun:arg_names)
 | optGreenCard = -- GreenCard output
    (text "%fun"  <+> ppName i <+> text "::" <+>  ppType t') $$ 
     text "%code" <+> assignRes <+> text fun <> ppTuple (arg_names)
 | optHugs = text "primitive" <+> ppName i <+> text "::" <+> ppType t'
 | otherwise = -- FFI decls.
     text "foreign import"                      <+> 
     ppCallConv False cconv                     <+>
        -- this is not quite right in the case of Hugs,
        -- since we will need to supply the name of the stub DLL.
     (if (null dllname || has_structs || optNoDllName) then empty else text (show dllname))     <+>
     let fun_name | has_structs = doubleQuotes (ppName i)
                  | otherwise   = text (show fun)
     in
     fun_name                                                       <+>
      (if optUnsafeCalls || not safe then text "unsafe" else empty) <+> 
      ppName i <+> text "::"                                        <+>
     ppType t
    where
      {-
        We keep the illusion that Integers are valid FFI types on the Hugs
        side right until the very last, when we expand out an Integer into
        a pair of Int32 arguments and results.
      -}
     t' 
       | optLongLongIsInteger = expandIntegers t
       | otherwise            = t
      
     -- Use the next line instead if you haven't go the latest
     -- GC sources (i.e., ones which support qualified names).
     -- t'              = unqualTy t

     assignRes =
       case res of
         TyApply _{-io-} [(TyCon tc)] ->
           case qName tc of
             "()" -> empty
             _    -> text "res1 ="
         _ -> empty
              

     (args,res) = splitFunTys t
     arg_names  = zipWith (\ arg _ -> text ("arg" ++ show arg)) [(1::Int)..] args

ppHDecl (PrimCast cconv i ty has_structs args res_ty)
 | optGreenCard =
    text "" $$
    text "%fun" <+> ppName i <+> text "::" <+>  ppType ty' $$ 
    text "%code" $$
    vsepPrefix (text "% ")
     [ ppDeclResult
     , text "typedef" <+> ppResultType <+> 
          parens ( text "__" <> ppCallConv True cconv <+> char '*' <+> text "__funptr") <+>
          ppTuple ppArgs <> semi
     , text "__funptr" <+> ppName i <> semi
     , ppName i <+> equals <+> text "(__funptr)arg1" <> semi
     , ppAssignResult <+> ppName i <> ppTuple ppCasmArgs <> semi
     ] $$
    text ""
 | optHugs = text "primitive" <+> ppName i <+> text "::" <+> ppType ty'
 | optTargetGhc =
   ppName i <+> text "::" <+>  ppType ty $$ 
   ppName i <+> hsep params <+> equals <+> text "_casm_" <+> 
   ppLitLit (
      ppDeclResult $$
      text "typedef" <+> ppResultType <+> 
        parens ( text "__" <> ppCallConv True cconv <+> char '*' <+> text "__funptr") <+>
        ppTuple ppArgs <> semi $$
      text "__funptr" <+> ppName i <> semi $$
      ppName i <+> equals <+> text "(__funptr)%0" <> semi $$
      ppAssignResult <+> ppName i <> ppTuple ppMethArgs <> semi $$
      ppReturnResult <> semi) <+> hsep params
 | otherwise =
   text "foreign import" <+> ppCallConv False cconv   <+> 
   (if has_structs then text (show i) else text "\"dynamic\"") <+>
   (if optUnsafeCalls then text "unsafe" else empty) <+> 
   ppName i <+> text "::" <+> ppType ty
  where
    ppLitLit x = text "``" <> x <> text "\'\'"

    ty' 
     | optLongLongIsInteger = expandIntegers ty
     | otherwise            = ty

    -- Use the next line instead if you haven't go the latest
    -- GC sources (i.e., ones which support qualified names).
    --ty' = unqualTy ty

    params = map (\ x -> text ('a':show x)) [1..(length args)]
    ppMethArgs = map (\ x -> text ('%':show x)) (tail [0..(length args - 1)])
    ppCasmArgs = map (\ x -> text ("arg"++show x)) [2..(length args)]

    ppArgs = map (\ (x, arg_ty) -> text (snd arg_ty) <+> text ('a':show x)) (zip [(1::Int)..] (tail args))

    (ppDeclResult, ppResultType, ppAssignResult, ppReturnResult) = 
     case res_ty of
       (_,"void") -> ( empty, text "void", empty, empty )
       (_,res)    -> ( text res <+> text "res1" <> semi
                     , text res
                     , text "res1" <+> equals
                     , text "%r=res1"
                     )


ppHDecl (Entry cconv ci hi t) = 
  text "foreign export" <+> ppCallConv False cconv <+> text (show ci) <+> ppName hi <+> text "::" <+>
  ppType t

ppHDecl (Callback cconv i t) = 
  text "foreign export" <+> ppCallConv False cconv <+> text "dynamic" <+> ppName i <+> text "::" <+>
  ppType t

ppHDecl (ExtLabel c_name h_name t)
 = text "foreign label" <+> text (show c_name) <+> text h_name <+> text "::" <+> ppType t

ppHDecl (TyD td) = ppTyDecl td
ppHDecl (Class ctxt cname tvrs decls) =
  hang (text "class"  <+> ppContext True ctxt <+>
        ppQName cname <+> hsep (map ppTyVar tvrs) <+>
        if (notNull decls) then text "where" else empty)
   2   (vsep (map ppHDecl decls))
ppHDecl (Instance ctxt cname t decls) =
  hang (text "instance" <+> ppContext True ctxt <+> ppQName cname <+> parens (ppType t) <+>
        if (notNull decls) then text "where" else empty)
   2   (vsep (map ppHDecl decls))
ppHDecl (Include s)
  | optGreenCard = text "%#include" <+> text s
  | otherwise    = empty
  
ppHDecl (Haskell s) = text s
ppHDecl (CCode s)   = text "{- BEGIN_C_CODE" $$ text s $$ text "END_C_CODE-}"
ppHDecl EmptyDecl   = empty
\end{code}

\begin{code}
expandIntegers :: Type -> Type
expandIntegers (TyFun t1@(TyCon t) t2)
  | qName t == "Integer"  = TyFun tyInt32 (TyFun tyInt32 (expandIntegers t2))
  | otherwise             = TyFun t1 (expandIntegers t2)
expandIntegers (TyFun t1 t2) = TyFun (expandIntegers t1) (expandIntegers t2)
expandIntegers t@(TyApply (TyCon tc) [(TyCon x)])
  | qName tc == "IO" && qName x == "Integer" = TyApply (TyCon tc) [TyTuple [tyInt32, tyInt32]]
  | otherwise = t
expandIntegers t = t
\end{code}


\begin{code}
ppPat :: Pat -> AbsHDoc a
ppPat (PatVar v)      = ppVarName v
ppPat (PatLit v)      = ppLit v
ppPat PatWildCard     = char '_'
ppPat (PatTuple pats) = ppTuple (map ppPat pats)
ppPat (PatAs v p)     = ppVarName v <> char '@' <> parens (ppPat p)
ppPat (PatCon v [])   = ppVarName v
ppPat (PatCon v pats) = parens (ppVarName v <+> hsep (map ppPat pats))
ppPat (PatList pats)  = ppList (map ppPat pats)
ppPat (PatIrrefut p)  = char '~' <> parens (ppPat p)
ppPat (PatRecord v fields) =
 ppVarName v <> braces (hsep (punctuate comma (map ppField fields)))
 where
  ppField (var,p) = ppVarName var <+> equals <+> ppPat p

ppCaseAlt :: CaseAlt -> AbsHDoc a
ppCaseAlt (Alt p [GExpr [] e]) = ppPat p <+> text "->" <+> ppExpr e
ppCaseAlt (Alt p ls) = 
    hang (ppPat p)
     2   (vsep (map (ppGuardedExpr (text "->")) ls))
ppCaseAlt (Default mb_v e) =
  pp_v <+> text "->" <+> ppExpr e
  where
   pp_v = case mb_v of Nothing -> char '_' ; Just v  -> ppVarName v

\end{code}

\begin{code}
type ExprDoc = PPDoc (Bool,Bool)

ifTop :: (ExprDoc -> ExprDoc )
      -> (ExprDoc -> ExprDoc )
      -> ExprDoc
      -> ExprDoc
ifTop onTrueF onFalseF d =
 getPPEnv             $ \ (top,flg) ->
 setPPEnv (False,flg) $
 if top then
    onTrueF d
 else
    onFalseF d

ifOnTop :: ExprDoc
        -> ExprDoc
        -> ExprDoc
ifOnTop ifIs ifIsn't =
 getPPEnv             $ \ (top,flg) ->
 setPPEnv (False,flg) $
 if top then
    ifIs
 else
    ifIsn't

ifDo :: ExprDoc -> ExprDoc -> ExprDoc
ifDo onTrue onFalse = 
 getPPEnv $ \ (_,flg) ->
 if flg then
    onTrue
 else
    onFalse

setDo :: Bool -> ExprDoc -> ExprDoc
setDo flg d = getPPEnv $ \ (top,_) -> setPPEnv (top,flg) d

setTop :: Bool -> ExprDoc -> ExprDoc
setTop flg d = getPPEnv $ \ (_,dof) -> setPPEnv (flg,dof) d
\end{code}

\begin{code}
ppExpr :: Expr -> PPDoc a
ppExpr e = setPPEnv (True, False) (ppExprDo e)

ppExprDo :: Expr -> ExprDoc
ppExprDo (Lit l)      = ppLit l
ppExprDo (Var v)      = ppVarName v
ppExprDo (Con v)      = ppConName v
ppExprDo (Lam [] e)   = ppExprDo e
ppExprDo (Lam pats e) = char '\\' <+> hsep (map ppPat pats) <+> text "->" <+> setDo False (ppExprDo e)
ppExprDo (Apply (Apply e args1) args2) = ppExprDo (Apply e (args1++args2))
ppExprDo (Apply e [])      = ppExprDo e
ppExprDo (Apply e@(Lam _ _) args) = parens (ppExprDo e) <+> hsep (map ppArg args)
ppExprDo (Apply e args)    = 
  ifOnTop (ppExprDo e <+> vsep (map ppArg args))
          (ppExprDo e <+> hsep (map ppArg args))
ppExprDo (RApply e1 (Lam pats e2))  =
  ppExprDo e1 <+> ppVarName dollarName <+> char '\\' <+> 
  hsep (map ppPat pats) <+> text "->" $$ ppExprDo e2
ppExprDo (RApply e1 e2)  =
  ppExprDo e1 <+> ppVarName dollarName <+> ppExprDo e2
ppExprDo (Tup args)         = ppTuple (map ppExprDo args)
ppExprDo (List elts)        = ppListVert (map ppExprDo elts)
ppExprDo (InfixOp e1 op e2) = ppExprDo e1 <+> ppr_op <+> ppExprDo e2
   where
     ppr_op 
       | not (isOpName op) = ppVarName op
       | otherwise         = char '`' <> ppVarName op <> char '`'

ppExprDo (BinOp bop e1 e2) = parens ( ppExprDo e1 <+> ppBinOp bop <+> ppExprDo e2)
ppExprDo (UnOp uop e)      = parens ( ppUnOp uop <+> ppExprDo e)
ppExprDo (Bind m p n)      =
   ifTop (\ d -> hang (text "do") 2 (setDo True d)) (id)
         (ifDo ((ppPat p <+> text "<-" <+> ppExprDo m) $$ ppExprDo n)
               (hang (ppExprDo m <+> ppQualName bindName <+> 
                          char '\\' <+> ppPat p <+> text "->")
                 0   (ppExprDo n)))
 -- this assumes that m has type "M ()", which is the
 -- case for HaskellDirect. ToDo: Record return type
 -- for the left arg to a bind, so that we can make sure
 -- that this is really the case.
 --
ppExprDo (Bind_ m (Return (Tup []))) = ppExprDo m

ppExprDo (Bind_ m n)       =
   ifTop (\ d -> hang (text "do") 2 (setDo True d)) (id)
         (ifDo ((ppExprDo m) $$ ppExprDo n)
               (hang (ppExprDo m <+> ppQualName bind_Name)
                 0   (ppExprDo n)))

ppExprDo (Return e@(Tup _)) = ppQualName prelReturn <+> ppExprDo e
ppExprDo (Return e)         = ppQualName prelReturn <+> parens (ppExprDo e)
ppExprDo (If c e1 e2)       = 
  hang (text "if" <+> ppExprDo c)
   2   (text "then" <+> ppExprDo e1 $$
        text "else" <+> ppExprDo e2)
ppExprDo (Case e alts)      =
  hang (text "case" <+> ppExprDo e <+> text "of")
   3   (vsep (map ppCaseAlt alts))
ppExprDo (Let [] e) = ppExprDo e
ppExprDo (Let binders (Let binders2 e)) = ppExprDo (Let (binders++binders2) e)
ppExprDo (Let binders e)    =
  ifDo ((text "let" <+> (vsep (map ppBinding binders))) $$ ppExprDo e)
       ((hang (text "let")
          1  (vsep (map ppBinding binders))) $$
        text "in" $$
        ppExprDo e)
ppExprDo (WithTy e ty) = parens (ppExprDo e <+> text "::" <+> ppType ty)
\end{code}

Expressions in argument position - leave out 
as many parens as possible:

\begin{code}
ppArg :: Expr -> ExprDoc
ppArg (Lit l)    = ppLit l
ppArg (Var v)    = ppVarName v
ppArg (Con v)    = ppConName v
ppArg e@(Tup _)  = ppExprDo e
ppArg e@(List _) = ppExprDo e
ppArg e          = parens (ppExprDo e)
\end{code}


\begin{code}
ppBinding :: Binding -> ExprDoc
ppBinding (Binder v e) = ppName v <+> equals <+> setTop False (ppExprDo e)

ppBinOp :: BinaryOp -> PPDoc a
ppBinOp op =
   case op of
     Xor     -> ppQName xorName
     Or      -> ppQName orName
     And     -> ppQName andName
     Shift d -> ppQOp (case d of { L -> shiftLName ; R -> shiftRName })
     Add     -> ppQName addName
     Sub     -> ppQName subName
     Div     -> ppQOp divName
     Mod     -> ppQOp modName
     Mul     -> ppQName mulName
     LogAnd  -> ppQName logAndName
     LogOr   -> ppQName logOrName
     Gt      -> ppQName gtName
     Ge      -> ppQName geName
     Eq      -> ppQName eqName
     Le      -> ppQName leName
     Lt      -> ppQName ltName
     Ne      -> ppQName neName

ppUnOp :: UnaryOp -> PPDoc a
ppUnOp op =
 case op of
  Minus  -> ppQName negateName
  Plus   -> ppQName addName
  Not    -> ppQName complementName
  Negate -> ppQName notName
  Deref  -> empty
\end{code}

\begin{code}
ppType :: Type -> AbsHDoc a
ppType ty = setPPEnv top_prec (ppTypePrec ty)

ppFunType :: Type -> AbsHDoc a
ppFunType ty = setPPEnv fun_prec (ppTypePrec ty)

ppConType :: Type -> AbsHDoc a
ppConType ty = setPPEnv tycon_prec (ppTypePrec ty)

type TypeDoc = PPDoc Int

setPrec :: Int -> TypeDoc -> TypeDoc
setPrec = setPPEnv

gePrec :: Int -> TypeDoc -> TypeDoc -> TypeDoc
gePrec prec onTrue onFalse = 
  getPPEnv $ \ val ->
  if val >= prec then
     onTrue
  else
     onFalse
     
ppTypePrec :: Type -> TypeDoc
ppTypePrec (TyVar _ tv)       = ppTyVar tv
ppTypePrec (TyCon tc)         = ppTyCon tc
ppTypePrec (TyApply con [])   = ppTypePrec con
ppTypePrec (TyApply con args) = 
   mbParen tycon_prec (setPrec tycon_prec $ hsep (map ppTypePrec (con:args)))
ppTypePrec (TyList t)         = 
   brackets (setPrec top_prec (ppTypePrec t))
ppTypePrec (TyTuple ts)       = 
   setPrec top_prec (ppTuple (map ppTypePrec ts))
ppTypePrec (TyCtxt ctxt t)    =
   ppContext True ctxt <+> ppTypePrec t
ppTypePrec (TyFun a b)        = 
   mbParen fun_prec ((setPrec fun_prec (ppTypePrec a)) <+> text "->" <+> setPrec top_prec (ppTypePrec b))

mbParen :: Int -> TypeDoc -> TypeDoc
mbParen new_prec d = gePrec new_prec (parens d) d

top_prec, fun_prec, tycon_prec :: Int
top_prec   = (0::Int)
fun_prec   = (1::Int)
tycon_prec = (2::Int)

ppVarName :: VarName -> PPDoc a
ppVarName v = ppQName v

ppConName :: ConName -> PPDoc a
ppConName cn = ppQName cn

ppTyVar :: TyVar -> PPDoc a
ppTyVar tv = ppQName tv

ppTyCon :: TyCon -> PPDoc a
ppTyCon s = ppQName s

ppQName :: QualName -> PPDoc a
ppQName = ppQualName

ppQOp :: QualName -> PPDoc a
ppQOp op = char '`' <> ppQualName op <> char '`'

{-
  = Con_1 ...
  | Con_2 ...
  ...
-}
ppConDecls :: [ConDecl] -> AbsHDoc a
ppConDecls [] = error "ppConDecls: shouldn't happen, invalid Haskell."
ppConDecls (dcon:dcons) =
  vsep
    ( (equals <+> ppConDecl dcon) :
      (map (\ dc -> text "|" <+> ppConDecl dc) dcons))

ppConDecl :: ConDecl -> AbsHDoc a
ppConDecl (RecDecl nm fields)
  | null fields = ppName nm 
  | otherwise   =
     ppName nm <+> braces (vsep (punctuate comma (map ppField fields)))
      where
       ppField (v, t)   = ppName v <+> text "::" <+> ppBangType t
ppConDecl (ConDecl nm args) =
      ppName nm <+> hsep (map ppBangType args)

ppBangType :: BangType -> AbsHDoc a
ppBangType (Banged ty)   = char '!' <> setPPEnv tycon_prec (ppTypePrec ty)
ppBangType (Unbanged ty) = setPPEnv tycon_prec (ppTypePrec ty)

\end{code}

\begin{code}
ppContext :: Bool -> Context -> AbsHDoc a
ppContext _ (CtxtTuple [])   = empty
ppContext withDArrow (CtxtTuple ts)
  = ppTuple (map (ppContext False) ts) <+> (if withDArrow then text "=>" else empty)
ppContext withDArrow (CtxtClass c ts) = ppQName c <+> hsep (map ppConType ts) <+> (if withDArrow then text "=>" else empty)

ppTyDeclKind :: TyDeclKind -> AbsHDoc a
ppTyDeclKind Newtype = text "newtype"
ppTyDeclKind Data    = text "data"
\end{code}

\begin{code}
vsepPrefix :: PPDoc a -> [PPDoc a] -> PPDoc a
vsepPrefix pre ls = vsep (map ((<>) pre) ls)

{- UNUSED:
unqualTy :: Type -> Type
unqualTy t =
 case t of
   TyVar f tv     -> TyVar f (unqualName tv)
   TyCon tc       -> TyCon (unqualName tc)
   TyApply f args -> TyApply (unqualTy f) (map unqualTy args)
   TyList tl      -> TyList (unqualTy tl)
   TyTuple ts     -> TyTuple (map unqualTy ts)
   TyCtxt c t1    -> TyCtxt c (unqualTy t1)
   TyFun a b      -> TyFun (unqualTy a) (unqualTy b)
 where
   unqualName qv = qv{qModule=Nothing,qDefModule=Nothing}
-}
\end{code}

\begin{code}
ppGuardedExprs :: [GuardedExpr] -> AbsHDoc a
ppGuardedExprs []             = empty -- bogus, but we won't flag this fact here.
ppGuardedExprs [(GExpr [] e)] = equals <+> (ppExpr e)
ppGuardedExprs ls             = 
   vcat (map (\ (GExpr gs e) ->
                text "|" <+> 
                hsep (punctuate comma (map ppExpr gs)) <+> 
                equals <+>
                (ppExpr e)) ls)

ppGuardedExpr :: AbsHDoc a -> GuardedExpr -> AbsHDoc a
ppGuardedExpr _    (GExpr [] e) = ppExpr e
ppGuardedExpr sepr (GExpr gs e) = 
  text "|" <+> hsep (punctuate comma (map ppExpr gs)) <+> sepr <+> ppExpr e

\end{code}

\begin{code}
ppTyDecl :: TyDecl -> AbsHDoc a
ppTyDecl (TypeSyn nm args ty) = text "type" <+> hsep (map text (nm:args)) <+> equals <+> ppType ty
ppTyDecl (TyDecl Data tycon ty_args [con_decl] derivs) =
  ppTyDeclKind Data       <+> 
  ppName tycon            <+>
  hsep (map text ty_args) <+> 
  equals                  <+>
  hang (ppConDecl con_decl)
   2   (ppDeriving derivs)

ppTyDecl (TyDecl kind tycon ty_args con_decls derivs) =
  hang (ppTyDeclKind kind <+> ppName tycon <+> hsep (map text ty_args)) 
   1   (ppConDecls con_decls $$ -- nb: ppConDecls insert the '='
        ppDeriving derivs)
\end{code}

\begin{code}
ppDeriving :: [QualName] -> AbsHDoc a
ppDeriving [] = text "" -- want a new line
ppDeriving ds = 
  text "deriving" <+> ppTuple (map ppQName ds) <> text ""
\end{code}

\begin{code}
ppValName :: QualName -> AbsHDoc a
ppValName i
 | not optQualInstanceMethods = ppName (qName i)
 | otherwise                  = ppQName i
\end{code}

\begin{code}
isOpName :: QualName -> Bool
isOpName q = 
  case qName q of
    ""    -> False
    (n:_) -> not (isAlpha n) && n /= '_'
\end{code}
