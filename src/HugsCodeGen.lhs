%
% (c) 1999, University of Glasgow & Sigbjorn Finne
%

Backend which emits C stubs compatible with Hugs' 'primitive' declarations.

\begin{code}
module HugsCodeGen ( hugsCodeGen ) where

import AbstractH
import AbsHUtils ( splitFunTys, isIOTy )
import PP
import BasicTypes
import Utils ( traceIf )
import Opts  ( optVerbose, optLongLongIsInteger,
               optGenHeader, optOneModulePerInterface
             )
import Data.List  ( nub, intersperse )
import Utils ( notNull, dropSuffix )
\end{code}

\begin{code}
hugsCodeGen :: String           -- (base)name of output file
            -> [HTopDecl]       -- Haskell decls to derive Hugs stubs from.
            -> String
hugsCodeGen c_nm ls = showPPDoc (hCode c_nm ls) ([],[])

type HugsStubCode = PPDoc ([(String,Int)],[String])

getStubEnv :: ([(String,Int)] -> HugsStubCode) -> HugsStubCode
getStubEnv cont env@(e,_) = cont e env

getDllEnv :: ([String] -> HugsStubCode) -> HugsStubCode
getDllEnv cont env@(_,e) = cont e env

addToStubEnv :: String -> Type -> HugsStubCode -> HugsStubCode
addToStubEnv nm ty cont (env,dlls) = cont (((nm,arity):env),dlls)
  where
   (args,res) = splitFunTys ty

   sizeofArg (TyCon tc) | optLongLongIsInteger && qName tc == "Integer" = (2::Int)
   sizeofArg _ = 1

   size_args = sum (map sizeofArg args)

   arity 
    | isIOTy res = size_args + 2
    | otherwise  = size_args
   
addToDllEnv :: String -> HugsStubCode -> HugsStubCode
addToDllEnv nm cont (env,dlls) = cont (env,nm:dlls)

hCode :: String -> [HTopDecl] -> HugsStubCode
hCode c_nm xs = whizz xs
 where 
  whizz [] = 
    getDllEnv  $ \ dlls ->
    let dlls_real = nub (filter notNull dlls) in
    traceIf (optVerbose && notNull dlls_real)
            ("\nStubs depend on entry points from the following DLLs/libraries:\n  " ++
             showList dlls_real (
             "\nyou may need to adjust your command-line when compiling the stubs to" ++
             "\ntake this into account.")) $
    getStubEnv $ \ env ->
    genTrailer env
  whizz (HLit _ : ls)     = whizz ls
  whizz (CLit s : ls) 
    | not optGenHeader  = text s $$ whizz ls
    | otherwise         = whizz ls
  whizz (HInclude s : ls) = text "#include" <+> text (escapeString s) $$
                            whizz ls
  whizz (HMod hm : ls)    = hMod c_nm hm (whizz ls)

  escapeString s@('"':_) = s -- "
  escapeString s@('<':_) = s
  escapeString s         = show s
 
hMod :: String -> HModule -> HugsStubCode -> HugsStubCode
hMod c_nm (HModule _ _ _ _ d) cont
  | optGenHeader && not optOneModulePerInterface = 
      text  "#include" <+> text (show (dropSuffix c_nm ++ ".h")) $$ code
  | otherwise = code
 where
  code = hDecl d cont

hDecl :: HDecl -> HugsStubCode -> HugsStubCode
hDecl (AndDecl d1 d2) cont = hDecl d1 (hDecl d2 cont)
hDecl (Primitive _ cc lspec nm ty _ c_args c_res) cont =
 addToDllEnv dll_name $
 addToStubEnv nm ty   $
 tdefFun lspec cc c_args c_res $$
 primHeader nm $$ 
 lbrace $$
   argAndResDecls ty c_args c_res $$ argAssign ty c_args $$
   performCall False lspec c_args ty $$
   pushResult c_res ty $$
 rbrace $$
 cont
 where
  (dll_name, _, _, _) = lspec

  tdefFun (_,Nothing,_,_) _ _ _ = empty
  tdefFun (_,Just _,fnm,_) cconv cargs cres = 
    text "extern" <+> text (snd cres) <+> ppCallConv True cconv <+> text fnm <+>
    parens (hsep (intersperse comma (map (text.snd) cargs))) <> semi

hDecl (PrimCast cc nm ty _ c_args c_res) cont =
 addToStubEnv nm ty              $
 tdefFunTy nm cc c_args c_res $$
 primHeader nm $$ 
 lbrace $$
   argAndResDecls ty c_args c_res $$ 
   text (nm++"__funptr __funptr__;") $$
   argAssign ty c_args $$
   text ("__funptr__ = ("++nm++"__funptr)arg0;") $$
   performCall True ("", Nothing, "__funptr__", Nothing) c_args ty $$
   pushResult c_res ty $$
 rbrace $$
 cont

hDecl (Include s) cont = text ("#include " ++ s) $$ cont
hDecl (CCode s)   cont
 | not optGenHeader  = text s $$ cont
 | otherwise         = cont
hDecl _ cont = cont

\end{code}

\begin{code}
primHeader :: Name -> HugsStubCode
primHeader nm = text "primFun" <> parens (text nm)

argAndResDecls :: Type -> [(Bool,String)] -> (Bool,String) -> HugsStubCode
argAndResDecls ty c_args c_res = ppDecls (zipWith declArg [0..] c_args) $$ declRes 
 where
  (_, res) = splitFunTys ty

  declRes
   | noResult   = empty
   | otherwise  = text (snd c_res) <+> text "res" <> semi

  declArg n (is_struct,t)
    | is_struct = text t <> char '*' <+> ppArg False n
    | otherwise = text t <+> ppArg False n

  noResult =
    case res of 
     (TyApply (TyCon _) [TyCon tc]) -> qName tc == "()"
     _ -> False

ppArg :: Bool -> Int -> HugsStubCode
ppArg isStructTy n 
 | isStructTy = text ("*arg"++show n)
 | otherwise  = text ("arg"++show n)

ppCTy :: Type -> HugsStubCode
ppCTy ty = 
 case ty of
   TyVar _ tv    -> text (degrokNm (qName tv))
   TyCon tc      
     | qName tc == "()" -> empty
     | otherwise        -> text (degrokNm (qName tc))
   TyApply (TyCon tc) [_] | qName tc == "StablePtr"  -> text "StablePtr"
   TyApply (TyCon tc) [_] | qName tc == "Ptr"        -> text "Addr"
   TyApply (TyCon tc) [_] | qName tc == "FunPtr"     -> text "Addr"
   TyApply (TyCon tc) [_] | qName tc == "ForeignPtr" -> text "Foreign"
   TyApply _ [t] -> ppCTy t   -- catches (IO t)
   TyApply t _   -> ppCTy t
   TyList  _     -> text "void*"
   TyTuple _     -> text "void*"
   TyFun _ _     -> text "void*"
   TyCtxt _ t    -> ppCTy t
 where
  degrokNm nm =
     case nm of
      'I':'n':'t':_     -> "Int"
      'W':'o':'r':'d':_ -> "Word"
      'F':'o':_         -> "Foreign"
      "Char"            -> "Char"
      "Double"          -> "Double"
      "Float"           -> "Float"
      _                 -> "Addr"

argAssign :: Type -> [(Bool,String)] -> HugsStubCode
argAssign ty c_args = ppDecls (zipWith3 declArg [0..] args c_args)
  where
  (args, _) = splitFunTys ty

  declArg n t@(TyCon tc) (_, c_ty) 
    | optLongLongIsInteger && qName tc == "Integer"
    = ppArg False n <+> equals    <+>
        parens (text c_ty) <>
        parens (text"hugs->get" <> ppCTy t <> text "()") <> semi $$
        ppArg False n <+> text ">>= 32" <> semi $$
        ppArg False n <+> text "+=" <+> parens (text c_ty) <> 
              parens (text"hugs->get" <> ppCTy t <> text "()")
  declArg n t (is_struct, c_ty) 
     = ppArg False n <+> equals    <+>
        parens ppr_c_ty <>
        parens (text"hugs->get" <> ppCTy t <> text "()")
   where
    ppr_c_ty
      | is_struct = text c_ty <> char '*'
      | otherwise = text c_ty 

\end{code}

\begin{code}
performCall :: Bool -> LocSpec -> [(Bool,String)] -> Type -> HugsStubCode
performCall is_dyn (_,_, fun, _) c_args ty = 
   ppAssign res <> text fun <> pp_fun_args <> semi
 where
  (args, res) = splitFunTys ty

  pp_fun_args
    | not (isIOTy res) && null args = empty -- hack to cope with constants
    | otherwise = ppTuple fun_args

  fun_args
    | is_dyn    = tail funArgs
    | otherwise = funArgs

  funArgs = zipWith funArg [0..] c_args

  funArg n (isStructTy,_) = ppArg isStructTy n

  ppAssign (TyApply (TyCon _) [TyCon tc])
    | qName tc == "()" = empty
  ppAssign _           = text "res" <+> equals

\end{code}

\begin{code}
pushResult :: (Bool,String) -> Type -> HugsStubCode
pushResult (isStructTy, c_ty) ty = 
  assignRes $$
  if isPure then 
      empty
  else
      text "hugs_returnIO" <> parens no_of_args <> semi
 where 
  (_, res) = splitFunTys ty

  isPure = 
    case res of 
      TyApply _ _ -> False
      _           -> True

  noResult =
    case res of 
     (TyApply (TyCon _) [TyCon tc]) -> qName tc == "()"
     _ -> False

  isIntegerRes =
    case res of 
     (TyApply (TyCon _) [TyCon tc]) -> qName tc == "Integer"
     _ -> False
     
  assignRes
    | noResult  = empty
    | isIntegerRes = text "hugs->putInt" <> parens ( text "(unsigned int)res" ) <> semi $$
                     text "hugs->putInt" <> parens ( text "(unsigned int)(res >> 32)" ) <> semi
    | otherwise = text "hugs->put" <> (ppCTy res) <>
                  parens (the_result) <> semi

  the_result
    | isStructTy = text "copyBytes" <> parens (
                        text "sizeof" <> parens (text c_ty) <>
                        text ", &res")
    | otherwise  = parens (text c_ty) <> text "res" 

  no_of_args
    | noResult     = text "0"
    | isIntegerRes = text "2"
    | otherwise    = text "1"

\end{code}

\begin{code}
tdefFunTy :: Name -> CallConv -> [(Bool,String)] -> (Bool,String) -> HugsStubCode
tdefFunTy nm cc c_args c_res =
 text "typedef" <+> ppResultTy <+>
   parens ( ppCallConv True cc <+> char '*' <+> 
            text (nm++"__funptr")) <+>
   ppTuple ppArgs <> semi
 where
  ppResultTy  = text (snd c_res)
  ppArgs = zipWith pp_arg [1..] (tail c_args)
  
  pp_arg n (_, t) = text t <+> ppArg False n

\end{code}

\begin{code}
genTrailer :: [(String,Int)] -> HugsStubCode
genTrailer [] = empty
genTrailer ls = 
  genPrimTable ls $$
  text "static struct hugs_primInfo prims = { 0, primTable, 0 };" $$
  text "#ifdef __cplusplus" $$
  text "extern \"C\" {" $$
  text "#endif" $$
  text "DLLEXPORT(void) initModule(HugsAPI4 *);" $$
  text "DLLEXPORT(void) initModule(HugsAPI4 *hugsAPI) {" $$
  text "  hugs = hugsAPI;" $$
  text "  hugs->registerPrims(&prims);" $$
  text "}" $$
  text "#ifdef __cplusplus" $$
  text "}"  $$
  text "#endif"
  
genPrimTable :: [(String,Int)] -> HugsStubCode
genPrimTable ls = 
  text "static struct hugs_primitive primTable[] = {" $$
  nest 2 (vsep (map genPrim ls)) $$
  nest 2 (text "{0,0,0}") $$
  text "};"
 where
  genPrim (nm, arity) = 
     lbrace <> text (show nm) <> comma <> text (show arity) <> comma <>
     text nm <> text "},"

\end{code}
