%
% (c) 1999, sof
%

Generate C stubs to make up the difference between the
set of return types allowed by the GHC/Hugs FFI and C
(C lets you return structs/unions by value, the FFI doesn't).

\begin{code}
module CStubGen (cStubGen) where

import AbstractH
import AbsHUtils ( splitFunTys )
import PP
import BasicTypes
import Utils ( traceIf, dropSuffix )
import Opts  ( optGenHeader, optVerbose, optOneModulePerInterface )
import List  ( nub )
import Maybe ( isJust )
import Utils ( notNull )
\end{code}

\begin{code}
cStubGen :: String       -- (base)name of output file
	 -> [HTopDecl]   -- Haskell decls to derive C code from.
	 -> String
cStubGen c_nm hs = inc_header ((showPPDoc (hCode hs)) [])
 where
  inc_header ls =
      -- avoid generating files containing just a singular #include.
    case ls of
      "" -> ls
      _  | optGenHeader && not optOneModulePerInterface -> "#include "++ show (dropSuffix c_nm ++ ".h") ++ '\n':ls
         | otherwise -> ls
\end{code}

The generated C code may contain bindings to DLLs that are linked
by ordinal (Win32 specific), so we keep track of which DLLs they
are and simply just inform the user that special steps needs to
be taken in order to have this work successfully (i.e., need to
get at the .a / .lib for that DLL or generate it from a .def file.)

\begin{code}
type CStubCode = PPDoc [String]

getDllEnv :: ([String] -> CStubCode) -> CStubCode
getDllEnv cont env = cont env env

addToDllEnv :: String -> CStubCode -> CStubCode
addToDllEnv nm cont env = cont (nm:env)

hCode :: [HTopDecl] -> CStubCode
hCode xs = whizz xs
 where 
  whizz [] = 
    getDllEnv  $ \ dlls ->
    let dlls_real = nub (filter notNull dlls) in
    traceIf (optVerbose && notNull dlls_real)
            ("\nStubs depend on entry points from the following DLLs/libraries:\n  " ++
	     showList dlls_real (
	     "\nyou may need to adjust your command-line when compiling the stubs to" ++
	     "\ntake this into account.")) empty
  whizz (HLit _ : ls)     = whizz ls
  whizz (CLit s : ls)
    | not optGenHeader    = text s $$ whizz ls
    | otherwise           = whizz ls
  whizz (HInclude s : ls) = text "#include" <+> text (escapeString s) $$
  			    whizz ls
  whizz (HMod hm : ls)    = hMod hm (whizz ls)

  escapeString s@('"':_) = s -- "
  escapeString s@('<':_) = s
  escapeString s         = show s

hMod :: HModule -> CStubCode -> CStubCode
hMod (HModule _ _ _ _ d) cont = hDecl d cont

hDecl :: HDecl -> CStubCode -> CStubCode
hDecl (AndDecl d1 d2) cont = hDecl d1 (hDecl d2 cont)
hDecl (Primitive _ cc lspec nm ty needs_wrapper c_args c_res) cont
 | not needs_a_wrapper = cont
 | otherwise	       =
   addToDllEnv dll_name $
   tdefFun lspec cc c_args c_res $$
   primHeader nm c_res c_args    $$ 
   lbrace $$
     declResult $$
     performCall False lspec ty c_args $$
     pushResult c_res ty $$
   rbrace $$
   cont
 where
  declResult = 
    case snd c_res of
      "void" -> empty
      x      -> text x <+> text "res" <> semi

  needs_a_wrapper = needs_wrapper || isJust mb_ord

  (dll_name, mb_ord, _, _) = lspec

  tdefFun (_,Nothing,_,_) _ _ _ = empty
  tdefFun (_,Just _,tnm,_) tcc args res = 
    text "extern" <+> text (snd res) <+> ppCallConv True tcc <+> text tnm <+>
    ppTuple (map (text.snd) args) <> semi

hDecl (PrimCast cc nm ty needs_wrapper c_args c_res) cont
 | not needs_wrapper = cont
 | otherwise	     = 
   tdefFunTy ty nm cc c_args c_res $$
   primHeader nm c_res c_args      $$ 
   lbrace $$
     declResult $$
     text (nm++"__funptr __funptr__;") $$
     text ("__funptr__ = ("++nm++"__funptr)arg0;") $$
     performCall True ("", Nothing, "__funptr__", Nothing) ty c_args $$
     pushResult c_res ty $$
   rbrace $$
   cont
 where
  declResult = 
    case snd c_res of
      "void" -> empty
      x      -> text x <+> text "res" <> semi

hDecl (Include s) cont = text ("#include " ++ s) $$ cont
hDecl (CCode s)   cont
  | not optGenHeader   = text s $$ cont
  | otherwise          = cont
hDecl _ cont = cont
\end{code}

\begin{code}
primHeader :: Name -> (Bool,String) -> [(Bool,String)] -> CStubCode
primHeader nm res args = 
  text the_res <+> text nm <+> 
  ppTuple (zipWith (\ x n -> text (showTy x) <+> ppArg n) args [(0::Int)..])
 where
  the_res = showTy res

showTy :: (Bool,String) -> String
showTy (is_str,ty_str)
 | is_str    = ty_str ++ "*"
 | otherwise = ty_str

ppArg :: Int -> CStubCode
ppArg n = text ("arg"++show n)

\end{code}

\begin{code}
performCall :: Bool -> LocSpec -> Type -> [(Bool,String)] -> CStubCode
performCall is_dyn (_, _, fun, _) ty c_args = 
   ppAssign res <> text fun <> ppTuple fun_args <> semi
 where
  (_, res) = splitFunTys ty

  fun_args
    | is_dyn    = tail funArgs
    | otherwise = funArgs

  funArgs = zipWith funArg [0..] c_args

  funArg n (is_struct, _)
     | is_struct  = char '*' <> ppArg n
     | otherwise  = ppArg n

  ppAssign t
    | noResultTy t = empty 
    | otherwise    = text "res" <+> equals

\end{code}

\begin{code}
pushResult :: (Bool, String) -> Type -> CStubCode
pushResult (isStructTy, c_ty) ty = assignRes 
 where 
  (_, res) = splitFunTys ty

  noResult = noResultTy res
  
  assignRes
    | noResult  = empty
    | otherwise = text "return" <+> parens (the_result) <> semi

  the_result
   | isStructTy = text "copyBytes" <> parens (
			text "sizeof" <> parens (text c_ty) <>
			text ", &res")
   | otherwise  = text "res"

\end{code}

\begin{code}
tdefFunTy :: Type -> Name -> CallConv -> [(Bool,String)] -> (Bool,String) -> CStubCode
tdefFunTy ty nm cc c_args (_,c_res) =
 text "typedef" <+> ppResultTy <+>
   parens ( ppCallConv True cc <+> char '*' <+> 
	    text (nm++"__funptr")) <+>
   ppTuple ppArgs <> semi
 where
  (_, res) = splitFunTys ty

  ppResultTy
   | noResult  = text "void"
   | otherwise = text c_res

  ppArgs = zipWith pp_arg [1..] (tail c_args)
  
  pp_arg n (_,t) = text t <+> ppArg n

  noResult = noResultTy res

noResultTy :: Type -> Bool
noResultTy (TyApply (TyCon _) [TyCon tc]) = qName tc == "()"
noResultTy _				  = False
\end{code}
