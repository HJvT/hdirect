%
% (c) 1999, sof
%

From an interface, generate a corresponding Java
class/interface which allows you to override its
methods with Haskell implementations of them.

The emitted code currently uses the FunctionPointer
invocation interface to call from Java into Haskell,
but will eventually be extended to also emit native
method declarations.

\begin{code}
module JavaProxy ( javaProxyGen, prepareDecls ) where

import CoreIDL
import CoreUtils ( isVoidTy )
import Attribute
import BasicTypes
import Literal
import PP
import PpCore ( showCore, ppType )
import Maybe  ( mapMaybe )
\end{code}

The generator is simple-minded - spit out a pretty
printed template of the class/interface - no abstract
Java syntax, no nothing.

\begin{code}
javaProxyGen :: Decl -> String
javaProxyGen (Interface i False _ ds)
 | is_class =
   showPPDoc 
     (emitHeader i <+> char '{' $$
       vsep (map emitMethod ds) $$
       emitConstructor i ds     $$
      text "}")
     ()
 where
  is_class = idAttributes i `hasAttributeWithName` "jni_class"

javaProxyGen _ = ""

type Doc = PPDoc ()
\end{code}

\begin{code}
prepareDecls :: [Decl] -> [(String,Decl)]
prepareDecls [] = []
prepareDecls (x:xs) = 
  case x of
    Typedef{} -> prepareDecls xs
    Interface{declId=i}  -> (idOrigName i ++ "Proxy", x) : prepareDecls xs
    Module{declDecls=ys} ->
      prepareDecls (ys ++ xs)
    Library{declDecls=ys} ->
      prepareDecls (ys ++ xs)
    _ -> prepareDecls xs

\end{code}


\begin{code}
emitHeader :: Id -> Doc
emitHeader i = 
  text "public" <+> pp_kind <+> pp_name <+> pp_inherit <+> pp_implements
 where
  attrs = idAttributes i

  ifaces_implemented = mapMaybe toNm (filterAttributes attrs ["jni_interface"])
    where
     toNm  (Attribute _ [ParamLit (StringLit s)]) = Just s
     toNm  _					  = Nothing

  is_class = attrs `hasAttributeWithName` "jni_class"
  
  pp_name = text ((idOrigName i) ++ "Proxy")

  pp_kind
   | is_class  = text "class"
   | otherwise = text "interface"
   
  pp_inherit = text "extends " <+> text (idOrigName i)

  pp_implements 
    | null ifaces_implemented = empty
    | otherwise = text "implements" <+> 
                  hsep (punctuate comma (map text ifaces_implemented))
    
\end{code}

\begin{code}
emitMethod :: Decl -> Doc
emitMethod (Method i _ res ps _)
  | is_ignorable = empty  
{-
  | is_field = 
    text "public" <+> pp_static <+> 
      emitType (resultType res) <+>
    text field_name <> semi
-}
  | otherwise = -- a trusty old method
    text "public" <+> pp_static <+>
      emitType (resultType res) <+> 
    text (idOrigName i) <+> ppTuple (zipWith emitParam ps [0..]) $$
    char '{' $$
     return_decl <+>
       castResult (resultType res) 
       		  (fptr_call <> ppTuple (zipWith emitParamUse ps [0..]))
		  <> semi $$
    char '}'
 where 
  attrs = idAttributes i

  fptr_call = text ("fptr_"++idOrigName i ++ ".call")
  
  return_decl 
    | isVoidTy (resultType res) = empty
    | otherwise                 = text "return"

{-
  field_name =
    case idOrigName i of
      'g':'e':'t':'_':xs -> xs
      ls -> ls
-}
   -- Java doesn't have a notion of read-only fields,
   -- so we simply ignore field setters and only
   -- generate the Java field decl when seeing the getter.
   --
   -- ...Leave out fields for the moment, as we don't have a
   -- good way of mapping them to a Haskell impl.

  is_ignorable = attrs `hasAttributeWithNames` 
                       ["jni_set_field", "jni_get_field", "jni_ctor"]
--  is_field = attrs `hasAttributeWithName` "jni_get_field"

  is_static = attrs `hasAttributeWithName` "jni_static"
  
  pp_static 
   | is_static = text "static"
   | otherwise = empty

emitMethod _ = empty

\end{code}

\begin{code}
emitType :: Type -> Doc
emitType ty = 
  case ty of
    Integer Short _     -> text "short"
    Integer Long  _     -> text "long"
    Integer LongLong  _ -> text "long"
    Integer Natural _   -> text "int"
    Float Short -> text "float"
    Float Long  -> text "double"
    Char _      -> text "char"
    Bool        -> text "boolean"
    Octet       -> text "byte"
    Object      -> text "java.lang.Object"
    String{}    -> text "java.lang.String"
    Name _ _ _ _ (Just t) _ -> emitType t
    Pointer _ _ t -> emitType t
    Array t []  -> emitType t <> text "[]"
    Void        -> text "void"
    Iface _ _ o _ _ _ -> text o
    _ -> error ("emitType: unknown type " ++ showCore (ppType ty))

\end{code}

\begin{code}
emitParam :: Param -> Int -> Doc
emitParam p idx = emitType (paramType p) <+> text ("arg"++show idx)
\end{code}

If the parameter is of an unboxed type, box it up before invoking
@call@

\begin{code}
emitParamUse :: Param -> Int -> Doc
emitParamUse p idx =  boxValue (paramType p) (text ("arg"++show idx))
\end{code}

\begin{code}
boxValue :: Type -> Doc -> Doc
boxValue ty d = 
 case ty of
    Integer _ _ -> text "new Integer" <> parens d
    Float Short -> text "new Float" <> parens d
    Float Long  -> text "new Double" <> parens d
    Char _      -> text "new Character" <> parens d
    Bool        -> text "new Boolean" <> parens d
    Octet       -> text "new Byte" <> parens d
    Object      -> d
    String{}    -> d
    Name _ _ _ _ (Just t) _ -> boxValue t d
    Pointer _ _ t -> boxValue t d
    Array _ []  -> d
    Iface{}     -> d
    _ -> error ("boxValue: unknown type " ++ showCore (ppType ty))

\end{code}

\begin{code}
castResult :: Type -> Doc -> Doc
castResult t d = 
  case t of
    Integer _ _ -> parens (text "Integer") <> d <> text ".value"
    Float Short -> parens (text "Float") <> d <> text ".floatValue"
    Float Long  -> parens (text "Double") <> d <> text ".doubleValue"
    Char _      -> parens (text "Character") <> d <> text ".value"
    Bool        -> parens (text "Character") <> d <> text ".booleanValue"
    Octet       -> parens (text "Character") <> d <> text ".byteValue"
    Object      -> d
    String{}    -> parens (text "String") <> d
    Name _ _ _ _ (Just ty) _ -> castResult ty d
    Pointer _ _ ty -> castResult ty d
    Array ty []  -> parens (castResult ty (text "[]" <> d)) -- won't work.
    Iface _ _ o _ _ _ -> parens (text o) <> d
    Void -> d
    _ -> error ("castResult: unknown type " ++ showCore (ppType t))
  
\end{code}

\begin{code}
emitConstructor :: Id -> [Decl] -> Doc
emitConstructor i ds = 
  vsep (map mkMethodPtr ms) $$
  text "public" <+> text (idOrigName i ++ "Proxy") <> 
    ppTuple (zipWith (\ x _ -> text ("FunctionPtr arg" ++ show x))
    		     idxs
		     ms) $$
    char '{' $$
    vsep (zipWith assignFptr idxs ms) $$
    char '}'
 where
  ms = filter isMethod ds

  idxs = [(0::Int)..]

  assignFptr idx m = 
    functionPtrName m <+> equals <+> text ("arg"++show idx) <> semi
     
  functionPtrName d = text ("fptr_" ++ idOrigName (declId d))

   -- should qualify FunctionPtr with its package name.
  mkMethodPtr d = 
     text "private FunctionPtr" <+> functionPtrName d <> semi

  isMethod d = not (idAttributes (declId d) `hasAttributeWithNames` 
  		    ["jni_set_field", "jni_get_field", "jni_ctor"])
\end{code}
