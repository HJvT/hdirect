% 
% (c) sof, 1999
%

All types are normalised before code generation, so that
we can avoid the generation of extraneous marshalling code
(and marshallers.)

Notice that the compiler makes a distinction between IDL types
that are used when generating Haskell type signatures and the
types that are used when generating marshalling code. The former
is not normalised in any way, since we want the Haskell signature to
mirror the IDL signature as much as possible; it is only the
latter that is subject to normalisation => we keep track of
both 

\begin{code}
module NormaliseType (normaliseType) where

import CoreIDL
import CoreUtils ( isConstructedTy )
\end{code}

\begin{code}
normaliseType :: Type -> Type
normaliseType ty = 
  case ty of
      -- types containing TypeInfo records are considered 'important',
      -- so we don't attempt to expand them out.
    Name _ _ _ _ _ (Just _) -> ty
      -- Chasing down name chains do run the risk of looping, should
      -- the desugarer have created a name which is the infinite expansion
      -- of itself. That Should Not Happen (any longer.)
    Name _ _ _ _ (Just t) _  | not (isConstructedTy t) -> normaliseType t
    FunTy cc res ps ->  FunTy cc (normaliseResult res) (map normaliseParam ps)
    String t u mb_e   ->  String (normaliseType t) u mb_e
    Sequence t mb_e mb_term ->  Sequence (normaliseType t) mb_e mb_term
    Struct i fs s   ->  Struct i (map normaliseField fs) s
    Union i1 t i2 i3 sws -> Union i1 (normaliseType t) i2 i3 (map normaliseSwitch sws)
    UnionNon i sws  ->  UnionNon i (map normaliseSwitch sws)
    CUnion i fs s   ->  CUnion i (map normaliseField fs) s
    Pointer pt isExp t ->  Pointer pt isExp (normaliseType t)
    Array t es      ->  Array (normaliseType t) es
    SafeArray t     ->  SafeArray (normaliseType t)
    _		    -> ty

normaliseResult :: Result -> Result
normaliseResult r = r{resultType=normaliseType (resultType r)}

normaliseParam :: Param -> Param
normaliseParam p = p{paramType=normaliseType (paramType p)}

normaliseField :: Field -> Field
normaliseField f = f{fieldType=normaliseType (fieldType f)}

normaliseSwitch :: Switch -> Switch
normaliseSwitch s@SwitchEmpty{} = s
normaliseSwitch s = s{switchType=normaliseType (switchType s)}

\end{code}
