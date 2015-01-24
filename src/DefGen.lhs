%
%
%

Generate module definition files (Win32 specific).

\begin{code}
module DefGen (defGen) where

import List
import AbstractH
import Utils
\end{code}

\begin{code}
defGen :: [(String, Bool, [HTopDecl])] -> [(String,String)]
defGen code = 
   map genDef $ 
   groupBy eqMod $
   sortBy cmpMod (foldr getLocSpecs [] (map (\ (_,_,x) -> getDecl x) code))
  where
   eqMod  (x,_,_,_) (y,_,_,_)  = x == y
   cmpMod (x,_,_,_) (y,_,_,_) = compare x y

   getDecl [] = error "defGen.getDecls: not supposed to happen!"
   getDecl ((HMod (HModule _ _ _ _ h)):_) = h
   getDecl (_:hs) = getDecl hs

   getLocSpecs (AndDecl h1 h2) acc = getLocSpecs h1 (getLocSpecs h2 acc)
   getLocSpecs (Primitive _ _ ls _ _ _ _ _) acc = ls:acc
   getLocSpecs _ acc = acc

   genDef ls = (defName ls, defExports ls)

   defName [] = "Anon"
   defName ((l,_,_,_):_) = (dropSuffix l) ++ ".def"

   defExports ls = unlines ("EXPORTS" : nub (map genExp ls))
      
   genExp (_,Nothing,nm,_)  = nm
   genExp (_,Just x,nm,dec) = nm' ++ ' ':'@':show x
	where
	 nm' =
	   case dec of
	     Nothing -> nm
	     Just i  -> nm ++ '@':show i

\end{code}
