%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Oct. 16th 2001  10:39  Sigbjorn Finne $
% @(#) $Contactid: sof@galconn.com $
%

A basic environment abstraction, hiding its representation
(currently, finite maps..oops, wasn't suppose to tell you this :-( )

\begin{code}
module Env 
       (
          Env{- key elt -} -- abstract.
        , newEnv           -- :: Env key elt

        , addToEnv      -- :: Ord a => Env a b -> a -> b -> Env a b
        , addToEnv_C    -- :: Ord a => (b -> b -> b) -> Env a b -> a -> b -> Env a b
        , replaceElt    -- :: Ord a => Env a b -> a -> b -> Env a b
        
        , delFromEnv    -- :: Ord a => Env a b -> a -> Env a b

        , addListToEnv   -- :: Ord a => Env a b -> [(a, b)] -> Env a b
        , addListToEnv_C -- :: Ord a => (b -> b -> b) -> Env a b -> [(a, b)] -> Env a b

        , lookupEnv      -- :: Ord a => Env a b -> a -> Maybe b
        , envToList      -- :: Env a b -> [(a,b)]
        
        , mapEnv         -- :: (a -> b -> c) -> Env a b -> Env a c
        , mapMaybeEnv    -- :: Ord a => (a -> b -> Maybe c) -> Env a b -> Env a c
        
        , unionEnvs      -- :: [Env a b] -> Env a b

       ) where

import FiniteMap
\end{code}

\begin{code}
type Env a b = FiniteMap a b

newEnv :: Env a b
newEnv = emptyFM

addToEnv :: Ord a => Env a b -> a -> b -> Env a b
addToEnv fm key elt = addToFM fm key elt

delFromEnv :: Ord a => Env a b -> a -> Env a b
delFromEnv fm key = delFromFM fm key

addToEnv_C :: Ord a => (b -> b -> b) -> Env a b -> a -> b -> Env a b
addToEnv_C f fm key elt = addToFM_C f fm key elt

-- The same behaviour as addToEnv, but it really doesn't hurt
-- to be explicit about this.
replaceElt :: Ord a => Env a b -> a -> b -> Env a b
replaceElt fm key new_elt = addToFM_C (\ _ new -> new) fm key new_elt

addListToEnv :: Ord a => Env a b -> [(a, b)] -> Env a b
addListToEnv fm ls = addListToFM fm ls

addListToEnv_C :: Ord a => (b -> b -> b) -> Env a b -> [(a, b)] -> Env a b
addListToEnv_C f fm ls = addListToFM_C f fm ls

lookupEnv :: Ord a => Env a b -> a -> Maybe b
lookupEnv fm k = lookupFM fm k

envToList :: Env a b -> [(a,b)]
envToList fm = fmToList fm

mapEnv :: (a -> b -> c) -> Env a b -> Env a c
mapEnv f fm = mapFM f fm

mapMaybeEnv :: Ord a => (a -> b -> Maybe c) -> Env a b -> Env a c
mapMaybeEnv f fm = mapMaybeFM f fm

unionEnvs :: (Ord a) => [Env a b] -> Env a b
unionEnvs ls = foldl (\ acc x -> plusFM x acc) newEnv ls
\end{code}
