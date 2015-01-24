%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[FiniteMap]{An implementation of finite maps}

[For Hugs only.]

``Finite maps'' are the heart of the compiler's
lookup-tables/environments and its implementation of sets.  Important
stuff!

This code is derived from that in the paper:
\begin{display}
	S Adams
	"Efficient sets: a balancing act"
	Journal of functional programming 3(4) Oct 1993, pp553-562
\end{display}

The code is SPECIALIZEd to various highly-desirable types (e.g., Id)
near the end (only \tr{#ifdef COMPILING_GHC}).

\begin{code}
module FiniteMap (
	FiniteMap,		-- abstract type

	emptyFM, unitFM, listToFM,

	addToFM,
	addToFM_C,
	addListToFM,
	addListToFM_C,
	delFromFM ,
	delListFromFM,

	plusFM,
	plusFM_C,
	minusFM,
	foldFM,

	intersectFM ,
	intersectFM_C ,
	mapFM , filterFM ,
	mapMaybeFM,

	sizeFM, isEmptyFM, elemFM, lookupFM, lookupWithDefaultFM,

	fmToList, keysFM, eltsFM

    ) where

import Maybe( isJust )

--import Bag	( foldBag )

\end{code}


%************************************************************************
%*									*
\subsection{The signature of the module}
%*									*
%************************************************************************

\begin{code}
--	BUILDING
emptyFM		:: FiniteMap key elt
unitFM		:: key -> elt -> FiniteMap key elt
listToFM	:: (Ord key) => [(key,elt)] -> FiniteMap key elt
			-- In the case of duplicates, the last is taken

--	ADDING AND DELETING
		   -- Throws away any previous binding
		   -- In the list case, the items are added starting with the
		   -- first one in the list
addToFM		:: (Ord key) => FiniteMap key elt -> key -> elt  -> FiniteMap key elt
addListToFM	:: (Ord key) => FiniteMap key elt -> [(key,elt)] -> FiniteMap key elt

		   -- Combines with previous binding
		   -- In the combining function, the first argument is the "old" element,
		   -- while the second is the "new" one.
addToFM_C	:: (Ord key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> key -> elt
			   -> FiniteMap key elt
addListToFM_C	:: (Ord key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> [(key,elt)]
			   -> FiniteMap key elt

		   -- Deletion doesn't complain if you try to delete something
		   -- which isn't there
delFromFM	:: (Ord key) => FiniteMap key elt -> key   -> FiniteMap key elt
delListFromFM	:: (Ord key) => FiniteMap key elt -> [key] -> FiniteMap key elt

--	COMBINING
		   -- Bindings in right argument shadow those in the left
plusFM		:: (Ord key) => FiniteMap key elt -> FiniteMap key elt
			   -> FiniteMap key elt

		   -- Combines bindings for the same thing with the given function
plusFM_C	:: (Ord key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt

minusFM		:: (Ord key) => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
		   -- (minusFM a1 a2) deletes from a1 any bindings which are bound in a2

intersectFM	:: (Ord key) => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
intersectFM_C	:: (Ord key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt

--	MAPPING, FOLDING, FILTERING
foldFM		:: (key -> elt -> a -> a) -> a -> FiniteMap key elt -> a
mapFM		:: (key -> elt1 -> elt2) -> FiniteMap key elt1 -> FiniteMap key elt2
filterFM	:: (Ord key) => (key -> elt -> Bool)
			   -> FiniteMap key elt -> FiniteMap key elt
mapMaybeFM      :: (Ord key)
		=> (key -> elt1 -> Maybe elt2)
		-> FiniteMap key elt1
		-> FiniteMap key elt2

--	INTERROGATING
sizeFM		:: FiniteMap key elt -> Int
isEmptyFM	:: FiniteMap key elt -> Bool

elemFM		:: (Ord key) => key -> FiniteMap key elt -> Bool
lookupFM	:: (Ord key) => FiniteMap key elt -> key -> Maybe elt
lookupWithDefaultFM
		:: (Ord key) => FiniteMap key elt -> elt -> key -> elt
		-- lookupWithDefaultFM supplies a "default" elt
		-- to return for an unmapped key

--	LISTIFYING
fmToList	:: FiniteMap key elt -> [(key,elt)]
keysFM		:: FiniteMap key elt -> [key]
eltsFM		:: FiniteMap key elt -> [elt]
\end{code}

%************************************************************************
%*									*
\subsection{The @FiniteMap@ data type, and building of same}
%*									*
%************************************************************************

Invariants about @FiniteMap@:
\begin{enumerate}
\item
all keys in a FiniteMap are distinct
\item
all keys in left  subtree are $<$ key in Branch and
all keys in right subtree are $>$ key in Branch
\item
size field of a Branch gives number of Branch nodes in the tree
\item
size of left subtree is differs from size of right subtree by a
factor of at most \tr{sIZE_RATIO}
\end{enumerate}

\begin{code}
data FiniteMap key elt
  = EmptyFM
  | Branch key elt	    	-- Key and elt stored here
    Int{-STRICT-}	-- Size >= 1
    (FiniteMap key elt)	    	-- Children
    (FiniteMap key elt)
\end{code}

\begin{code}
emptyFM = EmptyFM
{-
emptyFM
  = Branch bottom bottom 0 bottom bottom
  where
    bottom = panic "emptyFM"
-}

-- #define EmptyFM (Branch _ _ 0 _ _)

unitFM key elt = Branch key elt 1 emptyFM emptyFM

listToFM = addListToFM emptyFM

\end{code}

%************************************************************************
%*									*
\subsection{Adding to and deleting from @FiniteMaps@}
%*									*
%************************************************************************

\begin{code}
addToFM fm key elt = addToFM_C (\ _ new -> new) fm key elt

addToFM_C _        EmptyFM key elt = unitFM key elt
addToFM_C combiner (Branch key elt size fm_l fm_r) new_key new_elt
  = case compare new_key key of
	LT -> mkBalBranch key elt (addToFM_C combiner fm_l new_key new_elt) fm_r
	GT -> mkBalBranch key elt fm_l (addToFM_C combiner fm_r new_key new_elt)
	EQ -> Branch new_key (combiner elt new_elt) size fm_l fm_r

addListToFM fm key_elt_pairs = addListToFM_C (\ _ new -> new) fm key_elt_pairs

addListToFM_C combiner fm key_elt_pairs
  = foldl add fm key_elt_pairs	-- foldl adds from the left
  where
    add f (key,elt) = addToFM_C combiner f key elt
\end{code}

\begin{code}
delFromFM EmptyFM _ = emptyFM
delFromFM (Branch key elt _ fm_l fm_r) del_key
  = case compare del_key key of
	GT -> mkBalBranch key elt fm_l (delFromFM fm_r del_key)
	LT -> mkBalBranch key elt (delFromFM fm_l del_key) fm_r
	EQ -> glueBal fm_l fm_r

delListFromFM fm keys = foldl delFromFM fm keys
\end{code}

%************************************************************************
%*									*
\subsection{Combining @FiniteMaps@}
%*									*
%************************************************************************

\begin{code}
plusFM_C _        EmptyFM fm2 = fm2
plusFM_C _        fm1 EmptyFM = fm1
plusFM_C combiner fm1 (Branch split_key elt2 _ left right)
  = mkVBalBranch split_key new_elt
		 (plusFM_C combiner lts left)
		 (plusFM_C combiner gts right)
  where
    lts     = splitLT fm1 split_key
    gts     = splitGT fm1 split_key
    new_elt = case lookupFM fm1 split_key of
		Nothing   -> elt2
		Just elt1 -> combiner elt1 elt2

-- It's worth doing plusFM specially, because we don't need
-- to do the lookup in fm1.

plusFM EmptyFM fm2 = fm2
plusFM fm1 EmptyFM = fm1
plusFM fm1 (Branch split_key elt1 _ left right)
  = mkVBalBranch split_key elt1 (plusFM lts left) (plusFM gts right)
  where
    lts     = splitLT fm1 split_key
    gts     = splitGT fm1 split_key

minusFM EmptyFM _   = emptyFM
minusFM fm1 EmptyFM = fm1
minusFM fm1 (Branch split_key _ _ left right)
  = glueVBal (minusFM lts left) (minusFM gts right)
	-- The two can be way different, so we need glueVBal
  where
    lts = splitLT fm1 split_key		-- NB gt and lt, so the equal ones
    gts = splitGT fm1 split_key		-- are not in either.

intersectFM fm1 fm2 = intersectFM_C (\ _ right -> right) fm1 fm2

intersectFM_C _        _   EmptyFM = emptyFM
intersectFM_C _        EmptyFM _   = emptyFM
intersectFM_C combiner fm1 (Branch split_key elt2 _ left right)

  | isJust maybe_elt1	-- split_elt *is* in intersection
  = mkVBalBranch split_key (combiner elt1 elt2) (intersectFM_C combiner lts left)
						(intersectFM_C combiner gts right)

  | otherwise			-- split_elt is *not* in intersection
  = glueVBal (intersectFM_C combiner lts left) (intersectFM_C combiner gts right)

  where
    lts = splitLT fm1 split_key		-- NB gt and lt, so the equal ones
    gts = splitGT fm1 split_key		-- are not in either.

    maybe_elt1 = lookupFM fm1 split_key
    Just elt1  = maybe_elt1
\end{code}

%************************************************************************
%*									*
\subsection{Mapping, folding, and filtering with @FiniteMaps@}
%*									*
%************************************************************************

\begin{code}
foldFM _ z EmptyFM = z
foldFM k z (Branch key elt _ fm_l fm_r)
  = foldFM k (k key elt (foldFM k z fm_r)) fm_l

mapFM _ EmptyFM = emptyFM
mapFM f (Branch key elt size fm_l fm_r)
  = Branch key (f key elt) size (mapFM f fm_l) (mapFM f fm_r)

filterFM _ EmptyFM = emptyFM
filterFM p (Branch key elt _ fm_l fm_r)
  | p key elt		-- Keep the item
  = mkVBalBranch key elt (filterFM p fm_l) (filterFM p fm_r)

  | otherwise		-- Drop the item
  = glueVBal (filterFM p fm_l) (filterFM p fm_r)

mapMaybeFM _ EmptyFM = emptyFM
mapMaybeFM f (Branch key elt _ fm_l fm_r) =
  case f key elt of
    Nothing   -> glueVBal (mapMaybeFM f fm_l) (mapMaybeFM f fm_r)
    Just elt' -> mkVBalBranch key elt' (mapMaybeFM f fm_l) (mapMaybeFM f fm_r)

\end{code}

%************************************************************************
%*									*
\subsection{Interrogating @FiniteMaps@}
%*									*
%************************************************************************

\begin{code}
--{-# INLINE sizeFM #-}
sizeFM EmptyFM		     = 0
sizeFM (Branch _ _ size _ _) = size

isEmptyFM fm = sizeFM fm == 0

lookupFM EmptyFM _ = Nothing
lookupFM (Branch key elt _ fm_l fm_r) key_to_find
  = case compare key_to_find key of
	LT -> lookupFM fm_l key_to_find
	GT -> lookupFM fm_r key_to_find
	EQ -> Just elt

key `elemFM` fm
  = case (lookupFM fm key) of { Nothing -> False; _ -> True }

lookupWithDefaultFM fm deflt key
  = case (lookupFM fm key) of { Nothing -> deflt; Just elt -> elt }
\end{code}

%************************************************************************
%*									*
\subsection{Listifying @FiniteMaps@}
%*									*
%************************************************************************

\begin{code}
fmToList fm = foldFM (\ key elt rest -> (key,elt) : rest) [] fm
keysFM fm   = foldFM (\ key _   rest -> key : rest)       [] fm
eltsFM fm   = foldFM (\ _   elt rest -> elt : rest)       [] fm
\end{code}


%************************************************************************
%*									*
\subsection{The implementation of balancing}
%*									*
%************************************************************************

%************************************************************************
%*									*
\subsubsection{Basic construction of a @FiniteMap@}
%*									*
%************************************************************************

@mkBranch@ simply gets the size component right.  This is the ONLY
(non-trivial) place the Branch object is built, so the ASSERTion
recursively checks consistency.  (The trivial use of Branch is in
@unitFM@.)

\begin{code}
sIZE_RATIO :: Int
sIZE_RATIO = 5

mkBranch :: (Ord key) 		-- Used for the assertion checking only
	 => Int
	 -> key -> elt
	 -> FiniteMap key elt -> FiniteMap key elt
	 -> FiniteMap key elt

mkBranch _{-which-} key elt fm_l fm_r
  = --ASSERT( left_ok && right_ok && balance_ok )
    let
	result = Branch key elt (unbox (1 + left_size + right_size)) fm_l fm_r
    in
--    if sizeFM result <= 8 then
	result
--    else
--	pprTrace ("mkBranch:"++(show which)) (ppr PprDebug result) (
--	result
--	)
  where
{-
    left_ok  = case fm_l of
		EmptyFM	 -> True
		Branch{} -> let
			     biggest_left_key = fst (findMax fm_l)
			    in
			    biggest_left_key < key
    right_ok = case fm_r of
		EmptyFM	 -> True
		Branch{} -> let
			     smallest_right_key = fst (findMin fm_r)
			    in
			    key < smallest_right_key
    balance_ok = True -- sigh
-}
{- LATER:
    balance_ok
      = -- Both subtrees have one or no elements...
	(left_size + right_size <= 1)
-- NO	      || left_size == 0  -- ???
-- NO	      || right_size == 0 -- ???
    	-- ... or the number of elements in a subtree does not exceed
	-- sIZE_RATIO times the number of elements in the other subtree
      || (left_size  * sIZE_RATIO >= right_size &&
    	  right_size * sIZE_RATIO >= left_size)
-}

    left_size  = sizeFM fm_l
    right_size = sizeFM fm_r

    unbox :: Int -> Int
    unbox x = x
\end{code}

%************************************************************************
%*									*
\subsubsection{{\em Balanced} construction of a @FiniteMap@}
%*									*
%************************************************************************

@mkBalBranch@ rebalances, assuming that the subtrees aren't too far
out of whack.

\begin{code}
mkBalBranch :: (Ord key)
	    => key -> elt
	    -> FiniteMap key elt -> FiniteMap key elt
	    -> FiniteMap key elt

mkBalBranch key elt fm_L fm_R

  | size_l + size_r < 2
  = mkBranch 1{-which-} key elt fm_L fm_R

  | size_r > sIZE_RATIO * size_l	-- Right tree too big
  = case fm_R of
	Branch _ _ _ fm_rl fm_rr
		| sizeFM fm_rl < 2 * sizeFM fm_rr -> single_L fm_L fm_R
		| otherwise	   	          -> double_L fm_L fm_R
	-- Other case impossible
	EmptyFM -> error "FiniteMap.mkBalBranch: the impossible happened"

  | size_l > sIZE_RATIO * size_r	-- Left tree too big
  = case fm_L of
	Branch _ _ _ fm_ll fm_lr
		| sizeFM fm_lr < 2 * sizeFM fm_ll -> single_R fm_L fm_R
		| otherwise		          -> double_R fm_L fm_R
	-- Other case impossible
	EmptyFM -> error "FiniteMap.mkBalBranch: the impossible happened"

  | otherwise				-- No imbalance
  = mkBranch 2{-which-} key elt fm_L fm_R

  where
    size_l   = sizeFM fm_L
    size_r   = sizeFM fm_R

    single_L fm_l (Branch key_r elt_r _ fm_rl fm_rr)
	= mkBranch 3{-which-} key_r elt_r (mkBranch 4{-which-} key elt fm_l fm_rl) fm_rr
    single_L _ _ = error "FiniteMap.single_L: the impossible happened"

    double_L fm_l (Branch key_r elt_r _ (Branch key_rl elt_rl _ fm_rll fm_rlr) fm_rr)
	= mkBranch 5{-which-} key_rl elt_rl (mkBranch 6{-which-} key   elt   fm_l   fm_rll)
				 (mkBranch 7{-which-} key_r elt_r fm_rlr fm_rr)
    double_L _ _ = error "FiniteMap.double_L: the impossible happened"

    single_R (Branch key_l elt_l _ fm_ll fm_lr) fm_r
	= mkBranch 8{-which-} key_l elt_l fm_ll (mkBranch 9{-which-} key elt fm_lr fm_r)
    single_R _ _ = error "FiniteMap.single_R: the impossible happened"

    double_R (Branch key_l elt_l _ fm_ll (Branch key_lr elt_lr _ fm_lrl fm_lrr)) fm_r
	= mkBranch 10{-which-} key_lr elt_lr (mkBranch 11{-which-} key_l elt_l fm_ll  fm_lrl)
				 (mkBranch 12{-which-} key   elt   fm_lrr fm_r)
    double_R _ _ = error "FiniteMap.double_R: the impossible happened"
\end{code}


\begin{code}
mkVBalBranch :: (Ord key)
	     => key -> elt
	     -> FiniteMap key elt -> FiniteMap key elt
	     -> FiniteMap key elt

-- Assert: in any call to (mkVBalBranch_C comb key elt l r),
--	   (a) all keys in l are < all keys in r
--	   (b) all keys in l are < key
--	   (c) all keys in r are > key

mkVBalBranch key elt EmptyFM fm_r = addToFM fm_r key elt
mkVBalBranch key elt fm_l EmptyFM = addToFM fm_l key elt

mkVBalBranch key elt fm_l@(Branch key_l elt_l _ fm_ll fm_lr)
		     fm_r@(Branch key_r elt_r _ fm_rl fm_rr)
  | sIZE_RATIO * size_l < size_r
  = mkBalBranch key_r elt_r (mkVBalBranch key elt fm_l fm_rl) fm_rr

  | sIZE_RATIO * size_r < size_l
  = mkBalBranch key_l elt_l fm_ll (mkVBalBranch key elt fm_lr fm_r)

  | otherwise
  = mkBranch 13{-which-} key elt fm_l fm_r

  where
    size_l = sizeFM fm_l
    size_r = sizeFM fm_r
\end{code}

%************************************************************************
%*									*
\subsubsection{Gluing two trees together}
%*									*
%************************************************************************

@glueBal@ assumes its two arguments aren't too far out of whack, just
like @mkBalBranch@.  But: all keys in first arg are $<$ all keys in
second.

\begin{code}
glueBal :: (Ord key)
	=> FiniteMap key elt -> FiniteMap key elt
	-> FiniteMap key elt

glueBal EmptyFM fm2 = fm2
glueBal fm1 EmptyFM = fm1
glueBal fm1 fm2
	-- The case analysis here (absent in Adams' program) is really to deal
	-- with the case where fm2 is a singleton. Then deleting the minimum means
	-- we pass an empty tree to mkBalBranch, which breaks its invariant.
  | sizeFM fm2 > sizeFM fm1
  = mkBalBranch mid_key2 mid_elt2 fm1 (deleteMin fm2)

  | otherwise
  = mkBalBranch mid_key1 mid_elt1 (deleteMax fm1) fm2
  where
    (mid_key1, mid_elt1) = findMax fm1
    (mid_key2, mid_elt2) = findMin fm2
\end{code}

@glueVBal@ copes with arguments which can be of any size.
But: all keys in first arg are $<$ all keys in second.

\begin{code}
glueVBal :: (Ord key)
	 => FiniteMap key elt -> FiniteMap key elt
	 -> FiniteMap key elt

glueVBal EmptyFM fm2 = fm2
glueVBal fm1 EmptyFM = fm1
glueVBal fm_l@(Branch key_l elt_l _ fm_ll fm_lr)
	 fm_r@(Branch key_r elt_r _ fm_rl fm_rr)
  | sIZE_RATIO * size_l < size_r
  = mkBalBranch key_r elt_r (glueVBal fm_l fm_rl) fm_rr

  | sIZE_RATIO * size_r < size_l
  = mkBalBranch key_l elt_l fm_ll (glueVBal fm_lr fm_r)

  | otherwise		-- We now need the same two cases as in glueBal above.
  = glueBal fm_l fm_r
  where
    size_l = sizeFM fm_l
    size_r = sizeFM fm_r
\end{code}

%************************************************************************
%*									*
\subsection{Local utilities}
%*									*
%************************************************************************

\begin{code}
splitLT, splitGT :: (Ord key) => FiniteMap key elt -> key -> FiniteMap key elt

-- splitLT fm split_key  =  fm restricted to keys <  split_key
-- splitGT fm split_key  =  fm restricted to keys >  split_key

splitLT EmptyFM _ = emptyFM
splitLT (Branch key elt _ fm_l fm_r) split_key
  = case compare split_key key of
	LT -> splitLT fm_l split_key
	GT -> mkVBalBranch key elt fm_l (splitLT fm_r split_key)
	EQ -> fm_l

splitGT EmptyFM _ = emptyFM
splitGT (Branch key elt _ fm_l fm_r) split_key
  = case compare split_key key of
	GT -> splitGT fm_r split_key
	LT -> mkVBalBranch key elt (splitGT fm_l split_key) fm_r
	EQ -> fm_r

findMin :: FiniteMap key elt -> (key,elt)
findMin (Branch key elt _ EmptyFM _) = (key,elt)
findMin (Branch _   _   _ fm_l    _) = findMin fm_l
findMin _                            = error "FiniteMap.findMin: empty finite map"

deleteMin :: (Ord key) => FiniteMap key elt -> FiniteMap key elt
deleteMin (Branch _   _   _ EmptyFM fm_r) = fm_r
deleteMin (Branch key elt _ fm_l    fm_r) = mkBalBranch key elt (deleteMin fm_l) fm_r
deleteMin _                               = error "FiniteMap.deleteMin: empty finite map"

findMax :: FiniteMap key elt -> (key,elt)
findMax (Branch key elt _ _ EmptyFM) = (key,elt)
findMax (Branch _   _   _ _ fm_r)    = findMax fm_r
findMax _                            = error "FiniteMap.findMax: empty finite map"

deleteMax :: (Ord key) => FiniteMap key elt -> FiniteMap key elt
deleteMax (Branch _   _   _ fm_l EmptyFM) = fm_l
deleteMax (Branch key elt _ fm_l    fm_r) = mkBalBranch key elt fm_l (deleteMax fm_r)
deleteMax _                               = error "FiniteMap.deleteMax: empty finite map"
\end{code}

%************************************************************************
%*									*
\subsection{Output-ery}
%*									*
%************************************************************************

\begin{code}
instance (Eq key, Eq elt) => Eq (FiniteMap key elt) where
  fm_1 == fm_2 = (sizeFM   fm_1 == sizeFM   fm_2) &&   -- quick test
		 (fmToList fm_1 == fmToList fm_2)

{- NO: not clear what The Right Thing to do is:
instance (Ord key, Ord elt) => Ord (FiniteMap key elt) where
  fm_1 <= fm_2 = (sizeFM   fm_1 <= sizeFM   fm_2) &&   -- quick test
		 (fmToList fm_1 <= fmToList fm_2)
-}
\end{code}

%************************************************************************
%*									*
\subsection{FiniteSets---a thin veneer}
%*									*
%************************************************************************

\begin{code}
\end{code}

%************************************************************************
%*									*
\subsection{Efficiency pragmas for GHC}
%*									*
%************************************************************************

When the FiniteMap module is used in GHC, we specialise it for
\tr{Uniques}, for dastardly efficiency reasons.

\begin{code}
\end{code}
