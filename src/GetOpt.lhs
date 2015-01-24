%
% @(#) $Docid: Aug. 15th 2001  14:55  Sigbjorn Finne $
% @(#) $Contactid: sof@galconn.com $
%

A command-line options library - sof 1/97
[Updated to use more Haskell 1.4 features -- sof 1/98]

\begin{code}
module GetOpt
        (
         Opt(..)  -- instance Functor, Monad, MonadZero, MonadPlus

        {- The Opt monad primitives: -}

         -- add another item (to the front)
        , pushArg   -- :: String -> Opt a ()
         -- transform the threaded state
        , updState  -- :: (a -> a) -> Opt a ()
         -- aka zero
        , failed    -- :: Opt a b
         -- Opt try
        , catchOpt  -- :: Opt a b -> Opt a b -> Opt a b

        {- some useful Opt matchers: -}

          -- match if string is prefix of current element.
        , prefixMatch -- :: String -> Opt a String
        , prefixed    -- :: String -> Opt a b -> Opt a b
          -- if current option matches pred, try Opt argument.
        , matches     -- :: (String -> Bool) -> (String -> Opt a b) -> Opt a b
         -- test if flag is set
        , flag        -- :: String -> (a -> a) -> Opt a ()
        , flags       -- :: [(String,a->a)] -> Opt a ()
         -- n-way disjunction
        , opts        -- :: [Opt a b] -> Opt a b
        , orOpt       -- :: Opt a b -> Opt a b -> Opt a b
        , thenOpt     -- :: Opt a b -> Opt a b -> Opt a b

          -- try matching --{disable,enable}-foo
        , toggle     -- :: String 
                     -- -> String 
                     -- -> String 
                     -- -> (Bool -> a -> a) 
                     -- -> Opt a ()
        , toggles    -- :: String 
                     -- -> String 
                     -- -> [(String,Bool -> a->a)] 
                     -- -> Opt a ()

         -- try matching -ifoo (where -i is the prefix)
        , prefixArg  -- :: String -> (String -> a -> a) -> Opt a ()
         -- try matching -o foo
        , optionArg        -- :: String -> (String -> Opt a b) -> Opt a b
        , optionWithOptArg -- :: String -> Opt a b -> Opt a b
         -- exact string match
        , string           -- :: String -> Opt a ()

         -- useful combinators for when using attribute-lists
         -- to gather options
        , (-=)    -- :: String -> a -> Opt [a] ()
        , (-==)   -- :: String -> (String -> a) -> Opt [a] ()
        , (-===)  -- :: String -> (String -> a) -> Opt [a] ()
        , (-====) -- :: String -> (Maybe String -> a) -> Opt [a] ()
        , (-?)    -- :: (String -> Bool) -> (String -> a) -> Opt [a] ()

          -- Do the actual matching.

        , getOpts    -- :: Opt a b -> a -> [String] -> ([String],a)

        ) where

import Utils ( prefix )
import Control.Monad

infixr 1  `bindOpt`, `seqOpt`
-- needed for older Hugsen.
infixl 9 `orOpt`
\end{code}

Use a monad to encode the matching operations we want
to do on the command line contents, threading a value
that will record what we've seen so far plus the remainder
of the command-line.

\begin{code} 
data Opt a b = Opt ([String] -> a -> Maybe ([String],a,b))

-- bind & return over Opt
bindOpt :: Opt a b -> (b -> Opt a c) -> Opt a c
bindOpt (Opt opt_a) fopt = Opt (\ args st ->
   case opt_a args st of
     Nothing            -> Nothing
     Just (args',st',v) -> 
       case fopt v of Opt opt_b -> opt_b args' st')

seqOpt :: Opt a b -> Opt a c -> Opt a c
seqOpt a b = a `bindOpt` (\ _ -> b)

returnOpt :: b -> Opt a b
returnOpt v = Opt (\ args st -> Just (args,st,v))

{-
  The Opt primitives for pop and push of cmd line options, plus
  primitive for updating the threaded state.
-}

pushArg :: String -> Opt a ()
pushArg str = Opt (\ args st -> Just (str:args,st,()))

popArg :: Opt a String
popArg = Opt (\ args st ->
     case args of
       []     -> Nothing
       (x:xs) -> Just (xs,st,x)
    )

updState :: (a -> a) -> Opt a ()
updState f = Opt (\ args st -> Just (args, f st, ()))

{-
result :: a -> Opt a ()
result v = updState (\ _ -> v)
-}
  
 -- a not-that-useful operation on Opt.
mapOpt :: (b -> c) -> Opt a b -> Opt a c
mapOpt f (Opt opt) = Opt (\ args st ->
        case opt args st of
          Nothing -> Nothing
          Just (args',st',v) -> Just (args',st',f v))

-- Let's overload!

instance Monad (Opt s) where
  a >>= b = bindOpt a b
  return  = returnOpt

instance Functor (Opt s) where
  fmap = mapOpt

instance MonadPlus (Opt s) where
  mplus = thenOpt    
  mzero = failed
  
 -- no match.
failed :: Opt a b
failed = Opt (\ _ _ -> Nothing)


-- try left, if not successful, give right a spin.

catchOpt :: Opt a b -> Opt a b -> Opt a b
catchOpt (Opt opt_a) (Opt opt_b) = Opt (\ args st ->
     case opt_a args st of
       Nothing -> opt_b args st
       Just x  -> Just x)
\end{code}

Scanning a list of command-line options using
an Opt action that encodes what's interesting and
worth noting.

ToDo: add error support (in the monad?)

\begin{code}
getOpts :: Opt a b -> a -> [String] -> ([String],a)
getOpts _ st []                    = ([],st)
getOpts o@(Opt opt) st args@(x:xs) =
  case opt args st of
   Nothing            -> let (args',st') = getOpts o st xs in (x:args',st')
   Just (args',st',_) -> getOpts o st' args'

\end{code}

A number of useful matching combinators for command-line
options follow:

\begin{code}
prefixMatch :: String -> Opt a String
prefixMatch str = do
  arg <- popArg
  case prefix str arg of
   Nothing   -> failed
   Just arg' -> return arg'

prefixed :: String -> Opt a b -> Opt a b
prefixed pre n_opt = do
  arg <- prefixMatch pre
   -- push back what's left of the option, and continue.
  pushArg arg
  n_opt


matches :: (String -> Bool) -> (String -> Opt a b) -> Opt a b
matches matcher opt = do
   arg <- popArg
   if matcher arg 
    then opt arg
    else failed

flag :: String -> (a -> a) -> Opt a ()
flag str f = do
   arg <- popArg
   case prefix str arg of
    Nothing   -> failed
    Just{}    -> updState f

opts :: [Opt a b] -> Opt a b
opts ls = foldl1 (orOpt) ls

orOpt :: Opt a b -> Opt a b -> Opt a b
orOpt = catchOpt

thenOpt :: Opt a b -> Opt a b -> Opt a b
thenOpt opt_a opt_b = opt_a `seqOpt` opt_b
\end{code}


\begin{code}
flags :: [(String,a->a)] -> Opt a ()
flags ls = opts (map (\ (str,f) -> flag str f) ls)

toggle :: String -> String -> String -> (Bool -> a -> a) -> Opt a ()
toggle on off str f =
 ((prefixed on  (returnOpt True))   `orOpt`
  (prefixed off (returnOpt False))) >>= \ flg ->
 prefixed str (popArg >> updState (f flg))

toggles :: String -> String -> [(String,Bool -> a->a)] -> Opt a ()
toggles on off ls = opts (map (\ (str,f) -> toggle on off str f) ls)

prefixArg :: String -> (String -> a -> a) -> Opt a ()
prefixArg str f = do
  arg <- popArg
  case prefix str arg of
   Nothing   -> failed
   Just arg' -> updState (f arg')

optionArg :: String -> (String -> Opt a b) -> Opt a b
optionArg str f = do
    -- get current option
   arg <- popArg
   case prefix str arg of
    Nothing  -> failed
    Just{}   -> do
        -- get option value
       arg' <- popArg
       f arg'

optionWithOptArg :: String -> Opt a b -> Opt a b
optionWithOptArg str f = do
  arg <- popArg
  case prefix str arg of
   Nothing  -> failed
   Just _   -> f

string :: String  -> Opt a ()
string str =  do
  rest <- prefixMatch str
  case rest of
    [] -> returnOpt ()
    _  -> failed
  
(-=) :: String -> a -> Opt [a] ()
(-=) str v = flag str (v:)

(-==) :: String -> (String -> a) -> Opt [a] ()
(-==) str f = prefixArg str (\ ls -> ((f ls):))

(-===) :: String -> (String -> a) -> Opt [a] ()
(-===) str f = optionArg str (\ val -> updState ((f val):))

(-====) :: String -> (Maybe String -> a) -> Opt [a] ()
(-====) str f = 
  optionWithOptArg 
   str 
   (popArg >>= \ val -> updState ((f (Just val)):))
 
(-?) :: (String -> Bool) -> (String -> a) -> Opt [a] ()
(-?) matcher f = matches matcher (\ ls -> updState ((f ls):))
\end{code}
