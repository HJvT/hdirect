%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Aug. 21th 2001  23:14  Sigbjorn Finne $
% @(#) $Contactid: sof@galconn.com $
%

Basic name supply monad.

\begin{code}

module NameSupply
	( NSM		-- instance {Functor,Monad} NSM
        , getNewName	-- :: NSM String
	, withNewName   -- :: String -> NSM a -> NSM a
        , getNewNames	-- :: Int -> NSM [String]
	, mapNSM
        , runNS
	) where

\end{code}

%************************************************************************
%*									*
\subsection{Monadic plumbing for Name Supply}
%*									*
%************************************************************************

\begin{code}

type NameSupply = [String]

newtype NSM a = NSM (NameSupply -> (a, NameSupply))

mapNSM :: (a -> b) -> NSM a -> NSM b
mapNSM f (NSM g) = NSM (\ns -> let (a, ns') = g ns in (f a, ns'))

instance Monad NSM where
  (NSM f) >>= g	= 
    NSM (\ns -> let (result1, ns1)	= f ns
                    (NSM h)		= g result1 
                in h ns1)
  return a	= NSM (\ns -> (a, ns))

getNewNames :: Int -> NSM [String]
getNewNames i = NSM (\ns -> splitAt i ns)

getNewName :: NSM String
getNewName = NSM (\ns -> (head ns, tail ns))

withNewName :: String -> NSM a -> NSM a
withNewName n (NSM f) = NSM $ \ ns ->
  case f (n:ns) of
    (v, ns') -> case ns' of
    		  (x:xs) | x == n -> (v, xs) 
		      -- make sure we remove the name added.
		  _ -> (v, ns')

runNS :: NSM a -> [String] -> a
runNS (NSM f) ns = fst (f ns)

\end{code}
