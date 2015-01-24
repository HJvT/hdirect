%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Jun. 8th 2001  21:45  Sigbjorn Finne $
% @(#) $Contactid: sof@galconn.com $
%


\begin{code}
module MarshallMonad
       ( Mm
       , runMm
       , getMethodName
       , addCode
       , addToEnv
       , lookupName
       
       , MarshallInfo(..)
       , proxyMarshallInfo
       , stubMarshallInfo
       , structMarshallInfo
       
       ) where

import AbstractH ( Expr(..), Pat(..) )
import qualified Env
import BasicTypes
import LibUtils ( prelReturn )

data Mm a = Mm (Maybe String -> NameEnv -> (a, Cont, NameEnv))

type NameEnv = Env.Env String String
type Cont = Expr -> Expr
\end{code}

\begin{code}
runMm :: Maybe String -> [String] -> Expr -> Mm a -> Expr
runMm methName orig_names hole (Mm act) =
  case act methName env of
    (_, econt, _) -> peepHoleTop (econt hole)
  where
   env = Env.addListToEnv (Env.newEnv) (zip orig_names orig_names)

peepHoleTop :: Expr -> Expr
peepHoleTop e = 
  case e of
    Apply f [a] -> Apply f [(peepHole a)]  -- catch 'unsafePerformIO action'
    x           -> peepHole x

peepHole :: Expr -> Expr
peepHole e = 
  case e of
    Let bndrs e1 -> Let bndrs (peepHole e1)
    Bind (Apply (Var x) [Var y]) (PatVar z) e2
      | x == prelReturn && qName y == qName z -> peepHole e2
    Bind_ e1 e2  -> Bind_ (peepHole e1) (peepHole e2)
    Bind e1 p e2 -> 
        let
          p_e1 = peepHole e1
          p_e2 = peepHole e2
          p_e  = Bind p_e1 p p_e2
        in
        case p of
          PatVar x ->
            case p_e1 of
               Return (Var y) | qName x == qName y -> p_e2
               _ -> 
                 case p_e2 of
                   Return (Var y) | qName x == qName y -> p_e1
                   _ -> p_e
          _ -> p_e
    _ -> e

{- UNUSED
doMm :: [String] -> Expr -> Mm a -> (a, Expr)
doMm orig_names hole (Mm act) =
  case act Nothing env of
    (v, econt, _) -> (v, econt hole)
  where
   env = Env.addListToEnv (Env.newEnv) (zip orig_names orig_names)
-}

thenMm :: Mm a -> (a -> Mm b) -> Mm b
thenMm (Mm act) cont = 
  Mm ( \ env s ->
        case (act env s) of
          (v,cont1, s') -> 
            let (Mm b) = cont v in
            case b env s' of
             (x, cont2, s'') -> 
                 (x, cont1.cont2, s''))

returnMm :: a -> Mm a
returnMm v = Mm ( \ _ s -> (v,id,s))

instance Monad Mm where
  (>>=)   = thenMm
  return  = returnMm

getMethodName :: Mm (Maybe String)
getMethodName = Mm (\ nm env -> (nm, id, env))

addCode :: (Expr -> Expr) -> Mm ()
addCode cont1 = Mm ( \ _ s -> ((), cont1, s))

lookupName :: String -> Mm (Maybe String)
lookupName nm = Mm ( \ _ env ->
                      case (Env.lookupEnv env nm) of
                          Just n  ->  (Just n, id, env)
                          Nothing ->  (Nothing, id, env))

addToEnv :: String -> String -> Mm ()
addToEnv src_nm new_nm = 
  Mm (\ _ env -> 
        ((), id, Env.addToEnv env src_nm new_nm))
\end{code}

Marshalling IDL types isn't merely type-directed, it also needs to take
into account the context, are we generating proxy or stub marshallers,
for a struct etc. The @MarshallInfo@ encapsulates the context and
is passed to the functions which implements the marshalling rules.

\begin{code}
data MarshallInfo
 = MarshallInfo
      { forProxy  :: Bool
      , forStruct :: Bool
      , forInOut  :: Bool
      , forRef    :: Bool
      , doFree    :: Bool
      }

proxyMarshallInfo :: MarshallInfo
proxyMarshallInfo =
   MarshallInfo
      { forProxy  = True
      , forStruct = False
      , forInOut  = False
      , forRef    = False
      , doFree    = False
      }

stubMarshallInfo :: MarshallInfo
stubMarshallInfo =
   MarshallInfo
      { forProxy  = False
      , forStruct = False
      , forInOut  = False
      , forRef    = False
      , doFree    = False
      }

structMarshallInfo :: MarshallInfo
structMarshallInfo =
   MarshallInfo
      { forProxy  = False
      , forStruct = True
      , forInOut  = False
      , forRef    = True
      , doFree    = False
      }
\end{code}
