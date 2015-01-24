%
% (c) sof, 1999
%

Validating fragments of IDL.

\begin{code}
module Validate 
        ( 
          validateParam
        ) where

import CoreIDL
import CoreUtils
import BasicTypes
import Opts
import TypeInfo
import Utils

\end{code}

Out parameters need to (expand to) a pointed type. In the
case of interface pointers, the type has got to be a pointer
to one.

Errors are treated as warnings (at least for now), so the
validator will simply notify you of the error of your ways and
correct the type.

\begin{code}
validateParam :: String -> Param -> Param
validateParam msg p =
  case paramMode p of
    Out -> 
      case paramType p of
       Pointer pt isExp t | optCom && isIfaceTy t && not (isIfacePtr t) ->
                warnWrongOutParam msg "out"
                                  "pointer to an interface pointer (as it needs to be.)"
                                  p{ paramType=(Pointer pt isExp (Pointer Ref True t))
                                   , paramOrigType=(Pointer pt isExp (Pointer Ref True t))
                                   }
                   | otherwise -> p
       t@Iface{} | optCom -> 
                warnWrongOutParam msg "out"
                                  "pointer to an interface pointer (as it needs to be.)"
                                  p{ paramType=(Pointer Ref True (Pointer Ref True t))
                                   , paramOrigType=(Pointer Ref True (Pointer Ref True t))
                                   }
       WString{}  -> p
       String{}   -> p
       Array{}    -> p
       Sequence{} -> p
       Name _ _ _ _ _ (Just ti)
                   | is_pointed ti -> p
       t ->
        warnWrongOutParam msg "out"
                          "pointer to a type"
                          p{ paramType=Pointer Ref True t
                           , paramOrigType=Pointer Ref True (paramOrigType p)
                           }
    In ->
      case paramType p of
       ty@Iface{} | optCom -> 
                warnWrongOutParam msg "in"
                                  "*pointer* to an interface (as it needs to be.)"
                                  p{ paramType=(Pointer Ref True ty)
                                   , paramOrigType=(Pointer Ref True ty)
                                   }
       _ -> p
    _ -> p                         
\end{code}

\begin{code}
warnWrongOutParam :: String -> String -> String -> a -> a
warnWrongOutParam prefix pkind kind cont =
   trace ("Warning: [" ++ pkind ++ "] parameter " ++ show prefix ++
          " is not a " ++ kind ++ "\n Correcting it for you.\n")
         cont

\end{code}

