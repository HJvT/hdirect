%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Feb. 9th 2003  15:04  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Handling the marshalling of dependent arguments/fields.

\begin{code}
module MarshallDep 
	( marshallDependents
	, unmarshallDependents
	, freeDependent
	
	) where

import qualified AbstractH as Haskell (Expr)
import AbsHUtils
import CoreIDL
import CoreUtils

import MarshallMonad
import MarshallUtils
import MarshallType
import MarshallCore

import BasicTypes
import LibUtils

import List  ( nubBy )
import Maybe ( mapMaybe, fromMaybe, fromJust, isJust )
import Monad ( when )

\end{code}

%*
%
\section[marshall-dep]{Marshalling dependent parameters}
%
%*


\begin{code}
marshallDependents :: Bool
		   -> Bool
                   -> DependInfo
		   -> (Name -> Type)
		   -> Mm ()
marshallDependents inStruct forServer ls lookup_ty = do
  sequence (map marshallDep ls)
  return ()
 where
  {-
   Marshall the field members/parameters. A field/param
   is either a depender or a dependee (never both.)
  -}
  marshallDep (_, [])   = return () -- no dependencies - no code to generate here.
  marshallDep (i, deps) = marshallDependent inStruct forServer i lookup_ty deps
\end{code}

This function assumes that the dependencies are correct, that is,
well formed, no duplicates nor conflicting attributes were used
when the dependency information was computed.

   foo([in]int *len, [in,size_is(*len)]char* ls);

It performs the following tasks:

 - creates let bindings for the dependees, i.e.,

      let len = length ls

 - marshalls the dependers, i.e.,

      ls <- m_list s_Char len w_Char ls

 - in preparation for calling the external function,
   allocate the dependees, i.e.,

      len <- m_ref (allocOutPointer sizeofInt32) w_Int32 len

\begin{code}
marshallDependent :: Bool
		  -> Bool
		  -> Id
		  -> (Name -> Type)
		  -> [Dependent]
		  -> Mm ()
marshallDependent _ _ _ _ [Dep SwitchIs [DepVal{}]] = return ()
marshallDependent inStruct forServer i lookup_ty deps' = do
 addCode (hLets dep_binds)
 (if inStruct && isArrayTy (removeNames ty) then
     return ()
  else 
     addCode (bind (funApply marshall_list [real_nm]) real_nm))
 when (not forServer) (dep_ptrs  >> return ())
 return ()
  where
   nm	         = idName i
   ty	         = lookup_ty nm
   marshall_list = marshallList inStruct
   				True
                                ty lookup_ty
				   (varName m_list)
   				   trans_start_posns 
			   	   trans_end_posns 
				   alloc_sizes
   (trans_start_posns, trans_end_posns, alloc_sizes) 
                 = computeArrayConstraints False{-marshalling-} deps

   -- size information may be in part be specified as part of
   -- the (array) type, i.e.,  [size_is(len,)] char arr[][20];
   -- 
   -- we push this info into the dependency list here. It really
   -- should be done as part of desugaring. (ToDo.)
   deps         = 
    case ty of
      Array _ es -> 
         case es of
	  []      -> deps'
	  [e]     ->
            case break (isSizeIs) deps' of
	     (_,[]) -> ((Dep SizeIs [exprToDep e]):deps')
	     (as,(Dep SizeIs ds):bs) -> as++(Dep SizeIs (combine ds (exprToDep e))):bs
             _      -> error "MarshallDep.marshallDependent: expected a SizeIs attribute"		
          [e1,e2] -> [ Dep FirstIs [exprToDep e1]
	  	     , Dep LastIs  [exprToDep e2]
		     ] ++ deps'
          _      -> error "MarshallDep.marshallDependent: expected at most two attributes"
          where

	   combine (DepNone:ds) d = d:ds
	   combine d       _ = d

	   exprToDep e = 
	     case (findFreeVars e) of
	       []    -> DepVal Nothing e
	       (v:_) -> DepVal (Just v) e

      _ -> deps'

   size_deps = filter (\d -> sizeOrLength d && 
			     hasNonConstantExprs d) deps

   dep_binds   = nubBy (\ a b -> isVarsEq (fst a) (fst b)) $
		 concat       $ 
		 map toBinder size_deps
   dep_ptrs    = sequence       (
                 map allocPtr   $
		 nubBy (\ (DepVal (Just a) _)
		 	  (DepVal (Just b) _) -> a == b) $
		 filter isDeref $
   		 concat         $
		 map (\ (Dep _ ls) -> ls) size_deps)

   real_nm     = mkHVar i

   -- partial solution
   allocPtr (DepVal (Just v) (Unary Deref e)) =
	addCode (bind (funApply (marshallType stubMarshallInfo (lookup_ty v))
				[coreToHaskellExpr e]) (var v))
   allocPtr _ = error "MarshallDep.marshallDependent.allocPtr: unexpected value"

   isDeref (DepVal (Just _) (Unary Deref _)) = True
   isDeref _ = False

   -- convert a dependency list       
   toBinder (Dep _ ls) = mapMaybe toBinds ls
     where
      toBinds (DepVal (Just v) e@(Var _)) =
        Just ( var v, subst v len (coreToHaskellExpr e))
	where
	 len = mkLength (lookup_ty v)
         
      toBinds (DepVal (Just v) e) = 
         Just (var v, subst v' len (coreToHaskellExpr (solve v (Var v') e)))
         where
	  v'  = v ++ "'"
          len = mkLength (lookup_ty v)
         
      toBinds _ = Nothing

   mkLength to_ty = 
      -- if the external function expects a [unique] pointer to the
      -- value holding the length, wrap a Just around the length of the list.
     case to_ty of
        Pointer Unique _ _ -> just length_expr
	_		   -> length_expr
    where
      length_expr = 
	  coerceTy intTy (removePtrs to_ty) $
          case removeNames ty of 
	      -- if the depender is a wide string, use appropriate
	      -- length function.
	      -- ToDo: add a "Type -> QualName" function which
	      --  returns the name of the length computing function
	      --  to use for the given Core type.
	    WString{} -> funApp lengthWString [var nm]
	    _         -> funApp lengthName    [var nm]



\end{code}

\begin{code}
unmarshallDependents :: Bool  -- working inside a struct/union?
		     -> Bool  -- dealing with [out] parameters?
	             -> DependInfo
		     -> (Name -> Type)
		     -> Mm ()
unmarshallDependents inStruct is_out ls lookup_ty = do
 marshall_dependees
 sequence (map unmarshallDep ls)
 return ()
  where
   deps = concat (map snd ls)

    {-
     Marshal the field members/parameters. A field/param
     is either a depender or a dependee (never both.)
    -}
   unmarshallDep (_, [])    = return ()   -- no dependencies on this one, just continue.
   unmarshallDep (i, deps1) =
       case (findPtrType True (idAttributes i)) Void of
         Pointer Ptr _ _ -> return ()
	 _               -> unmarshallDependent inStruct is_out i lookup_ty deps1
			       -- split off into separate function for clarity.

   marshall_dependees = sequence (map toBinds code)

   code = nubBy theSame  $
	  concatMap (\ (Dep _ ds) -> filter nonConstantDep ds) $
	  filter (\d -> sizeOrLength d && 
	  		hasNonConstantExprs d &&
			not (isResult d)) deps

    -- HACK, need to take into account scoping, so that we don't
    -- run the risk of not unmarshalling dependent arguments named
    -- "result"!
   isResult (Dep _ xs) = any isRes xs
     where
      isRes (DepVal (Just "result") _) = True
      isRes _			       = False

   -- the actions we're creating here are only responsible for fishing
   -- out values from pointers, so we only need to do this once.
   theSame (DepVal (Just a) _) (DepVal (Just b) _) = a == b
   theSame _ _ = False 

   nonConstantDep (DepVal (Just _) _) = True
   nonConstantDep _		      = False

   toBinds (DepVal (Just v) (Var _))
       = let v'   = v
	     ty   = lookup_ty v'
         in
         addCode (bind (funApply (unmarshallType stubMarshallInfo ty) [var v']) (var v'))
   toBinds (DepVal (Just v) e) = 
         let 
             v'    = v ++ "'"
	     ty    = lookup_ty v
	 {-
	   For a case like the following: 
		    void foo([out]int *len,[out,size_is(*len+2)]char* ps[]);

	   we want to generate unmarshalling code for ps that gets at the
           value of len:
 
            len <- ((u_ref r_Int32) len)
            let len' = (len + 2)
            ps <- u_list s_Addr 0 (fromIntegral len') ....
 
	  the code below generates the first two lines, binding the value
	  read out of 
	  
	 -}    
	
         in do
         addCode (bind (funApply (unmarshallType stubMarshallInfo ty) [var v]) (var v))
	 addToEnv v v'
	 addCode (hLet (var v') (subst v (var v) (coreToHaskellExpr e)))

   toBinds _ = error "MarshallDep.unmarshallDependents.toBinds: unexpected value"

{-
 This function assumes that the dependencies are correct, that is,
 well formed, no duplicate nor conflicting attributes were used
 when the dependency information was computed.
-}
unmarshallDependent :: Bool
		    -> Bool
		    -> Id 
		    -> (Name -> Type) 
		    -> [Dependent] 
		    -> Mm ()
unmarshallDependent _ _ _ _ [Dep SwitchIs _] = return ()
unmarshallDependent inStruct is_out i lookup_ty deps' = do
 unmarsh <- unmarshallList inStruct
 			   True{-at top-level-}
                           ty 
 			   lookup_ty
                           (trans_start_posns)
			   (trans_end_posns)
			   (alloc_sizes)
 let 
      -- in the case of [out] parameters, de-reference the 
      -- the pointer to get at the goods. This is only done
      -- when the [out] parameter was (at least) a pointer to
      -- a pointer to something. If not, then the [out] pointer 
      -- points to the piece of a memory (we've already allocated)
      -- and are now ready to unmarshal.
     unmarsh' 
       | is_out && allocated_space_for = funApp r_ref [unmarsh]
       | otherwise = unmarsh

     unmarsh_and_free
       | is_out   =
	funApp doThenFree 
	       [ fromMaybe (varName trivialFree) (freeDependentE i lookup_ty deps')
	       , unmarsh'
	       ]
       | otherwise = unmarsh'

 addCode (bind (funApply unmarsh_and_free [nm_var]) nm_var)
  where
   nm	         = idName i
   tentative_ty  = lookup_ty nm
   ty        
     | should_peel = removePtr tentative_ty
     | otherwise   = tentative_ty

   nm_var        = mkHVar i

   {-
     Determine whether we had to allocate space for an [out] pointer.
     If we did, we need to deref this pointer before unmarshalling -- see above.
   -}
   allocated_space_for =
        is_out && 
	let
	 (_, _, cs1) = computeArrayConstraints False{-marshalling-} deps'
	in
	case cs1 of
	  (DepNone:_) -> True
	  _           -> False

   should_peel 
    | not is_out = False
    | otherwise  =
        case cs of
	  (DepNone:_) -> True
	  _	      -> False

{-
   -- size information may be in part be specified as part of
   -- the (array) type, i.e.,  [size_is(len,)] char arr[][20];
   -- 
   -- we push this info into the dependency list here. It really
   -- should be done as part of desugaring. (ToDo.)
   deps         = 
    case ty of
      Array _ es -> 
         case es of
	  []      -> deps'
	  [e]     ->
            case break (isSizeIs) deps' of
	     (as,[]) -> ((Dep SizeIs [exprToDep e]):deps')
	     (as,(Dep SizeIs ds):bs) -> as++(Dep SizeIs (combine ds (exprToDep e))):bs
          [e1,e2] -> [ Dep FirstIs [exprToDep e1]
	  	     , Dep LastIs  [exprToDep e2]
		     ] ++ deps'
          where

	   combine (DepNone:ds) d = d:ds
	   combine d       _ = d

	   exprToDep e = 
	     case (findFreeVars e) of
	       []    -> DepVal Nothing e
	       (v:_) -> DepVal (Just v) e

      _ -> deps'
-}

   (as, bs, cs) = computeArrayConstraints True{-unmarshalling-} deps'

   (trans_start_posns, trans_end_posns, alloc_sizes)
     | should_peel = (tail as, tail bs, tail cs) -- peel off the toplevel pointer for [out] params
     | otherwise   = (as, bs, cs)

\end{code}

\begin{code}
marshallList :: Bool
	     -> Bool
             -> Type
	     -> (Name -> Type)
	     -> Haskell.Expr
	     -> [DepVal]{-start index of transmits, one for each dim.-} 
	     -> [DepVal]{-end index of transmits-}
	     -> [DepVal]{-size to allocate (for each dimension)-}
	     -> Haskell.Expr
marshallList inStruct topLev ty _ _ [] [] [] = marshallElts inStruct topLev True ty
marshallList inStruct topLev ty lookup_ty marshaller
	     (_:starts) (_:ends) (_:sz_allocs)
 | (isPointerTy r_ty && not (isVoidPointerTy r_ty)) || isArrayTy r_ty  =
   funApply marshaller
            [ szType ty'
            , marshallList inStruct False ty' lookup_ty ref_marshaller starts ends sz_allocs
	    ]
 | otherwise	= marshallElts inStruct topLev True ty
  where
   r_ty = removeNames ty
   ref_marshaller = funApp w_list [varName alloc_list]
   
   alloc_list 
      | isStringTy ty'  || 
        isPointerTy ty' || 
	isArrayTy ty'	    = true
      | otherwise	    = false

   ty'    = removePtrAndArray r_ty

marshallList _ _ _ _ _ _ _ _ = error "MarshallDep.marshallList: the impossible happened"

unmarshallList :: Bool
	       -> Bool
               -> Type
	       -> (Name -> Type)
	       -> [DepVal]{-start index of transmits, one for each dim.-} 
	       -> [DepVal]{-end index of transmits-}
	       -> [DepVal]{-size to allocate (for each dimension)-}
	       -> Mm Haskell.Expr
unmarshallList inStruct topLev ty _ [] [] []  = return (marshallElts inStruct topLev False ty)
unmarshallList inStruct topLev ty l_ty
               (_:starts) (_:ends) (sz:sz_allocs)
 | (isPointerTy r_ty && not (isVoidPointerTy r_ty)) ||
   isArrayTy r_ty  = do
     rest <- unmarshallList inStruct False ty' l_ty starts ends sz_allocs
     len  <- mkLengthExpr sz l_ty
     return (funApp u_list [ szType ty'
			   , var "0"
			   , len
			   , rest
			   ])

 | otherwise	= return (marshallElts inStruct topLev False ty)
 where
   r_ty   = removeNames ty

   ty'    = removePtrAndArray r_ty
unmarshallList _ _ _ _ _ _ _ = error "MarshallDep.unmarshallList: the impossible happened"

mkLengthExpr :: DepVal -> (Name -> Type) -> Mm Haskell.Expr
mkLengthExpr sz lookup_ty = 
 case sz of
   DepNone           -> return nothing
   DepVal Nothing e  -> return (coerceTy intTy word32Ty (coreToHaskellExpr e))
   DepVal (Just v) e -> do
	 mb_nm <- lookupName v
	 mNm   <- getMethodName
	 let 
	   nm  = 
	     case mb_nm of
	       Nothing 
	         | v == "result" && isJust mNm -> 
		 	outPrefix ++ fromJust mNm
	         | otherwise ->
		        error ("MarshallDep.mkLengthExpr: unbound variable ('" ++
			       nm ++ "') encountered in length_is() attribute")
	       Just x -> x

	   ty  = lookup_ty v
	   h_e = subst v (var nm) (coreToHaskellExpr e)


	   coerce = coerceTy (removePtrs ty) word32Ty
				      
           {-
	    In the case the length is given via a [unique] pointer,
	    we will have at this stage unmarshalled it to a Maybe value.
	    Convert the Maybe value into a length here.
	   -}
	 case ty of
	   Pointer Unique _ _ -> return (
				funApp fromMaybeName
				       [ var "0"
				       , funApp mapName [lam [patVar "x"] 
				       			     (coerce (var "x")), h_e]
				       ])
	   Pointer Ptr _ _  -> error "mkLengthExpr: Ptr - no can do."
	   _		    -> return (coerce h_e)

marshallElts :: Bool -> Bool -> Bool -> Type -> Haskell.Expr
marshallElts inStruct topLev marshalling ty
   | marshalling && topLev = marshallType      mInfo ty
   | marshalling           = refMarshallType   mInfo ty
   | topLev                = unmarshallType    mInfo ty
   | otherwise		   = refUnmarshallType mInfo ty
 where
  mInfo = stubMarshallInfo{forStruct=inStruct,forRef=True}
\end{code}

When freeing up values that have been classified and marshalled as
dependent [in] params, we need to make sure we free the entire structure
that has been previously allocated. @freeDependent@ takes care of this,
by, in effect, by reconstructing what kind of pointer / array value that
the 'dependent arg' marshaller previously constructed.

\begin{code}
freeDependent :: Id -> (Name -> Type) -> [Dependent] -> Mm ()
freeDependent i lookup_ty deps = 
   case freeDependentE i lookup_ty deps of
     Nothing -> return ()
     Just f  -> addCode (bind_ (funApply f [real_nm]))
 where
  real_nm   = mkHVar i

freeDependentE :: Id -> (Name -> Type) -> [Dependent] -> Maybe Haskell.Expr
freeDependentE i lookup_ty deps = free_list
 where
  free_list = freeList ty lookup_ty trans_start_posns trans_end_posns alloc_sizes
  (trans_start_posns, trans_end_posns, alloc_sizes)
            = computeArrayConstraints False{-not unmarshaling-} deps

  ty = lookup_ty (idName i)


freeList :: Type
	 -> (Name -> Type)
	 -> [DepVal]{-start index of transmits, one for each dim.-} 
	 -> [DepVal]{-end index of transmits-}
	 -> [DepVal]{-size to allocate (for each dimension)-}
	 -> Maybe Haskell.Expr
freeList ty _  []    []    []    = freeElts (removePtrAndArray ty)
freeList ty _  (_:_) (_:_) (_:_)
 | (isPointerTy ty || isArrayTy ty) && needsFreeing ty' =
   Just $ (varName free)
{-
   funApp f_list
          [ szType ty'
	  , length_of sz
          , fromMaybe (varName trivialFree) (freeList ty' lookup_ty starts ends sz_allocs)
	  ]
-}
 | otherwise = freeElts ty
  where
   ty'       = removePtrAndArray ty

{-
   length_of (DepVal Nothing  e) = coreToHaskellExpr e
   length_of (DepVal (Just v) e) =
	coerceTy (removePtrs (lookup_ty v)) word32Ty (coreToHaskellExpr e)
-}

freeList _ _ _ _ _ = error "MarshallDep.freeList: the impossible happened"

freeElts :: Type -> Maybe Haskell.Expr
freeElts ty =
  case ty of
   Sequence{}  -> Just $ varName free 
      --Just $ funApp f_list [ szType t, freeElts' t ] -- wrong.
   Fixed{}          -> error "not implemented yet."
   SafeArray t      -> Just $ funApp f_list [ szType t, freeElts' t]
   Array Void (d:_) -> Just $ funApp f_list [ szType (Pointer Ptr True Void)
					    , coreToHaskellExpr d
					    , mkEltFreer (Pointer Ptr True Void)
					    ]
   Array t (d:_) -> Just $ funApp f_list   [ szType t, coreToHaskellExpr d, freeElts' t]
   String{}	 -> Just $ varName f_string
   WString{}	 -> Just $ varName f_wstring
   Pointer _ _ Iface{}    -> Nothing
   Pointer _ _ Void       -> Just $ varName free
   Pointer Ref _ (Char _) -> Just $ varName f_string
   Pointer pt _ pty
      | pt == Ref    -> Just $ funApp f_ref    [ freeElts' pty ]
      | pt == Unique -> Just $ funApp f_unique [ freeElts' pty ]
      | otherwise    -> Just $ varName f_ptr 
   _	| needsFreeing ty -> Just (mkEltFreer ty)
        | otherwise	  -> Nothing
  where
   freeElts' t   = fromMaybe (varName trivialFree) (freeElts t)

   mkEltFreer ety = varName (mkMarshaller freePrefix ety)

\end{code}



Constants referring to library marshallers:

\begin{code}
m_list, w_list, u_list, f_list :: QualName
m_list   = prefix marshallPrefix   (mkQVarName hdirectLib list)
w_list   = prefix marshallRefPrefix   (mkQVarName hdirectLib list)
u_list   = prefix unmarshallPrefix (mkQVarName hdirectLib list)
f_list   = prefix freePrefix (mkQVarName hdirectLib list)

{-
r_list :: QualName
r_list   = prefix unmarshallRefPrefix (mkQVarName hdirectLib list)
-}

f_string :: QualName
f_string = prefix freePrefix (mkQVarName hdirectLib stringName)

f_unique, r_ref, f_ref, f_wstring :: QualName
f_unique = prefix freePrefix   (mkQVarName hdirectLib unique)
r_ref    = prefix unmarshallRefPrefix (mkQVarName hdirectLib ref)
f_ref    = prefix freePrefix   (mkQVarName hdirectLib ref)
f_wstring = prefix freePrefix   (mkQVarName comLib wstring)

f_ptr :: QualName
f_ptr  = free
\end{code}

