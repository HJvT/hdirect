%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Nov. 24th 2003  10:04  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

The marshalling of an IDL method

\begin{code}
module MarshallMethod 
	( cgMethod
	, cgProperty

        , findParamDependents
	, removeDependers
	, removeDependees
	, removeDependents
	, allocateOutParams
	, marshallParams
	, freeInParamStorage
	, primDecl
	, mkResult
	) where

import BasicTypes
import Literal    ( iLit, Literal(..), IntegerLit(..) )
import AbstractH  ( HDecl )
import qualified AbstractH as Haskell ( Expr, Type )
import NativeInfo ( lONG_SIZE )
import AbsHUtils
import Attribute
import Env	 ( lookupEnv, replaceElt )
import Opts      ( optKeepHRESULT, optUseDispIDs,
		   optCoalesceIsomorphicMethods, optPrefixIfaceName,
		   optSubtypedInterfacePointers, 
		   optHaskellToC, optIgnoreHiddenMeths, optIgnoreRestrictedMeths,
		   optGenDefs, optExplicitIPointer, optHugs, optDualVtbl,
		   optLongLongIsInteger, optCorba, optNoShareFIDs, optCom
		 )
import CoreIDL
import CoreUtils ( DependInfo, DepVal(..),
		   findParamTy, findParam, isSimpleTy, addrTy,
		   mkHaskellVarName, mkHaskellTyConName, mkIfaceTypeName,
		   removePtrAndArray, computeArrayConstraints, removePtrAll,
		   isVoidTy, isPtrPointerTy, resultParam,
		   lookupDepender, mkParam, toCType, keepValueAsPointer,
		   isHRESULT, removePtr, sizeAndAlignModulus,
		   binParams, tyFun, word32Ty, iUnknownTy,
		   iPointerParam, isIntegerTy, isIfaceTy
		 )
import CgMonad
import MarshallMonad
import MarshallType  ( marshallType
		     , unmarshallType
		     , refUnmarshallType
		     , allocPointerTo
		     , coreToHaskellExpr
		     , mbFreeType 
		     , szType
		     , coerceTy
		     , coerceToInt
		     )
import MarshallCore  ( toHaskellTy
		     , paramToHaskellType
		     , toHaskellBaseTy
		     , toHaskellBaseMethodTy
		     , autoTypeToHaskellTy
		     , constrainIIDParams
		     )
import MarshallDep
import MarshallUtils
import MarshallAuto
import LibUtils ( comLib
		, iDispatch
		, iUnknown
		, ioExts
		, outPrefix
		, autoLib
		, allocBytes
		, fromIntegralName
		, fromMaybeName
		, mapName
		, mkPrimitiveName
		, mkPrimExportName
		, invokeAndCheck
		, primInvokeIt
		, invokeAndCheck
		, invokeIt
		, marshallPrefix
		, withForeignPtrName
		, check2HR
		, checkHR
		)

import Data.Maybe    ( fromMaybe, isJust, fromJust )
import Control.Monad ( when, mplus )
import Utils         ( concMaybe )

\end{code}

\begin{code}
cgMethod :: Id 
	 -> CallConv 
	 -> Result 
	 -> [Param] 
	 -> Maybe Int 
	 -> Maybe Name  -- name which implements the external call.
	 -> CgM HDecl
cgMethod i cconv result params offs mb_prim =
  -- fetch some state out of the monad and go..
 getDeclName $ \ mname -> do 
 dname     <- getDllName 
 iface     <- getIfaceName
 objFlag   <- getInterfaceFlag
 forClient <- getClientFlag
 isIEnum   <- getIEnumFlag
 methNo    <- getMethodNumber offs
 inh       <- getIfaceInherit
 hasIso    <- 
    (if (not optCoalesceIsomorphicMethods)
      then return Nothing
      else isIsomorphicMethod (idOrigName i) result params)
 let
        isObj      = objFlag /= StdFFI
        isAuto     = 
	   case objFlag of
	     ComIDispatch isDual ->
 	         not isDual || (not optDualVtbl && permissibleAutoSig result params)
	     _ -> False
        isServer   = 
		not forClient &&
		not (attrs `hasAttributeWithName` "source")

 case hasIso of
   Just False  -- already generated code for isomorphic method, so 
	       -- don't generate code for this one.
    | optCoalesceIsomorphicMethods -> return emptyDecl
   _ 
    | isHidden -> return emptyDecl -- ToDo: support this for binary interfaces??
    | otherwise -> do
      {- methods that are marked with call_as() are only
         used when generating proxy/stub code for remoting.
	 We use them when in 'COM mode' too, since these methods
	 have at times attributes that are more helpful to us.
	 [No, afraid not - the signature of the remoting 
	  method does not have to be isomorphic to the local
	  method. See comment in the desugaring code.]

	 In non-COM mode, call_as() is used as a means to
	 avoid having to have a 1-1 mapping between Haskell
	 function stub names and the name of the external
	 entry point to invoke.
      -}
    let
    	meth_i	    = i
        meth_nm	    = ieValue (mkHaskellVarName (idName meth_i))
	export_decl = addExport meth_nm
	
	enum_meth_ok = isIEnum && not isServer && isIEnumOK

    export_decl
    (flg, prim_decl, mb_prim2) <- primDecl isObj isServer (not enum_meth_ok && not isAuto)
    					   i dname mname cconv r_ty params
    let decl'       = mkMethod iface mname inh hasIso objFlag enum_meth_ok isServer
    				     mb_prim mb_prim2
				     cconv methNo meth_i result params
        decl        = helpString `andDecl` decl'
    case (isAuto || enum_meth_ok || isJust mb_prim) of
      True  -> return decl  -- the primitives are elsewhere!
      False -> do
	   needStubs flg
	   hasPrims
	   return ( decl `andDecl` prim_decl )
  where
   r_ty	       = resultType result
   attrs       = idAttributes i

   isHidden =
     attrs `hasAttributeWithName` "ignore" || 
     (( optIgnoreHiddenMeths || optIgnoreRestrictedMeths ) &&
       attrs `hasAttributeWithNames` ["hidden", "restricted"])
   
   helpString = helpStringComment i

   {-
    Some typelibs are in such a pitiful state, so we have to make
    sure that the enum method is in a good enough shape. 
    
   -}
   isIEnumOK    = 
	case idName i of
	_:'e':'x':'t':_ -> right_shape
	_:'e':'m':'o':'t':'e':'N':'e':'x':'t':_ -> right_shape
	_ -> True
    where
     right_shape = [In,Out,Out] == map (paramMode) params


     
\end{code}

The function that does the real work of generating a stub. It is
currently a mess, and is scheduled for a cleanup/rewrite Real Soon.

\begin{code}
mkMethod :: Name        -- interface the method belongs to
         -> String      -- it's module
	 -> [QualName]  -- interface inheritance (name) chain
	 -> Maybe Bool
	 -> IfaceType
	 -> Bool
	 -> Bool
	 -> Maybe Name
	 -> Maybe Name
	 -> CallConv
	 -> Int          -- method number (in vtbl)
	 -> Id           -- method Id / name.
	 -> Result
	 -> [Param]
	 -> HDecl
mkMethod iface mname inh hasIso objFlag isIEnum isServer
	 mb_prim mb_prim_nm cconv methNo methId result params 
  | isAuto     = auto_tysig `andDecl` auto_def
  | isIEnum    = enum_tysig `andDecl` enum_def
  | otherwise  = m_tysig    `andDecl` m_def
      
  where
   r_ty	    = resultType result
   isObj    = objFlag /= StdFFI
   (isAuto, isDual) =
     case objFlag of
       ComIDispatch isD -> (not isD || (not optDualVtbl && permissibleAutoSig result params), isD)
       _	        -> (False, False)

   m_name      = mkHaskellVarName name
   name        = idName methId

   m_tysig     = mkTypeSig m_name in_tys result_ty
   m_def       = funDef m_name in_pats m_rhs

   auto_tysig  = mkTypeSig m_name
			   in_tys
   			   (funTy i_pointer_ty (returnType (tuple res_ty)))

   enum_def    = funDef m_name in_pats enum_rhs
   enum_tysig  = mkTypeSig m_name enum_type_args enum_type_res

   enum_elt_ty = toHaskellTy True (removePtrAndArray (paramType (head results)))

   (enum_type_args, enum_type_res) =
     let
       (res_type:is) = (result_ty:in_tys)
--OLD: (r_ty:is) = relabelTypes (result_ty:in_tys)
     in
       {- Bad boy, go sit in the corner. -}
     case name of
	_:'e':'x':'t':_ -> (is, funTy i_pointer_ty (io (tyList enum_elt_ty)))
	_:'e':'m':'o':'t':'e':'N':'e':'x':'t':_ -> 
                           (is, funTy i_pointer_ty (io (tyList enum_elt_ty)))
	_ -> (is, res_type)

      
   enum_rhs    = funApp enum_fun args
     where
       enum_fun  = mkQVarName comLib ("enum" ++ enumName)
       
	{-
	  When MIDL generates a typelib for the following
	  
	     [object,...]
	     interface IA : IUnknown {
		[local]
	        HRESULT f ();
		[call_as(f)]
		HRESULT remoteF();
	     };

	  The type library gets a method named "remoteF". I'm not
	  sure that this is right, but what can you do?
	-}
       enumName =
         case idOrigName methId of
	   _:'e':'m':'o':'t':'e':'N':'e':'x':'t':_ -> "Next"
	   n -> n

	-- peel off two levels of indirection (one for the [out] pointer)
       elt_ty    = removePtr (paramType (head results))
       sz_of     = szType elt_ty
       write_elt = refUnmarshallType stubMarshallInfo elt_ty
       args      = 
	    -- too beautiful, man..
         case name of
	   _:'e':'x':'t':_ -> sz_of : write_elt : the_args
	   _:'e':'m':'o':'t':'e':'N':'e':'x':'t':_ -> sz_of : write_elt : the_args
	   _ -> the_args

       the_args = 
          case (map (mkHVar.paramId) meth_params) of
	    []    -> []
	    [x]    -> [x]
		-- the library impl of Skip() and Next() expects a Word32 as first
		-- arg, so make sure that's the case here.
	    (x:xs) -> coerceTy (paramType (head meth_params)) word32Ty x : xs

   auto_def    = funDef m_name in_pats auto_rhs
   auto_rhs    = funApp call_wrapper the_args
     where
       fun_id
         | with_dispids = integerLit dispid
	 | otherwise    = stringLit (idOrigName methId)

       with_dispids = optUseDispIDs && has_dispid
       call_wrapper = classifyCall methId with_dispids params result

       the_args
         | optExplicitIPointer = args ++ [var "iptr"]
	 | otherwise           = args

       args = fun_id :
              (hList (map marshallVariantParam ins)):
              map unmarshallVariantParam  (results)

       has_dispid    = isJust mb_dispid
       mb_dispid     = getDispIdAttribute (idAttributes methId)
       (Just dispid) = mb_dispid

   (pars, ins,outs,inouts,res) = binParams params

   param_names = map (idName.paramId) params

   returnType t
     | isPure    = t
     | otherwise = io t

   isPure = (idAttributes methId) `hasAttributeWithName` "pure"

   meth_params = 
     case mb_prim of
       Nothing -> meth_params'
       Just _  -> prim_param:meth_params'

    -- extend the param list if the 'result' parameter, so that
    -- we can use the result in dependent argument expression (
    -- e.g., [out,length_is(result)]int* f, ...
   params_and_result 
     = params ++ [resultParam (resultType result)]

   (prim_params, meth_params', result_ty)
    | isAuto
                  = ( []
		    , real_params ++ (if optExplicitIPointer then [iptr_param] else [])
		    , funTy i_pointer_ty (returnType (tuple res_ty))
		    )
    | isObj || isIEnum
	          = ( mptr_param:iptr_param:params
		    , real_params ++ [iptr_param]
		    , funTy i_pointer_ty (returnType (tuple res_ty))
		    )
    | otherwise = 
                  ( params
		  , real_params
		  , returnType (tuple res_ty)
		  )

	 
   i_pointer_ty
     | optSubtypedInterfacePointers && not isIsoMethod
     = tyCon (mkHaskellTyConName (mkIfaceTypeName iface)) [tyVar "a"]

     | (isAuto || isDual) && isIsoMethod && optSubtypedInterfacePointers
     = mkTyCon iDispatch [tyVar "a"]

     | isIsoMethod && optSubtypedInterfacePointers
     = mkTyCon iUnknown [tyVar "a"]

     | isAuto && isIsoMethod
     = mkTyConst iDispatch

     | optSubtypedInterfacePointers
     = tyCon (mkHaskellTyConName (mkIfaceTypeName iface)) [tyVar "a"]

     | otherwise
     = tyConst (mkHaskellTyConName iface)

   isIsoMethod = fromMaybe False hasIso

   -- building the Haskell type of the method/function.
   (in_tys_1, res_ty) = 
      constrainIIDParams
		  (paramToHaskellType par_deps isServer isAuto False)
                  (paramToHaskellType res_deps isServer isAuto True)
		  real_params
		  results
{-
   res_ty = map (paramToHaskellType res_deps isServer isAuto True) results
-}
   in_tys = 
	     -- if the primitive method is passed in as arg, prefix it
	     -- to the parameter list.
           (if isJust mb_prim then 
	         ((toHaskellBaseMethodTy False prim_params result):)
	      else
	         id) in_tys_1
--	   map (paramToHaskellType par_deps isServer isAuto False) real_params

   in_p_tys = map (\ p -> ( idName (paramId p)
   			  , toParamPrimTy isServer p
			  )
			  ) prim_params

   iptr_param       = iPointerParam iface
   mptr_param       = mkParam "methPtr" In
                               (Pointer Ptr True Void)
   prim_param       = mkParam (fromJust mb_prim) In
                               (tyFun cconv result prim_params)

   in_pats     = map (varPat.mkHVar.paramId) meth_params

   meth_result = mkResult results

   unsafeWrap e
      | isPure    = funApp (mkQVarName ioExts "unsafePerformIO") [e]
      | otherwise = e

   m_rhs       =
    unsafeWrap $      
      runMm (Just (mkHaskellVarName (idName methId))) param_names meth_result $ do
          marshallDependents False{-not inside struct-} False{-not for server proxies-}
	  		     par_deps (findParamTy params_and_result) -- in and in-out params
          allocateOutParams (raw_inout_deps++raw_out_deps) 
			    (findParam params_and_result)
			    (removeDependees raw_inout_deps outs)
          marshallParams True{-marshall-} False isServer (removeDependents in_deps real_ins)
          marshallParams True{-marshall-} False isServer (removeDependers raw_inout_deps raw_inouts)
          setupMethodCall isObj methNo iface inh result in_p_tys
	  		  (thePrimCall isObj mname mb_prim mb_prim_nm methId result prim_params)
	  freeInParamStorage in_deps ins
          okResult isObj methId result 
          unmarshallOutParams isServer (removeDependers (out_deps ++ inout_deps) real_outs)
	  when (not ignoreResult)
	       (unmarshallResult isServer methId{idName=outPrefix++idName methId} r_ty)
          marshallParams False False isServer (removeDependers (inout_deps++out_deps) real_inouts)
          unmarshallDependents False True out_deps    (findParamTy params_and_result)
          unmarshallDependents False False inout_deps (findParamTy params_and_result)

   (results, ignoreResult) =
      let results' 
	    | isAuto    = (outs ++ inouts)
	    | otherwise = real_res
      in	   
      case r_ty of
        Void -> (results', True)
	_ 
	  | (isHRESULT result && not optKeepHRESULT) ||
	    ((idAttributes methId) `hasAttributeWithName` "hs_ignore_result")
	     -> (results', True)
	  | otherwise -> (results' ++ [res_param], isSimpleTy r_ty && not (isIfaceTy r_ty))

   res_param = 
      let p = mkParam (outPrefix ++ name) Out r_ty in
      p{ paramOrigType=resultOrigType result
       , paramId=(paramId p){idAttributes=idAttributes methId}
       }  -- replace attributes.
      
   (real_params, par_deps)    = findParamDependents True pars
   (_, in_deps)               = findParamDependents True ins
   (real_ins, _)             = findParamDependents False ins
     -- For the [out] params, there's a wondrous special case:
     --   [out,size_is(x),length_is(*px)]void* pv
     --
     -- Here we don't want to unmarshall pv into a list (it's
     -- really a chunk of mem), but want to consider pv to
     -- be a dependent argument for the purposes of allocating
     -- a chunk of memory to pass in. So, use a pair of DependInfos,
     -- one to use for marshalling ('out_deps') and another
     -- to use when allocating out-param storage ('raw_out_deps.')
   (real_outs', out_deps)    = findParamDependents True outs
   (_, raw_out_deps)         = findParamDependents False outs
   real_outs                 = filter (not.(`hasAttributeWithName` "ptr").idAttributes.paramId) real_outs'
   (real_inouts, inout_deps) = findParamDependents False inouts
   (raw_inouts, raw_inout_deps) = findParamDependents True inouts
   (real_res, res_deps)         = findParamDependents True res
\end{code}

Generating code for dispinterface properties is real straightforward,


\begin{code}
cgProperty :: Id -> Type -> Id -> Id -> CgM HDecl
cgProperty i ty seti geti = do
  if_name <- getIfaceName
  let
   prop_iface_ty
     | optSubtypedInterfacePointers = tyCon if_name [tyVar "a"]
     | otherwise		    = tyConst if_name

   get_prop_tysig = mkTypeSig get_prop_name
			      [prop_iface_ty]
   			      (io (prop_ty Out))

   set_prop_tysig = mkTypeSig set_prop_name
			      [prop_ty In, prop_iface_ty]
   			      (io tyUnit)

   prop_ty kind = autoTypeToHaskellTy kind ty

   get_prop_decl  = funDef get_prop_name [] get_prop_rhs
   set_prop_decl  = funDef set_prop_name [varPat (var "prop")] set_prop_rhs

   get_prop_name  = if_prefix ++ idName geti
   set_prop_name  = if_prefix ++ idName seti

   if_prefix
     | optPrefixIfaceName = mkHaskellVarName (mkIfaceTypeName if_name) ++ "_"
     | otherwise	  = ""

   get_prop_rhs   = funApp getProp [ prop_id, hList []
			           , marshallVariant "out" ty
			           ]
   set_prop_rhs   = funApp setProp [ prop_id
				   , hList [funApply (marshallVariant "in" ty) [var "prop"]]
			           ]
   prop_id 
    | optUseDispIDs && has_dispid = integerLit d_id
    | otherwise			  = stringLit (idName i)

   setProp  = mkQVarName autoLib ("propertySet" ++ dispid)
   getProp  = mkQVarName autoLib ("propertyGet" ++ dispid)

   has_dispid  = isJust mb_dispid
   mb_dispid   = getDispIdAttribute (idAttributes i)
   (Just d_id) = mb_dispid

   dispid
    | optUseDispIDs = "ID"
    | otherwise     = ""

  return ( get_prop_tysig `andDecl`
           get_prop_decl  `andDecl`
	   set_prop_tysig `andDecl`
           set_prop_decl
	 )

\end{code}


\begin{code}
marshallParams :: Bool -> Bool -> Bool -> [Param] -> Mm ()
marshallParams marsh don'tFree isServer ps = do
   sequence (map marshallParam ps)
   return ()
  where
   marshallParam p
    |  isVoidTy ty
    || isSimpleTy ty 
    || keepValueAsPointer ty -- keep pointers to complex types
			     -- external.
    = return ()

    | otherwise  = 
	let  nm   = idName (paramId p)
	     nm'  = "in__" ++ nm
	     pats
	      | isIntegerTy ty = tuplePat [patVar (nm ++ "_hi"), patVar (nm ++ "_lo")]
	      | otherwise      = patVar nm
	     pats' 
	      | isIntegerTy ty = tuplePat [patVar (nm' ++ "_hi"), patVar (nm' ++ "_lo")]
	      | otherwise      = patVar nm'
	in
	  -- The hack to prefix the value with res__ in the proxy/server case 
	  -- requires that the same thing is done in MarshallServ.marshallMethod.meth_result
	  --
	  -- Ditto for in__ prefixing the result of unmarshaling proxy args; 
	  -- MarshallServ.marshallMethod needs to be in on this (less-than-tasteful) game.
	if isServer 
	 then if marsh
	       then let res__nm = var ("res__" ++ nm) in
	            if (paramMode p == InOut)
	             then addCode (bind_ (funApply (marshaller p ty) [ var nm, res__nm ]))
		     else addCode (bind  (funApply (marshaller p ty) [res__nm]) res__nm)
	       else if (paramMode p == InOut)
	             then addCode (genBind (funApply (marshaller p ty) [var nm]) pats')
		     else addCode (genBind (funApply (marshaller p ty) [var nm]) pats)
         else addCode (genBind (funApply (marshaller p ty) [var nm]) pats)
      where
       ty = paramType p

   marshaller p
    | marsh     = marshallType   stubMarshallInfo{forInOut=(paramMode p == InOut), forProxy=isServer}
    | otherwise = unmarshallType stubMarshallInfo{ forInOut=(paramMode p == InOut)
    						 , doFree=not (don'tFree || don'tFree_t)
						 , forProxy=isServer
						 }
           where
	     don'tFree_t = (idAttributes (paramId p)) `hasAttributeWithName` "nofree"


unmarshallOutParams :: Bool -> [Param] -> Mm ()
unmarshallOutParams isServer ls = do
   sequence (map (unmarshallOutParam isServer) ls)
   return ()

unmarshallOutParam :: Bool -> Param -> Mm ()
unmarshallOutParam isServer p
 | isVoidTy ty           || 
   keepValueAsPointer ty ||
   isPtrPointerTy ty 

{- doesn't make sense.
   (optHaskellToC   &&
    isAbstractTy ty &&
    not (isAbstractFinalTy ty))
-}
    = return ()

 | otherwise =
   addCode (bind (funApply (unmarshallType stubMarshallInfo{ forInOut=(paramMode p == InOut)
   							   , doFree=not don'tFree
							   , forProxy=isServer}
					   ty) [nm]) nm)
 where
  ty = paramType p
  nm = var (idName (paramId p))
  don'tFree = (idAttributes (paramId p)) `hasAttributeWithName` "nofree"


unmarshallResult :: Bool -> Id -> Type -> Mm ()
unmarshallResult _ _ Void  = return ()
unmarshallResult isServer i   ty  = 
   addCode (bind (funApply (unmarshallType stubMarshallInfo{doFree= not don'tFree, forProxy=isServer} ty) [nm]) nm)
   where
     nm		= var (idName i)
     don'tFree  = (idAttributes i) `hasAttributeWithName` "nofree"

allocateOutParams :: DependInfo -> (Name -> Param) -> [Param] -> Mm ()
allocateOutParams deps lookup_param params = do
  sequence (map allocate params)
  return ()
 where
   allocate p
     | isVoidTy ty = return ()
     | otherwise   = addCode (bind allocOut (var nm))
    where
     i	      = paramId p
     ty	      = paramType p
     ty'      = removePtrAll ty
     nm       = idName i

     allocOut =
       case (lookupDepender deps i) of
         Nothing -> allocPointerTo ty
	 Just ls -> 
	 	let
		 (_,_,sz_allocs) = computeArrayConstraints False{-not unmarshaling-} ls
		in
	         -- allocate space big enough to hold what function is going
		 -- to return back, i.e.,
                 --        [out]int*    -> allocBytes s[Int32]  (returning pointer to it.)
		 --        [out]int**   -> allocBytes s[t*]
		 --
		 -- Note: if the [out] param is a constructed type, we don't need to 
		 -- to allocate space for the objects pointed to by any embedded pointers
		 -- (that's the task of the callee.)
		 --  [ No need to plug the pointers with NULLs either, AFAIK. -- sof 5/98 ]
		 --		    
		 -- 
		 -- ToDo: assert that the ty is of a pointer or array nature.
		case sz_allocs of
		  []			       -> allocPointerTo ty
		  (DepNone:_)		       -> allocPointerTo ty
		    -- The next case is wrong, even if we've got
		    -- [size_is(*e)] pinned onto an [out] parameter,
		    -- we'll need to allocate enough space to hold 
		    -- (*e) elements.
		  --(DepVal _  (Unary Deref _):_) -> allocPointerTo addrTy
		  (DepVal Nothing  e:_) -> 
		  	case ty' of
			  Pointer _ _ Void -> funApp allocBytes [coerceToInt e]
			  _ ->
			    funApp allocBytes
				   [ binOp Mul
				           (funApp fromIntegralName [szType ty'])
					   (coerceToInt e)
			           ]
		  (DepVal (Just v) e:_) -> 
			    let
			     coerce = varName fromIntegralName
			     h_e    = coreToHaskellExpr e 
			    in
			    case paramType (lookup_param v) of
			      Pointer Unique _ _ -> 
					funApp allocBytes
						 [binOp Mul
						    (funApp fromIntegralName [szType ty'])
						    (funApp fromMaybeName
						            [ var "0"
						            , funApp mapName [coerce, h_e]
						            ])]
			      _ -> 
				let e' = coerceToInt e in
				case ty' of
				   Pointer _ _ Void -> funApp allocBytes [e']
				   _ ->  funApp allocBytes
					       [ binOp Mul
						       (funApp fromIntegralName [szType ty'])
						       e'
					       ]
						      

-- parameter list has all (in)out parameters plus function result.
mkResult :: [Param] -> Haskell.Expr
mkResult ps =
  case ps of
    [p] | isVoidTy (paramType p) -> ret unit
        | otherwise		 -> ret (mkVal p)
    _				 -> ret (tup (map mkVal ps))
 where
  mkVal p = var (idName (paramId p))
\end{code}

Create the FFI declaration for the foreign function/object method
we're interfacing to. If it's a (COM) method, we give it two
extra Addr arguments, one contains the address of the method, the
other the interface pointer.

If we end up wanting to perform an indirect callout

\begin{code}
primDecl :: Bool
         -> Bool
	 -> Bool
	 -> Id
	 -> String
	 -> String
	 -> CallConv
	 -> Type
	 -> [Param]
	 -> CgM (Bool, HDecl, Maybe Name)
primDecl isObj isServer trySharing f dname mname cc res params
 | isServer    = do
     let sig = mkTySig param_h_tys res_hs_ty
     mb_res <- lookupDynStub sig
     case mb_res of
       Just (True,r) | not optNoShareFIDs -> 
       	   return (False, emptyDecl, Just r)
       _ -> do
           when (not optNoShareFIDs) (addDynStub server_nm sig True)
           return (False, fexport cc  Nothing server_nm server_ty, Nothing)
 | not isObj   =
     return (needs_wrapper, prim cc loc_spec prim_nm prim_ty needs_wrapper c_ty_args c_res_ty, Nothing)
 | otherwise   = do
     let sig = mkTySig param_h_tys res_hs_ty
     mb_res <- lookupDynStub sig
     case mb_res of
       Just (False,r) | not optNoShareFIDs && trySharing && not has_structs -> do
	 return (False, emptyDecl, Just r)
       _      -> do
                 when (trySharing && not has_structs) (addDynStub prim_nm sig False)
       	         return ( has_structs
                        , primcst cc prim_nm prim_ty has_structs c_ty_args c_res_ty
			, Nothing
			)
 where
  nm           = idName f
  orig_nm      = idOrigName f
  attrs        = idAttributes f
  the_dname    = 
    case (findAttribute "dllname" attrs) of
     Just (Attribute _ [ParamLit (StringLit s)]) -> s
     _ -> dname
  
  loc_spec     =
     case concMaybe (findAttribute "call_as" attrs)
                    (findAttribute "entry" attrs)   of
       Nothing					   -> (the_dname, Nothing, orig_nm, Nothing)
       Just (Attribute _ [ParamVar v])             -> (the_dname, Nothing, v, Nothing)
       Just (Attribute _ [ParamLit (IntegerLit (ILit _ x))]) -> 
	    let
	     stub_nm     = mkPrimitiveName (show x)
	    in
	    (the_dname, Just x, stub_nm, sz)
       Just (Attribute _ [ParamLit (StringLit v)]) -> (the_dname, Nothing, v, Nothing)
       _ -> (the_dname, Nothing, orig_nm, Nothing)

   {-
     Hugs' primitives live in an thoroughly flat namespace - if you have got
     two primitive decls called "prim_foo", the first that's loaded, is used
     throughout. Better not let that happen here, so we prepend the module
     name.
     
     [I've submitted a fix for this for Hugs98; hopefully it will be included..]

     8/99 - add module prefix on the non-Hugs side too ; otherwise we run into trouble
            when linking object files that contain identically named stubs.
   -}
  prim_nm = mkPrimitiveName (mname ++ '_':nm)

  needs_wrapper = 
    has_structs || 
    case loc_spec of
      (_, Just _, _, _) -> True
      _		        -> False

  has_structs = any (fst) ls
  ls@(c_res_ty:c_ty_args) = map (isStruct.toCType) (res:param_tys')
     where
       isStruct (Left x)  = (False, x)
       isStruct (Right x) = (True, x)

  sz
   | not optGenDefs = Nothing
   | otherwise      = 
     case cc of
       Stdcall -> 
         let stuff    = map ((sizeAndAlignModulus Nothing).paramType) params
	     p_sz     = foldl (al_param) 0 stuff
	     al_param siz (sz_t,_) = 
		let --sz' = align sz modu  -- hmm
		    sz_t'
		      | sz_t < lONG_SIZE = lONG_SIZE -- everything that's less than word size
						     -- is rounded up to be exactly that.
		      | otherwise        = sz_t
		in (siz + sz_t')
	 in
	 Just p_sz
       _       -> Nothing
   

  server_nm    = mkPrimExportName nm

  server_ty    = 
     case generaliseTys [prim_ty] of
       ([t], mb) -> mbCtxtTyApp mb (funTy t (io tyAddr))

  prim_ty      = funTys  param_hs_tys (io res_hs_ty)

  {-
    We're using toHaskellBaseTy here to map to the *primitive*
    Haskell representation of an IDL type.
  -}
  param_hs_tys
   | isServer  = tyPtr (uniqueTyVar "a") : param_h_tys
   | isObj     = 
       if optCom || attrs `hasAttributeWithName` "finaliser" then
          tyAddr : tyAddr : param_h_tys
       else
          tyAddr : tyAddr : param_h_tys
   | otherwise = param_h_tys

  (param_h_tys, _) =
      constrainIIDParams (toPtrTy .(toParamPrimTy isServer))
                         (toPtrTy .(toParamPrimTy isServer))
			 params
			 outs

--   param_h_tys = map (toPtrTy.toParamPrimTy isServer) params

  (_, _, outs, _, _) = binParams params

  param_tys'
   | isServer  = addrTy:p_tys
   | isObj     = addrTy:iUnknownTy:p_tys
   | otherwise = p_tys
      where
       p_tys   = map paramType params

  res_hs_ty = toHaskellBaseTy True res


toParamPrimTy isServer p
  | pattrs `hasAttributeWithName` "foreign" = tyForeignObj
  | otherwise = toHaskellBaseTy (isResult || isServer) (paramType p)
  where
    pattrs   = idAttributes (paramId p)
    mode     = paramMode p
    isResult = mode == Out   || 
--	       mode == InOut ||
 	       paramDependent p

\end{code}

If we're compiling a COM method, we need to swizzle the function
pointer out of the interface pointer in order to perform the
actual call. @setMethodCall@ does this, dereferencing the i-pointer
(assume it is in scope as @iptr@), binding the function pointer
to @methPtr@.

\begin{code}
setupMethodCall :: Bool 
		-> Int 
		-> String 
		-> [QualName]
	        -> Result 
		-> [(Name, Haskell.Type)]
		-> (Haskell.Expr, Maybe Haskell.Expr) 
		-> Mm ()
setupMethodCall isObj methNo ifaceName inh result param_tys methCall
 | not isObj	     = addCode ( binder mCall )
 | isHRESULT result  = addCode (
      binder
         (funApp
	     invokeAndCheck 
	         [ lam [varPat methPtr, varPat iptr] mCall
		 , offset
		 , iptr
		 ]))
 | otherwise = addCode ( binder invokeMethod )
   where
    (mCall0, mbRes) = methCall
    mCall = unravel mCall0

    invokeMethod 
      | optHaskellToC || optCorba
      = 		funApp primInvokeIt
			       [ lam [varPat methPtr, varPat iptr] mCall
			       , offset
			       , funApp m_iptr [iptr]
			       ]
      | otherwise     = funApp invokeIt
			       [ lam [varPat methPtr, varPat iptr] mCall
			       , offset
			       , iptr
			       ]

    m_iptr = 
       case inh of
         []    -> prefix marshallPrefix (mkQVarName Nothing ifaceName)
	 (x:_) -> prefix marshallPrefix x

     -- unwrap the ForeignPtrs.
    unravel cont 
      = foldr (\ (f,_) acc -> funApp withForeignPtrName [var f, lam  [patVar f] acc])
    	      cont
	      fs
      where fs = filter (isFOTy.snd) param_tys

    binder = 
      case mbRes of
        Nothing -> bind_
	Just v  -> (\ m n -> bind m v n) -- fp; dontcha just love it!

    iptr      = var "iptr"
    methPtr   = var "methPtr"
    offset    = lit (iLit methNo)

\end{code}

Call the primitive. Assume that all arguments have been
marshalled into an appropriate form and that their marshalled
representations are in scope as the names given in the parameter list. 

\begin{code}
thePrimCall :: Bool
            -> String
	    -> Maybe Name
	    -> Maybe Name
	    -> Id
	    -> Result
	    -> [Param]
	    -> (Haskell.Expr, Maybe Haskell.Expr)
thePrimCall isComMeth mname mb_prim mb_prim_nm f res params
   | isVoidTy r_ty || (isComMeth && isHRESULT res) = (f_app, Nothing)
   | otherwise				           = (f_app, Just r_res)
 where
   f_app     = funApp (mkVarName meth_name) args
   r_ty      = resultType res
   r_res     = var (outPrefix ++ idName f)
   args      = foldr mkArg [] params

   mkArg p acc = 
     case paramType p of
       Integer LongLong _ | optHugs && optLongLongIsInteger
			  -> var (nm ++ "_lo") : var (nm ++ "_hi") : acc
       _ -> var nm  : acc
    where
     nm = idName (paramId p)

    -- see primDecl comment.
   fun_nm  = mname ++ '_':idName f

   meth_name =
     case mb_prim `mplus` mb_prim_nm of
       Nothing -> mkPrimitiveName fun_nm
       Just x  -> x

{-
 The predicate may look a bit odd - check the
 HRESULT return code if it is *not* an object
 method. The reason for this is that the HRESULT
 return code have already been checked for by
 special object method call wrappers.
 
 If the result has the attribute [usesgetlasterror],
 then in the event of error, we fish out the return
 code and msg by calling GetLastError() - Win32 specific,
 although we could give [usesgetlasterror] a valid
 interpretation on most Unices too (use errno.)

-}
okResult :: Bool -> Id -> Result -> Mm ()
okResult isObj f res
  | attrs `hasAttributeWithName` "error_handler" = 
      case findAttribute "error_handler" attrs of
        Just (Attribute _ [ParamLit (StringLit s)]) ->
	   addCode (bind_ (funApp (toQualName s) [r_res]))
	_ -> return ()
  | isObj = return ()
  | attrs `hasAttributeWithName` "usesgetlasterror" =
	addCode (bind_ (funApp check2HR [r_res]))
  | not (isHRESULT res) = return ()
  | otherwise = 
	addCode (bind_ (funApp checkHR [r_res]))
 where
  r_res = var (outPrefix ++ idName f)
  {-
   Note: for method Ids, the renaming stage has transferred
   all attributes from the result type onto the method Id, so
   we don't need to fish out the result type attributes here.
  -}
  attrs = idAttributes f

\end{code}

In order to toss some of the input parameters over the fence, the stub
function may have to marshall 'em and store them in an external, non-moveable heap.
Before returning from the stub, we make sure that any such allocations are freed
up.

We currently don't assume the presence of a marshalling arena for the
allocation of marshalled values, so we cannot free the allocated memory
in one fell swoop.

\begin{code}
freeInParamStorage :: DependInfo -> [Param] -> Mm ()
freeInParamStorage dep_info ps = do
   sequence (map freeParam ps)
   return ()
  where
   freeParam p
        -- special fall-thru case for [sequence, length_is()] - sigh.
    | isJust dep_res && not has_seq = freeDependent i (findParamTy ps) deps
    | attrs `hasAttributeWithName` "nofree" = return ()
    | otherwise	     =
       case (mbFreeType (paramType p)) of
         Nothing -> return ()
         Just e  -> addCode (bind_ (funApply e [var (idName (paramId p))]))
    where
      i		  = paramId p
      attrs       = idAttributes i
      has_seq     = hasSeqAttribute attrs
      dep_res     = lookupDepender dep_info i
      (Just deps) = dep_res
\end{code}

\begin{code}
isIsomorphicMethod :: String -> Result -> [Param] -> CgM (Maybe Bool)
isIsomorphicMethod nm res params = do
  env <- getIsoEnv 
  case lookupEnv env nm of
    Nothing   -> return Nothing
    Just alts ->
      case break (match) alts of
        (_,[]) -> return Nothing
	(as,(flg,r,ps):bs) -> do
	      setIsoEnv (replaceElt env nm ((False,r,ps):as++bs))
	      return (Just flg)
 where
  len_params = length params

  match (_,r,ps) =
    resultType r == resultType res && 
    len_params == length ps		   &&
    all (\ (p1,p2) -> paramMode p1 == paramMode p2 &&
		      paramType p1 == paramType p2)  -- ToDo: check attributes too.
	(zip ps params)

\end{code}
