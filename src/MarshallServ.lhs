%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Dec. 1st 2003  07:24  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Generating marshalling code for Haskell COM servers.

\begin{code}
module MarshallServ ( cgServMethod, mkServVTBL, mkServMain ) where

-- IDL representation: 
import CoreIDL
import Attribute
import Literal ( iLit )
import CoreUtils

-- code/defs to generate Haskell code:

import AbstractH   ( HDecl, Pat )
import PpAbstractH ( showAbstractH, ppType )
import AbsHUtils

-- marshalling code: 
import MarshallMethod
import MarshallMonad
import MarshallUtils
import MarshallType
import MarshallDep
import MarshallCore

import CgMonad

-- utility libraries:
import BasicTypes
import LibUtils
import Opts
	( optKeepHRESULT
        , optExportListWithTySig
	, optAnonTLB
	, optUseStdDispatch
	, optCom
        )

-- standard libraries:
import Monad ( when )
import Maybe

\end{code}

What it generates:

  interface IA : IU {
     HRESULT f ([in]int x);
  };

 primf :: Addr -> Int -> IO HRESULT
 primf iptr x = do
     v  <- getIfaceState iptr
     catch (f x v >> return s_OK)
	   (\ _ -> return s_FAIL)

 mkIA_vtbl init_state = ....

\begin{code}
cgServMethod :: Id
	     -> Result
	     -> [Param]
	     -> Bool
	     -> Bool
	     -> CgM HDecl
cgServMethod i result params isSource isDisper = do
 objFlag   <- getInterfaceFlag
 let
        isObj        = objFlag /= StdFFI
	isDispSource = 
	   isDisper ||
	   (isSource && 
	    case objFlag of { ComIDispatch _ -> True ; _ -> False })

 return ( helpStringComment i   `andDecl` 
	  marshallMethod i isObj isDispSource result params
        )
\end{code}

\begin{code}
marshallMethod :: Id 
	       -> Bool
	       -> Bool
	       -> Result
	       -> [Param]
	       -> HDecl
marshallMethod i isObj isDispSource result params =
   server_stub_tysig `andDecl` server_stub_def
 where
   m_name      = mkHaskellVarName (name ++ "_meth") -- increased chance of uniqueness..
   name        = idName i

   r_ty	       = resultType result

   -- The server code is split into two:
   --   * the method boilerplate (type sig and lhs of method)
   --   * the method stub that takes care of exposing the
   --     Haskell method to the outside world.
   -- 

   server_stub_tysig = genTypeSig server_stub_name meth_ctxt server_stub_type
   server_stub_type  
     | isDispSource = funTys [method_ty, tyList tyVariant, tyVar "objState"]
                           (io (tyMaybe tyVariant))
     | otherwise    = 
         case generaliseTys ((returnType stub_res_ty):stub_param_tys) of
	   ((r:ts),mbc) -> mbCtxtTyApp mbc (funTys ts r)
   server_stub_def   = funDef server_stub_name server_stub_pats server_stub_rhs
   server_stub_rhs
     | isDispSource     = stub_rhs
     | isHRESULT result = funApp returnHR [stub_rhs]
     | otherwise        = stub_rhs

   server_stub_pats 
     | isDispSource = [patVar m_name]
     | otherwise    = (patVar m_name) : in_stub_pats

   server_stub_name
     | isObj     = mkPrimitiveName  name
     | otherwise = mkWrapperName  name

   obj_state_ty   = uniqueTyVar "objState"

   (method_ty,meth_ctxt) = (funTys ps m_res_ty, ctxt)
      where
	m_res_ty
	  | isObj     = funTy obj_state_ty res
	  | otherwise = res

	(m_ty, ctxt) = toHaskellMethodTy isPure (isDispSource || isObj) False Nothing params result
        (ps,res)     = splitFunTys m_ty


   stub_param_tys = method_ty : stub_tys

   (stub_tys_1, _) = 
      constrainIIDParams (\ p -> toHaskellBaseTy True (paramType p))
      			 (\ p -> toHaskellBaseTy True (paramType p))
			 params
			 out_params

   stub_tys       = map (groundTyVars {-. (toHaskellBaseTy True)-}) stub_tys_core
   stub_tys_core
     | isObj      = toHaskellBaseTy True iUnknownTy : stub_tys_1
     | otherwise  = stub_tys_1

   stub_res_ty    = toHaskellBaseTy True r_ty
   stub_res
     | isObj            = ret unit
     | isHRESULT result = ret unit
     | otherwise        = ret meth_result_expr

   stub_rhs
     | isDispSource = dispatch_rhs
     | otherwise    = prim_stub_rhs

   prim_stub_rhs  =
      runMm (Just name) param_names stub_res $ do
          unmarshallDependents False False in_deps    (findParamTy params)
          unmarshallDependents False False inout_deps (findParamTy params)
          marshallParams False{-unmarshall-} True{-don't free-} True{-is server-} (removeDependents in_deps real_ins)
          marshallParams False  True{-don't free-} True{-is server-} (removeDependers (inout_deps++out_deps) real_inouts)
          when isObj unmarshallIfacePointer
          methCall (i{idName=m_name}) meth_result meth_params
	  when (not ignoreResult) (marshallResult i{idName=outPrefix++idName i} r_ty)
          marshallDependents True{-inside struct-} True inout_deps (findParamTy params) -- in and in-out params
          marshallParams True{-marshall-} False True{-is server-} out_params
          marshallParams True{-marshall-} False True{-is server-} (removeDependers inout_deps real_inouts)
	  marshallDependents False{-not inside struct-}
	                     True{-for a proxy-}
			     out_deps
			     (findParamTy params)
          writeOutParams (removeDependees inout_deps outs)

   dispatch_rhs = foldr unmarshallArg (apply m_name) params'
    where
      params'
       | isHRESULT result || isVoidTy (resultType result) = params
       | otherwise = params ++ [Param (mkId "the_res" "the_res" Nothing 
					    [AttrMode Out, Attribute "retval" []])
                                      Out Void Void False]

      apply nm = 
        funApp (appendStr (show out_arity) applyName)
	       ((funApply (var nm) in_args) : out_args)

      out_arity = length out_args

      out_args = 
        map toOutArgName $
	filter (\ p -> paramMode p /= In) params'
       where
         toOutArgName p =
	   var $ 
	   case (paramMode p) of
	     InOut -> "out_" ++ mkHaskellVarName (idName (paramId p))
	     _     -> mkHaskellVarName (idName (paramId p))

      in_args  = 
        map (var.mkHaskellVarName.idName.paramId) $
	filter (\ p -> paramMode p /= Out) params'

      unmarshallArg p acc = 
        let
         mode     = paramMode p
	 t        = paramType p
	 lam_pat  = mkHaskellVarName (idName (paramId p))
	 
	 (msheller, lams) =
	  case mode of
	    In    | isIUnknownTy t -> (inIUnknownArgName, [patVar lam_pat])
	    	  | otherwise      -> (inArgName, [patVar lam_pat])
	    Out
	     | isRetVal  -> (retValName, [patVar lam_pat])
	     | otherwise -> (outArgName, [patVar lam_pat])
	    InOut        -> (inoutArgName, [patVar lam_pat, patVar ("out_" ++ lam_pat)])
  
         isRetVal  = (idAttributes (paramId p)) `hasAttributeWithName` "retval"
        in
	contApply (varName msheller) (lam lams acc)

   (meth_params, prim_params)
    | isObj 
    = ( real_params ++ [obj_param]
      , iptr_param:params
      )
    | otherwise
    = ( real_params
      , params
      )

   param_names  = map (idName.paramId) params
   in_stub_pats = map mkPat prim_params
       where
         mkPat p
	   | paramMode p == Out = patVar ("out_" ++ (idName (paramId p)))
	   | otherwise          = patVar (idName (paramId p))

   iptr_param       = iPointerParam (idName i)
   obj_param        = objParam (idName i)

   (results, ignoreResult) =
      let results' = (real_outs ++ real_inouts)
      in	   
      case r_ty of
        Void -> (results', True)
	_    -> 
	  case isHRESULT result && not optKeepHRESULT of
	    True -> (results', True)
	    _    -> (results' ++ [res_param], isSimpleTy r_ty)

    -- Note: prefixing the result vals with res__ is reqd in the proxy case
    -- to distinguish it from the name for the out/in-out param (i.e., 'res__foo'
    -- rather than 'foo'.)
   (meth_result, meth_result_expr) = 
     case results of
       []  -> (Nothing, unit)
       _   -> (Just (tuplePat (map patVar res_names)), tup (map var res_names))
         where
	  res_names = map (("res__"++).idName.paramId) results

   isPure = (idAttributes i) `hasAttributeWithName` "pure"

   returnType t = io t

   res_param = 
      let p = mkParam (outPrefix ++ name) Out r_ty in
      p{ paramOrigType=resultOrigType result
       , paramId=(paramId p){idAttributes=idAttributes i}
       }  -- replace attributes.
      
   out_params = map remPtr (removeDependents out_deps real_outs)
    where
     remPtr p 
       | not (isConstructedTy (nukeNames t') && not (isEnumTy t')) &&
         not (isVariantTy t') =	
	 	case paramType p of
		   Pointer Unique isExp (Pointer _ _ x) -> p{paramType=Pointer Unique isExp x}
		   _				        -> p{paramType=t'}
       | otherwise	      = p
      where
       t' = removePtr (paramType p)

   (pars, ins,outs,inouts,_) = binParams params
   (real_params', _)         = findParamDependents False pars
   (real_ins, in_deps)       = findParamDependents False ins
   (real_outs, out_deps)     = findParamDependents False outs
   (real_inouts, inout_deps) = findParamDependents False inouts

   real_params		     = map jiggleInOut real_params'
   
    -- the unmarshaling of inout params leaves the results bound to p__in; hence,
    -- we need to append "__in" to the method parameters here.
   jiggleInOut p 
     | paramMode p == InOut  = p{paramId=(paramId p){idName= "in__" ++ idName (paramId p)}}
     | otherwise             = p

\end{code}
     
\begin{code}
mkServVTBL :: Id -> Bool -> Bool -> [InterfaceDecl] -> CgM HDecl
mkServVTBL iface_id isDispSource justVTBL decls =
    getDeclName $ \ mname   -> do
    dname   <- getDllName 
    let 
     meths = filter isMethod decls
  
     mkFFIDecl m = do
	let res              = methResult m 
	(_, prim_decl,mb_prim) <-
	       primDecl True{-isObj-} True{-isServer-} True{-try to re-use other decls-}
		        (declId m) dname mname
		        (methCallConv m)
		        (resultType res)
		        (methParams m)
	return (prim_decl, mb_prim)

     ffi_decls 
       | isDispSource = return []
       | otherwise    = mapM mkFFIDecl meths

    fs_stuff <- ffi_decls
    let
     (fs, prim_nms) = unzip fs_stuff
     
     mk_vtbl = 
       genTypeSig mk_vtbl_nm (listToMaybe (catMaybes ctxts))
       			     (funTys meth_tys mk_vtbl_res_ty) `andDecl`
       funDef  mk_vtbl_nm mk_vtbl_pats mk_vtbl_rhs

     mk_vtbl_pats   = map (patVar.mkHaskellVarName.idName.declId) meths
     mk_vtbl_nm     = "mk" ++ qName (vtblName iface_id)
     mk_vtbl_res_ty = io (mkTyCon comVTableTy [iid_ty, iface_ty])
    

     mk_vtbl_args   = zipWith (\ _ x -> var ("meth_arg"++show x)) meths [(0::Int)..]
     mk_vtbl_rhs    = 
       foldr
	 (uncurry binder)
	 (funApp vtbl_creator [(hList mk_vtbl_args)])
         (zip (zipWith export_prim meths (prim_nms ++ repeat Nothing)) mk_vtbl_args)
      where
       binder
         | isDispSource = \ m v n -> hLet v m n
	 | otherwise    = bind

       vtbl_creator
         | isDispSource = createDispVTable
	 | otherwise    = createComVTable

     iface_ty    = uniqueTyVar "objState" -- needs to be branded 'unique' so that
     					  -- when we come to constraining type variables
					  -- for the VTBL methods (Variant overloaded), we
					  -- leave 'objState' parameter out of it.
     iid_ty      = tyQCon (idModule iface_id)
     			  (idName iface_id) [tyUnit]

     (meth_tys, ctxts) =
       unzip $
       map (\ m -> toHaskellMethodTy False
       				     True{- is server -}
				     False
				     (Just iface_ty) (methParams m) 
				     (methResult m))
           meths

     export_prim m mb_prim_nm
       | isDispSource = funApp mkDispMethod [ stringLit f_nm, dispid, wrap_up ]
       | otherwise    = funApply (var prim_nm) [wrap_up]
      where
       prim_nm =
         case mb_prim_nm of
	   Just x -> x
	   _      -> mkPrimExportName nm

       i    = declId m
       f_nm = idOrigName i
       dispid = 
          case getDispIdAttribute (idAttributes i) of
	     Nothing -> lit (iLit (0::Int))
	     Just il -> integerLit il

       wrap_up = funApply (var (mkPrimitiveName nm)) 
			  [var (mkHaskellVarName nm)]
       nm = idName i

    addExport (ieValue mk_vtbl_nm)
    if justVTBL then
       return mk_vtbl
     else do
       return (andDecls (mk_vtbl:fs))

vtblName :: Id -> QualName
vtblName i = mkQualName (idModule i) (idName i ++ "_vtbl")
\end{code}

\begin{code}
methCall :: Id -> Maybe Pat -> [Param] -> Mm ()
methCall f mb_res params = 
  addCode $
  case mb_res of
      -- i.e., [pure] has no effect of funs return void.
      --          [pure]void f(...); => f :: .... -> IO ()
    Nothing -> bind_ f_app
    Just p 
     | isPure    -> \ e -> hCase f_app [alt p e]
     | otherwise -> genBind  f_app p
 where
  isPure    = (idAttributes f) `hasAttributeWithName` "pure"
  f_app     = funApp (mkVarName meth_name) args
  args      = map (var.idName.paramId) params

  meth_name = mkHaskellVarName (idName f)

unmarshallIfacePointer :: Mm ()
unmarshallIfacePointer =
 addCode (bind (funApp getIfaceState [var iptr]) obj)
   where
    obj     = var "obj"

marshallResult :: Id -> Type -> Mm ()
marshallResult _ Void = return ()
marshallResult i   ty =
--   addCode (bind_ (funApply (marshallType proxyMarshallInfo ty) [nm]))
   addCode (bind (funApply (marshallType proxyMarshallInfo ty) [nm]) nm)
   where
     nm		= var ("res__" ++ idName i)

\end{code}


\begin{code}
writeOutParams :: [Param] -> Mm ()
writeOutParams params = do
  sequence (map writeOut params)
  return ()
 where
   writeOut p
     | isVoidTy ty = return ()
     | otherwise   = addCode (bind_ wOut)
    where
     i	      = paramId p
     ty	      = paramType p
     ty'      = removeNames (removePtr ty)
     nm       = idName i
     
     resV n = var ("res__" ++ n)
     
     wOut 
      | isSimpleTy ty' 
      = funApply (refMarshallType proxyMarshallInfo ty') [var ("out_"++nm), resV nm]
      | isEnumTy ty' || isBoolTy ty'
      = funApply (refMarshallType proxyMarshallInfo intTy) [var ("out_"++nm), resV nm]
      | otherwise
      = case ty' of
	  Name "VARIANT_BOOL" _ _ _ _ _ | optCom ->
               funApply (refMarshallType proxyMarshallInfo int16Ty) [var ("out_"++nm), resV nm]
	  _ -> funApp w_mshall [castPtr (var ("out_"++nm)), resV nm]
   	where
	  w_mshall = prefix marshallRefPrefix (mkQVarName hdirectLib ptrKind)
	  
	  ptrKind
	   | isFinalised = fptr
	   | otherwise   = "Ptr"

	  isFinalised = isFOTy (toHaskellBaseTy False ty')

\end{code}

What's generated for a coclass decl:

\begin{code}
mkServMain :: String -> Id -> [CoClassDecl] -> CgM HDecl
mkServMain lib_nm i cdecls = do
   mapM_ addExp exports
   return ( register_class     `andDecl` 
            new_instance       `andDecl`
	    component_info_def `andDecl`
	    vtbl_decls         `andDecl`
	    ifaces_decl	)
  where
   addExp (nm,ty)
    | optExportListWithTySig = addExportWithComment nm (":: " ++ showAbstractH (ppType ty))
    | otherwise		     = addExport nm

   exports = [ (ieValue component_info_nm, component_info_ty) ]

   class_name = idName i
   clsid_nm   = mkCLSIDName class_name
   libid_nm   = mkLIBIDName lib_nm

   implemented_ifaces = filter nonSourceIface cdecls
   
   nonSourceIface d = not ((idAttributes (coClassId d)) `hasAttributeWithName` "source")

   component_info_nm    = "componentInfo"
   component_info_ty    = mkTyConst componentInfo
   component_info_tysig = typeSig component_info_nm component_info_ty
   component_info_def   = 
	    component_info_tysig `andDecl`
	    valDef component_info_nm component_info_rhs

   component_info_rhs   = 
    (if usesTlb then
         \ x -> funApp hasTypeLib [x]
     else 
        id) $
     funApp mkComponentInfo
	    [ var clsid_nm
	    , var register_class_nm
	    , var new_instance_nm
	    ]

   register_class =
     register_class_sig  `andDecl`
     register_class_def

   register_class_sig = typeSig register_class_nm register_class_ty
   register_class_nm = "register_" ++ class_name
   register_class_ty = funTys [tyString, tyBool] io_unit

   register_class_def = 
      funDef register_class_nm [wildPat, wildPat] register_class_rhs

   register_class_rhs = ret unit --ToDo.

   new_instance = 
      new_instance_sig `andDecl`
      new_instance_def

   new_instance_sig = typeSig new_instance_nm new_instance_ty
   new_instance_nm  = "new" ++ class_name
   new_instance_ty  = 
    funTy tyString  $
    funTy (io_unit) $
    funTy (mkTyCon iID [mkTyCon iUnknown [tyVar "iid"]])
	  (io (mkTyCon iUnknown [tyVar "iid"]))

   component_mod = Just (idName i)
   component_new = qvar component_mod "new"

   new_instance_def = funDef new_instance_nm 
			     [ patVar "dll_path"
			     , patVar "finaliser"
			     , patVar "iid"
			     ]
			     new_instance_rhs
   new_instance_rhs = 
       bind  component_new obj_state $
       funApp createComInst [dll_path, obj_state, var "finaliser", var ifaces_nm, var "iid"]
  
   dll_path  = var "dll_path"
   obj_state = var "obj_state"
   ifaces_nm = "ifaces_" ++ class_name

   objStateTy = tyQConst component_mod "State"

   ifaces_decl =
      ifaces_ty_sig `andDecl`
      ifaces_def

   ifaces_ty     = tyList (mkTyCon comInterfaceTy [objStateTy])
   ifaces_ty_sig = typeSig ifaces_nm ifaces_ty
   ifaces_def = 
     valDef ifaces_nm
            ifaces_rhs
	 
   usesTlb   = not optUseStdDispatch && any derivesFromIDispatch implemented_ifaces 
   ifaces_rhs = hList (map mkIface implemented_ifaces)
   
   mkIface d
     | isDual    = funApp mkDualInterface l_args
     | isAuto    = funApp mkDispInterface l_args
     | otherwise = funApp mkComInterface    args
     where
       isDual = (idAttributes cid) `hasAttributeWithName` "dual"
       isAuto = derivesFromIDispatch d

       tlb_arg 
         | optAnonTLB || optUseStdDispatch = nothing
	 | otherwise  = just (var libid_nm)

       l_args  = (tlb_arg : args)
       args    = [qvar md ("iid" ++nm), var (vtbl_nm ++ "_vtbl")]
       cid     = coClassId d
       md      = idModule cid
       nm      = idName cid
       vtbl_nm = mkHaskellVarName nm

   vtbl_decls = andDecls (map mk_vtbl implemented_ifaces)
   
   mk_vtbl d = 
      typeSig vtbl_nm (mkTyCon comVTableTy [iid_ty, objStateTy]) `andDecl`
      valDef vtbl_nm  vtbl_rhs
    where
      iid_ty      = tyQCon (idModule (coClassId d)) 
      			   (idName (coClassId d)) [tyUnit]
      vtbl_nm_raw = vtblName (coClassId d)
      vtbl_nm     = mkHaskellVarName (qName vtbl_nm_raw)
      vtbl_rhs    = funApp uPerformIO [funApp (prefix "mk" vtbl_nm_raw) meths]

      meths    = 
        case coClassDecl d of
	   Nothing  -> error "Stuck"
	   Just dcl -> 
               let
	        decls = 
		  case dcl of
		    DispInterface{dispExpandedFrom=ii} | isJust ii -> declDecls (fromJust ii)
		    _ -> declDecls dcl

	        the_meths = filter (isMethod) decls
	       in
	       map ((qvar component_mod).mkHaskellVarName.idName.declId) the_meths

\end{code}
