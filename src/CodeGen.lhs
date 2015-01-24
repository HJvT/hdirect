%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Mar. 31th 2003  08:52  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Toplevel module for converting IDL into abstract Haskell code,
ready for dumping out to a file.

\begin{code}
module CodeGen ( codeGen ) where

import MarshallUtils
import MarshallStruct
import MarshallEnum
import MarshallType
import MarshallMethod
import MarshallUnion
import MarshallAbstract  ( marshallAbstract )
import MarshallServ
import MarshallCore
import MarshallFun
import MarshallJNI
import MarshallJServ
import Skeleton
import qualified CustomAttributes

import BasicTypes
import Literal
import AbstractH hiding (Type(..), Expr(..),CaseAlt(..))
import qualified AbstractH as Haskell ( HDecl(..),
					TyDecl(..),
					ConDecl(..),
					BangType(..)
				      )
import AbsHUtils
import MkImport      ( mkImportLists )
import LibUtils
import Opts ( optGenHeader, optNoExportList,
	      optOneModulePerInterface, optServer,
	      optUnparamedInterfacePointers,
	      optSubtypedInterfacePointers, optExportAbstractly,
	      optDualVtbl, optDon'tGenBinaryComInterfaces,
	      optSkel, optIgnoreDispInterfaces, optNoLibIds,
	      optIgnoreSourceIfaces, optNoEnumMagic, optHaskellToC,
	      optJNI, optCorba, optIgnoreMethsUpto, optUseStdDispatch,
	      optOutputHTo, optEnumsAsFlags, optGenNumInstance, optGenBitsInstance
	    )
import PpAbstractH ( ppType, showAbstractH )
import PpCore ( showCore, ppDecl )
import CgMonad

import CoreIDL
import CoreUtils
import Attribute
import Data.List  ( partition, intersperse, isPrefixOf )
import Utils ( dropSuffix, trace, basename, split,
	       splitdir, prefixDir, notNull
	     )
import Data.Maybe ( mapMaybe, isJust )
import Control.Monad ( when )

import Env
\end{code}

External interface, convert a set of toplevel IDL
declarations into their Haskell form.

A translation unit can at the toplevel consist of:

 - one or more modules
 - one or more libraries
 - one or more (disp)interfaces/coclasses.

IDL grouping declarations map onto Haskell modules as follows:
 
 - modules and libraries get their own Haskell module.
 - optionally, each (disp)interface/coclass can be put into
   a file of their own. (doing this eases the problem of
   namespace clashes, relying instead on the Haskell module
   system to operate satisfactorily..)

\begin{code}
codeGen :: (Either String String, Maybe String)
               {- User-supplied output name -}
	-> Env String [(Result, [Param])]
	       {- environment carrying type isomorphic methods info -}
	-> Env String (Maybe Id)
	       {- environment containing ifaces that should be ignored -}
        -> [Decl]
	-> ( [(String, Decl)]		        {- possible header file output  -}
	   , [(String, Bool, [HTopDecl])]       {- Haskell output		-}
	   )
codeGen (o_fname, mname) iso_env iface_env decls =
  ( generateHeader top_module_name decls
  , (\ x -> if optSkel then x ++ cgSkeleton decls else x) $
    liftOut (map (cGen iso_env iface_env) modules)
  )
 where
  liftOut [] = []
  liftOut (m@(nm, has_struct, ds) : ms) =
     case break isHMod ds of
       (ls,[x,HMod md@(HModule nm1 _ _ _ _)]) -> 
		    (nm, has_struct, ls ++ [x]) : 
		    (nm1 ++ ".hs", False, [HMod md]) : liftOut ms
       _ -> m : liftOut ms

  isHMod (HMod _) = True
  isHMod _	  = False

   -- The set of modules to generate. If there's any leftovers at the
   -- top (i.e., stuff that appear outside a module/library/interface etc. grouping),
   -- we put these bits into the top_module. In the one-module-per-interface 
   -- case, this means that for typedefs that appear inside interfaces will
   -- be lifted out and be generated with the outermost module (arguably they
   -- shouldn't be lifted out, but that's how things are organised at the moment.)
  modules = 
   case mods_and_libs of
     [_] -> [one_module] -- make sure we emit everything.
     _   ->
      case rest of
        [] -> mods_and_libs
        _  -> top_module : mods_and_libs

  toStdOut = 
    case o_fname of
      Right "-" -> True
      _		-> False

  -- use output filename to generate a plausible looking
  -- Haskell module name.
  (top_module_name, output_fname) = 
     case mname of
       Just x -> (x, snd ofnames)
       _      -> ofnames
    where
     ofnames =
         case o_fname of
          Right "-" -> 
	  	case mods_and_libs of 
		   ((_,m,_):_) -> (m, "-")
		   _	       -> ("Anon", "-")
	  Left  x   -> let 
	  		(_,base) = splitdir x
			y = mkHaskellTyConName (dropSuffix base)
		       in
		       (y, x)
	  Right x   ->
	    case mods_and_libs of
	      [(_,m,_)] -> let
	                    (dir,_) = splitdir x 
	      		   in (m, prefixDir dir (m ++ ".hs"))

	      _	        -> (mkModName x, x)

     mkModName x  = mkHaskellTyConName (basename (dropSuffix x))

  top_module = ( output_fname
	       , top_module_name
	       , mkTopModule (mkId top_module_name
	       			   top_module_name
				   Nothing
				   [{-no attrs-}])
				   rest
	       )

  one_module = ( output_fname
	       , top_module_name
	       , mkTopModule (mkId top_module_name
	       			   top_module_name
				   Nothing
				   [{-no attrs-}])
				   flattened_decls
	       )

   -- Hoist out some of these. Currently they all have to be at the top,
   -- we don't scan through the whole file looking for them.
  mkTopModule i (d@(HsLiteral _) : xs) = d : mkTopModule i xs
  mkTopModule i (d@(CInclude _) : xs)  = d : mkTopModule i xs
  mkTopModule i (d@(CLiteral _) : xs)  = d : mkTopModule i xs
   -- delayed addition of the attributes for the module itself.
  mkTopModule i ds@(d@Module{} : _)    = [Module i{idAttributes=idAttributes (declId d)} ds]
  mkTopModule i ds@(d@Library{} : _)   = [Module i{idAttributes=idAttributes (declId d)} ds]
  mkTopModule i ds		       = [Module i ds]

  mods_and_libs = map ( \ d -> let (x,y) = mkModuleLibName d in (x, y, [d])) mods_and_libs'
  (mods_and_libs', rest) = partition inSeparateHaskellModule flattened_decls

  flattened_decls = flattenDecls decls

  mkModuleLibName d 
    | toStdOut  = ("-", hnm)
    | otherwise = (hnm ++ ".hs", hnm)
      where
	   hnm = mkHaskellTyConName nm
	   nm_raw = idName (declId d)
	   nm
	    | optServer = nm_raw ++ "Proxy"
	    | otherwise = nm_raw
\end{code}

\begin{code}
cGen :: Env String [(Result, [Param])]
     -> Env String (Maybe Id)
     -> (String, String, [Decl]) 
     -> (String, Bool, [HTopDecl])
cGen iso_env iface_env (oname, mod_name, ds) = (oname, flg, ds')
 where
  (ds',flg) = foldr mkHTop ([],False) ds

  mkHTop d (acc, has_structs) = 
   case d of
     Library i ms ->
       case (runCgM iso_env iface_env (withDeclName (idName i) (cgLibrary i ms))) of
        (decl, expo, imps, flg1, has_prim) -> 
	  let qual_imps = mkImportLists mod_name (getHsImports i) [decl]
	      real_imps = qual_imps ++ imps

	  in
	  (modDecl i has_prim [decl] real_imps expo : acc, has_structs || flg1)
     DispInterface i _ _ _ ->
       case (runCgM iso_env iface_env (withIfaceDeclName (idName i) (cgDecl d))) of
         (decl, expo, imps, flg1, has_prim) -> 
	   let qual_imps = mkImportLists mod_name (getHsImports i) [decl] 
	       real_imps = qual_imps ++ imps
	   in
	   (modDecl i has_prim [decl] real_imps expo : acc, has_structs || flg1)
     Interface i is_ref inherit _
       | not is_ref ->
         case (runCgM iso_env iface_env (withIfaceDeclName (idName i) (cgDecl d))) of
           (decl, expo, imps, flg1, has_prim) -> 
	     let qual_imps = mkImportLists mod_name (getHsImports i) [decl] 
	         real_imps = qual_imps ++ imps

		 attrs	    = idAttributes i
		 iface_deps = filterAttributes attrs ["depender"]
		 new_attrs  = filterOutAttributes attrs ["depender"]
		 i'	    = i{idAttributes=new_attrs}
		 (acc', has_structs')
		    | not optOneModulePerInterface || null iface_deps = (acc, has_structs)
		    | otherwise	 = 
			case mkHTop (Interface i' False inherit []) (acc, has_structs) of
			  (HMod (HModule nm a b c d1) : ls, e) ->
			     (HMod (HModule (nm ++ "Ty") a b c d1) : ls , e)
			  x -> x
	     in
	     (modDecl i has_prim [decl] real_imps expo : acc', has_structs' || flg1)
       | otherwise -> (acc, has_structs)
     CoClass{} -> 
       case (runCgM iso_env iface_env (cgDecl d)) of
         (decl, expo, imps, flg1, has_prim) -> 
	   let qual_imps = mkImportLists mod_name (getHsImports (declId d)) [decl]
	       real_imps = qual_imps ++ imps

	   in
	   if isEmptyDecl decl then
	       (acc, has_structs)
	   else
   	       (hModule mod_name
		        has_prim
  		        (map (\ (x,_,y) -> hExport x y) expo)
  		        (map (\ (x,y,z) -> hImport x y z) real_imps)
 		        decl : acc, has_structs || flg1)
     Module i ms -> 
       case (runCgM iso_env iface_env (withDeclName (idName i) (cgModule i ms))) of
         (decl, expo,imps,flg1, has_prim) -> 
	    let qual_imps = mkImportLists mod_name (getHsImports i) [decl]
	        real_imps = qual_imps ++ imps
	    in
	    ( modDecl (i{idName=mod_name}) has_prim [decl] real_imps expo : acc
	    , has_structs || flg1
	    )
     HsLiteral s -> (hMeta s    : acc, has_structs)
     CLiteral s  -> (cMeta s    : acc, has_structs)
     CInclude s  -> (hInclude s : acc, has_structs)
     _		 -> error ("Odd decl: " ++ showCore (ppDecl d))

  modDecl _ mflg decls imps expo
    | optNoExportList =
        hModule mod_name mflg
                (map (\ (x,_,y) -> hExport x y) $ filter (\ (_,x,_) -> x) expo)
                (map (\ (x,y,z) -> hImport x y z) imps)
                (andDecls decls)
    | otherwise =
      hModule mod_name mflg
	      (map (\ (x,_,y) -> hExport x y) expo)
	      (map (\ (x,y,z) -> hImport x y z) imps)
	      (andDecls decls)
\end{code}

From the .idl input, we optionally generate C header file information. This is
useful when the master copy of the .h file is the .idl specification (or, you
don't have the header file already available.)

\begin{code}
generateHeader :: String -> [Decl] -> [(String, Decl)]
generateHeader o_fname decls
 | not optGenHeader = []
 | otherwise	    = 
    case optOutputHTo of
      (x:_) -> 
         let
	  nm = dropSuffix x
	 in
	 [(oname, Module (mkId nm nm Nothing []) decls)]
      _ -> 
       case (concatMap mkHeaderDecls decls) of
        []      -> [(oname, Module (mkId oname oname Nothing []) decls)]
        [(_,s)] -> [(oname, s)]
        ls	-> ls
 where
  oname = 
    case optOutputHTo of
      (x:_) -> x
      _ ->
       case o_fname of
          "-" -> "-"
          _   -> dropSuffix o_fname ++ ".h"

  mkHeaderNm i = mkHaskellTyConName (idName i) ++ ".h"

  mkHeaderDecls d =
    case d of
      Module	i _	    -> [(mkHeaderNm i, d)]
      Interface i _ _ _     -> [(mkHeaderNm i, d)]
      DispInterface i _ _ _ -> [(mkHeaderNm i, d)]
      Library i _	    -> [(mkHeaderNm i, d)]
      _			    -> []

\end{code}

Generating code for the various Core IDL declarations:

\begin{code}

cgDecl :: Decl -> CgM HDecl
cgDecl d =
 case d of
   Typedef n t _	            -> cgTypedef n t
   Constant i t o_t e	            -> cgConstant i t o_t e
   Interface i _ inherit decls -> 
     withIfaceInherit (map fst inherit) $ 
     hoistInClass (idName i)		$ \ mb_cls -> do
     cls_d <-
         {-
	   Check to see if we should include a CLSID declaration
	   as well. Do this in the case where we've got
	     
 	     interface _A { ... }; coclass A { interface _A; };
           
           and A has the only use of _A. Useful in one-module-per-interface mode,
	   as it avoids creating a (v simple) module for the coclass.
	 -}
       case mb_cls of
         Nothing -> return emptyDecl
	 Just ci
	    | notNull deps -> return emptyDecl
	    | otherwise	   -> do
	   let ci'
	        | idName i `isPrefixOf` idName ci = ci{idName=idName i}
		| otherwise			  = ci

           ud <- setInterfaceFlag (ComIDispatch False) (uuidDecl ci' [] Clsid)
           return (infoHeader (CoClass ci' [CoClassInterface i Nothing]) `andDecl` ud)

     forClient <- getClientFlag
     let is_source = hasSourceAttribute (idAttributes i)
     setSourceIfaceFlag is_source $ do
      dserv <- 
        if (is_source && optIgnoreSourceIfaces) then
           return emptyDecl
        else 
          if (is_source && forClient) || (not forClient && not is_source) then
              marshallServ i inherit decls
           else
             cgInterface i inherit decls
      return (cls_d `andDecl` dserv) 
    where
      deps = filterAttributes (idAttributes i) ["depender"]

   Method i cc res ps offs     -> do
     forClient <- getClientFlag
     is_source <- getSourceIfaceFlag
     if (is_source && forClient) || (not forClient && not is_source) then do
        k <- getInterfaceFlag
        case k of
	  StdFFI -> getDeclName $ \ nm ->
		    marshallFun (Just nm) i (FunTy cc res ps) 
          _ | optJNI    ->  cgJServMethod i res ps
	    | otherwise ->  do
	        isInDisp <- isInDispInterface
	    	cgServMethod i res ps is_source (optUseStdDispatch && isInDisp)
      else
	if optJNI then
	   cgJNIMethod i res ps
	 else
  	   cgMethod i cc res ps offs Nothing

   Property i ty _ s g      -> cgProperty i ty s g
   HsLiteral str	    -> return (Haskell str)
   CLiteral str             -> return (CCode str)
   CInclude  fname          -> return (Include fname)
   DispInterface i ii ps ms -> cgDispInterface i ii ps ms
   CoClass i mems	    -> cgCoClass i mems
   Library i decls	    -> cgLibrary i decls
   Module i ds		    -> cgModule i ds
   _			    -> return emptyDecl

\end{code}


%
%
<sect2>Typedefs
<label id="sec:typedef">
<p>
%
%

An IDL typedef have the following form:

  typedef type name;

The translation into a Haskell type declaration is
implemented as follows:

<verb>
 D[typedef type name] = "DT[type] Con[name] = T[type]"
</verb>

where <tt/Con[]/ is the mapping from an IDL name to a
Haskell type constructor name and <tt/DT[]/ uses the IDL
type to determine what kind of Haskell user-defined data
type declaration to use. 

The meat of the translation is done by the <tt/T[]/ mapping
scheme, which is implemented by <tt/toHaskellTy/ in @MarshallType@.

\begin{code}
cgTypedef :: Id -> Type -> CgM HDecl
cgTypedef tdef_id ty
  | (idAttributes tdef_id) `hasAttributeWithName` CustomAttributes.ignoreAttr
  = return emptyDecl
  | otherwise = do
    addExport (ieType hname (is_tysyn || optExportAbstractly))
    d <- cgMarshallTy tdef_id ty
    return (typedef `andDecl` d)
  where
    attrs = idAttributes tdef_id
    hname = mkHaskellTyConName (idName tdef_id)
    
    isNewType = attrs `hasAttributeWithName` CustomAttributes.newtypeAttr
    isPure    = attrs `hasAttributeWithName` CustomAttributes.pureAttr

     -- the support for represent_as() is currently
     -- restricted to types in typedefs (i.e., no
     -- support for hooking in your own marshallers.)
    (the_ty, tvs) =
      case findAttribute "represent_as" attrs of
         Just (Attribute _ (ParamLit (StringLit s):_)) -> (tyConst s, [])
	 _ -> 
	  case (unconstrainType (groundTyVars (toHaskellTy True ty))) of
	     (ls,t) -> (purify t, map (qName.snd) ls)

    purify ty1
      | isPure     = purifyType ty1
      | otherwise  = ty1

     -- the DT[] translation. IDL simple types, arrays and pointers
     -- are mapped onto type synonyms, the rest are algebraic data types.
 
    is_tysyn = 
         isSimpleTy ty
      || isAbstractTy ty
      || isBoolTy ty
      || isArrayTy ty
      || isSafeArrayTy ty
      || isPointerTy ty
      || isSynTy ty
      || isFunTy ty
      || isIntegerTy ty
      || isStringTy ty  
      || isSeqTy ty
    typedef
      | isNewType && null (tail conDecls) = dataType
      | is_tysyn  = tySyn hname tvs the_ty
      | otherwise = dataType

    conDecls = mkHaskellConDecls hname attrs ty
    dataType = 
	 TyD $
         TyDecl  (if isNewType then Newtype else Data)
                 hname
		 tvs
	         (if isNewType then (map recToConDecl conDecls) else conDecls)
		 derivings

    derivings =
      case findAttribute CustomAttributes.derivingAttr attrs of
        Just (Attribute _ [ParamLit (StringLit s)]) -> map toQualName (split ',' s) ++ ds
        _ -> ds

    ds = addFlagDerivings $
      case ty of
         Enum _ kind vs |  genDerivedEnumInstanceFor kind vs && not forceFlag -> [enumClass]
	 _ -> []

    addFlagDerivings
     | not optGenNumInstance && not optGenBitsInstance = id
     | forceFlag = \ x -> (eqClass:showClass:x)
     | otherwise =
       case ty of
         Enum _ EnumFlags{} _ -> \ x -> (eqClass:showClass:x)
	 _ -> id

    forceFlag = optEnumsAsFlags || 
    		attrs `hasAttributeWithName` CustomAttributes.flagAttr
\end{code}

%
%
<sect2>Constants
<label id="sec:constants">
<p>
%
%

Generating the Haskell equivalent of a constant is pretty
straightforward, just introduce a constant declaration (+ type signature)
for it.

\begin{code}
cgConstant :: Id -> Type -> Type -> Expr -> CgM HDecl
cgConstant i t o_t e = do
  addExport (ieValue hname)
  return (typeSig hname ty `andDecl` 
          funDef  hname [] expr)
 where
   ty    = toHaskellTy True o_t
   hname = mkHaskellVarName (idName i)
   expr  = 
        case t of
	  WString{} -> funApp mkWString [coreToHaskellExpr e]
	  String{}  -> coreToHaskellExpr e
	  Integer{} -> coreToHaskellExpr e
	  Char{}    -> coreToHaskellExpr e
	  Float{}   -> coreToHaskellExpr e
	  WChar{}   -> coreToHaskellExpr e -- this is currently mapped to Char, btw.
	  Bool      -> coreToHaskellExpr e
	  Octet     -> coreToHaskellExpr e
	  Pointer{} -> funApp intToAddr [ coreToHaskellExpr e ]
	  _	    -> error ("cgConstant: don't know how to handle constant of type: " ++ 
	  		      showCore (ppType ty) ++ showParen True (shows (idName i)) "")

\end{code}

%
%
<sect2>Translating interfaces
<label id="sec:translate:interface">
<p>
%
%

\begin{code}
cgInterface :: Id -> InterfaceInherit -> [InterfaceDecl] -> CgM HDecl
cgInterface if_nm inherit decls = setIfaceName iface_name $ do
  let 
      (iface_kind, is_bin)
       | not (is_object || is_idispatch ) = 
	    if (idAttributes if_nm) `hasAttributeWithName` "odl" then
		(VTBLObject, True)
	    else
		if notNull decls || optCorba || optJNI then
    		   (VTBLObject, True)
		else
		   (StdFFI, False)
       | is_idispatch  = (ComIDispatch isDual, False)
       | otherwise     = (VTBLObject, True)

      the_decls_to_use
       | is_idispatch =
         case optIgnoreMethsUpto of
	   Nothing -> decls
	   Just x  -> 
	     case break (isEqualMethod x) decls of
	       (ds,[])   -> ds
	       (ds,_:xs) -> 
	           -- keep any typedefs, but remove the methods
	          filter (not.isMethod) ds ++ xs
       | otherwise = decls

      isEqualMethod x d = isMethod d && idOrigName (declId d) == x
        

  if is_bin && optDon'tGenBinaryComInterfaces then
      trace ("Ignoring (binary) interface: "++ show iface_name) $
      return emptyDecl
   else do
  setInterfaceFlag iface_kind $ do
  setIEnumFlag is_ienum       $ do
  setMethodNumber startOffset
  body <- setIfaceAttributes attrs (mapM coGen the_decls_to_use)
  let typeInSepModule = optOneModulePerInterface && notNull deps
  when typeInSepModule $ do
	 let ty_mod_nm = idName if_nm ++ "Ty"
         addVitalExport (ieModule ty_mod_nm)
         when (optNoExportList) (addVitalExport (ieModule (idName if_nm)))
	 addExplicitImports [(False, ty_mod_nm)]

  ud   <- 
     if is_javeh_interface then do
         c <- cgJNIInterface if_nm typeInSepModule
	 return (infoHeader (Interface if_nm False inherit the_decls_to_use) `andDecl` c)
      else if is_javeh_class then do
         c <- cgJNIClass if_nm typeInSepModule
	 d <- setInterfaceFlag VTBLObject (uuidDecl if_nm inh Iid)
	 let
	  ds 
	   | typeInSepModule = infoHeader (Interface if_nm False inherit the_decls_to_use) `andDecl` c
	   | otherwise       = infoHeader (Interface if_nm False inherit the_decls_to_use) `andDecl` d `andDecl` c
	 return ds
       else if typeInSepModule then do
	 return emptyDecl
        else do
         d <- uuidDecl if_nm inh Iid
	 return (infoHeader (Interface if_nm False inherit the_decls_to_use) `andDecl` d)

  return (andDecls (ud:body))
 where
   is_javeh_interface = optJNI && not is_javeh_class
   is_javeh_class     = optJNI && attrs `hasAttributeWithName` CustomAttributes.jniClassAttr

   attrs      = idAttributes if_nm

   iface_name = idName if_nm
   deps       = findDeps if_nm

   coGen d
    | isMethod d = do
         d' <- cgDecl d
	 incMethodNumber
	 return d'
    | otherwise  = cgDecl d
	    
   startOffset
    | iface_name == "IUnknown" = 0
    | otherwise		       = sum (map snd inh)

   isDual = attrs `hasAttributeWithName` "dual"

      {-
        Figuring out what kind of interface we've been presented with
	is a little bit involved:
	 
	  - if iface has [oleautomation] attr ==> it's an IDispatch thing.
	  - [dual] 			      ==> IDispatch
	  - inherits from IDispatch derived iface  ==> IDispatch
	  - [object] (but none of the above)  ==> IUnknown (binary invoc., really.)
	  - [odl] (but none of the above)     ==> same.

        Whether the generated stubs for IDispatch interfaces are declared
	as being dual should use Invoke() or binary invocation is controlled
	by the -fdual-vtbl (optDualVtbl) flag.
      -}

   is_object = 
      attrs `hasAttributeWithName` "object" ||
      any (\ x -> qName (fst x) == "IUnknown") inherit

      -- extremely simplistic..
   is_ienum = 
       not optNoEnumMagic &&
       ("IEnum" `isPrefixOf` iface_name &&
        (length (filter isMethod decls)) == 4 &&
        ok_looking_enum_names)
  
   -- Dear, oh dear. ToDo: control this with a cmd-line switch instead.
   ok_looking_enum_names =
      case (map (idName.declId) (filter isMethod decls)) of
	['r':'e':'m':'o':'t':'e':'N':'e':'x':'t':_
	 ,'s':'k':'i':'p':_, 'r':'e':'s':'e':'t':_
	 ,'c':'l':'o':'n':'e' : _
	 ] -> True
	['n':'e':'x':'t':_
	 ,'s':'k':'i':'p':_, 'r':'e':'s':'e':'t':_
	 ,'c':'l':'o':'n':'e' : _
	 ] -> True
        _ -> False

   is_idispatch = 
       -- Not right, [oleautomation] only constrains the set of valid types.
       --is_oleaut	           ||
       (isDual && not optDualVtbl) ||
       (any (\ x -> qName (fst x) == "IDispatch" &&
       		    iface_name    /= "IDispatchEx") inherit &&
       not (isDual && optDualVtbl))

   inh =
    case inherit of
      []  | optCorba -> [(cObject, 0)]
          | optHaskellToC || idName if_nm == "IUnknown" -> []
	  | otherwise		       -> [(iUnknown, 3)]
      xs@((x,_):_)
	   -- if we've decided that we're processing an 
	   -- Automation interface, but we've got an IUnknown
	   -- in our hand as the interface we're inheriting from.
	   -- Ignore, and pretend it's an IDispatch instead.
	| is_idispatch && (not is_object) && qName x == "IUnknown" -> 
	        trace ("Odd, interface " ++ show (idName if_nm) ++ 
		       "inherits from IUnknown, but has been classified as an Automation interface\n" ++
		       "(it will be treated as an Automation interface.)") 
		[(iDispatch, 7)]
	   -- special case for IUnknown:
	| is_object && idName if_nm == "IUnknown" -> []
	| otherwise -> map toStdNames xs
	   where
	     -- Just to make sure that we're using them 
	     -- in a proper qualified manner...
	    toStdNames (n,meths) = 
		case (qName n) of
{- Ensuring that re-defns of these two are short-circuited to the
   library-provided impls, is now done by the desugarer. 
   
   ToDo: delete this once we're certain that this catches all of 'em.
		  "IDispatch" -> (iDispatch,7)
		  "IUnknown"  -> (iUnknown,3)
-}
		  nm          -> (n{qName=mkIfaceTypeName nm}, meths)
\end{code}


\begin{code}

data GuidKind 
 = Iid | Clsid | Libid 
   deriving ( Eq )

uuidDecl :: Id -> InterfaceInherit -> GuidKind -> CgM HDecl
uuidDecl i inherit guidKind = do
 flg       <- getInterfaceFlag
 forClient <- getClientFlag
 case flg of
   StdFFI | forClient -> do
     addExport (ieType tycon_nm optExportAbstractly)
     decls <- marshallAbstract i
     return (abs_ty `andDecl` decls)
   _       -> do
     case (getUuidAttribute attrs) of
       Nothing  
        | guidKind == Iid  -> do
	   addExport (ieType iface_ptr_ty_nm False)
           addExport (ieType i_tycon_nm_dummy False)
	   ds <- 
	      if null inherit && optHaskellToC then do
	         ds <- marshallAbstract i
		 return ( abs_ty `andDecl` ds)
	       else
	         return emptyDecl
	     
	   return ( ds 		   `andDecl`
	            iface_dummy_ty `andDecl`
		    iface_ptr_ty
		  )
	| otherwise -> return emptyDecl

       Just guid -> do
	   when (guidKind == Iid) (addExport (ieType iface_ptr_ty_nm True))
           when (guidKind == Iid && not_iunknown) (addExport (ieType i_tycon_nm_dummy True))
	   when (not no_libids) (addExport (ieValue iid_name))
	   return (iface_dummy_ty `andDecl` 
		   iface_ptr_ty   `andDecl`
		   iid_def)
	  where
	   no_libids    = guidKind == Libid && (optNoLibIds || optUseStdDispatch)

	   iid_name     = iid_prefix ++ iface_ptr_ty_nm
	   iid_def  
	    | no_libids = emptyDecl
	    | otherwise = iid_tysig `andDecl` iid_decl

	   iid_tysig    = typeSig iid_name (iid_tycon iid_tycon_args)
	   iid_decl     = funDef iid_name [] iid_rhs
	   iid_tyarg    = 
		tyCon
		  iface_ptr_ty_nm
		  (if not optSubtypedInterfacePointers then
		      []
		   else
		      [mkTyConst groundInterface])

	   iid_rhs = funApp mk_iid [lit (StringLit ('{':(concat (intersperse "-" guid)) ++ "}"))]

	   iid_tycon_args =case guidKind of { Iid -> [iid_tyarg] ; _   -> [] }

	   mk_iid  =
	    case guidKind of
	      Iid   -> mkIID 
	      Clsid -> mkCLSID
	      Libid -> mkLIBID

	   (iid_prefix, iid_tycon) =
	    case guidKind of
	       Iid    -> ("iid",   mkTyCon iID)
	       Clsid  -> ("clsid", \ _ -> mkTyConst cLSID)
	       Libid  -> ("libid", \ _ -> mkTyConst lIBID)


      where
         not_iunknown = idName i /= "IUnknown"

          {-
	    For interface 'ITest', we generate:
	    
	       data Test  a = Test__
	       type ITest a = IUnknown (Test a)
	       
	    where 'IUnknown' is the interface ITest inherits from.
	  -}
	 iface_dummy_ty 
            | (guidKind /= Iid)  = emptyDecl
	    | isAuto && optUnparamedInterfacePointers  = tySyn i_tycon_nm [] (mkTyConst iDispatch)
	    | optSubtypedInterfacePointers = 
	       case idName i of
		"IUnknown" -> emptyDecl  -- it's in a library.
	        _          -> dataTy (i_tycon_nm_dummy) ["a"] [conDecl (i_tycon_nm++"__") []]
	    | otherwise = emptyDecl

	 iface_ptr_ty
	    | (guidKind /= Iid) = emptyDecl -- coclass decl doesn't get a type syn.
	    | optSubtypedInterfacePointers =
		case idName i of
		  "IUnknown"             -> 
		  	tySyn iface_ptr_ty_nm ["a"] (mkTyCon iUnknown [tyVar "a"])
		  _ | notNull inherit -> 
		  	tySyn iface_ptr_ty_nm ["a"]
			      (mkTyCon inh_from [tyCon (i_tycon_nm_dummy) [tyVar "a"]])
		    | otherwise          ->
		        emptyDecl

            | null inherit = emptyDecl
	    | otherwise    = tySyn iface_ptr_ty_nm [] (mkTyConst inh_from)

         i_tycon_nm_dummy = i_tycon_nm ++ "_"

	 isAuto = 
	   case flg of
	     ComIDispatch _ -> True
	     _		    -> False

         iface_ptr_ty_nm 
	    | guidKind == Iid  = mkIfaceTypeName (idName i)
	    | otherwise        = mkHaskellTyConName (idName i)

	 inh_from = 
	   let q_nm = fst (head inherit) in
	   case qName q_nm of
	    "IUnknown" | isDual -> iDispatch 
			 -- I'm not kidding - this kind of bogosity
			 -- does appear in Real Life.
	    _  -> q_nm

 where
   -- drop common prefixes from the (disp)interface/coclass name..
  i_tycon_nm = mkHaskellTyConName (mkIfaceTypeName (idName i))

  attrs      = idAttributes i
  tycon_nm   = mkHaskellTyConName (idName i)

  abs_ty_args  = 
    case findAttribute CustomAttributes.tyArgsAttr attrs of
      Just (Attribute _ [ParamLit (StringLit s)]) -> words s
      _ -> []

  abs_ty = newTy tycon_nm abs_ty_args (conDecl tycon_nm [the_abs_ty]) derivings
  
  abs_ty_h = tyCon tycon_nm (map tyConst abs_ty_args)

  isDual = attrs `hasAttributeWithName` "dual"

  derivings =
    case findAttribute CustomAttributes.derivingAttr attrs of
      Just (Attribute _ [ParamLit (StringLit s)]) -> map toQualName (split ',' s)
      _ -> []

  the_abs_ty 
    | attrs `hasAttributeWithName` CustomAttributes.finaliserAttr
    = tyForeignPtr abs_ty_h
    | otherwise = tyPtr abs_ty_h


findDeps :: Id -> [String]
findDeps i = map remove $  filterAttributes attrs ["depender"]
  where
   remove (Attribute _ [ParamLit (LitLit s)]) = s
   remove _				      = ""

   attrs = idAttributes i
\end{code}

%
%
<sect2>Translating dispinterfaces
<label id="sec:translate:dispinterface">
<p>
%
%

Very much like the translation of a `normal' interface:

\begin{code}
cgDispInterface :: Id -> Maybe Decl -> [Decl] -> [Decl] -> CgM HDecl
cgDispInterface i ii props meths = 
    withIfaceInherit [iUnknown,iDispatch]  $
    setIfaceName (idName i)                $
    inDispInterface			   $
    setInterfaceFlag (ComIDispatch False)  $ do
    is_src   <- getSourceIfaceFlag
    let is_source = is_src || hasSourceAttribute (idAttributes i)
    setSourceIfaceFlag is_source $ do
    forClient <- getClientFlag
    body <- mapM cgDecl (props ++ meths)
    ud   <- uuidDecl i [(iDispatch,7)] Iid
    if is_source && optIgnoreSourceIfaces then
        return (infoHeader (DispInterface i ii props meths) `andDecl` ud)
     else do
     {-
      It may look a bit funny to generate a VTBL for a
      dispinterface server too, but that's how we implement
      'em. The type library based IDispatch marshaller needs to
      be passed a method table.
     -}
      vtbl <- 
        if (forClient && not is_source) || (not forClient && is_source) then
            return emptyDecl
         else
	    let
	      is_wrapper = isJust ii
	      stuff = 
	        case ii of
		  Just (Interface{declDecls=ds}) -> ds
		  _ -> props ++ meths
            in
            mkServVTBL i (is_source || optUseStdDispatch) is_wrapper stuff

      return (infoHeader (DispInterface i ii props meths) `andDecl`
              ud				          `andDecl`
	      vtbl				          `andDecl`
              andDecls body
	     )
    
\end{code}

%
%
<sect2>Translating coclasses
<label id="sec:translate:dispinterface">
<p>
%
%

If we're splitting up the IDL input up into one Haskell module per (disp)interface,
a coclass declaration gives rise to a Haskell module that re-exports the interfaces
it implements plus it defines its CLSID.

\begin{code}
cgCoClass :: Id -> [CoClassDecl] -> CgM HDecl
cgCoClass i cdecls 
 | optOneModulePerInterface = do
     don't_gen_code_for_cls <-
       case cdecls of
         [x] | isJust (coClassDecl x) -> 
	    hoistInClass (idName (coClassId x)) $ \ mb_cls ->
	    return (isJust mb_cls)
	 _ -> return False

     if don't_gen_code_for_cls then
        return emptyDecl
      else do
        addExplicitImports import_modules
        sequence (map (addExport.ieModule.snd) import_modules)
        ud <- setInterfaceFlag (ComIDispatch False) (uuidDecl i [] Clsid)
        return (prettyPrint ud)

 | otherwise = do
     forClient <- getClientFlag
     ud        <- setInterfaceFlag (ComIDispatch False) (uuidDecl i [] Clsid)
     body      <-
        if (not forClient) then
	   getDeclName $ \ lib_nm ->
           mkServMain lib_nm i cdecls
	else
	   return emptyDecl
     return ( prettyPrint ud `andDecl` body)

 where
  import_modules = map (\ x -> (True, idName (coClassId x))) the_cdecls

   -- weed out the dispinterfaces, if we've been told to do so.
  isIface CoClassInterface{} = True
  isIface _		     = False

  the_cdecls
    | optIgnoreDispInterfaces = filter (isIface) cdecls
    | otherwise		      = cdecls

  prettyPrint d  = infoHeader (CoClass i cdecls) `andDecl` d 

\end{code}

\begin{code}
cgLibrary :: Id -> [Decl] -> CgM HDecl
cgLibrary i lib_decls = 
   withDeclName (idName i) $ do
     ud <- setInterfaceFlag (ComIDispatch False) (uuidDecl i [] Libid)
     ds <- mapM cgDecl lib_decls
     return ( prettyPrint ud `andDecl` andDecls ds)
 where
  prettyPrint d 
   | optNoLibIds = emptyDecl
   | otherwise   = infoHeader (Library i lib_decls) `andDecl` d 

\end{code}


%
%
<sect2>Translating modules
<label id="sec:translate:module">
<p>
%
%

Translating a module isn't too much work, set the flag
to indicate that we're processing 'static' decls inside
the module and tell everyone what DLL it's coming from
(if any.)

\begin{code}
cgModule :: Id -> [Decl] -> CgM HDecl
cgModule i ds =
   setInterfaceFlag StdFFI $ 
   withDeclName (idName i) $ 
   mbSetDllName		   $ do
       hs <- mapM cgDecl ds
       return (infoHeader (Module i ds) `andDecl` andDecls hs)
  where
   mbSetDllName x = 
     case (findAttribute CustomAttributes.dllNameAttr (idAttributes i)) of
       (Just (Attribute _ [ParamLit (StringLit s)])) -> setDllName s x
       _ -> x

\end{code}


%
%
<sect2>Translating types
<label id="sec:translate:type">
<p>
%
%

Converting an IDL type into its Haskell equivalent. The translation
is in some cases guided by attributes, e.g., the presence of the
`string' attribute turns char* into a `String'.

\begin{code}
mkHaskellConDecls :: Name -> [Attribute] -> Type -> [ConDecl]
mkHaskellConDecls nm attrs ty =
  map (groundTys) $ 
  case ty of
   Enum _ k vals          -> mkHEnumDef  nm attrs k vals
   Union _ _ _ _ switches -> mkHUnionDef nm switches
   UnionNon _    switches -> mkHUnionDef nm switches
   CUnion _ fields _ -> mkCUnionDef fields
   Struct tag mem _  -> [mkHStructDef tag mem]

    -- [abstract] is undoc'ed and will probably disappear soon,
    -- don't use..
   Pointer _ _ Iface{}
      | attrs `hasAttributeWithName` CustomAttributes.abstractAttr
      -> [ConDecl nm [Unbanged abs_ty]]
	where
	  abs_ty 
	   | attrs `hasAttributeWithName` CustomAttributes.finaliserAttr
	   = tyForeignObj
	   | otherwise = tyAddr
	      
   Pointer pt _ t ->
       case pt of 
         Ptr    -> [ConDecl "Ptr" [Unbanged (toHaskellTy True t)]]
         Ref    -> mkHaskellConDecls nm attrs t
         Unique -> [ConDecl "Maybe" [Unbanged (toHaskellTy True t)]]
   _ -> let
	 str = showAbstractH (ppType (toHaskellTy True ty))
	in
	trace ("mkHaskellConDecls: odd argument: " ++ str)
	      [ConDecl str []]
 where
   -- remove any left-over 
  groundTys (ConDecl n bs) = ConDecl n (map groundBangTy bs)
  groundTys (RecDecl n rs) = RecDecl n (map (\ (i,t) -> (i,groundBangTy t)) rs)
  
  groundBangTy (Banged t)   = Banged   (groundTyVars t)
  groundBangTy (Unbanged t) = Unbanged (groundTyVars t)

\end{code}

Top level function for creating code for (constructed)
IDL types:

 * we don't generate any marshalling code for the different
   IDL pointer types. Instead, we provide parameterised marshallers
   in a library and just parameterise them approp. when we need
   to swizzle pointers to/from the outside world. i.e., we're
   interested in keeping the size of the generated code down.

\begin{code}
cgMarshallTy :: Id -> Type -> CgM HDecl
cgMarshallTy i ty =
 case ty of
  Struct tag members mb_pack -> 
    case (mkHaskellConDecls (idName tag) (idAttributes tag) ty) of
      [cdecl]       ->  marshallStruct (idName i) tag cdecl members mb_pack
      _             -> error "cgMarshallTy{Struct}: expected one condecl"
  Enum _ k fields   -> marshallEnum i k (genDerivedEnumInstanceFor k fields) fields
  Union _ tag_ty tag _ sws ->
      marshallUnion (idName i) (Left (tag,tag_ty)) False sws Nothing
  UnionNon tag sws ->
    let
     as     = idAttributes i ++ idAttributes tag
     tag_ty = getTagTy as
    in
    marshallUnion (idName i) (Right tag_ty) False sws Nothing

  CUnion tag fields mb_pack ->
    let
     as     = idAttributes i ++ idAttributes tag
     tag_ty = getTagTy as

     switches = map fieldToSwitch fields
     
     fieldToSwitch (Field fi t ot _ _) =  Switch fi labs t ot
        where
	  labs = 
	    case findAttribute "case" (idAttributes i) of
	      Just (Attribute _ ls) -> mapMaybe toCase ls
	      _			    -> []

     toCase (ParamExpr e) = Just (Case e)
     toCase (ParamVar v)  = Just (Case (Var v))
     toCase _	          = Nothing
    in
    marshallUnion (idName i) (Right tag_ty) True{-is C union-} switches mb_pack

  FunTy _ _ _  -> marshallFun Nothing i ty
  _            -> return emptyDecl
 where
  getTagTy as = 
	case findAttribute "switch_type" as of
	  Just (Attribute _ [ParamType t])	 -> t
	  _ ->
		case getSwitchIsAttribute as of
		   Just (Cast t _) -> t
		   _ -> Integer Long True

\end{code}

\begin{code}
marshallServ :: Id 
	     -> InterfaceInherit
	     -> [InterfaceDecl]
	     -> CgM HDecl
marshallServ ifaceId inherit decls = do
  let 
      iface_kind
       | not (isObject || isIDispatch ) = 
	    if attrs `hasAttributeWithName` "odl" || optCorba || optJNI then
		VTBLObject
	    else
	        StdFFI
       | isIDispatch  = ComIDispatch isDual
       | otherwise    = VTBLObject

  setInterfaceFlag iface_kind $ do
  setIfaceName iface_name     $ do
  is_src <- getSourceIfaceFlag
  setMethodNumber startOffset
  body <- mapM coGen decls
  ud   <- 
      if is_javeh_interface then
         return emptyDecl
       else 
         uuidDecl ifaceId inherit Iid
  vtbl <- 
     if optJNI then
       cgJClass ifaceId decls
     else
       mkServVTBL ifaceId (is_src && isIDispatch) False decls
  return (infoHeader (Interface ifaceId False inherit decls) `andDecl`
          ud						     `andDecl`
	  vtbl						     `andDecl`
          andDecls body)

 where
  iface_name = idName ifaceId
  attrs	     = idAttributes ifaceId

  is_javeh_interface = optJNI && not is_javeh_class
  is_javeh_class     = optJNI && attrs `hasAttributeWithName` CustomAttributes.jniClassAttr

  coGen d
   | isMethod d = do
         d' <- cgDecl d
	 incMethodNumber
	 return d'
   | otherwise  = cgDecl d

  startOffset
   | iface_name == "IUnknown" = 0
   | otherwise		      = sum (map snd inherit)

  isIDispatch = 
      (isDual && not optDualVtbl) ||
      (any (\ x -> qName (fst x) == "IDispatch" && 
      		   iface_name /= "IDispatchEx") inherit && not (isDual && optDualVtbl))

  isDual   = attrs `hasAttributeWithName` "dual"
  isObject = attrs `hasAttributeWithName` "object" ||
	     any (\ x -> qName (fst x) == "IUnknown") inherit

\end{code}
