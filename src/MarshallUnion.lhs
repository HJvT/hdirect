%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Feb. 9th 2003  08:00  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Generating code for the various types of IDL unions.

\begin{code}
module MarshallUnion ( marshallUnion ) where

import BasicTypes ( Name, QualName, qName, BinaryOp(..) )
import AbstractH  ( HDecl )
import AbsHUtils
import Literal ( iLit )
import CgMonad

import CoreIDL
import MarshallMonad
import MarshallType
import MarshallCore ( toHaskellBaseTy )
import CoreUtils
import LibUtils
import Utils        ( trace, traceIf )
import Data.Char    ( ord )
import Opts         ( optCom )

\end{code}

<sect> Encapsulated unions
<label id="sec:encap-union">
<p>

\begin{code}
marshallUnion :: Name 
	      -> Either (Id,Type) Type
	      -> Bool
	      -> [Switch]
	      -> Maybe Int
	      -> CgM HDecl
marshallUnion tdef_name tag_info isCUnion switches_raw mb_pack
 | null switches = trace ("Empty union: "++ tdef_name) $ return emptyDecl
 | otherwise     = do 
    ds <- mapM exportDecl decl_list
    return (andDecls ds)
   where
    decl_list =
     [ ( m_name, m_tysig `andDecl`  m_def)
     , ( u_name, u_tysig `andDecl`  u_def)
     , ( w_name, w_tysig `andDecl`  w_def)
     , ( r_name, r_tysig `andDecl`  r_def)
     , ( s_name, s_tysig `andDecl`  s_def)
     , ( f_name, f_tysig `andDecl`  f_def)
     ]

    switches = moveEmptiesBackwards [] switches_raw 
      where
       moveEmptiesBackwards acc [] = acc
       moveEmptiesBackwards acc (s@(SwitchEmpty _):ss) = moveEmptiesBackwards (s:acc) ss
       moveEmptiesBackwards acc (s:ss) = s: moveEmptiesBackwards acc ss

    name      = mkConName tdef_name

    isEncapsulated =
      case tag_info of
        Left  _ -> True
	Right _ -> False
    
    (un_tag, tag_ty) =
      case tag_info of
        Left (tg,t) -> (tg,t)
	Right t     -> ( error "marshallUnion: not supposed to touch - unencapsulated unions don't have embedded tags"
		       , t
		       )

    un_tag_ty = UnionNon un_tag switches

    expanded_ty_fields
      | isEncapsulated = 
         [ Field un_tag    tag_ty    tag_ty Nothing Nothing -- the tags are ignored, so it
	 , Field un_tag un_tag_ty un_tag_ty Nothing Nothing -- doesn't matter what's used.
	 ]
      | otherwise      =
         [ Field un_tag un_tag_ty un_tag_ty Nothing Nothing -- doesn't matter what's used.
	 ]

    ( (st_size,_), 
      (_: ~(un_off:_))) = computeStructSizeOffsets mb_pack expanded_ty_fields

    v		 = var "v"
    write_tag    = var "write_tag"
    read_tag     = var "read_tag"
    tag          = var "tag"
    ptr          = var "ptr"
    ptr_cast     = castPtr ptr

    addRef_fields = isFinalisedType True  (Union undefined undefined undefined undefined switches)
    final_fields  = isFinalisedType False (Union undefined undefined undefined undefined switches)

    union_ptr 
      | isEncapsulated = addPtr ptr (lit (iLit un_off))
      | otherwise      = ptr_cast

    t_ty         = tyConst tdef_name
    b_ty         = tyConst tdef_name
    b_tag_ty     = toHaskellBaseTy False tag_ty

    m_name   = qName (prefix marshallPrefix name)
    m_tysig  = typeSig m_name (funTy t_ty (io (tuple [b_ty, b_tag_ty])))
    m_def    = funDef m_name [] m_rhs
    m_rhs    = funApp marshUnion [stringLit m_name]

    u_name   = qName (prefix unmarshallPrefix name)
    u_tysig  = typeSig u_name (funTys [b_ty, b_tag_ty] (io t_ty))
    u_def    = funDef u_name [] u_rhs
    u_rhs    = funApp unmarshUnion [stringLit u_name]

    w_name   = qName (prefix marshallRefPrefix name)
    w_tysig 
     | isEncapsulated = typeSig w_name (funTys (mk_w_type [tyPtr b_ty, t_ty]) io_unit)
     | otherwise      = typeSig w_name (funTys (mk_w_type [funTy b_tag_ty io_unit, tyPtr b_ty, t_ty]) io_unit)

    mk_w_type
     | optCom && addRef_fields = (tyBool:)
     | otherwise	       = id

    w_def    
     | isEncapsulated = funDef w_name (mk_w_pats [varPat ptr, varPat v]) w_rhs
     | otherwise      = funDef w_name (mk_w_pats [varPat write_tag, varPat ptr, varPat v]) w_rhs
     
    mk_w_pats
     | optCom && addRef_fields = ((patVar "addRefMe__") :)
     | otherwise	       = id

    w_rhs    = hCase v w_alts
    w_alts   = concatMap mk_w_alt switches
       
    mk_w_alt (Switch i labs ty _) =
	let 
	 elt_nm = idName i
	 elt    = var elt_nm
	 the_tag = 
	   case filter (/=Default) labs of
	     (Case e : _ ) -> coreToHaskellExpr e
	     []         -> 
		traceIf (not isCUnion && null labs)
				   ("Warning: union member `" ++ elt_nm ++ "' of typedef `" ++ 
				     tdef_name ++ "' doesn't have an associated tag") $
	        intLit ((-1)::Int) -- default case, not right.
  
             _ -> error "MarshallUnion.marshallUnion.mk_w_alt: unexpected, something's badly wrong"
	in
	[ alt (conPat (mkConName (mkHaskellTyConName elt_nm)) [varPat elt]) $
	     bind_ (if isEncapsulated then
	              funApply (refMarshallType stubMarshallInfo tag_ty) [ptr_cast, the_tag]
		    else
		       funApply write_tag [the_tag]) $
	     -- assume that enough space have been allocated for us to fill
	     -- in the tag's value. I'm not sure this is always the case!
	     -- For instance, if the tag's value is a string, we will
	     -- just have allocated space for a char* pointer, not the
	     -- string (i.e., size is currently considered a property of
	     -- the type and not any different from values of that type.)
	     --
	     -- However, as a first approximation, this will have to do..
	     -- 
	     -- ToDo: revisit this issue (hopefully before users are bitten by this!)
	     -- 
	    let un_ptr
	         | isEncapsulated = addPtr ptr (lit (iLit un_off))
		 | otherwise	  = ptr_cast
	    in
	    funApply (refMarshallType structMarshallInfo ty) [un_ptr, elt]
       ]

     -- not much we can do but match the anonymous constructor, i.e.,
     --		Foo -> return ()
    mk_w_alt (SwitchEmpty mb_labs) = 
       case mb_labs of
         Nothing -> [alt (conPat (mkConName (tdef_name ++ "_Anon")) []) (ret unit)]
	 Just ls -> map toAlt ls
	  where
	   toAlt (Default, tg_nm) = 
	        alt (conPat (mkConName (tdef_name ++ tg_nm)) [])
		    (ret unit)
	   toAlt (Case e, tg_nm)  =
	        alt (conPat (mkConName (tdef_name ++ tg_nm)) [])
		    (bind_
		      (funApply (refMarshallType stubMarshallInfo tag_ty) [ptr, coreToHaskellExpr e])
		      (ret unit) -- why do I need this?
		      )

    r_name   = qName (prefix unmarshallRefPrefix name)
    r_tysig  
      | isEncapsulated = typeSig r_name (funTys (mk_r_type [tyPtr b_ty]) (io t_ty))
      | otherwise      = typeSig r_name (funTys (mk_r_type [io b_tag_ty, tyPtr b_ty]) (io t_ty))

    mk_r_type
      | final_fields   = (tyBool:)
      | otherwise      = id

    r_def    
      | isEncapsulated = funDef r_name (mk_r_pats [varPat ptr]) r_rhs
      | otherwise      = funDef r_name (mk_r_pats [varPat read_tag, varPat ptr]) r_rhs
    
    mk_r_pats
      | final_fields   = (patVar "finaliseMe__":)
      | otherwise      = id

    r_rhs   = 
      mkAlts 
          r_name
          (if isEncapsulated then
	     funApply (refUnmarshallType stubMarshallInfo tag_ty) [ptr_cast]
	   else
	     read_tag)
	  tag
	  (concat $ map mkGuard switches)

    mkAlts nm _ _ [] = 
        trace ("Warning: `" ++ nm ++"': no tag info to interpret. (generating bogus stub - please fix it or the original IDL spec.)") $
	funApp prelError [stringLit (nm ++": I am the " ++ thing)]
       where
        sum_nm = foldr (\ x acc -> ord x + acc) 0 nm

        thing
	 | odd sum_nm = "walrus."
	 | otherwise  = "eggman."
	 
    mkAlts _ rtag tg alts = bind rtag tg (hCase tag alts)

    mkGuard (Switch i labs ty _) = map mkSwitch labs
     where
      mkSwitch Default  = 
	 alt wildPat
	     (bind (funApply (refUnmarshallType structMarshallInfo ty) [union_ptr]) v $
	      ret  (dataCon (mkConName (mkHaskellTyConName (idName i))) [v]))

      mkSwitch (Case e) = 
	let h_expr = coreToHaskellExpr e in
	case (exprToPat h_expr) of
	  Just simple_pat -> 
		alt simple_pat
		    (bind (funApply (refUnmarshallType structMarshallInfo ty) [union_ptr]) v $
		     ret  (dataCon (mkConName (mkHaskellTyConName (idName i))) [v]))
	   -- either totally bogus or a non-simple expression
	   -- Emit a 
	  Nothing -> 
		genAlt (patVar "x") (binOp Eq (var "x") h_expr)
		    (bind (funApply (refUnmarshallType structMarshallInfo ty) [union_ptr]) v $
		     ret  (dataCon (mkConName (mkHaskellTyConName (idName i))) [v]))
		       
		    
    mkGuard (SwitchEmpty mb_labs) =
       case mb_labs of
         Nothing -> [alt wildPat $
	             ret (dataCon (mkConName (tdef_name ++ "_Anon")) [])]
	 Just ls ->
	   map (\ x -> alt wildPat (ret (dataCon (mkConName (tdef_name ++ x)) [])))
	       ls_nm
	   where
	    ls_nm = map snd ls

    s_name   = qName (prefix sizeofPrefix name)
    s_tysig  = typeSig s_name tyWord32
    s_def    = funDef s_name [] s_rhs
    s_rhs    = intLit st_size

     -- the auto-generated free routine releases a packed rep.
     -- of the union - perhaps also generate one for the unpacked
     -- version?

    f_name   = qName (prefix freePrefix name)
    f_tysig  
       | isEncapsulated = typeSig f_name (funTy (tyPtr b_ty) (io_unit))
       | otherwise      = typeSig f_name (funTys [b_tag_ty, tyPtr b_ty] (io_unit))

    f_def    
       | isEncapsulated = funDef f_name [varPat ptr] f_rhs
       | otherwise      = funDef f_name [varPat read_tag, varPat ptr] f_rhs
    f_rhs    = 
       mkAlts
          f_name
          (if isEncapsulated then
               funApply (refUnmarshallType stubMarshallInfo tag_ty) [ptr]
	   else
	       ret read_tag)
	  tag
	  (concat $ map freeGuard switches)
     where
       freeGuard (Switch _ labs _ orig_ty) = map mkSwitch labs
        where
	 free_it
	  | not (needsFreeing orig_ty) = ret unit
	  | otherwise                  = funApply (freeType orig_ty) [union_ptr]

         mkSwitch Default  = alt wildPat free_it
         mkSwitch (Case e) = 
	    let h_expr = coreToHaskellExpr e in
	    case (exprToPat h_expr) of
	      Just pat -> alt pat free_it
	      Nothing  -> genAlt (patVar "x") (binOp Eq (var "x") h_expr) free_it

       freeGuard (SwitchEmpty _) = [alt wildPat (ret unit) ]

\end{code}
