%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Feb. 8th 2003  07:20  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Pass to construct import lists from a list of Haskell
declarations.
 
\begin{code}
module MkImport 
       (
        mkImportLists
       ) where

import AbstractH
import AbsHUtils ( ieType, andDecls, ieValue, mkQVarName, tyInt32Name )
import Bag
import Env
import LibUtils  ( bitsLib, prelude, fromEnumName, toEnumName )
import BasicTypes

import Data.List   ( nub )
import Opts   ( optNoImportLists, optQualInstanceMethods, optNoQualNames,
		optLongLongIsInteger )
import Utils  ( concMaybe )
\end{code}

\begin{code}
mkImportLists :: String
              -> [QualName]
	      -> [HDecl]
	      -> [(String, Bool, [HIEEntity])]
mkImportLists local_nm hs_imports decls = import_list
  where
   import_list = 
		    -- sigh, to avoid pointless trouble with the generated
		    -- sources due to Hugs' lack of support for qualified
		    -- instance names, we optionally allow method names
		    -- to be emitted in unqual'ed form. GHC (quite rightfully)
		    -- complains when it is fed such input, since it has
		    -- only got the qualified method name in scope.
		    --
		    -- To avoid the unfortunate situation that GHC and Hugs
		    -- require separate Haskell stubs for normal stuff, we
		    -- explicitly bring the unqual'ed Enum instance methods
		    -- into scope.
		    --
		    -- A hack that doesn't solve the general problem.
		    -- 
   		 (if not optQualInstanceMethods && not optNoQualNames then 
		     (("Prelude", False, [ ieValue (qName fromEnumName)
		     			 , ieValue (qName toEnumName)
					 ]):)
		  else
		     id) $
		 ("Prelude", True, []):
   		 filter ofInterest (map mkImpList (envToList (go (andDecls decls) new_env )))

   new_env
     | optLongLongIsInteger = addQName (\ nm -> ieType nm False) tyInt32Name base_env
     | otherwise	    = base_env

   base_env = foldr (addQName ieValue) newEnv hs_imports

   ofInterest ("Prelude",_,_) = False
   ofInterest (nm, _, _)      = nm /= local_nm

   mkImpList (x, bag)	    = (x, True, nub (bagToList bag))

   go :: HDecl -> Env String (Bag HIEEntity) -> Env String (Bag HIEEntity)
   go (AndDecl d1 d2)	         env = go d2 (go d1 env)
   go (TypeSig _ ctxt ty)        env = gatherTyImports ty (gatherTyContext ctxt env)
   go (ValDecl _ _ es)	         env = foldr gatherGExprImports env es
   go (TyD td)		         env = gatherTyDeclImports td env
   go (Primitive _ _ _ _ ty _ _ _) env = gatherTyImports ty env
   go (PrimCast _ _ ty _ _ _ )     env = gatherTyImports ty env
   go (Entry _ _ _ ty)           env = gatherTyImports ty env
   go (Callback _ _ ty)          env = gatherTyImports ty env
   go (ExtLabel _ _ ty)          env = gatherTyImports ty env
   go (Instance ctxt cname t ds) env =
       gatherTyContext (Just ctxt) $
       addQName (\ nm -> ieType nm False) cname $
       gatherTyImports t $
       foldr go env ds
   go _                        env = env

gatherTyImports :: Type -> Env String (Bag HIEEntity) -> Env String (Bag HIEEntity)
gatherTyImports ty env = 
 case ty of
   TyVar _ tv     -> addQName (\ nm -> ieType nm True) tv env 
   TyCon tc	  -> addQName (\ nm -> ieType nm True) tc env 
   TyApply f args -> foldr gatherTyImports env (f:args)
   TyList t	  -> gatherTyImports t env
   TyTuple es	  -> foldr gatherTyImports env es
   TyCtxt c t     -> gatherTyContext (Just c) (gatherTyImports t env)
   TyFun f a	  -> gatherTyImports a (gatherTyImports f env)

addQName :: (String -> a) -> QualName -> Env String (Bag a) -> Env String (Bag a)
addQName f qn env = 
   case concMaybe (qDefModule qn) (qModule qn) of
     Nothing  -> env
     v@(Just md)
	    -- don't record what we're picking up from the Prelude.
        | v == prelude -> env  
	| otherwise    -> addToEnv_C (unionBags) env md nm
       where
        nm 
	 | optNoImportLists = emptyBag
	 | otherwise        = unitBag (f (qName qn))

gatherTyDeclImports :: TyDecl -> Env String (Bag HIEEntity) -> Env String (Bag HIEEntity)
gatherTyDeclImports (TypeSyn _ _ t) env     = gatherTyImports t env
gatherTyDeclImports (TyDecl _ _ _ ds _) env = foldr gatherConDeclImport env ds
 where
  gatherConDeclImport (ConDecl _ ts) env1 = foldr gatherTyImports env1 (map debang ts)
  gatherConDeclImport (RecDecl _ fs) env1 = foldr gatherTyImports env1 (map (debang.snd) fs)
  
  debang (Banged t)   = t
  debang (Unbanged t) = t


gatherTyContext :: Maybe Context -> Env String (Bag HIEEntity) -> Env String (Bag HIEEntity)
gatherTyContext Nothing env = env
gatherTyContext (Just ctxt) env = go ctxt env
  where
   go (CtxtClass nm _) env1 = addQName (\ n -> ieType n True) nm env1
   go (CtxtTuple ls)   env1 = foldr go env1 ls

gatherGExprImports :: GuardedExpr -> Env String (Bag HIEEntity) -> Env String (Bag HIEEntity)
gatherGExprImports (GExpr es e) env = foldr gatherExprImports env (e:es)

gatherExprImports :: Expr -> Env String (Bag HIEEntity) -> Env String (Bag HIEEntity)
gatherExprImports e env =
 case e of
   Lam _ e1 -> gatherExprImports e1 env
   Apply f args -> foldr gatherExprImports env (f:args)
   Tup es -> foldr gatherExprImports env es
   List es -> foldr gatherExprImports env es
   BinOp bop e1 e2  -> gatherExprImports e2 (gatherExprImports e1 (gatherBinOpImports bop env))
   InfixOp e1 op e2 -> gatherExprImports e1 (gatherExprImports e2 (addQName (ieValue) op env))
   RApply e1 e2  -> gatherExprImports e1 (gatherExprImports e2 env)
   UnOp op e1    -> gatherExprImports e1 (gatherUnaryOpImports op env)
   Bind e1 _ e2  -> gatherExprImports e2 (gatherExprImports e1 env)
   Bind_ e1 e2   -> gatherExprImports e2 (gatherExprImports e1 env)
   Return e1     -> gatherExprImports e1 env
   Case e1 alts  -> foldr gatherAltImports (gatherExprImports e1 env) alts
   If e1 e2 e3   -> gatherExprImports e1 (gatherExprImports e2 (gatherExprImports e3 env))
   Let binds e1  -> foldr gatherBindingImports (gatherExprImports e1 env) binds
   Var v 	 -> addQName (ieValue) v env 
   Con c	 -> addQName (ieValue) c env
   WithTy e1 ty  -> gatherExprImports e1 (gatherTyImports ty env)
   _		 -> env

   where
     gatherBindingImports (Binder _ e1) env1 = gatherExprImports e1 env1

     gatherAltImports (Alt _ gs) env1    = foldr gatherExprImports env1 es
       where
        es = concatMap (\ (GExpr gs1 e1) -> e1:gs1) gs
     gatherAltImports (Default _ e1) env1 = gatherExprImports e1 env1


     gatherUnaryOpImports op env1 =
        case op of
	  Not -> addBitsImp "complement" env1
	  _   -> env1

     gatherBinOpImports op env1 =
        case op of
	  Xor -> addBitsImp "xor" env1
	  Or  -> addBitsImp "(.|.)" env1
	  And -> addBitsImp "(.&.)" env1
	  Shift L -> addBitsImp "shiftL" env1
	  Shift R -> addBitsImp "shiftR" env1
	  _ -> env1
	  
	  
     addBitsImp nm env1 = addQName ieValue (mkQVarName bitsLib nm) env1

\end{code}
