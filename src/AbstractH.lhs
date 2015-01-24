%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Feb. 8th 2003  06:45  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

A (restricted) abstract syntax for Haskell.

\begin{code}
module AbstractH where

import Literal
import BasicTypes
\end{code}

The @HTopDecl@ includes Haskell modules, but also
an escape mechanism for inserting stuff that goes
outside it (literate markup/pragmas etc.)

\begin{code}
data HTopDecl
 = HMod     HModule
 | HInclude String
 | HLit     String  -- ToDo: nuke.
 | CLit     String
     -- literal code/comments/whatever that appears outside a module block.
\end{code}

Represent a Haskell module abstractly by @HModule@, which

\begin{code}
data HModule = 
  HModule
    Name
    Bool    -- True => module contains FFI declarations (of some sort).
    [HExport]
    [HImport]
    HDecl
\end{code}

\begin{code}
data HExport = HExport HIEEntity (Maybe String)

data HIEEntity
 = IEModule Name
 | IEVal    Name
 | IEClass  Name
 | IEType   Name Bool{-abstractly?-}
 deriving Eq
\end{code}

\begin{code}
data HImport 
  = HImport 
      Bool                  --qualified?
      (Maybe Name)          --qualify as
      Name                  --module name
      (Maybe [HIEEntity])   -- stuff to import, 
                            --   Nothing => the lot.
                            --   Just [] => ()  (i.e., bring instances into scope.)
\end{code}


\begin{code}
data HDecl 
 = AndDecl HDecl HDecl  -- for easy glomming together
                        -- of code fragments.
 
    -- function signature foo :: ctxt => type
 | TypeSig  Name (Maybe Context) Type  

    -- f a1 .. an 
    --   | g_1 = rhs_1
    --     ...
    --   | g_n = rhs_n
    --
    --
    -- Note: allow qualified names here, which is
    -- illegal for toplevel valdecls in Haskell, but
    -- required (by Haskell 98 and ghc) for class
    -- methods in instance decls if the module that
    -- defines the class is imported qualified.
    -- (Unfortunately, Hugs does not currently allow
    -- said qualified method names, so a separate option
    -- is provided for turning off the use of qualified
    -- names on valdecls.)
 | ValDecl  QualName [Pat] [GuardedExpr]

    -- primitive stdcall ["winmm.TimeGetTime"] [safe] timeGetTime :: ty
 | Primitive Bool{-safe?-} CallConv LocSpec Name Type Bool [(Bool,String)] (Bool,String)

    -- primcast stdcall applyIntFun :: Addr -> (Int -> IO Int)
 | PrimCast CallConv Name Type Bool [(Bool,String)] (Bool,String)

    -- entry stdcall ["tmgtm"] timeGetTime :: ty
 | Entry CallConv Name{-C name-} Name{-H name-} Type

    -- callback stdcall mkIntFun :: (Int -> IO Int) -> IO Addr
 | Callback CallConv Name Type
 
    -- 'foreign label'
 | ExtLabel Name{-C name-} Name{-H name-} Type
    -- (type|newtype|data) Nm a1..an = ty
 | TyD TyDecl

    -- class ctxt    => CName a1 [..an] where { decls }
 | Class Context ClassName [TyVar] [HDecl] -- sigs and def. meths

    -- instance ctxt => CName ty where { .. decls .. }
 | Instance Context ClassName Type [HDecl] -- val decls only.

    -- %#include "foo" or {-# OPTIONS -#include "foo" #-}
 | Include String -- ghc/gc specific - will disappear eventually.

 | Haskell String -- Haskell code brought along from source file.
 | CCode   String -- C code

 | EmptyDecl

type LocSpec = (String, Maybe Integer, String, Maybe Int)

type ClassName = QualName

data Context
 = CtxtTuple [Context]   -- (C1 .., C2 ..) =>
 | CtxtClass ClassName [Type]
   deriving ( Eq, Show )
\end{code}

@Type@ represent Haskell types - no explicit
management of the scoping of type variables.

\begin{code}
data Type
 = TyVar   Bool TyVar
 | TyCon   TyCon 
 | TyApply Type [Type]
 | TyList  Type
 | TyTuple [Type]
 | TyFun   Type Type
    -- abstract syntax allows contexts to be added to any types - makes
    -- generation of overloaded tysigs easier.
 | TyCtxt  Context Type
   deriving ( Eq, Show ) 
\end{code}

Patterned on @HsDecls.TyDecl@ (no support for
tycon contexts tho')

\begin{code}
data TyDecl
 = TypeSyn Name [Name] Type
 | TyDecl TyDeclKind
          Name          -- type constructor
          [Name]        -- type vars
          [ConDecl]     -- data constructors
          [QualName]    -- derivings


data TyDeclKind = Data | Newtype

data ConDecl
 = ConDecl Name [BangType]
 | RecDecl Name [(Name, BangType)]

data BangType = Banged Type | Unbanged Type

type TyVar = QualName
type TyCon = QualName
\end{code}

\begin{code}
data GuardedExpr = GExpr [Expr] Expr

data Expr
 = Lit    Literal
 | Lam    [Pat] Expr
 | Apply Expr [Expr]
 | RApply Expr Expr
 | Tup    [Expr]
 | List   [Expr]
 | InfixOp Expr VarName Expr
 | BinOp  BinaryOp Expr Expr
 | UnOp   UnaryOp  Expr
 | Bind   Expr Pat Expr
 | Bind_  Expr Expr
 | Return Expr
 | Case   Expr [CaseAlt]
 | If     Expr Expr Expr
 | Let    [Binding] Expr
 | Var    VarName
 | Con    ConName
 | WithTy Expr Type

data Binding = Binder Name Expr

type VarName = QualName
type ConName = QualName

\end{code}

\begin{code}
data Pat
 = PatVar VarName
 | PatLit Literal
 | PatWildCard
 | PatTuple [Pat]
 | PatAs   VarName Pat
 | PatCon  ConName [Pat]
 | PatList [Pat]
 | PatIrrefut Pat
 | PatRecord VarName [(VarName,Pat)]

data CaseAlt
 = Alt Pat [GuardedExpr]
 | Default (Maybe VarName) Expr
\end{code}
