% 
% (c) The Foo Project, Universities of Glasgow & Utrecht, 1997-8
%
% @(#) $Docid: Nov. 16th 2001  17:13  Sigbjorn Finne $
% @(#) $Contactid: sof@galconn.com $
%

The CoreIDL interface defines the intermediate representation
used for IDL specifications.

\begin{code}
module CoreIDL where

import Literal
import BasicTypes
import Data.Int ( Int32 )
import TypeInfo
\end{code}

\begin{code}
data Decl
  = Typedef       { declId :: Id, declType :: Type, declOrigType :: Type }
  | Constant      { declId       :: Id
  		  , declType     :: Type
		  , declOrigType :: Type
		  , declExpr     :: Expr
		  }
  | Interface     { declId :: Id
		  , isReference :: Bool -- => True, not a defining occurrence, but a use.
					-- need to make the distinction when dealing with
					--
					--    library {  interface IA; ... } 
					-- 
					-- and 
					-- 
					--    dispinterface { interface IA; }
					--
		  , declInherit :: InterfaceInherit
		  , declDecls :: [InterfaceDecl]
		  }
  | Module        { declId :: Id, declDecls :: [ModuleDecl]  }
  | DispInterface { declId :: Id
		  , dispExpandedFrom :: Maybe Decl 
		          -- Just IA => expanded from "dispinterface DA { interface IA; };"
                  , declProps :: [DispInterfaceDecl]
		  , declDecls :: [DispInterfaceDecl]
		  }
  | CoClass       { declId :: Id, declCoDecls :: [CoClassDecl] }
  | Library       { declId :: Id, declDecls :: [LibraryDecl] }
  | Method        { declId :: Id
		  , methCallConv :: CallConv
		  , methResult   :: Result
		  , methParams   :: [Param]
		  , methOffset   :: Maybe Int   -- used by type libraries only.
		  }
  | Property      { declId     :: Id
  		  , declType   :: Type
		  , propOffset :: Maybe Int
		  , declSetId  :: Id
		  , declGetId  :: Id
		  }
  | HsLiteral	  String -- literal Haskell code.
  | CInclude	  String
  | CLiteral	  String -- literal C code (used when generating header files from IDL spec.)

type InterfaceDecl     = Decl
  -- Typedef, Constant, Method

type ModuleDecl        = Decl
  -- Constant, Method

type DispInterfaceDecl = Decl
  -- Method, Property

data CoClassDecl 
 = CoClassInterface     { coClassId :: Id, coClassDecl :: Maybe Decl }
 | CoClassDispInterface { coClassId :: Id, coClassDecl :: Maybe Decl }

type LibraryDecl       = Decl
  -- any (apart from Library)

type InterfaceInherit = 
   [( QualName
    , Int	    -- Just x => interface contains m methods.
    )]

\end{code}

\begin{code}
data Expr
 = Binary BinaryOp Expr Expr
 | Cond   Expr Expr Expr
 | Unary  UnaryOp Expr
 | Var    Name
 | Lit    Literal
 | Cast   Type Expr
 | Sizeof Type
   deriving ( Eq
{-
-- trick to get conditional code blocks work
-- for Haskell systems that do/do not offer CPP preprocessing.
-- (for systems that don't (i.e. Hugs), the code block will be
-- used.
#ifdef DEBUG
-}
	    , Show
{-
#endif
-}
	    )
\end{code}

The type language for Core IDL:

\begin{code}
data Type
 = Integer Size Signed
 | StablePtr
 | FunTy   CallConv Result [Param]
 | Float   Size
 | Char    Signed
 | WChar
 | Bool   | Octet | Any  | Object
 | String Type Bool (Maybe Expr) | WString Bool (Maybe Expr)
 | Sequence Type (Maybe Expr) (Maybe Expr)
 | Fixed Expr IntegerLit
 | Name     Name
 	    Name		-- original name
 	    (Maybe Name)	-- where it is defined.
	    (Maybe [Attribute]) -- the attributes at its definition site.
	    (Maybe Type)	-- the type it expands to.
	    (Maybe TypeInfo)    -- custom/off-line type info.

 | Struct   Id 
            [Field]
	    (Maybe Int)		-- packed (in bytes.)

 | Enum     Id EnumKind [EnumValue]
 | Union    Id Type Id Id [Switch]
 | UnionNon Id [Switch]    -- non-encapsulated union.
 | CUnion   Id [Field]     -- C-style union
	       (Maybe Int) -- packed (in bytes.)

 | Pointer  PointerType
 	    Bool           -- True  => pointer type was explicitly given
	                   -- False => not explicit, default type.
 	    Type
 | Array    Type [Expr]
 | Void
 | Iface    Name              -- an interface/abstract type
	    (Maybe Name)      -- the module it was imported from.
 	    Name	      -- it's original name
	    [Attribute]       -- attributes used at definition site.
	    Bool	      -- True => derives from IDispatch.
	    InterfaceInherit  -- what it extends (in *reverse* order, i.e., base iface comes first, since
			      -- we're mostly interested in the iface/object's base when marshalling.)

     -- the following are only produced in MS IDL mode:
 | SafeArray Type
   deriving ( Eq
{-
#ifdef DEBUG
-}
	    , Show
{-
#endif
-}
	    )

type Signed = Bool

\end{code}

In Core, we pin attributes on Ids: 

\begin{code}
data Id 
 = Id {
      idName       :: Name,	    -- a unique, Haskellised name
      idOrigName   :: Name,	    -- source name
      idModule     :: Maybe Name,   -- where it is defined, Nothing => local.
      idAttributes :: [Attribute]
   }
   deriving ( Eq
{-
#ifdef DEBUG
-}
	    , Show
{-
#endif
-}
	    )

\end{code}


\begin{code}
data Attribute 
 = AttrMode ParamDir
 | AttrDependent {
      atReason    :: DepReason,
      atParams    :: [AttributeParam]
   }
 | Attribute {
      atName      :: Name,
      atParams    :: [AttributeParam]
   } 
   deriving ( Eq
{-
#ifdef DEBUG
-}
	    , Show
{-
#endif
-}
	    )

data AttributeParam
 = ParamLit    Literal
 | ParamType   Type
 | ParamExpr   Expr
 | ParamVar    Name
 | ParamVoid
 | ParamPtr AttributeParam
    deriving ( Eq
{-
#ifdef DEBUG
-}
	     , Show
{-
#endif
-}
	     )

data DepReason
 = SizeIs  | LengthIs
 | LastIs  | FirstIs
 | MaxIs   | MinIs
 | SwitchIs
   deriving ( Eq, Show )
\end{code}

\begin{code}
data Result =
   Result { 
     resultType     :: Type,  -- used 
     resultOrigType :: Type
   }  
     deriving ( Eq
{-
#ifdef DEBUG
-}
	      , Show
{-
#endif
-}
	      )

data Param
 = Param {
     paramId        :: Id,
     paramMode      :: ParamDir,
     paramType      :: Type,  -- used for marshalling purposes
     paramOrigType  :: Type,  -- un-reduced version.
     paramDependent :: Bool
   }
     deriving ( Eq
{-
#ifdef DEBUG
-}
	      , Show
{-
#endif
-}
	      )

data Switch    
 = Switch {
     switchId	    :: Id,
     switchLabels   :: [CaseLabel],
     switchType     :: Type,
     switchOrigType :: Type
   } 
 | SwitchEmpty  (Maybe [(CaseLabel,String)]) 
                            -- Nothing      => default switch.
			    -- Just [(x,y)] => y is the tag name.
   deriving ( Eq
{-
#ifdef DEBUG
-}
	    , Show
{-
#endif
-}
	    )

data CaseLabel 
 = Case Expr | Default
   deriving ( Eq
{-
#ifdef DEBUG
-}
	    , Show
{-
#endif
-}
	    )

-- for struct & C union fields
data Field 
 = Field {
     fieldId       :: Id,
     fieldType     :: Type,
     fieldOrigType :: Type,
     fieldSize     :: Maybe Int,
     fieldOffset   :: Maybe Int
   } deriving ( Eq
{-
#ifdef DEBUG
-}
	      , Show
{-
#endif
-}
	      )

data EnumValue 
 = EnumValue {
     enumName  :: Id,
     enumValue :: (Either Int32 Expr)
   } deriving ( Eq
{-
#ifdef DEBUG
-}
	      , Show
{-
#endif
-}
	      )

\end{code}
