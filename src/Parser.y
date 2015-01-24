{
{-
%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
% @(#) $Docid: Jan. 15th 2004  10:33  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

A grammar for IDL, DCE / MS (IDL/ODL) style.

Conflicts:
   - 1 reduce/reduce conflict due to `default'
     both being an attribute and a keyword.
   - 7 shift/reduce conflicts due to the overloading
     of `const' (t. qualifier and keyword.)

ToDo: 
  - fix above conflicts.

-}
module Parser ( parseIDL ) where
 
import LexM
import Lex
import IDLToken
import IDLSyn
import IDLUtils ( mkFunId, mkMethodId, toCConvAttrib,
                  mkGNUAttrib, toPackedAttrib, exprType )
import BasicTypes
import Literal
import System.IO ( hPutStrLn, stderr )
{-
BEGIN_GHC_ONLY
import GlaExts
END_GHC_ONLY
-}
}

%name parseIDL
%tokentype { IDLToken }
%monad { LexM } {thenLexM } { returnLexM }
%lexer { lexIDL } { T_eof }
%token
	';'	      { T_semi }
        MODULE	      { T_module }
        INTERFACE     { T_interface }
        '('           { T_oparen }
        ')'           { T_cparen }
        '{'           { T_ocurly }
        '}'           { T_ccurly }
        ':'           { T_colon  }
        ','           { T_comma }
        '.'           { T_dot }
        '...'         { T_dotdotdot }
        CONST         { T_const }
        VOLATILE      { T_volatile }
        '='           { T_equal }
        '=='          { T_eqeq }
        '!='          { T_neq }
        '|'           { T_or }
        '||'          { T_rel_or }
        '^'           { T_xor }
        '&'           { T_and }
        '&&'          { T_rel_and }
        SHIFT         { T_shift $$ }
        '/'           { T_div }
        '%'           { T_mod }
        '~'           { T_not }
        '!'           { T_negate }
        '?'           { T_question }
        TYPEDEF       { T_typedef }
        EXTERN        { T_extern }
        TYPE          { T_type $$ }
	ITYPE         { T_idl_type $$ }
        FLOAT         { T_float $$ }
	SHORT         { (T_int Short) }
	LONG          { (T_int Long) }
	LONGLONG      { (T_int LongLong) }
	ULONGLONG     { (T_uint LongLong) }
        INT           { (T_int Natural) }
        UNSIGNED      { T_unsigned }
        SIGNED        { T_signed }
        CHAR          { T_char }
        WCHAR         { T_wchar }
        STRUCT        { T_struct }
        UNION         { T_union }
        SWITCH        { T_switch }
        CASE          { T_case }
        DEFAULT       { T_default }
        ENUM          { T_enum }
        '<'           { T_lt }
        '<='          { T_le }
        '>'           { T_gt }
        '>='          { T_ge }
        '['           { T_osquare }
        ']'           { T_csquare }
        SIZEOF        { T_sizeof }
        VOID          { T_void }
        MODE          { T_mode $$ }
        LITERAL       { T_literal $$ }
        STRING_LIT    { T_string_lit $$ }
	CALLCONV      { T_callconv $$ }
	ID	      { T_id $$ }
	DISPINTERFACE { T_dispinterface }
	COCLASS       { T_coclass }
	LIBRARY       { T_library }
	'+'	      { T_plus }
	'*'	      { T_times }
	'-'	      { T_minus }
	STRING        { T_string }
	WSTRING       { T_wstring }
	METHODS	      { T_methods }
	PROPERTIES    { T_properties }
	CPP_QUOTE     { T_cpp_quote }
	HS_QUOTE      { T_hs_quote }
	INCLUDE	      { T_include $$ }
        IMPORTLIB     { T_importlib }
        INCLUDE_START { T_include_start $$ }
        INCLUDE_END   { T_include_end }
	ATTRIBUTE     { T_gnu_attribute }
        IMPORT        { T_import }
	PRAGMA        { T_pragma $$ }
        HDEFINE       { T_hdefine }
	SAFEARRAY     { T_safearray }
        UNKNOWN       { T_unknown $$ }
%%

{-
 Share the same parser for DCE IDL input and ASFs. To avoid
 running into gratuitous conflicts, ASF input is assumed to
 be prefixed by a '=' (inserted by the compiler.)
-}

specification :: { Either [Defn] [(Name, Bool, [Attribute])] }
   : definitions	     { Left  (reverse $1) }
   | '=' attr_defs           { Right (reverse $2) }

definitions  :: { [Defn] }
   :                         {    []   }
   | definitions definition  { $2 : $1 }

attr_defs :: { [(String, Bool, [Attribute])] }
   : {- empty -}             {    []   }
   | attr_defs attr_def      { $2 : $1 }

attr_def :: { (String, Bool, [Attribute] ) }
   : attr_name ':' opt_attributes  { ($1, True, $3) }
   | attr_name '=' opt_attributes  { ($1, False, $3) }

attr_name :: { String }
   : ID    { $1 }
   | TYPE  { $1 }

definition   :: { Defn }
   : type_dcl      ';'            { $1 }
   | DISPINTERFACE identifier ';' {% let (Id i) = $2 in addIfaceTypedef i >>= \ v -> return (Forward v) }
   | attributed    semi           { $1 }
   | cpp_quote     semi           { $1 }
   | hs_quote      semi           { $1 }
   | define                       { $1 }
   | import_dcl    ';'            { $1 }
   | pragma_dcl                   { $1 }

attributed   :: { Defn }
   : attributes attr_defn { Attributed $1 $2 }
   | attr_defn            { $1 }

attr_defn    :: { Defn }
   : interface_dcl             {         $1 }
   | MODULE identifier         { Forward $2 }
   | MODULE identifier  '{' definitions semi '}'
                               { Module $2 (reverse $4) }
   | dispinterface             { $1 }
   | COCLASS identifier '{' coclass_members ';' '}'
                               {% let (Id i) = $2 in addIfaceTypedef i >>= \ v -> return (CoClass v (reverse $4)) }
   | COCLASS TYPE '{' coclass_members ';' '}'
                               { CoClass (Id $2) (reverse $4) }
   | LIBRARY identifier '{' definitions '}'
	                       { Library $2 (reverse $4) }
   | op_decl                   { $1 }

{- op_decls at the toplevel isn't legal, but we allow it to be parsed
   anyhow, and report the presence of these later on -}

dispinterface :: { Defn }
   : dispinterface_hdr '{'
       PROPERTIES ':' attr_id_list 
       METHODS    ':' method_decls
     '}'    {% let (Id i) = $1 in addIfaceTypedef i >>= \ v -> return (DispInterface v $5 (reverse $8)) }
   | dispinterface_hdr '{' 
       INTERFACE simple_declarator ';'
     '}'    {% let (Id i) = $1 in addIfaceTypedef i >>= \ v -> return (DispInterfaceDecl v $4) }

attr_id_list  :: { [([Attribute], Type, Id)] }
   : {- empty -} 			            { [] } 
   | attr_id_list opt_attributes type_spec declarator ';' { ($2,$3,$4):$1 }

coclass_members :: { [CoClassMember] }
   : coclass_member 		        {   [$1]  }
   | coclass_members ';' coclass_member { $3 : $1 }

coclass_member  :: { CoClassMember }
   : opt_coclass_attrs INTERFACE TYPE           { (True,  Id $3, $1) }
   | opt_coclass_attrs DISPINTERFACE TYPE       { (False, Id $3, $1) }
   | opt_coclass_attrs INTERFACE identifier     { (True,  $3, $1) }
   | opt_coclass_attrs DISPINTERFACE identifier { (False, $3, $1) }

opt_coclass_attrs :: { [Attribute] }
   : {- empty -}                   {  [] }
   | '[' cc_attributes ']'         {  $2 }

interface_dcl :: { Defn }
   : interface_hdr { Forward $1 }
   | interface_hdr inheritance_spec '{' exports '}' 
       { Interface $1 $2 (reverse $4) }

{- 
  We eagerly add the name of an interface as a type name, so
  that typedefs inside the interface body can refer to it.
-}

interface_hdr :: { Id }
   : INTERFACE identifier {% let (Id i) = $2 in addIfaceTypedef i >>= return  }
   | INTERFACE TYPE       { (Id $2) }
   | INTERFACE ITYPE      { (Id "Object") }

dispinterface_hdr :: { Id }
   : DISPINTERFACE identifier {% let (Id i) = $2 in addIfaceTypedef i >>= return  }
   | DISPINTERFACE TYPE       { (Id $2) }

cpp_quote  :: { Defn }
   : CPP_QUOTE '(' STRING_LIT ')' { CppQuote $3 }

hs_quote  :: { Defn }
   : HS_QUOTE '(' STRING_LIT ')' { HsQuote $3 }
   | INCLUDE  			 { CInclude $1 }

exports   :: { [Defn] }
   : {-empty -}      {    []   }
   | exports export  { $2 : $1 }


export    :: { Defn }
   :  attributed_exp  ';' { $1 }
   |  type_dcl   ';'      { $1 }
   |  cpp_quote semi      { $1 }
   |  hs_quote  semi      { $1 }
   |  import_dcl ';'      { $1 }

attributed_exp :: { Defn }
   : attributes op_decl  { Attributed $1 $2 }
   | op_decl             { $1 }

inheritance_spec :: { Inherit }
   : 	            { [] }
   | ':' TYPE       { [ $2 ]  }
   | ':' identifier { let (Id i) = $2 in [ i ]  }

import_dcl     :: { Defn }
   : IMPORT string_lit_list       {% slurpImports (parseIDL >>= \ (Left y) -> return y) $2 }
   | IMPORTLIB '(' STRING_LIT ')' {% handleImportLib (parseIDL >>= \ (Left y) -> return y) $3 }

{- No validation here of what's on the #pragma line -}
pragma_dcl  :: { Defn }
   : PRAGMA 		       { Pragma $1 }
   | INCLUDE_START	       { IncludeStart $1 }
   | INCLUDE_END               { IncludeEnd }
   
define :: { Defn }
   : HDEFINE ID const_expr     { Constant (Id $2) [] (exprType (TyInteger Natural) $3) $3 }

string_lit_list :: { [String] }
   : STRING_LIT			  { [ $1 ] }
   | STRING_LIT ',' string_lit_list { ($1 : $3) }

{- 
 Not using type_spec for constant types minimises
 the shift/reduce conflicts (i.e., no CONST qualifiers
 allowed below.)
-}
const_type    :: { Type }
   : integer_ty         { $1 }
   | CHAR               { TyChar  }
   | WCHAR              { TyWChar }
   | FLOAT              { TyFloat $1 } 
   | VOID		{ TyVoid }
   | VOID '*'           { TyPointer TyVoid }
   | CHAR '*'           { TyString Nothing }
   | WCHAR '*'          { TyWString Nothing }
   | string_type        { $1 }
   | const_type '[' ']' { TyArray $1 [] }
     -- my word, what junk C allows here.
   | SIGNED               { TySigned True  }
   | UNSIGNED             { TySigned False }
   | UNSIGNED integer_ty        { TyApply (TySigned False) $2 }
   | SIGNED   integer_ty        { TyApply (TySigned True)  $2 }
   | UNSIGNED CHAR        { TyApply (TySigned False) TyChar }
   | integer SIGNED INT   { TyApply (TySigned True)  $1 }
   | integer UNSIGNED INT { TyApply (TySigned False) $1 }
   | SIGNED CHAR        { TyApply (TySigned True)  TyChar }
   | ID                 { TyName $1 Nothing }
   | TYPE               { TyName $1 Nothing }
   | TYPE '*'           { TyPointer (TyName $1 Nothing) }
   | ITYPE		{ $1 }
   | error              {% dumpErrMsg >> return TyVoid }

integer :: { Type }
   : SHORT      { TyInteger Short }
   | LONG       { TyInteger Long  }
   | LONG LONG  { TyInteger LongLong }
   | LONGLONG   { TyInteger LongLong }
   | ULONGLONG  { TyApply (TySigned False) (TyInteger LongLong) }

integer_ty :: { Type }
   : integer      { $1 }
   | integer INT  { $1 }
   | INT          { TyInteger Natural }

const_type_cast    :: { Type }
   : integer_ty         { $1 }
   | CHAR               { TyChar  }
   | WCHAR              { TyWChar }
   | FLOAT              { TyFloat $1 } 
   | VOID		{ TyVoid }
   | string_type        { $1 }
   | const_type '[' ']' { TyArray $1 [] }
   | SIGNED               { TySigned True  }
   | UNSIGNED           { TySigned False }
   | UNSIGNED integer_ty        { TyApply (TySigned False) $2 }
   | SIGNED integer_ty    { TyApply (TySigned True)  $2 }
   | integer UNSIGNED INT { TyApply (TySigned False) $1 }
   | integer SIGNED INT   { TyApply (TySigned True)  $1 }
   | UNSIGNED CHAR        { TyApply (TySigned False) TyChar }
   | SIGNED CHAR          { TyApply (TySigned True)  TyChar }
   | TYPE                 { TyName $1 Nothing }
   | ITYPE		  { $1 }
   | error                {% dumpErrMsg >> return TyVoid }

const_expr    :: { Expr }
   : cond_expr	 { $1 }

cond_expr     :: { Expr }
   : log_or_expr		              { $1 }
   | cond_expr '?' const_expr ':' log_or_expr { Cond $1 $3 $5 }

log_or_expr   :: { Expr }
   : log_and_expr			{ $1 }
   | log_or_expr '||' log_and_expr	{ Binary LogOr $1 $3 }
   
log_and_expr  :: { Expr }
   : or_expr			 { $1 }
   | log_and_expr '&&' or_expr   { Binary LogAnd $1 $3 }

or_expr     :: { Expr }
   : xor_expr	           { $1 }
   | or_expr '|' xor_expr  { Binary Or $1 $3 }

xor_expr      :: { Expr }
   : and_expr	           { $1 }
   | xor_expr '^' and_expr { Binary Xor $1 $3 }

and_expr      :: { Expr }
   : eq_expr		  { $1 }
   | and_expr '&' eq_expr { Binary And $1 $3 }

eq_expr       :: { Expr }
   : rel_expr			{ $1 }
   | eq_expr eq_op rel_expr	{ Binary Eq $1 $3 }

eq_op :: { BinaryOp }
   : '=='  { Eq }
   | '!='  { Ne }

rel_expr      :: { Expr }
   : shift_expr			{ $1 }
   | rel_expr '<'  shift_expr   { Binary Lt $1 $3 }
   | rel_expr '<=' shift_expr   { Binary Le $1 $3 }
   | rel_expr '>=' shift_expr   { Binary Ge $1 $3 }
   | rel_expr '>'  shift_expr   { Binary Gt $1 $3 }

shift_expr    :: { Expr }
   : add_expr		    { $1 }
   | shift_expr SHIFT add_expr  { Binary (Shift $2) $1 $3 }

add_expr      :: { Expr }
   : mult_expr		 { $1 }
   | add_expr '+' mult_expr  { Binary Add $1 $3 }
   | add_expr '-' mult_expr  { Binary Sub $1 $3 }

mult_expr     :: { Expr }
   : cast_expr	         { $1 }
   | mult_expr '*' cast_expr { Binary Mul $1 $3 }
   | mult_expr '/' cast_expr { Binary Div $1 $3 }
   | mult_expr '%' cast_expr { Binary Mod $1 $3 }

cast_expr     :: { Expr }
   : unary_expr		      { $1 }
   | '(' const_type_cast ')' cast_expr { Cast $2 $4 }

unary_expr    :: { Expr }
   : primary_expr  	          { $1 }
   | SIZEOF '(' const_type ')'    { Sizeof $3 }
   | unary_operator primary_expr  { Unary $1 $2}

unary_operator :: { UnaryOp }
   : '-' { Minus } | '+' { Plus } | '~' { Not } | '!' { Negate }

primary_expr  :: { Expr }
   : identifier  	 { let (Id i) = $1 in Var i }
   | LITERAL	         { Lit $1 }
   | STRING_LIT	         { Lit (StringLit $1) }
   | '(' const_expr ')'  { $2 }

type_dcl      :: { Defn }
   : mb_gnu_attributes TYPEDEF opt_attributes type_spec declarators mb_gnu_attributes 
     {% let decls = reverse $5 in addTypes decls >> return (Typedef $4 $3 decls) }

   | attributes struct_or_union_or_enum_spec               { Attributed $1 (TypeDecl $2) }
   | struct_or_union_or_enum_spec 		           { TypeDecl $1 }
   | attributes CONST const_type identifier '=' const_expr { Constant $4 $1 $3 $6 }
   | CONST const_type identifier '=' const_expr            { Constant $3 [] $2 $5 }
   | mb_gnu_attributes EXTERN mb_gnu_attributes type_spec declarators mb_gnu_attributes
     {% let decls = reverse $5 in addTypes decls >> return (ExternDecl $4 decls) }

type_spec     :: { Type }
   : type_specifier                { $1 }
   | type_spec '[' ']'             { TyArray $1 [] }
   | type_spec '[' const_expr ']'  { TyArray $1 [$3] }
   | type_specifier type_qualifier { TyApply (TyQualifier $2) $1 }
   | type_qualifier type_spec      { TyApply (TyQualifier $1) $2 }

type_spec_no_leading_qual :: { Type }
   : type_specifier		       { $1 }
   | type_spec_no_leading_qual '[' ']' { TyArray $1 [] }
   | type_specifier type_qualifier     { TyApply (TyQualifier $2) $1 }

type_qualifier  :: { Qualifier }
   : CONST     { Const    }
   | VOLATILE  { Volatile }

type_specifier  :: { Type }
   : FLOAT                { TyFloat $1 }
   | CHAR                 { TyChar  }
   | WCHAR                { TyWChar }
   | integer_ty           { $1 }
   | VOID                 { TyVoid }
   | SIGNED               { TySigned True  }
   | UNSIGNED             { TySigned False }
   | SIGNED signed_type_specifier          { TyApply (TySigned True) $2  }
   | SIGNED CONST signed_type_specifier    { TyApply (TySigned True) $3  }
   | UNSIGNED signed_type_specifier        { TyApply (TySigned False) $2 }
   | UNSIGNED CONST signed_type_specifier  { TyApply (TySigned False) $3 }
   | integer SIGNED INT   { TyApply (TySigned True)  $1 }
   | integer UNSIGNED INT { TyApply (TySigned False) $1 }
   | ID                   { TyName $1 Nothing } 
   | TYPE                 { TyName $1 Nothing } 
   | ITYPE		{ $1 }
   | SAFEARRAY safearray_type_spec ')' { TySafeArray $2 }  {- the oparen is part of the SAFEARRAY lexeme..-}
   | struct_or_union_spec { $1 }
   | enum_type            { $1 }
   | error                {% dumpErrMsg >> return TyVoid }

signed_type_specifier :: { Type }
                      : integer_ty   { $1 }
		      | CHAR         { TyChar }
		      | FLOAT        { TyFloat $1 }
--		      | ID           { TyName $1 Nothing }
		      | TYPE         { TyName $1 Nothing }
   		      | ITYPE	     { $1 }


safearray_type_spec :: { Type }
         : type_specifier  { $1 }
	 | safearray_type_spec '*' { TyPointer $1 }

struct_or_union_or_enum_spec :: { Type }
     : struct_or_union_spec  { $1 }
     | enum_type	     { $1 }

struct_or_union_spec :: { Type }
     : struct_type      { $1 }
     | union_type       { $1 }

struct_type :: { Type }
     : STRUCT simple_declarator '{' member_list '}' mb_gnu_attributes { TyStruct (Just $2) (reverse $4) (toPackedAttrib $6) }
     | STRUCT            '{' member_list '}'        mb_gnu_attributes { TyStruct Nothing (reverse $3) (toPackedAttrib $5) }
     | STRUCT simple_declarator                     { TyStruct (Just $2) [] Nothing }
              

union_type           :: { Type }
     : UNION switch_id SWITCH  '(' switch_type_spec identifier ')' 
       switch_id '{' switch_body  '}' { TyUnion $2 $5 $6 $8 (reverse $10) }

     | UNION switch_id  '{' member_or_switch_list '}' mb_gnu_attributes
                        { case $4 of { Left sw -> TyUnionNon $2 (reverse sw) ; Right mem -> TyCUnion $2 (reverse mem) (toPackedAttrib $6) } }
     | UNION identifier mb_gnu_attributes { TyCUnion (Just $2) [] (toPackedAttrib $3) }

member_or_switch_list :: { Either [Switch] [Member] }
     : switch_body       { Left $1 }
     | member_list       { Right $1 }

declarators        :: { [ Id ] }
     : declarator		  { [ $1 ]  }
     | declarators ',' declarator { $3 : $1 }

declarator :: { Id }
     : callconv_or_attr pointer_declarator  { $1 $2 }
     | pointer_declarator           { $1 }
     | callconv_or_attr direct_declarator   { $1 $2 }
     | direct_declarator            { $1 }

callconv_or_attr :: { (Id -> Id) }
     : gnu_attributes    { toCConvAttrib $1 }
     | CALLCONV          { CConvId $1 }

direct_declarator :: { Id }
     : simple_declarator          { $1 }
     | '(' declarator ')'         { $2 }
     | function_declarator        { $1 }
     | array_declarator           { $1 }

simple_declarator :: { Id }
     : identifier	       { $1 }
         -- bit field info is currently thrown away. ToDo: fix.
     | identifier ':' LITERAL  { (let { (Id nm) = $1 ; x = mkBitField nm $3 } in BitFieldId x $1) }
     | TYPE ':' LITERAL        { (let x = mkBitField $1 $3 in BitFieldId x (Id $1)) }
     | ':' LITERAL             { (let x = mkBitField "" $2 in BitFieldId x (Id "")) }
     | TYPE                    { (Id $1) }
     | MODE                    { (if $1 == In then Id "in" else Id "out") }

pointer_declarator :: { Id }
     : pointer direct_declarator {  Pointed $1 $2 }
     | pointer CALLCONV direct_declarator {  Pointed $1 (CConvId $2 $3) }
     | pointer gnu_attributes direct_declarator {  Pointed $1 (toCConvAttrib $2 $3) }

array_declarator :: { Id }
     : direct_declarator '[' ']'             { ArrayId $1 []   } 
     | direct_declarator '[' '*' ']'         { ArrayId $1 []   }
     | direct_declarator '[' const_expr ']'  { ArrayId $1 [$3] }
     | direct_declarator '[' primary_expr '.' '.' primary_expr ']'  { ArrayId $1 [$3,$6] }

function_declarator :: { Id }
     : direct_declarator '(' param_type_spec_list ')' { mkFunId $1 (reverse $3) }
     | direct_declarator '(' ')'		      { mkFunId $1 [] }

pointer  :: { [[Qualifier]] }
     : '*'		      {   [[]]  }
     | '*' type_quals 	      {   [$2]  }
     | '*' pointer            { [] : $2 }
     | '*' type_quals pointer { $2 : $3 }


{- Storage qualifiers on pointers -}

type_quals   :: { [ Qualifier ] }
     : type_qualifier	          { [$1] }
     | type_quals type_qualifier  { $2 : $1 }


member_list  :: { [Member] }
     : member mb_gnu_attributes ';'             { [ $1 ]  }
     | member_list member mb_gnu_attributes ';' { $2 : $1 }

member    :: { Member  }
     : opt_attributes type_spec declarators { ($2, $1, reverse $3) }
     | opt_attributes type_spec             { ($2, $1, []) }
     -- The last one is unpleasant, unnamed members.

switch_id	 :: { Maybe Id }
     : 			{ Nothing }
     | identifier	{ Just $1 }

switch_type_spec :: { Type }
     : integer_ty           { $1 }
     | CHAR	            { TyChar }
     | enum_type	    { $1 }
     | struct_or_union_spec { $1 }
     | ID                   { TyName $1 Nothing }
     | TYPE                 { TyName $1 Nothing }

switch_body      :: { [Switch] }
     : case ';'			{ [ $1 ]  }
     | switch_body case ';'	{ $2 : $1 }

case	 :: { Switch }
     : case_labels switch_arm          { Switch  $1  $2 }
     | '[' case_label1 ']' switch1_arm { Switch [$2] $4 }

switch1_arm  :: { Maybe SwitchArm }
     : {- empty -} 	    { Nothing }
     | type_spec declarator { Just (Param $2 $1 []) }
     | type_spec            { Just (Param (Id "") $1 []) }

switch_arm  :: { Maybe SwitchArm }
     : {- empty -} 			   { Nothing }
     | opt_attributes type_spec declarator { Just (Param $3 $2 $1) }
     | opt_attributes type_spec            { Just (Param (Id "") $2 $1) }

case_labels      :: { [CaseLabel] }
     : case_label	      { [ $1 ] }
     | case_labels case_label { $2 : $1 }

case_label	 :: { CaseLabel }
     : CASE const_expr ':' { Case [$2] }
     | DEFAULT ':'   	   { Default }

case_label1	 :: { CaseLabel }
     : CASE '(' const_expr_list ')' { Case (reverse $3) }
     | DEFAULT                      { Default }

const_expr_list  :: { [Expr] }
     : const_expr		      { [ $1 ]  }
     | const_expr_list ',' const_expr { $3 : $1 }

expr_list :: { [Expr] }
     : {- empty -}      { [] }
     | const_expr_list  { $1 }

enum_type	 :: { Type }
   : ENUM '{' enumerators opt_comma '}' { TyEnum Nothing (reverse $3) }
   | ENUM identifier '{' 
          enumerators opt_comma
     '}'                    { TyEnum (Just $2) (reverse $4) }
   | ENUM identifier        { TyEnum (Just $2) [] }

{- NOTE: MIDL does allow a comma trailing the enumerator list -}
enumerators      :: { [(Id, [Attribute], Maybe Expr)] }
   : enumerator	                { [ $1 ]  }
   | enumerators ',' enumerator { $3 : $1 }

enumerator  :: { (Id, [Attribute], Maybe Expr) }
   : identifier	                           { ($1, [], Nothing) } 
   | attributes identifier                 { ($2, $1, Nothing) } 
   | identifier '=' const_expr             { ($1, [], Just $3) }
   | attributes identifier '=' const_expr  { ($2, $1, Just $4) }

string_type :: { Type }
   : string_or_wstring '<' shift_expr '>' { $1 (Just $3) }
   | string_or_wstring		          { $1 Nothing   }

string_or_wstring :: { (Maybe Expr -> Type) }
   : STRING         { TyString  }
   | WSTRING        { TyWString }

method_decls :: { [Defn] }
   : {- empty -}	          {    []   }
   | method_decls method_decl ';' { $2 : $1 }

method_decl  :: { Defn }
   : opt_attributes op_type_spec declarator mb_gnu_attributes
      { let m_id = mkMethodId $3 in (Attributed $1 (Operation m_id $2 Nothing Nothing)) }

op_decl	     :: { Defn }
--   : op_type_spec declarator { Operation $2 $1 Nothing Nothing }
    : op_type_spec declarator mb_gnu_attributes { let m_id = mkMethodId $2 in (Operation m_id $1 Nothing Nothing) }


opt_attributes   :: { [Attribute] }
    : opt_attributes1  { concat $1 }

opt_attributes1  :: { [[Attribute]] }
    : {- empty -} { [] }
    | attributes opt_attributes1 { $1 : $2 }

attributes       :: { [Attribute] }
    : '[' ']'                       {  []          }
    | '[' attributes1 opt_comma ']' { (reverse $2) }

attributes1      :: { [Attribute] }
    : attribute                 {  [ $1 ] }
    | attributes1 ',' attribute { $3 : $1 }

attribute        :: { Attribute }
    : identifier opt_attr_params   { Attrib $1 $2 }
    | STRING		           { Attrib (Id "string") [] }
    | MODE		           { Mode $1 }

cc_attributes     :: { [Attribute] }
    : cc_attribute                   {  [ $1 ] }
    | cc_attributes ',' cc_attribute { $3 : $1 }

cc_attribute        :: { Attribute }
    : identifier                   { Attrib $1 [] }
    | DEFAULT			   { Attrib (Id "default") [] }

opt_attr_params  :: { [AttrParam] }
    : {- empty -}	    { [] }
    | '(' attr_params ')'   { (reverse $2) }

attr_params	 :: { [AttrParam] }
    : attr_param		 {   [$1] }
    | attr_params ',' attr_param {  $3:$1 }

attr_param	:: { AttrParam }
    : const_expr		      { (AttrExpr $1) }
    | {-empty-}			      { EmptyAttr }
    | TYPE			      { (AttrLit (TypeConst $1)) }
    | UNSIGNED TYPE                   { (AttrLit (TypeConst ("unsigned " ++ $2))) }
    | SIGNED TYPE                     { (AttrLit (TypeConst ("signed " ++ $2))) }
    | '{' LITERAL '}'                 { AttrLit $2 }  {- just a guid here, please! -}
    | '*' attr_param		      { (AttrPtr $2)  }
    
op_type_spec   :: { Type }
    : type_spec_no_leading_qual    { $1 }
    | string_type                  { $1 }

param_decl :: { Param }
    : opt_attributes type_spec                      { Param (Id "") $2 $1 }
    | opt_attributes type_spec declarator           { Param $3 $2 $1  }
    | opt_attributes type_spec abstract_declarator  { Param $3 $2 $1  }
    | '...'				            { Param (Id "vararg") TyVoid [] }

param_type_spec_list :: { [Param] }
    : param_decl 	                  { [$1]  }
    | param_type_spec_list ',' param_decl { $3 : $1 }

abstract_declarator :: { Id }
    : pointer                               { Pointed $1 (Id "") }
    | callconv_or_attr abstract_declarator  { $1 $2 }
    | direct_abstract_declarator            { $1 }
    | pointer direct_abstract_declarator    { Pointed $1 $2 }

direct_abstract_declarator :: { Id }
    : '(' abstract_declarator ')'        { $2 }
    | '(' ')'                            { FunId (Id "") Nothing [] }
    | '(' param_type_spec_list ')'       { FunId (Id "") Nothing $2 }
    | direct_abstract_declarator '(' ')' { FunId $1 Nothing [] }
    | direct_abstract_declarator '(' param_type_spec_list ')' { FunId $1 Nothing $3 }

mb_gnu_attributes :: { [GNUAttrib] }
    : {- empty -}    { [] }
    | gnu_attributes { (reverse $1) }

gnu_attributes :: { [GNUAttrib] }
    : gnu_attribute                {   [$1]  }
    | gnu_attributes gnu_attribute { ($2:$1) }

gnu_attribute :: { GNUAttrib }
    : ATTRIBUTE '(' '(' gnu_attrib ')' ')' { $4 }

gnu_attrib :: { GNUAttrib }
    : a_word            { mkGNUAttrib $1 [] }
    | CALLCONV          { CConv $1 }
    | a_word '(' ID ')' { mkGNUAttrib $1 [Var $3] }
    | a_word '(' ID ',' const_expr_list ')' { mkGNUAttrib $1 (Var $3:$5) }

a_word :: { String }
    : ID     { $1 }
    | TYPE   { $1 }

identifier :: { Id }
    : ID   { (Id $1) }
    
semi ::     { () }
    : error { () }
    | ';'   { () }

opt_comma ::      { () }
    : {- empty -} { () }
    | ','         { () }

{------------------ END OF GRAMMAR --------------}

{

addTypes :: [Id] -> LexM ()
addTypes ids = do
  sequence (map addTypedef ls)
  return ()
 where
  ls = map getName ids

  getName (Id s) = s
  getName (ArrayId i _) = getName i
  getName (Pointed _ i) = getName i
  getName (CConvId _ i) = getName i
  getName (FunId i _ _) = getName i

addIfaceTypedef :: String -> LexM Id
addIfaceTypedef nm = addTypedef nm >> return (Id nm)

mkBitField :: String -> Literal -> Int
mkBitField nm l = 
  case l of
    IntegerLit (ILit _ i) -> fromInteger i
    _ -> error ("bitfield " ++ show nm ++ " not an int.")

warningMsg :: String -> LexM ()
warningMsg msg = do
  l <- getSrcLoc
  ioToLexM (hPutStrLn stderr (show l ++ ": warning: "++msg))

dumpErrMsg :: LexM ()
dumpErrMsg = do
 l   <- getSrcLoc
 str <- getStream
 ioToLexM (ioError (userError (show l ++ ": Parse error on input: " ++ takeWhile (/='\n') str)))

happyError :: LexM a
happyError = do
 l   <- getSrcLoc
 str <- getStream
 ioToLexM (ioError (userError (show l ++ ": Parse error: " ++ takeWhile (/='\n') str)))
}
