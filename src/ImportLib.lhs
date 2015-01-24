%
% Daan Leijen, 1997,  leijen@fwi.uva.nl
% Sigbjorn Finne, 1998-.
% 

Com/OLE type library front-end.

This code is COM dependent, so you'll have to explicitly
compile it in (see Makefile).

[sof 11/98 - rewritten, extended and integrated into H/Direct sources.]

\begin{code}
module ImportLib
        (
         importLib
        ) where
        
import IDLSyn

{- BEGIN_NOT_SUPPORT_TYPELIBS -}
importLib :: String -> IO Defn
importLib nm = return (Pragma ("importLib: type library reader not compiled in. " ++ nm))
{- END_NOT_SUPPORT_TYPELIBS -}

{- BEGIN_SUPPORT_TYPELIBS 
-- to the end of the file.
import 
       HDirect
import 
       Com	   hiding (GUID)
import
       qualified Com ( GUID )
import
       ComPrim     ( lOCALE_USER_DEFAULT )
import 
       Automation  hiding (GUID,DISPID, Member)
import 
       TypeLib

import BasicTypes
import Literal
import Opts
import IDLUtils hiding ( noAttrs )
import Utils ( notNull )
import Foreign.Ptr
{-
#ifdef DEBUG
-}
import PpIDLSyn ( showIDL, ppType )
{-
#endif
-}
import System.IO
import Word	( Word32 )
import Int
import NumExts  ( floatToDouble )

import IO
import Bits
import Monad    ( when )
import List
import Maybe	( catMaybes )

\end{code}

%-----------------------------------------------------------
%-- Read TypeLib
%-----------------------------------------------------------

\begin{code}
importLib :: String -> IO Defn
importLib libfile = do
   -- pretend you didn't see the next line..
  writeIORef libs_seen_ref []
#ifdef DEBUG
  hPutStrLn stderr ("typelib: " ++ libfile)
#endif
  unk       <- (loadTypeLib libfile `catch` \ _ -> ioError (userError ("couldn't load: "++libfile))) 
  typelib   <- unk     # queryInterface iidITypeLib
  (libName, docString, hContext, hString) <- typelib # getLibName
#ifdef DEBUG
  hPutStrLn stderr ("typelib name: " ++ libName)
#endif
  (hStringContext, hStringDll) <-
      catch 
       ( do
	  typelib2 <- typelib # queryInterface iidITypeLib2
	  (_,ctxt, dlln) <- typelib2 # getDocumentation2TL (-1) lOCALE_USER_DEFAULT
	  return (ctxt, dlln))
       ( \ _ -> return (0,""))
	
  ~(Just libAttr) <- typelib # getLibAttr
  maxItem   <- typelib # getTypeInfoCount
#ifdef DEBUG
  hPutStrLn stderr ("typelib contains " ++ show maxItem ++ " items")
#endif
  let
      indices 
        | maxItem == 0 = []
	| otherwise    = [0 .. (fromIntegral maxItem)-1]

  customs	       <- typelib # getCustomTL
  libItems	       <- mapM (\ x -> typelib # readItem [libName] x) indices
  let 
      lFlags = wLibFlags libAttr
      attrs =
	catMaybes
        [ versionAttr	     (wMajorVerNum0 libAttr) (wMinorVerNum0 libAttr)
	, Just $ uuidAttribute      (guid1 libAttr)
	, helpStringAttr     docString
	, helpContextAttr    (toInteger hContext)
	, helpFileAttr       hString
	, helpStringDllAttr  hStringDll
	, helpStringCtxtAttr (toInteger hStringContext)
	, lcidvalAttribute   (toInteger (lcid0 libAttr))
	, isSet lFlags 0x1 restrictedAttribute
	, isSet lFlags 0x2 controlAttribute
	, isSet lFlags 0x4 hiddenAttribute
	] ++ customs

      sorted_libItems = sortDefns libItems
  rpath <- 
	catch
	   (queryPathOfRegTypeLib (guid1 libAttr) 
				  (wMajorVerNum0 libAttr)
				  (wMinorVerNum0 libAttr))
	   ( \ _ -> return "")
  ls <- readIORef libs_seen_ref
  let ls'  = filter (/=rpath) ls
      imps = map ImportLib ls'
  return (Attributed attrs
		     (Library (Id libName)
			      (imps ++ sorted_libItems)))

\end{code}
             
\begin{code}
readItem :: Level -> Int -> ITypeLib a -> IO Defn
readItem level index typelib = do
#ifdef DEBUG
 hPutStrLn stderr ("readItem: " ++ show index)
#endif
 itemName       <- typelib # getItemName (fromIntegral index)
#ifdef DEBUG
 hPutStrLn stderr ("readItem:" ++ show itemName)
#endif
 let level' = level ++ [itemName]
 when optDebug (hPutStrLn stderr (show level'))
 typeinfo       <- typelib  # getTypeInfo (fromIntegral index)
   -- make sure we're holding on to this (forever, basically.)
   -- The reason for doing this is that we're here unravelling stuff
   -- from the TI, including GUIDs (c.f., 'guid' field in the TYPEATTR)
   -- which we don't copy, but merely store a (finalised) pointer to.
   --
   -- Even though we're not releasing the TYPEATTR, releasing the TI
   -- seems to release it for us. Uncool behaviour (where is *this*
   -- documented??), but not much we can do about it. So, increase
   -- the ref. count on the TI by one, so that we ensure it is held onto
   -- (until we eventually shut down.)
   -- 
   -- ToDo: avoid these troubles by copying GUIDs and VARIANTs into 
   --       freshly allocated memory...? 
   -- 
 typeinfo # addRef
 (Just typeAttr) <- typeinfo # getTypeAttr 
 case (typekind typeAttr) of
   TKIND_ALIAS  -> typeinfo # readAlias  itemName level' typeAttr
   TKIND_ENUM   -> typeinfo # readEnum   itemName level' typeAttr
   TKIND_RECORD -> typeinfo # readRecord itemName level' True typeAttr
   TKIND_UNION  -> typeinfo # readRecord itemName level' False typeAttr
   TKIND_INTERFACE -> typeinfo # readInterface itemName level' typeAttr
   TKIND_DISPATCH
        | isDual typeAttr -> typeinfo # readDual itemName level'
        | otherwise       -> typeinfo # readDispatch itemName level' typeAttr
   TKIND_COCLASS	  -> typeinfo # readCoClass itemName level' typeAttr
   TKIND_MODULE		  -> typeinfo # readModule itemName level' typeAttr
   _		          -> do
	hPutStrLn stderr "Something else"
        return (CppQuote "")

-- To identify the context we're processing, pass down a list of names
-- (v. useful when dumping out warnings.)
type Level      = [Name]

\end{code}

-----------------------------------------------------------
-- READ DISPATCH, we should do a lot more error checking
-- here (SAFEARRAY, dispatch types etc.)
-----------------------------------------------------------

\begin{code}
readDispatch :: Name -> Level -> TYPEATTR -> ITypeInfo a -> IO Defn
readDispatch name level typeAttr typeinfo = do
#ifdef DEBUG
  hPutStrLn stderr ("readDispatch: " ++ show name)
#endif
  methods <- mapM (\ x -> typeinfo # readDispMember level x) [0..((fromIntegral (cFuncs typeAttr))-1)]
  vars    <- mapM (\ x -> typeinfo # readDispVar level x) [0..((fromIntegral (cVars typeAttr))-1)]
  (libName, docString, hContext, hString, hStringContext, hStringDll) 
	  <- typeinfo # getHelpAttributesTI (-1)
  customs <- typeinfo # getCustomTI
  let
   tFlags = wTypeFlags typeAttr

   real_methods = filter (not.isStdJunk) methods

   isStdJunk (Attributed _ (Operation (FunId (Id nm) _ _) _ _ _)) = nm `elem` std_meths
   isStdJunk _ = False

    -- we're less than interested in these.
   std_meths = ["QueryInterface", "AddRef", "Release",
		"GetTypeInfoCount", "GetTypeInfo", "GetIDsOfNames",
		"Invoke"
	       ]

   disp_attrs =
     catMaybes
        [ Just $ uuidAttribute (guid typeAttr)
        , versionAttr   (wMajorVerNum typeAttr) (wMinorVerNum typeAttr)
	, isSet tFlags 0x10   hiddenAttribute
	, helpStringAttr     docString
	, helpContextAttr    (toInteger hContext)
	, helpFileAttr       hString
	, helpStringDllAttr  hStringDll
	, helpStringCtxtAttr (toInteger hStringContext)
	] ++ customs
	
  return (Attributed disp_attrs (DispInterface (Id name) vars real_methods))

readDispMember :: Level -> Int -> ITypeInfo a -> IO Defn
readDispMember level' index typeinfo = do
#ifdef DEBUG
   hPutStrLn stderr ("readDispMember: " ++ show level')
#endif
   ~(Just funcDesc) <- typeinfo # getFuncDesc (fromIntegral index)
   metName          <- typeinfo # getMemberName (memid funcDesc)
   customs	    <- typeinfo # getCustomTI
   (libName, docString, hContext, hString, hStringContext, hStringDll) 
		    <- typeinfo # getHelpAttributesTI (fromIntegral (memid funcDesc))
   let
      fFlags = wFuncFlags funcDesc
      mId    = memid funcDesc
      mId'   = toInteger (int32ToWord32 mId)

      iKind    = invkind funcDesc

      disp_attrs =
	(case iKind of
	   INVOKE_FUNC -> id
	   INVOKE_PROPERTYGET -> ((Attrib (Id "propget") []):)
	   INVOKE_PROPERTYPUT -> ((Attrib (Id "propput") []):)
	   INVOKE_PROPERTYPUTREF -> ((Attrib (Id "propputref") []):)) $
         catMaybes
	   [ Just $ Attrib (Id "id") [AttrLit (IntegerLit (ILit 16 mId'))]
	   , isSet fFlags 0x4   (Attrib (Id "bindable") [])
	   , isSet fFlags 0x20  (Attrib (Id "defaultbind") [])
	   , isSet fFlags 0x100 (Attrib (Id "defaultcollelem") [])
	   , isSet fFlags 0x10  (Attrib (Id "displaybind") [])
	   , isSet fFlags 0x40  hiddenAttribute
	   , isSet fFlags 0x1000 (Attrib (Id "immediatebind") [])
	   , isSet fFlags 0x400  (Attrib (Id "nonbrowsable") [])
	   , isSet fFlags 0x800  (Attrib (Id "replaceable") [])
	   , isSet fFlags 0x8    (Attrib (Id "requestedit") [])
	   , isSet fFlags 0x1    (Attrib (Id "restricted")  [])
	   , isSet fFlags 0x2    (Attrib (Id "source") [])
	   , isSet fFlags 0x200  (Attrib (Id "uidefault") [])
	   , helpStringAttr     docString
	   , helpContextAttr    (toInteger hContext)
	   , helpFileAttr       hString
	   , helpStringDllAttr  hStringDll
	   , helpStringCtxtAttr (toInteger hStringContext)
--	   , Just (Attrib (Id "offset") [AttrLit (IntegerLit (ILit 10 (toInteger (memid funcDesc))))])
	   ] ++ customs

      level = level' ++ [metName]

   is_dispatch  <- checkKind level (funckind funcDesc)
   (params,metType) <- readMethodType iKind level funcDesc typeinfo
   let cc	 = Just (toCallConv (callconv funcDesc))
   let (res_ty, fun_id) =
         case metType of
	   TyPointer t -> (t, FunId (Pointed [[]] (Id metName)) cc params)
	   _	       -> (metType, FunId (Id metName) cc params)
     
   return (Attributed disp_attrs (Operation fun_id res_ty Nothing Nothing))

 where
  checkKind level kind  = 
   case kind of
    FUNC_DISPATCH       -> return True
    _ -> do 
        giveWarning level ["cannot translate non-dispatch functions (you used the '-auto' flag)"]
        return False

readDispVar :: Level -> Int -> ITypeInfo a -> IO ([Attribute], Type, Id) --Defn
readDispVar level index typeinfo = do
#ifdef DEBUG
  hPutStrLn stderr ("readDispVar: " ++ show level)
#endif
  ~(Just varDesc) <- typeinfo # getVarDesc (fromIntegral index)
  metName    <- typeinfo # getMemberName (memid0 varDesc)
  customs    <- typeinfo # getCustomTI
  (libName, docString, hContext, hString, hStringContext, hStringDll) 
	     <- typeinfo # getHelpAttributesTI (fromIntegral (memid0 varDesc))
  propType   <- typeinfo # readElemType (level++[metName]) (elemdescVar varDesc)
  propOffset <- case (iHC_TAG_7 varDesc) of
                 OInst x        -> return x
                 _              -> error "ImportLib.readDispVar: VARDESC is non-ideal"
  let 
      vFlags = wVarFlags varDesc
      mId    = memid0 varDesc
      mId'   = toInteger (int32ToWord32 mId)
      prop_attrs = 
         catMaybes
	   [ Just $ Attrib (Id "id") [AttrLit (IntegerLit (ILit 16 mId'))]
	   , isSet vFlags 0x1   (Attrib (Id "readonly") [])
	   , isSet vFlags 0x2   (Attrib (Id "source") [])
	   , isSet vFlags 0x4   (Attrib (Id "bindable") [])
	   , isSet vFlags 0x8   (Attrib (Id "requestedit") [])
	   , isSet vFlags 0x10  (Attrib (Id "displaybind") [])
	   , isSet vFlags 0x20  (Attrib (Id "defaultbind") [])
	   , isSet vFlags 0x40  hiddenAttribute
	   , isSet vFlags 0x80  (Attrib (Id "restricted")  [])
	   , isSet vFlags 0x100 (Attrib (Id "defaultcollelem") [])
	   , isSet vFlags 0x200  (Attrib (Id "uidefault") [])
	   , isSet vFlags 0x400  (Attrib (Id "nonbrowsable") [])
	   , isSet vFlags 0x800  (Attrib (Id "replaceable") [])
	   , isSet vFlags 0x1000 (Attrib (Id "immediatebind") [])
	   , helpStringAttr     docString
	   , helpContextAttr    (toInteger hContext)
	   , helpFileAttr       hString
	   , helpStringDllAttr  hStringDll
	   , helpStringCtxtAttr (toInteger hStringContext)
	   ] ++ customs

--      propId = mkId metName metName prop_attrs

  return (prop_attrs, propType, Id metName)

\end{code}

-----------------------------------------------------------
-- ALIAS
-----------------------------------------------------------

\begin{code}
readAlias :: Name -> Level -> TYPEATTR -> ITypeInfo a -> IO Defn
readAlias i_name level typeAttr typeinfo = do
#ifdef DEBUG
  hPutStrLn stderr ("readAlias: " ++ show i_name)
#endif
  alias <- typeinfo # readType level (tdescAlias typeAttr)
  (libName, docString, hContext, hString, hStringContext, hStringDll) 
	<- typeinfo # getHelpAttributesTI (-1)
  let 
       tFlags = wTypeFlags typeAttr
       gd     = guid typeAttr
       the_uuid_attr
         | gd == nullGUID = Nothing
	 | otherwise	  = Just (uuidAttribute gd)

       alias_attrs =
	 catMaybes
	   [ Just $ Attrib (Id "public") []
	   , the_uuid_attr
	   , helpStringAttr     docString
	   , helpContextAttr    (toInteger hContext)
	   , helpFileAttr       hString
	   , helpStringDllAttr  hStringDll
	   , helpStringCtxtAttr (toInteger hStringContext)
	   , isSet tFlags 0x10   hiddenAttribute
	   , isSet tFlags 0x200  restrictedAttribute
	   ]
	   
       -- the desugarer likes to see the Id as pointed if
       -- the type is, so we'll oblige.
  case alias of
--    TyPointer t@(TyIface _) -> return (Typedef t alias_attrs [Id i_name])
    TyPointer t -> return (Typedef t alias_attrs [Pointed [[]] (Id i_name)])
    _		-> return (Typedef alias alias_attrs [Id i_name])

\end{code}

-----------------------------------------------------------
-- ENUM
-----------------------------------------------------------

\begin{code}
readEnum :: Name -> Level -> TYPEATTR -> ITypeInfo a -> IO Defn
readEnum e_name level typeAttr typeinfo = do
  values <- mapM ((typeinfo # ).readEnumValue level) [0..((fromIntegral (cVars typeAttr))-1)]
   -- bizarrely, some enumerations were either not defined in 
  let s_values = sortBy cmp values
  
      cmp (_,_, Just (Lit (IntegerLit (ILit _ x))))
          (_,_, Just (Lit (IntegerLit (ILit _ y)))) = compare x y

  return (Typedef (TyEnum (Just (Id e_name)) s_values) [] [Id e_name])
             
readEnumValue :: Level -> Int -> ITypeInfo a -> IO (Id, [Attribute], Maybe Expr)
readEnumValue level index typeinfo = do
#ifdef DEBUG
  hPutStrLn stderr ("readEnumValue: " ++ show level)
#endif
  (Just varDesc) <- typeinfo # getVarDesc (fromIntegral index)
  enumName	 <- typeinfo # getMemberName (memid0 varDesc)
#ifdef DEBUG
  hPutStrLn stderr ("readEnumValue: " ++ show enumName)
#endif
  (libName, docString, hContext, hString, hStringContext, hStringDll) 
	         <- typeinfo # getHelpAttributesTI  (fromIntegral (memid0 varDesc))
  enumValue	 <- 
    case (varkind varDesc) of
      VAR_CONST   -> do
        case (iHC_TAG_7 varDesc) of
          LpvarValue (Just var) -> do
             vt <- readVarEnum var
	     case vt of
               VT_I2     -> readVarInt     (castPtr var)
               VT_I4     -> readVarInt     (castPtr var)
               _         -> do 
	        giveWarning level ["Expecting integer intializer for enumeration",
                                   "assume: " ++ show index]
                return (fromIntegral index)
          _  -> do
	      giveWarning level
		          [ "ImportLib.readEnumValue: unpack unionVARDESC is bogus."
		          , "Assuming " ++ show index ++ " instead."
		          ]
	      return (fromIntegral index)
      _      -> do 
         giveWarning (level++[enumName]) ["enumeration tag / constant is not constant!",
                                          "assume: " ++ show index]
         return (fromIntegral index)
  let
   attrs = 
	catMaybes
	   [ helpStringAttr     docString
	   , helpContextAttr    (toInteger hContext)
	   , helpFileAttr       hString
	   , helpStringDllAttr  hStringDll
	   , helpStringCtxtAttr (toInteger hStringContext)
	   ]

  return (Id enumName, attrs, Just (Lit (iLit enumValue)))

-- ToDo: merge with the above VAR_CONST de-pickling code.
readConst :: Level -> Int -> ITypeInfo a -> IO Defn
readConst level index typeinfo = do
  (Just varDesc) <- typeinfo # getVarDesc (fromIntegral index)
  cName	         <- typeinfo # getMemberName (memid0 varDesc)
#ifdef DEBUG
  hPutStrLn stderr ("readConst: " ++ cName)
#endif
  (libName, docString, hContext, hString, hStringContext, hStringDll) 
	         <- typeinfo # getHelpAttributesTI (-1)
  customs	 <- typeinfo # getCustomTI
  (ty,val)	 <- 
    case (varkind varDesc) of
      VAR_CONST   -> do
        case (iHC_TAG_7 varDesc) of
          LpvarValue (Just var) -> do
             vt <- readVarEnum var
	     case vt of
               VT_I1     -> do
		  v <- readVarInt (castPtr var)
		  return (TyApply (TySigned True) TyChar, Lit (iLit v))
               VT_I2     -> do
		  v <- readVarInt (castPtr var)
		  return (tyInt16, Lit (iLit v))
               VT_I4     -> do
	          v <- readVarInt (castPtr var)
		  return (tyInt32, Lit (iLit v))
               VT_UI1     -> do
		  v <- readVarInt (castPtr var)
		  return (tyWord16, Lit (iLit v))
               VT_UI2     -> do
		  v <- readVarInt (castPtr var)
		  return (tyWord16, Lit (iLit v))
               VT_UI4     -> do
	          v <- readVarInt (castPtr var)
		  return (tyWord32, Lit (iLit v))
	       VT_LPSTR  -> do
	          (pbstr,_) <- readVarString  (castPtr var)
                  str       <- readBSTR (castPtr pbstr)
		  return (tyString, Lit (StringLit str))
	       VT_BSTR  -> do
	          (pbstr,_) <- readVarString  (castPtr var)
                  str       <- readBSTR (castPtr pbstr)
		  return (tyString, Lit (StringLit str))
	       VT_LPWSTR  -> do
	          (pbstr,_) <- readVarString  (castPtr var)
                  str       <- readBSTR (castPtr pbstr)
		  return (tyString, Lit (StringLit str)) -- ToDo: fix.
               VT_BOOL   -> do
		  v <- readVarBool var
		  return (tyVariantBool, Lit (BooleanLit v))
               _         -> do 
	        giveWarning level ["Expecting integer intializer for enumeration (found: " ++ 
				     show (fromEnum vt) ++ ")",
                                   "assume : " ++ show index]
                return (tyInt32, Lit (iLit index))
          _  -> do
	      giveWarning level
		          [ "ImportLib.readConst: unpack unionVARDESC is 'odd'."
		          , "Assuming " ++ show index ++ " (signed long) instead."
		          ]
	      -- Anything to avoid stopping (the user can patch up the output
	      -- instead!)
	      return (tyInt32, Lit (iLit index))
      _      -> do 
         giveWarning (level++[cName]) ["constant is not constant!",
                                       "assume: " ++ show index,
				       " (signed long)"]
         return (tyInt32, Lit (iLit index))
  let
   vFlags = wVarFlags varDesc
    -- most of these don't apply, but what the h...
   attrs =
     catMaybes
       [ isSet vFlags 0x1   (Attrib (Id "readonly") [])
       , isSet vFlags 0x2   (Attrib (Id "source") [])
       , isSet vFlags 0x4   (Attrib (Id "bindable") [])
       , isSet vFlags 0x8   (Attrib (Id "requestedit") [])
       , isSet vFlags 0x10  (Attrib (Id "displaybind") [])
       , isSet vFlags 0x20  (Attrib (Id "defaultbind") [])
       , isSet vFlags 0x40  hiddenAttribute
       , isSet vFlags 0x80  (Attrib (Id "restricted")  [])
       , isSet vFlags 0x100 (Attrib (Id "defaultcollelem") [])
       , isSet vFlags 0x200  (Attrib (Id "uidefault") [])
       , isSet vFlags 0x400  (Attrib (Id "nonbrowsable") [])
       , isSet vFlags 0x800  (Attrib (Id "replaceable") [])
       , isSet vFlags 0x1000 (Attrib (Id "immediatebind") [])
       , helpStringAttr     docString
       , helpContextAttr    (toInteger hContext)
       , helpFileAttr       hString
       , helpStringDllAttr  hStringDll
       , helpStringCtxtAttr (toInteger hStringContext)
       ] ++ customs

  return (Constant (Id cName) attrs ty val)

\end{code}

-----------------------------------------------------------
-- RECORD
-----------------------------------------------------------

\begin{code}
readRecord :: Name -> Level -> Bool -> TYPEATTR -> ITypeInfo a -> IO Defn
readRecord name level is_struct typeAttr typeinfo = do
  fields <- mapM ((typeinfo # ).readMember level) [0..((fromIntegral (cVars typeAttr)) -1)]
  let 
   uuid  = guid typeAttr

   attrs 
     | uuid == nullGUID = []
     | otherwise        = [uuidAttribute uuid]

      --Attrib (Id "sz") [AttrLit (IntegerLit (ILit 10 (toInteger sz)))]

--   sz    = cbSizeInstance typeAttr

   tycon 
    | is_struct = TyStruct
    | otherwise = TyCUnion 

   ty = tycon (Just (Id name)) fields Nothing

   ret_un_ty = do
	when (not is_struct)
             (giveWarning level ["unions are only partially supported."
                                ,"if this is an output parameter or a field member,"
                                ,"make sure to supply the right tag-reader."])
	return (Typedef ty attrs [Id name])
    {-
      This special case evaluates remoting unions to pick the inproc
      case. Horrible, but quite convenient.
    -}
  case fields of
     ((_,_,[Id "hInproc"]):(_,_, [Id "hRemote"]):_) 
	    | not is_struct -> return (Typedef (TyInteger Long) attrs [Id name])
     _ 
      | name == "_RemotableHandle" -> return (Typedef (TyInteger Long) attrs [Id name])
      | otherwise -> ret_un_ty
                             
readMember :: Level -> Int -> ITypeInfo a -> IO Member
readMember level index typeinfo = do
#ifdef DEBUG
  hPutStrLn stderr ("readMember: " ++ show level)
#endif
  (Just varDesc) <- typeinfo # getVarDesc (fromIntegral index)
  memName     <- typeinfo # getMemberName (memid0 varDesc)
#ifdef DEBUG
  hPutStrLn stderr ("readMember: " ++ show memName)
#endif
  memType     <- typeinfo # readElemType (level++[memName]) (elemdescVar varDesc)
  memOffset   <- case (iHC_TAG_7 varDesc) of
                 OInst x     -> return x
                 _           -> error "ImportLib.readMember: VARDESC has non-ideal shape"
  let attrs = [] --Attrib (Id "offset") [AttrLit (IntegerLit (ILit 10 (toInteger memOffset)))]]
  return (memType, attrs, [Id memName])
\end{code}

-----------------------------------------------------------
-- COCLASS
-----------------------------------------------------------

\begin{code}
readCoClass :: Name -> Level -> TYPEATTR -> ITypeInfo a -> IO Defn
readCoClass name level typeAttr typeinfo = do
#ifdef DEBUG
  hPutStrLn stderr ("readCoClass: " ++ show name)
#endif
  (libName, docString, hContext, hString, hStringContext, hStringDll) 
	<- typeinfo # getHelpAttributesTI (-1)
  customs	    <- typeinfo # getCustomTI
  let
   tFlags = wTypeFlags typeAttr

   ifaces :: [Int]
   ifaces = [0..(fromIntegral (cImplTypes typeAttr))-1]

   attrs = 
     catMaybes
       [ Just   (uuidAttribute (guid typeAttr))
       , versionAttr   (wMajorVerNum typeAttr) (wMinorVerNum typeAttr)
       , isSet tFlags 1    (Attrib (Id "appobject") [])
       , isSet tFlags 1024 (Attrib (Id "aggregatable") [])
       , isSet tFlags 32   controlAttribute
       , isSet tFlags 16   hiddenAttribute
       , isSet tFlags 4     (Attrib (Id "licensed") [])
       , isn'tSet tFlags 2  (Attrib (Id "noncreatable") [])
       , helpStringAttr     docString
       , helpContextAttr    (toInteger hContext)
       , helpFileAttr       hString
       , helpStringDllAttr  hStringDll
       , helpStringCtxtAttr (toInteger hStringContext)
       ] ++ customs
   
   getOne n =
    catch ( do
     href <- typeinfo # getRefTypeOfImplType (fromIntegral n)
     tp   <- typeinfo # getRefTypeInfo href
     (tpl,nIndex) <- tp # getContainingTypeLib
     tk   <- tpl      # getTypeInfoType nIndex
     v    <- typeinfo # getImplTypeFlags (fromIntegral n)
     ~(Just tA) <- tp # getTypeAttr
     nm   <- tp	      # getMemberName (-1)
     let
       i_attrs = 
         catMaybes
	   [ isSet v 1	    (Attrib (Id "default") [])
	   , isSet v 2	    (Attrib (Id "source") [])
	   , isSet v 4	    restrictedAttribute
	   , isSet v 0x800  (Attrib (Id "defaultvtable") [])
	   ]

       kind =
	case tk of
          TKIND_DISPATCH | not (dualBitSet (wTypeFlags tA)) -> False
	  _  -> True

     return (Just (kind, Id nm, i_attrs)))
    (\ _ -> do
       giveWarning level ["trouble reading coclass item " ++ show n ++ ", ignoring."]
       return Nothing)

  mems_mb <- mapM getOne ifaces
  let mems = catMaybes mems_mb
  return (Attributed attrs $ CoClass (Id name) mems)

\end{code}

\begin{code}
readModule :: Name -> Level -> TYPEATTR -> ITypeInfo a -> IO Defn
readModule name level typeAttr typeinfo = do
  methods  <- mapM (\ x -> typeinfo # readMethod level x True)
  		   [0..(fromIntegral (cFuncs typeAttr) - 1)]
  (libName, docString, hContext, hString, hStringContext, hStringDll) 
	   <- typeinfo # getHelpAttributesTI (-1)
  customs  <- typeinfo # getCustomTI
  flg      <- (typeinfo # getImplTypeFlags (-1)) `catch` \ _ -> return 0
  consts   <- mapM ((typeinfo # ).readConst level) [0..((fromIntegral (cVars typeAttr))-1)]
  let
     getDllName [] = return "<no entry points>"
     getDllName (x:xs) =
       catch
         (do { (a,b,c) <- typeinfo # getDllEntry 0x60000000 x ; return a })
	 (\ _ -> getDllName xs)       

     invs = [ INVOKE_FUNC
            , INVOKE_PROPERTYGET
            , INVOKE_PROPERTYPUT
            , INVOKE_PROPERTYPUTREF
	    ]
         
  dll_name <- getDllName invs
  let
    tFlags = wTypeFlags typeAttr

    module_attrs = 
      (catMaybes $
	 [ Just $ Attrib (Id "dllname") [AttrLit (StringLit dll_name)]
	 , Just $ uuidAttribute (guid typeAttr)
         , helpStringAttr     docString
         , helpContextAttr    (toInteger hContext)
         , helpFileAttr       hString
         , helpStringDllAttr  hStringDll
         , helpStringCtxtAttr (toInteger hStringContext)
         , isSet tFlags 16   hiddenAttribute
	 ]) ++ customs
 
  return (Attributed module_attrs $ Module (Id name) (concat methods ++ consts))

\end{code}

-----------------------------------------------------------
-- DISPATCH-DUAL
-----------------------------------------------------------

\begin{code}
readDual :: Name -> Level -> ITypeInfo a -> IO Defn
readDual name level typeinfo  = do
#ifdef DEBUG
  hPutStrLn stderr ("readDual: " ++ show name)
#endif
    -- aargh,  now we know why functional programming matters !
  href            <- typeinfo # getRefTypeOfImplType (-1)   
  typeinfoIface   <- typeinfo # getRefTypeInfo href
  (Just typeAttrIface)  <- typeinfoIface # getTypeAttr 
  typeinfoIface # readInterface name level typeAttrIface

\end{code}

-----------------------------------------------------------
-- INTERFACE
-----------------------------------------------------------

\begin{code}
readInterface :: Name -> Level -> TYPEATTR -> ITypeInfo a -> IO Defn
readInterface name level typeAttr typeinfo = do
#ifdef DEBUG
  hPutStrLn stderr ("readInterface: " ++ show name)
#endif
  (libName, docString, hContext, hString, hStringContext, hStringDll) 
	<- typeinfo # getHelpAttributesTI (-1)
  methods <- mapM (\ x -> typeinfo # readMethod level x False)
  		  [0..(fromIntegral (cFuncs typeAttr) - 1)]
    -- figure out who it inherits from.
  iherit  <- (do
	hr	        <- typeinfo  # getRefTypeOfImplType 0 
	itypeinfo       <- typeinfo  # getRefTypeInfo hr
	nm	        <- itypeinfo # getMemberName (-1)
        (typelib,_)     <- itypeinfo # getContainingTypeLib
        ~(Just libAttr) <- typelib   # getLibAttr
        rpath	        <- 
	    catch
	          (queryPathOfRegTypeLib (guid1 libAttr) 
				         (wMajorVerNum0 libAttr)
				         (wMinorVerNum0 libAttr))
		  (\ _ -> return "")
        when (notNull rpath) $ do
            rs <- readIORef libs_seen_ref
            case rpath `elem` rs of
             False -> writeIORef libs_seen_ref (rpath:rs)
	     True  -> return ()
	return nm
      ) `catch` \ _ -> return "IUnknown"
  customs <- typeinfo # getCustomTI
  let
    tFlags    = wTypeFlags typeAttr

    meth_attr =
	catMaybes
	  [ Just (Attrib (Id "odl") [])
	  , Just (uuidAttribute (guid typeAttr))
	  , isSet tFlags 0x40 (Attrib (Id "dual") [])
	  , isSet tFlags 0x10 hiddenAttribute
	  , isSet tFlags 0x80  (Attrib (Id "nonextensible") [])
	  , isSet tFlags 0x100 (Attrib (Id "oleautomation") [])
	  , isSet tFlags 0x200 restrictedAttribute
          , helpStringAttr     docString
          , helpContextAttr    (toInteger hContext)
          , helpFileAttr       hString
          , helpStringDllAttr  hStringDll
          , helpStringCtxtAttr (toInteger hStringContext)
	  ] ++ customs

  return (Attributed meth_attr $ Interface (Id name) [iherit] (concat methods))

\end{code}

-----------------------------------------------------------
-- METHOD
-----------------------------------------------------------

\begin{code}
readMethod :: Level -> Int -> Bool -> ITypeInfo a -> IO [Defn] -- methods.
readMethod level' index in_module typeinfo = do
#ifdef DEBUG
  hPutStrLn stderr ("readMethod: " ++ show level')
#endif
  ~(Just funcDesc)  <- typeinfo # getFuncDesc   (fromIntegral index)
  r@(libName, docString, hContext, hString, hStringContext, hStringDll) 
	<- typeinfo # getHelpAttributesTI (fromIntegral (memid funcDesc))
#ifdef DEBUG
  hPutStrLn stderr ("readMethod3: " ++ show docString)
#endif
  metName           <- typeinfo # getMemberName (memid funcDesc)
#ifdef DEBUG
  hPutStrLn stderr ("readMethod4: " ++ show metName)
#endif
--  ~(Just varDesc)   <- typeinfo # getVarDesc	(fromIntegral index)
  customs	    <- typeinfo # getCustomTI
  let
      fId      = memid funcDesc
      fId'     = toInteger (int32ToWord32 fId)
      iKind    = invkind funcDesc

  cons_entry        <- 
       if not in_module then
          return id
	else do
           (dname, nm, ord) <- typeinfo # getDllEntry fId iKind
	   let dllname   = Attrib (Id "dllname") [AttrLit (StringLit dname)]
	       entry_a v = Attrib (Id "entry") [AttrLit v]
	   case nm of
	     "" -> return (\ x -> dllname : entry_a (IntegerLit (ILit 10 (toInteger ord))) : x)
	     _  -> return (\ x -> dllname : entry_a (StringLit nm) : x)
  let level = level' ++ [metName]

      fFlags   = wFuncFlags funcDesc

      id_nm    = "id"

      metAttrs =
	 cons_entry $
	 ((Attrib (Id id_nm) [AttrLit (IntegerLit (ILit 16 fId'))]):) $
	 (case iKind of
	   INVOKE_FUNC -> id
	   INVOKE_PROPERTYGET -> ((Attrib (Id "propget") []):)
	   INVOKE_PROPERTYPUT -> ((Attrib (Id "propput") []):)
	   INVOKE_PROPERTYPUTREF -> ((Attrib (Id "propputref") []):)) $
         catMaybes
	   [ isSet fFlags 0x100 (Attrib (Id "defaultcollelem") [])
	   , isSet fFlags 0x40  hiddenAttribute
	   , isSet fFlags 0x800 (Attrib (Id "replaceable") [])
	   , isSet fFlags 0x2   (Attrib (Id "source") [])
	   , isSet fFlags 0x200 (Attrib (Id "uidefault") [])
	   , isSet fFlags 0x1   (Attrib (Id "restricted") [])
           , helpStringAttr     docString
           , helpContextAttr    (toInteger hContext)
           , helpFileAttr       hString
           , helpStringDllAttr  hStringDll
           , helpStringCtxtAttr (toInteger hStringContext)
--	   , Just $ Attrib (Id "offset") [AttrLit (IntegerLit (ILit 10 (toInteger (oVft funcDesc))))]
	   ] ++ customs

  ok         <- checkKind level (funckind funcDesc)
  if (not ok)
   then return []
   else do 
     (params, metType) <- typeinfo # readMethodType iKind level funcDesc
     let 
	 cc = Just (toCallConv (callconv funcDesc))
         (res_ty, fun_id) =
           case metType of
	    TyPointer t -> (t, FunId (Pointed [[]] (Id metName)) cc params)
	    _	        -> (metType, FunId (Id metName) cc params)

     return [Attributed metAttrs (Operation fun_id res_ty Nothing Nothing)]
 where
  checkKind level kind =
    case kind of
      FUNC_NONVIRTUAL -> do 
         giveWarning level ["cannot translate non-virtual functions"]
	 return False
	-- The rest can be handled, but let's just spell it out what
	-- kinds they are..
      FUNC_VIRTUAL     -> return True
      FUNC_PUREVIRTUAL -> return True
      FUNC_STATIC      -> return True -- Legal inside module decls
      FUNC_DISPATCH    -> return True

toCallConv :: CALLCONV -> CallConv
toCallConv cv = 
    case cv of 
      CC_CDECL       -> Cdecl
      CC_MPWCDECL    -> Cdecl
                                 
      CC_MSCPASCAL   -> Pascal
      CC_PASCAL      -> Pascal
      CC_MACPASCAL   -> Pascal
      CC_MPWPASCAL   -> Pascal
                                 
      CC_SYSCALL     -> Stdcall
      _              -> Stdcall
                                 

readMethodType :: INVOKEKIND -> [String] -> FUNCDESC -> ITypeInfo a -> IO ([Param], Type)
readMethodType iKind level funcDesc typeinfo = do
#ifdef DEBUG
  hPutStrLn stderr ("readMethodType: " ++ show level)
#endif
  metType      <- typeinfo # readElemType level (elemdescFunc funcDesc)
                      
#ifdef DEBUG
  hPutStrLn stderr ("readMethodType2: " ++ show (showIDL (ppType metType)))
#endif
  let no_params = fromIntegral $ length (lprgelemdescParam funcDesc)  + 1
  parnames'	<- typeinfo # getNames (memid funcDesc) no_params
#ifdef DEBUG
  hPutStrLn stderr ("readMethodType3: " ++ show parnames')
#endif
  let parnames   = (tail parnames') ++ map tempname [(0::Int)..]
      tempname i = ("x" ++ show i)
      parlevels  = map (\name -> level++[name]) parnames
                      
  params  <- 
     mapM (\(l,name,elem) -> typeinfo # readParam iKind name l elem) 
          (zip3 parlevels parnames (lprgelemdescParam funcDesc))

  return (params, metType)

readParam :: INVOKEKIND
	  -> Name
	  -> Level 
          -> ELEMDESC 
	  -> ITypeInfo a
	  -> IO Param
readParam iKind name level elemDesc typeinfo = do
  partp   <- typeinfo # readElemType level elemDesc
  customs <- typeinfo # getCustomTI
  let 
      pdesc  = paramdesc elemDesc
      pFlags = wParamFlags pdesc
      inout  = pARAMFLAG_FIN .|. pARAMFLAG_FOUT

      parMode  | pFlags .&. inout	   == inout	     = InOut
               | pFlags .&. pARAMFLAG_FIN  == pARAMFLAG_FIN  = In
               | pFlags .&. pARAMFLAG_FOUT == pARAMFLAG_FOUT = Out
               | otherwise = 
	        (\ x -> unsafePerformIO $ do
			when (optDebug) (giveWarning level ["non-moded parameter seen, " ++ 
							    name ++", assuming [" ++ show x ++ "]"])
			return x) $
		    {-
		      A little bit of (hard won) AI..
		    -}
		   case iKind of
		     INVOKE_PROPERTYGET | isPointed -> Out
		     _ 
		       | isRetVal  -> Out
		       | otherwise -> In

      isPointed = 
        case partp of
	  TyPointer _ -> True
	  TyArray _ _ -> True
	  TyIface _   -> True
	  _           -> False

      isRetVal  = pFlags .&. pARAMFLAG_FRETVAL == pARAMFLAG_FRETVAL

  defVal <-
        case pparamdescex pdesc of
	  Nothing -> return Nothing
	  Just pd -> do
	    let var = varDefaultValue pd
	    vt       <- readVarEnum var
	    case vt of
               VT_I1     -> do
		  v <- readVarInt var
		  return (Just (iLit v))
               VT_BOOL   -> do
		  v <- readVarBool var
		  return (Just (BooleanLit v))
               VT_I2     -> do
		  v <- readVarInt var
		  return (Just (iLit v))
               VT_I4     -> do
	          v <- readVarInt var
		  return (Just (iLit v))
               VT_UI1     -> do
		  v <- readVarInt var
		  return (Just (iLit v))
               VT_UI2     -> do
		  v <- readVarInt var
		  return (Just (iLit v))
               VT_UI4     -> do
	          v <- readVarInt var
		  return (Just (iLit v))
               VT_R4     -> do
	          v <- readVarFloat var
		  return (Just (FloatingLit (show v, floatToDouble v)))
               VT_R8     -> do
	          v <- readVarDouble var
		  return (Just (FloatingLit (show v,v)))
	       VT_LPSTR  -> do
	          (pbstr,_) <- readVarString  var
                  str       <- readBSTR (castPtr pbstr)
		  return (Just (StringLit str))
	       VT_BSTR  -> do
	          (pbstr,_) <- readVarString  var
                  str       <- readBSTR (castPtr pbstr)
		  return (Just (StringLit str))
	       VT_LPWSTR  -> do
	          (pbstr,_) <- readVarString  var
                  str       <- readBSTR (castPtr pbstr)
		  return (Just (StringLit str)) -- ToDo: fix.
	       _ -> 
		catch (do
	         (pbstr,_) <- readVarString  var
                 str       <- readBSTR (castPtr pbstr)
		 return (Just (StringLit str)))
		 (\ _ -> return Nothing)
  let
      par_attrs =
	catMaybes
	 [ Just $ Mode parMode
	 , isSet   pFlags pARAMFLAG_FRETVAL	retValAttribute
	 , isSet   pFlags pARAMFLAG_FOPT	optionalAttribute
	 , isSetMb pFlags pARAMFLAG_FHASDEFAULT	(defaultAttribute defVal)
	 , isSet   pFlags pARAMFLAG_FLCID	lcidAttribute
	 ] ++ customs

      parType  = partp
                              
{-
  case parType of 
    TyIface typeName | not (typeName `elem` ["IUnknown", "IDispatch"]) -> do 
      giveWarning level ["abstract interface seen",typeName]
      return ()
    _  -> return ()
-}
  let par = Param (Id name) parType par_attrs
  return par

\end{code}                

%-----------------------------------------------------------
%-- Types
%-----------------------------------------------------------

\begin{code}
readElemType :: Level 
	     -> ELEMDESC 
	     -> ITypeInfo a
	     -> IO Type
readElemType level elemDesc typeinfo = typeinfo # readType level (tdesc elemDesc) 

readType :: Level -> TYPEDESC -> ITypeInfo a -> IO Type
readType level typeDesc typeinfo = do
#ifdef DEBUG
  hPutStrLn stderr ("readType: " ++ show level)
#endif
--  recordExternalTlbDependency typeinfo index
  case (toEnum (fromIntegral (vt typeDesc))) of
    VT_I2     -> return tyShort
    VT_I4     -> return tyInt
    VT_R4     -> return tyFloat
    VT_R8     -> return tyDouble

    VT_LPSTR  -> return tyString
    VT_LPWSTR -> return tyWString
    VT_BSTR   -> return tyBSTR

    VT_ERROR   -> return tyHRESULT
    VT_HRESULT -> return tyHRESULT
    VT_UNKNOWN -> return (TyPointer tyIUnknown)

    VT_BOOL    -> return tyVariantBool
    VT_I1      -> return tyChar
    VT_UI1     -> return tyByte
    VT_UI2     -> return tyWord16
    VT_UI4     -> return tyWord32
    VT_I8      -> return tyInt64
    VT_UI8     -> return tyWord64
    VT_INT     -> return tyInt32
    VT_UINT    -> return tyWord32
    VT_VOID    -> return tyVoid
    VT_PTR     -> do
      tp <- case (iHC_TAG_1 typeDesc) of
              Lptdesc (Just td) -> typeinfo # readType level td 
	      Lpadesc _	        -> ioError (userError "ImportLib.readType: arraydesc, what to do?")
	      Hreftype href     -> typeinfo # trackType level href
              _                 -> ioError (userError "ImportLib.readType: anon - que pasa?")
      case tp of
        TyArray t []	   -> return (TyPointer t) -- Don't like this.
        _                  -> return (TyPointer tp)

    VT_CARRAY   -> do
		case (iHC_TAG_1 typeDesc) of
                     Lpadesc (Just ad) -> do
			  elemType <- typeinfo # readType level (tdescElem ad) 
                          let bounds = rgbounds ad
			      exprs  = map (Lit . iLit . cElements) bounds
			  case elemType of
			    TyChar | length exprs == 1 -> return (TyPointer TyChar) -- hack (but a convenient one!) :)
			    _      -> return (TyArray elemType exprs)
                                           
	             Lpadesc _	      -> 
				ioError (userError "ImportLib.readType{carray}: arraydesc, what to do?")
	             Hreftype href    -> 
				typeinfo # trackType level href
                     _		      -> 
				ioError (userError "ImportLib.readType{carray}: anon - que pasa?")

    VT_USERDEFINED  ->
       case (iHC_TAG_1 typeDesc) of
         Hreftype href -> typeinfo # trackType level href
         _             -> ioError (userError "ImportLib.readType{userdefined}: unpack unionTYPEDESC.hreftype error")


    VT_CLSID    ->  return tyGUID
    VT_DISPATCH ->  return (TyPointer tyIDispatch)
    VT_VARIANT  ->  return tyVARIANT
    VT_SAFEARRAY -> 
		case (iHC_TAG_1 typeDesc) of
                     Lpadesc (Just ad) -> do
			  elemType <- typeinfo # readType level (tdescElem ad) 
                          return (tySafeArray elemType)
	             Lptdesc (Just td) -> do
				elemType <- typeinfo # readType level td
				return (tySafeArray elemType)
	             Hreftype href    -> 
				typeinfo # trackType level href
                     _  -> ioError (userError "ImportLib.readType{sarray}: unpack unionTYPEDESC.lpadesc error")

    VT_CY    -> return tyCURRENCY

    VT_DATE     -> return tyDATE
    VT_FILETIME -> return tyFILETIME

    VT_BLOB    -> return tyAddr
    VT_STREAM  -> return tyIStream
    VT_STORAGE -> return tyIStorage

    VT_NULL  -> errtype "null"
    VT_EMPTY -> errtype "empty"
    VT_STREAMED_OBJECT -> errtype "streamed object"
    VT_STORED_OBJECT   -> errtype "stored object"
    VT_BLOB_OBJECT     -> errtype "BLOB object"
    _	               -> errtype "Unknown"

   where
    errtype msg  = do 
      giveWarning level ["unknown type: "++ show msg ++", interpreting it as void*"]
      return (TyPointer TyVoid)

trackType :: Level -> HREFTYPE -> ITypeInfo a -> IO Type
trackType level href typeinfo = catch (do
  typeinfoRef     <- typeinfo    # getRefTypeInfo href
  (typelib,index) <- typeinfoRef # getContainingTypeLib
  ~(Just libAttr) <- typelib     # getLibAttr
  rpath		  <- 
	catch
	   (queryPathOfRegTypeLib (guid1 libAttr) 
				  (wMajorVerNum0 libAttr)
				  (wMinorVerNum0 libAttr))
	   (\ _ -> return "")
  when (notNull rpath) $ do
      rs <- readIORef libs_seen_ref
      case rpath `elem` rs of
        False -> writeIORef libs_seen_ref (rpath:rs)
	True  -> return ()
  (Just typeAttr) <- typeinfoRef # getTypeAttr
  case (typekind typeAttr) of
    TKIND_ENUM    -> do
         ename   <- typelib # typeNamed (fromIntegral index)
	 return (TyName ename Nothing)
    TKIND_RECORD  -> do
         sname   <- typelib # typeNamed (fromIntegral index)
	 return (TyName sname Nothing)
    TKIND_UNION   -> do 
      uname  <- typelib # typeNamed (fromIntegral index)
      fields <- mapM ((typeinfoRef # ).readMember level) [0..((fromIntegral (cVars typeAttr)) -1)]
      let
	{-
	 This special case evaluates remoting unions to pick the inproc
	 case. Horrible, but quite convenient.
	-}
        ret_un_ty = do
          giveWarning level ["unions are only partially supported.",
                             "if this is an output parameter or a field member,",
                             "make sure to supply the right tag-reader."]
	  return (TyName uname (Just (TyCUnion (Just (Id ("tag"++uname))) fields Nothing)))
      case fields of
	((_,_,[Id "hInproc"]):(_,_, [Id "hRemote"]):_) -> return (TyInteger Long)
	_ -> ret_un_ty

    TKIND_INTERFACE -> do
      iname   <- typelib # typeNamed (fromIntegral index)
      return (TyIface iname) --(TyPointer (TyIface iname))
      -- the [default] interface, really.
    TKIND_COCLASS -> do
      cname   <- typelib # typeNamed (fromIntegral index)
       {-
         Life is never boring when you're writing a typelib reader...
	 Some typelibs contain types that refer to the coclass, which
	 doesn't make much sense, so we pick the [default] interface
	 of the class and use that as the type.
       -}
      cocl    <- typeinfoRef # readCoClass cname level typeAttr
      let 
        (Attributed _ (CoClass _ mems)) = cocl
        iname = 
	 case filter (isDefaultIface) mems of
	   [] -> cname
	   ((_, Id i, _):_) -> i

	isDefaultIface (_,_,attrs) =
	   hasAName "default" attrs && not (hasAName "source" attrs)

        hasAName _  [] = False
	hasAName nm ((Attrib (Id i) _):ls) = i == nm || hasAName nm ls
	hasAName nm (_:xs) = hasAName nm xs

      return (TyIface iname) --(TyPointer (TyIface iname))
    TKIND_DISPATCH  -> do
      iname   <- typelib # typeNamed (fromIntegral index)
      return (TyIface iname) --(TyPointer (TyIface iname))
    TKIND_ALIAS     -> do 
      nm	<- typelib     # typeNamed (fromIntegral index)
      aliasType <- typeinfoRef # readType level (tdescAlias typeAttr)
      return (TyName nm (Just aliasType))
    _ -> do 
      giveWarning level ["unexpected user defined type encountered, assume void*"]
      return (TyPointer TyVoid))
  ( \ _ -> do
       giveWarning level [ "failed to unswizzle type info from typelib; treat it as void*"]
       return (TyPointer TyVoid))

libs_seen_ref :: IORef [String]
libs_seen_ref = unsafePerformIO (newIORef [])

typeNamed :: Int32 -> ITypeLib a -> IO String
typeNamed index typelib = typelib # getItemName index

-----------------------------------------------------------
-- Extend ITypeLib with easy name retrieval
-----------------------------------------------------------

getLibName :: ITypeLib a -> IO (String, String, Word32, String)
getLibName typelib = typelib # getDocumentationTL (-1)

getItemName :: Int32 -> ITypeLib a -> IO String
getItemName index typelib = do
  (name,_,_,_) <- typelib # getDocumentationTL index
  return name

getMemberName :: Int32 -> ITypeInfo a -> IO Name
getMemberName index typeinfo = do
  (name,_,_,_) <- typeinfo # getDocumentation index
  return name

-----------------------------------------------------------
-- Utils
-----------------------------------------------------------

getHelpAttributesTI :: Int -> ITypeInfo a -> IO (String, String, Word32, String, Word32, String)
getHelpAttributesTI index typeinfo = do
  (libName, docString, hContext, hString) <- typeinfo # getDocumentation (fromIntegral index)
  (hStringContext, hStringDll) <-
      catch 
       ( do
	  typeinfo2 <- typeinfo # queryInterface iidITypeInfo2
	  (_,ctxt, dlln) <- typeinfo2 # getDocumentation2 (-1) lOCALE_USER_DEFAULT
	  return (ctxt, dlln))
       ( \ _ -> return (0,""))
  return (libName, docString, hContext, hString, hStringContext, hStringDll)

getCustomTL :: ITypeLib a -> IO [Attribute]
getCustomTL typelib = do
  citems <- 
	catch
	   (do 
	      typelib2 <- typelib # queryInterface iidITypeLib2
	      (TagCUSTDATA cs) <- typelib2 # getAllCustDataTL
	      return cs)
	   (\ _ -> return [])

  mapM toCustom citems

getCustomTI :: ITypeInfo a -> IO [Attribute]
getCustomTI itypeinfo = do
  citems <- 
	catch
	   (do 
	      itypeinfo2 <- itypeinfo # queryInterface iidITypeInfo2
	      (TagCUSTDATA cs) <- itypeinfo2 # getAllCustData
	      return cs)
	   (\ _ -> return [])
  mapM toCustom citems

toCustom :: CUSTDATAITEM -> IO Attribute
toCustom ci = do
    (pstr,_) <- readVarString (varValue ci)
    str      <- readBSTR (castPtr pstr)
    return (Attrib (Id "custom") [ AttrLit (GuidLit [show (guid0 ci)])
			         , AttrLit (StringLit str)
			         ])

\end{code}

-----------------------------------------------------------
-- Warnings
-----------------------------------------------------------

\begin{code}

giveWarning :: Level -> [String] -> IO ()
giveWarning level msg = 
    hPutStrLn stderr ("warning: at '" ++ showLevel level ++ 
                      "': " ++ showMsg msg ++ "\n")
 where
   showMsg xs       = concat (map ("\n     "++) xs)
        
   showLevel []     = ""
   showLevel (x:xs) = x ++ concat (map ("."++) xs)

isSet :: (Eq a, Bits a) => a -> a -> b -> Maybe b
isSet val flag yes 
  | val .&. flag == flag = Just yes
  | otherwise		 = Nothing

isSetMb :: (Eq a, Bits a) => a -> a -> Maybe b -> Maybe b
isSetMb val flag yes 
  | val .&. flag == flag = yes
  | otherwise		 = Nothing

isn'tSet :: (Num a, Eq a, Bits a) => a -> a -> b -> Maybe b
isn'tSet val flag no 
  | val .&. flag == 0 = Just no
  | otherwise	      = Nothing

uuidAttribute :: Com.GUID -> Attribute
uuidAttribute g  = Attrib (Id "uuid")  [AttrLit (LitLit g_sans_braces)]
  where
   g_sans_braces = tail (init (show g))

-- horror horror
-- [Why? Isn't this just checking whether TYPEFLAG_FDUAL is set? -- sof]
isDual :: TYPEATTR -> Bool
isDual typeAttr = dualBitSet (wTypeFlags typeAttr)

dualBitSet :: Word16 -> Bool
dualBitSet v = v .&. 0x40 == 0x40

  END_SUPPORT_TYPELIBS -}
\end{code}
