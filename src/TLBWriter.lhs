% 
% (c) The Foo Project, University of Glasgow 1998
%
% @(#) $Docid: Jun. 6th 2003  16:35  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

Generating type libraries from Core IDL.

\begin{code}
module TLBWriter ( writeTLB ) where

import CoreIDL
{- BEGIN_SUPPORT_TYPELIBS 
-- import IO
import BasicTypes
import System.IO
import Data.Bits
import Control.Monad ( when )
import Data.List  ( intersperse, partition )
import PpCore

import Attribute
import Literal
import CoreUtils
import Data.Int
import Data.Word
import Control.Monad
import Data.Maybe ( isJust, fromMaybe )
import Utils ( safe_init, notNull )
import TypeInfo

-- tricking mkdependHS
import 
       HDirect
import
       WideString
import 
       Com         hiding (GUID)
import qualified
       Com         ( GUID )
import 
       Automation  hiding (GUID,DISPID, Member)
import
       AutoPrim    ( writeVarInt, writeVarString )
import 
       TypeLib
import Foreign.Ptr
  END_SUPPORT_TYPELIBS -}
\end{code}

\begin{code}
writeTLB :: [String] -> [Decl] -> IO ()
{- BEGIN_NOT_SUPPORT_TYPELIBS -}
writeTLB _ _ = ioError (userError ("writeTLB: type library writer code not compiled in"))
{- END_NOT_SUPPORT_TYPELIBS -}

{- BEGIN_SUPPORT_TYPELIBS 
-- to the end of the file.
\end{code}

\begin{code}
writeTLB ofnames decls = do
   case interesting_decls of 
        -- If one library is present, write it out to 
        -- the type library file name given last on the command line.
     []  -> return ()
     [x] -> 
       case ofnames of
         (_:_) -> wTLB (Just (last ofnames)) x
         _     -> wTLB Nothing x -- whatever the default name is.
     _   -> 
       mapM_ (wTLB Nothing) interesting_decls
 where
  interesting_decls = filter ofInterest decls

  ofInterest (Library _ _) = True
  ofInterest _             = False 
  
wTLB :: Maybe String -> Decl -> IO ()
wTLB ofname decl = do
      setupTyInfoCache  -- ahem.
#ifdef DEBUG
      hPutStrLn stderr ("wTLB: " ++ show ofname) >> hFlush stderr
#endif
      plib <- (createTypeLib tlib_nm `catch` \ _ -> ioError (userError ("couldn't load: " ++ tlib_nm)))
      plib # setTLBAttrs decl
      catch 
        (do
          plib2 <- plib # queryInterface iidICreateTypeLib2
          setCustInfo (\ x y -> plib2 # setCustDataCTL x y) tlib_id)
        (\ _ -> return ())
#ifdef DEBUG
      hPutStrLn stderr ("wTLB: " ++ show ofname) >> hFlush stderr
#endif
      mapM_ (\ x -> plib # writeDecl x) tlib_decls
#ifdef DEBUG
      hPutStrLn stderr ("wTLB: " ++ show ofname) >> hFlush stderr
#endif
      plib # saveAllChanges
#ifdef DEBUG
      hPutStrLn stderr ("wTLB: " ++ show ofname) >> hFlush stderr
#endif
      return ()
   where
    tlib_id    = declId decl

    tlib_nm    = 
        case ofname of
          Nothing -> idOrigName tlib_id ++ ".tlb"
          Just x  -> x

    tlib_decls = sortDecls (declDecls decl)

setTLBAttrs :: Decl -> ICreateTypeLib a -> IO ()
setTLBAttrs decl typelib = do
  setHelpInfo (\ x -> typelib # setDocStringCTL x)
              (\ x -> typelib # setHelpContextCTL x) 
              i
  setGuidInfo (\ x -> typelib # setGuidCTL x) i
  typelib # setLibFlags (fromIntegral lib_flags)

  when (lcid /= ((-1)::Int)) 
       (typelib # setLcid (fromIntegral lcid))

  tlib_nm_wide <- stringToWide tlib_nm
  typelib # setNameCTL tlib_nm_wide
  setVersionInfo (\ maj min -> typelib # setVersionCTL maj min)
                 i
 where
  i       = declId decl
  attrs   = idAttributes i
  tlib_nm = idOrigName i

  lib_flags = controlFlag + restrictedFlag + hiddenFlag

  controlFlag
    | attrs `hasAttributeWithName` "control" = fromEnum LIBFLAG_FCONTROL
    | otherwise                              = 0

  restrictedFlag
    | attrs `hasAttributeWithName` "restricted" = fromEnum LIBFLAG_FRESTRICTED
    | otherwise                                 = 0

  hiddenFlag
    | attrs `hasAttributeWithName` "hidden"   = fromEnum LIBFLAG_FHIDDEN
    | otherwise                               = 0

  lcid = 
     case findAttribute "lcid" attrs of
       Just (Attribute _ [ParamLit (IntegerLit (ILit _ x))]) -> (fromIntegral x)
       _                                              -> ((-1)::Int)


\end{code}

\begin{code}
writeDecl :: Decl -> ICreateTypeLib a -> IO ()
writeDecl d typelib = 
  case d of
    Typedef{}           -> typelib # writeTypedef d 
     -- Constants are only handled within modules,
     -- so writeModule takes care of filtering these out
     -- and writing 'em out.
    Constant{}          -> return ()
    Interface{}         -> typelib # writeInterface d
    DispInterface{}     -> typelib # writeDispInterface d 
    CoClass{}           -> typelib # writeCoClass d 
    Module{}            -> typelib # writeModule d 
    _                   -> return ()
 where
   -- currently unused, all typedefs are exported.
  -- hasPublicAttr i = (idAttributes i) `hasAttributeWithName` "public"

\end{code}

\begin{code}
writeTypedef :: Decl -> ICreateTypeLib a -> IO ()
writeTypedef (Typedef i t o) typelib
  | isConstructedUnionTy = do
        case unionToStruct t of
          (Nothing, t')       -> typelib # writeTypedef (Typedef i t' o) 
          (Just (u_i,u_t), s_t) -> do
              typelib # writeTypedef (Typedef u_i u_t o)
              typelib # writeTypedef (Typedef i s_t o)
  | isConstructedTy = do
      wstr    <- stringToWide (idOrigName i)
#ifdef DEBUG
      hPutStrLn stderr ("writeTypedef: " ++ show (idOrigName i)) >> hFlush stderr
#endif
      tinfo1  <- typelib # createTypeInfo wstr tkind
      itinfo1 <- tinfo1  # queryInterface iidITypeInfo
      addTyInfo (idOrigName i) itinfo1
      addTyInfo (idName i) itinfo1
      tinfo1 # (case tkind of
                  TKIND_ENUM   -> writeEnum t
                  TKIND_RECORD -> writeRecord t typelib
                  TKIND_UNION  -> writeUnion t typelib)
      setHelpInfo (\ x -> tinfo1 # setDocString x)
                  (\ x -> tinfo1 # setHelpContext x)
                  i
      catch 
        (do
          ti <- tinfo1 # queryInterface iidICreateTypeInfo2
          setCustInfo (\ x y -> ti # setCustData x y) i)
        (\ _ -> return ())
      tinfo1 # layOut
      return ()

  | otherwise = do
      wstr  <- stringToWide (idOrigName i)
      tinfo <- typelib # createTypeInfo wstr TKIND_ALIAS
      itinfo1 <- tinfo # queryInterface iidITypeInfo
      addTyInfo (idOrigName i) itinfo1
      addTyInfo (idName i) itinfo1
      tinfo # setTypeDescAlias (typedesc typelib tinfo t)
      setHelpInfo (\ x -> tinfo # setDocString x)
                  (\ x -> tinfo # setHelpContext x)
                  i
      catch 
        (do
          ti <- tinfo # queryInterface iidICreateTypeInfo2
          setCustInfo (\ x y -> ti # setCustData x y) i)
        (\ _ -> return ())
      tinfo # layOut
      return ()
 where
  (isConstructedTy, isConstructedUnionTy, tkind) =
    case t of
      Struct{}   -> (True, False, TKIND_RECORD)
      Union{}    -> (True, True,  TKIND_UNION)
      UnionNon{} -> (True, True,  TKIND_UNION)
      CUnion{}   -> (True, False, TKIND_UNION)
      Enum{}     -> (True, False, TKIND_ENUM)
      _          -> (False, False, TKIND_MAX)

typedesc :: ICreateTypeLib b -> ICreateTypeInfo a -> Type -> TYPEDESC
typedesc tlib ti t =  
  case t of
      Float Short        -> simpleDesc VT_R4
      Float Long         -> simpleDesc VT_R8
      Integer Short signed
         | signed    -> simpleDesc VT_I2
         | otherwise -> simpleDesc VT_UI2
      Integer Long  signed
         | signed    -> simpleDesc VT_I4
         | otherwise -> simpleDesc VT_UI4
      Integer Natural  signed
         | signed    -> simpleDesc VT_I4
         | otherwise -> simpleDesc VT_UI4
      Integer LongLong  signed
         | signed    -> simpleDesc VT_I8
         | otherwise -> simpleDesc VT_UI8
      Char False         -> simpleDesc VT_UI1
      Char True          -> simpleDesc VT_I1
      WChar              -> simpleDesc VT_I2  -- in line with what MIDL does.
      String{}           -> simpleDesc VT_LPSTR
      WString{}          -> simpleDesc VT_LPWSTR
      Void               -> simpleDesc VT_VOID
      SafeArray ty       -> 
            let td = typedesc tlib ti ty in
            TagTYPEDESC (Lptdesc (Just td))
                        (fromIntegral (fromEnum VT_SAFEARRAY))
      Array ty bnds       -> 
            let td   = typedesc tlib ti ty
                lens = map (fromIntegral.evalExpr) bnds
                ad   = TagARRAYDESC td (map (\ x -> TagSAFEARRAYBOUND (fromIntegral x) 0)
                                            lens)
            in
            
            TagTYPEDESC (Lpadesc (Just ad))
                        (fromIntegral (fromEnum VT_CARRAY))
      Name "VARIANT" _ _ _ _ _ -> simpleDesc VT_VARIANT
      Name _ "VARIANT" _ _ _ _ -> simpleDesc VT_VARIANT
      Name "IHC_TAG_3" _ _ _ _ _ -> simpleDesc VT_VARIANT
      Name "HRESULT" _ _ _ _ _ -> simpleDesc VT_HRESULT
      Pointer _ _ ty       -> ptrDesc (typedesc tlib ti ty)
      Name nm _ _ _ origTy mb_ti ->
        case lookupTyInfo nm of
          Just it -> unsafePerformIO $ do -- proof obligation! :)
             hr <- ti # addRefTypeInfo it
             return (TagTYPEDESC (Hreftype hr) (fromIntegral (fromEnum VT_USERDEFINED)))
          Nothing -> unsafePerformIO $
             case mb_ti of
               Just tyinfo | isJust (auto_vt tyinfo) -> do
                let (Just vt) = auto_vt tyinfo
                return (simpleDesc vt)
               _ -> do
                hPutStrLn stderr ("failed to find: " ++ show nm)
                case origTy of
                  Nothing -> do
                    hPutStrLn stderr ("..and it's type expansion. That's a shame - interpreting it as a VARIANT*")
                    return (simpleDesc VT_VARIANT) -- ToDo: emit *warning/error*
                  Just e_t  -> do
                    hPutStrLn stderr ("but found type expansion - everything's cool.")
                    tlib # writeDecl (Typedef (mkId nm nm Nothing []) e_t e_t)
                     -- retry...shouldn't loop, but if it does the user will see..
                    return (typedesc tlib ti t)
      Iface nm _ _ _ _ _ ->
                case lookupTyInfo nm of
                    Just it -> unsafePerformIO $ do -- proof obligation! :)
                        hr <- ti # addRefTypeInfo it
                        return (TagTYPEDESC (Hreftype hr) (fromIntegral (fromEnum VT_USERDEFINED)))
                    Nothing -> simpleDesc VT_UNKNOWN -- ToDo: emit *warning/error*
      _ -> error ("typedesc: can't handle " ++ showCore (ppType t))

 where                          
  ptrDesc td = TagTYPEDESC (Lptdesc (Just td))
                           (fromIntegral (fromEnum VT_PTR))

  simpleDesc x = TagTYPEDESC IHC_TAG_3_Anon (fromIntegral (fromEnum x))

\end{code}

An enumeration is stored as a set of constant values:

\begin{code}
writeEnum :: Type -> ICreateTypeInfo a -> IO ()
writeEnum (Enum i _ vals) tinfo = do
#ifdef DEBUG
  hPutStrLn stderr ("writeEnum: " ++ show (idOrigName i)) >> hFlush stderr
#endif
  sequence (map writeEnumTag (zip [(0::Word32)..] vals))
  tinfo # setTypeFlags tflags
  setGuidInfo (\ x -> tinfo # setGuid x) i
  setHelpInfo (\ x -> tinfo # setDocString x)
              (\ x -> tinfo # setHelpContext x)
              i
  setVersionInfo (\ maj min -> tinfo # setVersion maj min)
                 i
  catch 
    (do
      ti <- tinfo # queryInterface iidICreateTypeInfo2
      setCustInfo (\ x y -> ti # setCustData x y) i)
    (\ _ -> return ())
  return ()
 where
  writeEnumTag (index, val) = do
    tinfo # addVarDesc index vardesc
    wstr <- stringToWide (idName (enumName val))
    tinfo # setVarName index wstr
     -- helpstrings on enum tags..nothing's stopping us, I suppose..
    setHelpInfo (\ x -> tinfo # setVarDocString index x)
                (\ x -> tinfo # setVarHelpContext index x) 
                (enumName val)
    catch 
      (do
        ti <- tinfo # queryInterface iidICreateTypeInfo2
        setCustInfo (\ x y -> ti # setVarCustData index x y) i)
      (\ _ -> return ())
    return ()
   where
    vardesc = TagVARDESC (fromIntegral index) nullWideString 
                         (LpvarValue (Just v)) ed 0 VAR_CONST
    ed = TagELEMDESC td pd
     --ToDo: honour v1_enum (or its abscence, as the case might be here.)
    td = TagTYPEDESC IHC_TAG_3_Anon (fromIntegral (fromEnum VT_I4))
    pd = TagPARAMDESC Nothing 0
    v  = unsafePerformIO $ 
         case (enumValue val) of
            Left value -> do
                var <- allocBytes (fromIntegral sizeofVARIANT)
                writeVarInt value var
                return var
            Right e -> do
                var <- allocBytes (fromIntegral sizeofVARIANT)
                writeVarInt (fromIntegral (evalExpr e)) var
                return var

  tflags = computeTypeFlags i


\end{code}

\begin{code}
writeRecord :: Type -> ICreateTypeLib b -> ICreateTypeInfo a -> IO ()
writeRecord s_ty@(Struct i fields _) typelib tinfo = do
  let (_,offs) = computeStructSizeOffsets Nothing fields
  zipWithM_ writeField [0..] (zip offs fields)
  tinfo # setTypeFlags tflags
  setGuidInfo (\ x -> tinfo # setGuid x) i
  setHelpInfo (\ x -> tinfo # setDocString x)
              (\ x -> tinfo # setHelpContext x)
              i
  setVersionInfo (\ maj min -> tinfo # setVersion maj min)
                 i
  tinfo # setAlignment (fromIntegral struct_align)
  catch 
        (do
          tinfo2 <- tinfo # queryInterface iidICreateTypeInfo2
          setCustInfo (\ x y -> tinfo2 # setCustData x y) i)
        (\ _ -> return ())
   -- writeTypedef will call 'layOut' for us.
  return ()
 where
  tflags = computeTypeFlags i
  
   -- hmm, might we run into unwanted problems here?
   -- If so, define 'struct_align' to be 1 so as to
   -- make it the 'natural' alignment.
  (_, struct_align) = sizeAndAlignModulus Nothing s_ty

  writeField idx (off, field) = do
    tinfo # addVarDesc idx vardesc
    wstr <- stringToWide (idOrigName (fieldId field))
    tinfo # setVarName idx wstr
    setHelpInfo (\ x -> tinfo # setVarDocString idx x)
                (\ x -> tinfo # setVarHelpContext idx x) 
                i
    catch 
        (do
          tinfo2 <- tinfo # queryInterface iidICreateTypeInfo2
          setCustInfo (\ x y -> tinfo2 # setVarCustData idx x y) i)
        (\ _ -> return ())
    return ()
   where
    vardesc = TagVARDESC (fromIntegral idx) nullWideString 
                         (OInst (fromIntegral off)) ed wflags VAR_PERINSTANCE
    ed      = TagELEMDESC td pd
    td      = typedesc typelib tinfo (fieldType field)
    pd      = TagPARAMDESC Nothing 0
    wflags  = computeVarFlags (fieldId field)

\end{code}

\begin{code}
writeUnion :: Type -> ICreateTypeLib b -> ICreateTypeInfo a -> IO ()
writeUnion (CUnion i fields _) typelib tinfo = do
  zipWithM_ writeField [0..] fields
  tinfo # setTypeFlags tflags
  setGuidInfo (\ x -> tinfo # setGuid x) i
  setHelpInfo (\ x -> tinfo # setDocString x)
              (\ x -> tinfo # setHelpContext x)
              i
  setVersionInfo (\ maj min -> tinfo # setVersion maj min)
                 i
  tinfo # setAlignment 1 --(fromIntegral struct_align)
  catch 
        (do
          tinfo2 <- tinfo # queryInterface iidICreateTypeInfo2
          setCustInfo (\ x y -> tinfo2 # setCustData x y) i)
        (\ _ -> return ())
   -- writeTypedef will call 'layOut' for us.
  return ()
 where
  tflags = computeTypeFlags i
  
   -- hmm, might we run into unwanted problems here?
   -- If so, define 'struct_align' to be 1 so as to
   -- make it the 'natural' alignment.
--  (_, struct_align) = sizeAndAlignModulus Nothing s_ty

  writeField idx field = do
    tinfo # addVarDesc idx vardesc
    wstr <- stringToWide (idOrigName (fieldId field))
    tinfo # setVarName idx wstr
    setHelpInfo (\ x -> tinfo # setVarDocString idx x)
                (\ x -> tinfo # setVarHelpContext idx x) 
                i
    catch 
        (do
          tinfo2 <- tinfo # queryInterface iidICreateTypeInfo2
          setCustInfo (\ x y -> tinfo2 # setVarCustData idx x y) i)
        (\ _ -> return ())
    return ()
   where
    vardesc = TagVARDESC (fromIntegral idx) nullWideString 
                         (OInst 0) ed wflags VAR_PERINSTANCE
    ed      = TagELEMDESC td pd
    td      = typedesc typelib tinfo (fieldType field)
    pd      = TagPARAMDESC Nothing 0
    wflags  = computeVarFlags (fieldId field)

writeUnion _ _ _ = return ()
\end{code}

\begin{code}
writeInterface :: Decl -> ICreateTypeLib a -> IO ()
writeInterface (Interface i is_ref inherits decls) typelib 
 | is_ref    = return ()
 | otherwise = do
  wstr   <- stringToWide (idOrigName i)
  tinfo <- typelib # createTypeInfo wstr TKIND_INTERFACE
    -- stash away the ITypeInfo for later references to this
    -- iface to make use of.
  ti    <- tinfo # queryInterface iidITypeInfo
  addTyInfo (idOrigName i) ti
  let (ms, non_ms) = partition isMethod decls
  let (_, non_cs)  = partition isConst non_ms
    -- lift the non-method/const decls out to the top (notably tydefs.)
    -- Do these first since the methods may refer to the typedefs, so
    -- their names had better be in the ITypeInfo-cache.
    --
  tinfo # setInherit inherits
  mapM_ (\ x -> typelib # writeDecl x) non_cs
  zipWithM_ (writeMethod True Nothing typelib tinfo) [0..] ms
    -- we blatantly ignore constants inside interface{}s
    -- (as does MIDL), as the typelib format ain't up to it.
    -- Oh well, no one will notice..
  tinfo # setTypeFlags tflags
  setGuidInfo (\ x -> tinfo # setGuid x) i
  setHelpInfo (\ x -> tinfo # setDocString x)
              (\ x -> tinfo # setHelpContext x)
              i
  setVersionInfo (\ maj min -> tinfo # setVersion maj min)
                 i
  catch 
      (do
        tin <- tinfo # queryInterface iidICreateTypeInfo2
        setCustInfo (\ x y -> tin # setCustData x y) i)
      (\ _ -> return ())
  tinfo # layOut
  return ()
 where
  attrs = idAttributes i

  isDual = attrs `hasAttributeWithName` "dual" 

  is_idispatchy = 
    isDual || "IDispatch" `elem` map (qName.fst) inherits

  tflags =
    (ifSet (tflags_raw .&. (fromEnum32 TYPEFLAG_FDUAL) /= 0)
           (fromEnum32 TYPEFLAG_FOLEAUTOMATION)) .|.
    (ifSet is_idispatchy 
           (fromEnum32 TYPEFLAG_FDISPATCHABLE)) .|.
    tflags_raw

  tflags_raw = computeTypeFlags i
\end{code}

\begin{code}
paramDesc :: Param -> PARAMDESC
paramDesc p = TagPARAMDESC desc_ex flags
 where
  attrs = idAttributes (paramId p)

  has_def_val = attrs `hasAttributeWithName` "defaultvalue"

  flags = 
    ifSet (attrs `hasAttributeWithName` "lcid")         pARAMFLAG_FLCID       .|.
    ifSet (attrs `hasAttributeWithName` "retval")       pARAMFLAG_FRETVAL     .|.
    ifSet (attrs `hasAttributeWithName` "optional")     pARAMFLAG_FOPT        .|.
    ifSet has_def_val                                   pARAMFLAG_FHASDEFAULT .|.
    (case (paramMode p) of
      In    -> pARAMFLAG_FIN
      Out   -> pARAMFLAG_FOUT
      InOut -> pARAMFLAG_FOUT .|. pARAMFLAG_FIN)
    
  desc_ex 
   | has_def_val = Just (TagPARAMDESCEX 24 def_var)
   | otherwise   = Nothing

  def_var =
       case findAttribute "defaultvalue" attrs of
         Just (Attribute _ [ParamLit (StringLit  x)]) -> unsafePerformIO $ do
                        p_bstr <- marshallBSTR x
                        var    <- allocBytes (fromIntegral sizeofVARIANT)
                        writeVarString (castPtr p_bstr) var -- poorly named, should be writeVarBSTR
                        return var
         Just (Attribute _ [ParamLit (IntegerLit (ILit _ x))]) -> unsafePerformIO $ do
                       var <- allocBytes (fromIntegral sizeofVARIANT)
                       writeVarInt (fromIntegral x) var
                       return var
         _ -> unsafePerformIO $ do
            var <- allocBytes (fromIntegral sizeofVARIANT)
            writeVarInt 0 var
            return var

\end{code}

Deceptively similar to what's done for an 'interface'; record
properties as 'variables' (via writeProp).

\begin{code}
writeDispInterface :: Decl -> ICreateTypeLib a -> IO ()
writeDispInterface (DispInterface i ii props meths) typelib = do
  wstr   <- stringToWide (idOrigName i)
  tinfo <- typelib # createTypeInfo wstr TKIND_DISPATCH
  tinfo # setTypeFlags tflags
    -- stash away the ITypeInfo for later references to this
    -- iface to make use of.
  ti    <- tinfo # queryInterface iidITypeInfo
  addTyInfo (idOrigName i) ti
  mapM_ (writeProp typelib tinfo) props
  (case lookupTyInfo "IDispatch" of
        Nothing -> return ()
        Just it -> do
             hr <- tinfo # addRefTypeInfo it
             tinfo # addImplType 0 hr
             return ())
  (case ii of
     Just (Interface{declId=id}) ->
      case lookupTyInfo (idName id) of
        Nothing -> 
          let nm = idName id in
          hPutStrLn stderr ("Help - inherited from interface: " ++ show nm ++
                            " , but couldn't find its ITypeInfo")
        Just it -> do
             hr <- tinfo # addRefTypeInfo it
             tinfo # addImplType 1 hr
             return ()
     _ -> return ())
  when (not (isJust ii)) (zipWithM_ (writeMethod False Nothing typelib tinfo) [0..] meths)
  setVersionInfo (\ maj min -> tinfo # setVersion maj min)
                 i
  setGuidInfo (\ x -> tinfo # setGuid x) i
  setHelpInfo (\ x -> tinfo # setDocString x)
              (\ x -> tinfo # setHelpContext x)
              i
  catch 
      (do
        tin <- tinfo # queryInterface iidICreateTypeInfo2
        setCustInfo (\ x y -> tin # setCustData x y) i)
      (\ _ -> return ())
  tinfo # layOut
  return ()
 where
   -- SetTypeFlags() barfs if you pass it this for a dispinterface.
   -- Beats me why it needs to be so strict.
  tflags = tflags_raw .&. (complement (fromIntegral (fromEnum TYPEFLAG_FOLEAUTOMATION)))
  tflags_raw = computeTypeFlags i

\end{code}

\begin{code}
writeCoClass :: Decl -> ICreateTypeLib a -> IO ()
writeCoClass (CoClass i ds) typelib = do
    wstr  <- stringToWide (idOrigName i)
    tinfo <- typelib # createTypeInfo wstr TKIND_COCLASS
    setGuidInfo (\ x -> tinfo # setGuid x) i
    foldM (writeCoClassDecl tinfo) 0 ds
    tinfo # setTypeFlags c_flags
    setVersionInfo (\ maj min -> tinfo # setVersion maj min)
                   i
    setHelpInfo (\ x -> tinfo # setDocString x)
                (\ x -> tinfo # setHelpContext x)
                i
    catch 
      (do
        ti <- tinfo # queryInterface iidICreateTypeInfo2
        setCustInfo (\ x y -> ti # setCustData x y) i)
      (\ _ -> return ())
    tinfo # layOut
    return ()
  where
   attrs = idAttributes i

   writeCoClassDecl tinfo idx d = 
       let nm = idOrigName (coClassId d) in
       case lookupTyInfo nm of
         Nothing -> do
            hPutStrLn stderr ("writeCoClass: Warning - couldn't find type info for " ++ show nm)
            case (coClassDecl d) of
              Nothing -> return idx
              Just de -> do
                  typelib # writeDecl de
                    -- it should have been added to the cache by now.
                  writeCoClassDecl tinfo idx d
         Just it -> do
            hr <- tinfo # addRefTypeInfo it
            tinfo # addImplType idx hr
            tinfo # setImplTypeFlags idx d_flags
            catch 
                 (do
                   ti <- tinfo # queryInterface iidICreateTypeInfo2
                   setCustInfo (\ x y -> ti # setImplTypeCustData idx x y) i)
                 (\ _ -> return ())
            return (idx+1)
      where
        i_attrs = idAttributes (coClassId d)

        d_flags :: Int32
        d_flags = 
           foldr (\ (nm, val) acc -> ifSet (i_attrs `hasAttributeWithName` nm) val .|. acc) 
                 0
                 [ ("default", 0x1)
                 , ("source",  0x2)
                 , ("restricted", 0x4)
                 , ("defaultvtable", 0x800)
                 ]

   c_flags  :: Word32
   c_flags
    | attrs `hasAttributeWithName` "noncreatable" = c_flags'
    | otherwise                                   = c_flags' .|. 0x02

   c_flags' :: Word32
   c_flags' = computeTypeFlags i

\end{code}

\begin{code}
writeModule :: Decl -> ICreateTypeLib a -> IO ()
writeModule (Module i ds) typelib = do
    wstr  <- stringToWide (idOrigName i)
    tinfo <- typelib # createTypeInfo wstr TKIND_MODULE
    setGuidInfo (\ x -> tinfo # setGuid x) i
    tinfo # setTypeFlags m_flags
    setVersionInfo (\ maj min -> tinfo # setVersion maj min)
           i
    setHelpInfo (\ x -> tinfo # setDocString x)
                (\ x -> tinfo # setHelpContext x)
                i
    let (ms, non_ms) = partition isMethod ds
    let (cs, non_cs) = partition isConst non_ms
     -- MIDL magically lifts typedefs out of a module{} in 
     -- the tlb it generates. So, to follow suit, do we.
    mapM_ (\ x -> typelib # writeDecl x) non_cs
    zipWithM_ (writeMethod False (Just dllname) typelib tinfo) [0..] ms
    zipWithM_ (writeConst typelib tinfo) [0..] cs
    catch 
        (do
          tinfo2 <- tinfo # queryInterface iidICreateTypeInfo2
          setCustInfo (\ x y -> tinfo2 # setCustData x y) i)
        (\ _ -> return ())
    tinfo # layOut

  where
   m_flags = computeTypeFlags i

   dllname = 
       case findAttribute "dllname" (idAttributes i) of
         Just (Attribute _ [ParamLit (StringLit  x)]) -> x
         _ -> ""

\end{code}

Writing out methods in (disp)interfaces:

\begin{code}
writeMethod :: Bool -> Maybe String -> ICreateTypeLib b -> ICreateTypeInfo a -> Word32 ->  Decl -> IO ()
writeMethod isBinary hasDllName typelib tinfo idx (Method f cc res params _) = do
   tinfo # addFuncDesc idx fdesc
   wnames <- mapM stringToWide names
   tinfo # setFuncAndParamNames idx wnames
   setHelpInfo (\ x -> tinfo # setFuncDocString idx x)
               (\ x -> tinfo # setFuncHelpContext idx x)
               f
   when isDllMethod $ do
      w_dll   <- stringToWide dllname
       -- Why, oh why - if the high word of w_entry
       -- is zero, then the low word contains the DLL ordinal. If not,
       -- it contains the entry name. Lovely.
      w_entry <- 
          if isOrdinal then
             word16ToWideString ordinal
          else
             stringToWide entry
      tinfo # defineFuncAsDllEntry idx w_dll w_entry
    -- set custom attributes for the method...
   catch 
        (do
          tinfo2 <- tinfo # queryInterface iidICreateTypeInfo2
          setCustInfo (\ x y -> tinfo2 # setCustData x y) f)
        (\ _ -> return ())
    -- ...and for its parameters.
   catch 
        (do
          ti <- tinfo # queryInterface iidICreateTypeInfo2
          let
            setParamCust i p = 
               setCustInfo (\ x y -> ti # setParamCustData idx i x y) (paramId p)
          zipWithM_ setParamCust [0..] params)
        (\ _ -> return ())
   return ()
  where
      -- Oh yeah, the last parameter of put and putref accessors are unnamed.
      -- Why? Beats me, but it produced some interesting swearwords at the moment
      -- it was discovered that this was why writeMethod was failing!
      -- 
     names       = idOrigName f : (if isPropPut then safe_init param_names else param_names)
     param_names = map (idOrigName.paramId) params

     attrs = idAttributes f

     (entry, ordinal, isOrdinal) =
       case findAttribute "entry" attrs of
         Just (Attribute _ [ParamLit (IntegerLit (ILit _ x))]) -> ("", fromIntegral x, True)
         Just (Attribute _ [ParamLit (StringLit  x)])          -> (x, 0, False)
         _ -> ("", 0, True)
      

     isDllMethod    = isJust hasDllName
     (Just dllname) = hasDllName

     fkind
      | isBinary    = FUNC_PUREVIRTUAL
      | isDllMethod = FUNC_STATIC
      | otherwise   = FUNC_DISPATCH

     fdesc = 
       TagFUNCDESC memid [] elemdesc_params
                   fkind invkind
                   cc_fd no_opt_params ovft
                   elemdesc_res f_flags
                   
     ovft
      | isDllMethod = fromIntegral memid
      | otherwise   = fromIntegral mEMBER_NULL

     memid 
       | not isDllMethod = 
          case findAttribute "id" attrs of
            Just (Attribute _ [ParamLit (IntegerLit (ILit _ x))]) -> fromIntegral x
            _ -> fromIntegral idx

         -- This one is odd, for some reason the memberid has be an offset of
         -- the below value. I can't make out why bits 2 and 3 of the msb
         -- needs to be set just from looking at the docs for a MEMBERID. 
         -- (I figured this one out by peering at the memid fields produced
         -- by MIDL.)
       | otherwise = fromIntegral (0x60000000 + fromIntegral idx)

     (invkind , isPropPut)
       | attrs `hasAttributeWithName` "propget"    = (INVOKE_PROPERTYGET, False)
       | attrs `hasAttributeWithName` "propput"    = (INVOKE_PROPERTYPUT, True)
       | attrs `hasAttributeWithName` "propputref" = (INVOKE_PROPERTYPUTREF, True)
       | otherwise                                 = (INVOKE_FUNC, False)

     elemdesc_params = 
        map (\ p -> TagELEMDESC (typedesc typelib tinfo (paramType p))
                                (paramDesc p)) params

     elemdesc_res = 
         TagELEMDESC
            (typedesc typelib tinfo (resultOrigType res))
            (TagPARAMDESC Nothing 0)

     cc_fd =
       case cc of
         Stdcall  -> CC_STDCALL
         Pascal   -> CC_PASCAL
         Cdecl    -> CC_CDECL
         Fastcall -> CC_FASTCALL

     no_opt_params = fromIntegral $
                     length (filter (hasOptionalAttr.idAttributes.paramId) params)
     
     hasOptionalAttr at = at `hasAttributeWithName` "optional"

     f_flags :: Word16
     f_flags =
        foldr (\ (nm, val) acc -> ifSet (attrs `hasAttributeWithName` nm) val .|. acc) 0 
              [ ("restricted",        0x1)
              , ("source",            0x2)
              , ("bindable",          0x4)
              , ("requestedit",       0x8)
              , ("displaybind",      0x10)
              , ("defaultbind",      0x20)
              , ("hidden",           0x40)
              , ("usesgetlasterror", 0x80)
              , ("defaultcollelem", 0x100)
              , ("uidefault",       0x200)
              , ("nonbrowsable",    0x400)
              , ("replaceable",     0x800)
              , ("immediatebind",  0x1000)
              ]

writeMethod _ _ _ _ _ _ = return ()

writeProp :: ICreateTypeLib b -> ICreateTypeInfo a -> Decl -> IO ()
writeProp typelib tinfo (Property i ty _ _ _) = do
    tinfo # addVarDesc memid vardesc
    wstr <- stringToWide (idOrigName i)
    tinfo # setVarName memid wstr
    setHelpInfo (\ x -> tinfo # setVarDocString memid x)
                (\ x -> tinfo # setVarHelpContext memid x)
                i
    catch 
        (do
          tinfo2 <- tinfo # queryInterface iidICreateTypeInfo2
          setCustInfo (\ x y -> tinfo2 # setVarCustData memid x y) i)
        (\ _ -> return ())
    return ()
 where
    attrs   = idAttributes i

    vardesc = TagVARDESC (fromIntegral (fromIntegral memid)) nullWideString 
                         (LpvarValue (Just v)) ed wflags VAR_DISPATCH
    ed = TagELEMDESC td pd
    td = typedesc typelib tinfo ty
    pd = TagPARAMDESC Nothing 0
    v  = unsafePerformIO $ do
                var <- allocBytes (fromIntegral sizeofVARIANT)
                writeVarInt 0 var
                return var
    wflags = computeVarFlags i

    memid  =
      case findAttribute "id" attrs of
        Just (Attribute _ [ParamLit (IntegerLit (ILit _ x))]) -> fromIntegral x
        _ -> 0

writeProp _ _ _ = return ()

\end{code}

\begin{code}
writeConst :: ICreateTypeLib b -> ICreateTypeInfo a -> Word32 -> Decl -> IO ()
writeConst typelib tinfo idx (Constant i ty _ e) = do
   tinfo # addVarDesc memid vardesc
   wstr <- stringToWide (idOrigName i)
   tinfo # setVarName memid wstr
   setHelpInfo (\ x -> tinfo # setVarDocString memid x)
               (\ x -> tinfo # setVarHelpContext memid x)
               i
   catch 
     (do
       ti <- tinfo # queryInterface iidICreateTypeInfo2
       setCustInfo (\ x y -> ti # setVarCustData memid x y) i)
     (\ _ -> return ())
   return ()
 where
    attrs   = idAttributes i

    vardesc = TagVARDESC 0 nullWideString 
                         (LpvarValue (Just v)) ed 0{-no VARFLAGS-} VAR_CONST
    ed = TagELEMDESC td pd
    td = typedesc typelib tinfo ty
    pd = TagPARAMDESC Nothing 0
    v  = unsafePerformIO $ 
         case e of
            Lit l -> do
              p_bstr <- marshallBSTR (litToString l)
              var    <- allocBytes (fromIntegral sizeofVARIANT)
              writeVarString (castPtr p_bstr) var -- poorly named, should be writeVarBSTR
              return var
            _ -> do
                -- ToDo: look for other exprs.
                var <- allocBytes (fromIntegral sizeofVARIANT)
                hPutStrLn stderr "writeConst: cannot handle expr"
                writeVarInt 1 var
                return var

    memid  =
      case findAttribute "entry" attrs of
        Just (Attribute _ [ParamLit (IntegerLit (ILit _ x))]) -> fromIntegral x
        _ -> idx

writeConst _ _ _ _ = return ()

\end{code}

\begin{code}
-- Not the same as MEMBER_NIL, but the value we
-- use to fill in empty slots (which LayOut() will
-- decorate for us) with.
mEMBER_NULL :: Int16
mEMBER_NULL = 0

mEMBER_NIL :: Int16
mEMBER_NIL = (-1)

\end{code}

Given the ICreateTypeInfo for an interface and its list of
interface names it inherits from - set the inheritance
info.

In the case of it being "IUnknown" or "IDispatch", we know
their home (stdole2) and set the inheritance info accordingly.

Note: the assumption is that the ITypeInfo for any non-builtin
interfaces will have been put in the Href-cache by now. If not,
you lose.

\begin{code}
setInherit :: InterfaceInherit -> ICreateTypeInfo () -> IO ()
setInherit []         _     = return ()
setInherit ((qn,_):_) tinfo = do
     case lookupTyInfo (qName qn) of
        Nothing -> 
          let nm = qName qn in
          hPutStrLn stderr ("Help - inherited from interface: " ++ show nm ++
                            " , but couldn't find its ITypeInfo")
        Just it -> do
             hr <- tinfo # addRefTypeInfo it
             tinfo # addImplType 0 hr
             return ()

setupTyInfoCache :: IO ()
setupTyInfoCache = do
     resetTyInfoCache
      -- create a cross-reference to IUnknown / IDispatch impl in Stdole32
     let guid    = mkGUID "{00020430-0000-0000-C000-000000000046}"
         majVer  = 2::Int
         minVer  = 0::Int
         lcid   =  0::Int
     tlbOle <- loadRegTypeLib guid majVer minVer lcid
     count  <- tlbOle # getTypeInfoCount
     mapM_ (addTy tlbOle) [(0::Word32)..(count-1)]
      -- some aliases.
     addTyInfo "IID" (fromMaybe (error "failed to find IID") -- 
                                (lookupTyInfo "GUID"))
     addTyInfo "CLSID" (fromMaybe (error "failed to find CLSID") -- 
                                  (lookupTyInfo "GUID"))
     return ()
 where 
  addTy tlb i = do
    (name,_,_,_) <- tlb # getDocumentationTL (word32ToInt32 i)
    if (ofInterest name) then do
       ti <- tlb # getTypeInfo i
       addTyInfo name ti
     else
       return ()

  ofInterest n = n `elem` prim_ls

  prim_ls = [ "IUnknown"
            , "IDispatch"
            , "GUID"
            ]

\end{code}

Secret mapping of type names to ITypeInfo* for types
we've already grabbed hold of. The i-pointers get mapped
to a HREFTYPE val at the point of use.

\begin{code}
tyi_refs :: IORef [(String, ITypeInfo ())]
tyi_refs = unsafePerformIO (newIORef [])

resetTyInfoCache :: IO ()
resetTyInfoCache = writeIORef tyi_refs []

addTyInfo :: String -> ITypeInfo () -> IO ()
addTyInfo nm iptr = do
  ls <- readIORef tyi_refs
  writeIORef tyi_refs ((nm, iptr):ls)

lookupTyInfo :: String -> Maybe (ITypeInfo ())
lookupTyInfo nm = unsafePerformIO $ do
  ls <- readIORef tyi_refs
  return (lookup nm ls)
\end{code}

\begin{code}
ifSet :: (Num a) => Bool -> a -> a
ifSet True x  = x
ifSet _    _  = 0

fromEnum32 :: Enum a => a -> Word32
fromEnum32 x = fromIntegral (fromEnum x)

fromEnum16 :: Enum a => a -> Word16
fromEnum16 x = fromIntegral (fromEnum x)
\end{code}

Helper functions which abstract away from methods with
identical functionality that's provided by both ICreateTypeLib
and ICreateTypeInfo.

\begin{code}
setHelpInfo :: (WideString -> IO ()) -- write out helpstring
            -> (Word32 -> IO ())     -- write out helpcontext
            -> Id
            -> IO ()
setHelpInfo wr_str wr_ctxt i = do
  when (notNull doc_str) $ do
            wstr <- stringToWide doc_str
            wr_str wstr
  when (h_ctxt /= 0) (wr_ctxt h_ctxt)
  return ()
 where
  attrs   = idAttributes i
  doc_str =
    case findAttribute "helpstring" attrs of
      Just (Attribute _ [ParamLit (StringLit str)]) -> str
      _ -> []

  h_ctxt :: Word32
  h_ctxt =
     case findAttribute "helpcontext" attrs of
       Just (Attribute _ [ParamLit (IntegerLit (ILit _ v))]) -> fromIntegral v
       _ -> 0

setVersionInfo :: (Word16 -> Word16 -> IO ())
               -> Id
               -> IO ()
setVersionInfo wr_version i = do
  when (isJust versionInfo) $ 
       wr_version (fromIntegral major) (fromIntegral minor)
 where   
  attrs       = idAttributes i

  versionInfo =
     case findAttribute "version" attrs of
       Just (Attribute _ [ParamLit (FloatingLit (d,_))]) -> 
                -- sigh, brittle allright.
                let (maj,min) = break (=='.') d in
                Just (read maj,read (tail min))
       _ -> Nothing

  Just (major, minor) = versionInfo

setGuidInfo :: (Com.GUID -> IO ())
            -> Id
            -> IO ()
setGuidInfo wr_guid i = when (notNull guid_str) 
                             (wr_guid (mkGUID guid_str))
 where
  attrs    = idAttributes i

  guid_str = 
    case getUuidAttribute attrs of
      Just [g] -> 
        case g of
          '{':_ -> g
          _     -> '{':g ++ "}"
       -- shouldn't happen, but who cares.
      Just gs  -> '{':concat (intersperse "-" gs) ++ "}"
      _        -> []

setCustInfo :: (Com.GUID -> VARIANT -> IO ())
            -> Id
            -> IO ()
setCustInfo wr_cust i = mapM_ writeCustom customs
 where
  writeCustom (guid, v) = do
    let p_guid = mkGUID guid
    p_bstr <- marshallBSTR v
    var   <- allocBytes (fromIntegral sizeofVARIANT)
    writeVarString (castPtr p_bstr) var -- poorly named, should be writeVarBSTR
    wr_cust p_guid var
 
  attrs   = idAttributes i

  customs = 
    map customise (filterAttributes attrs ["custom"])
    
  customise (Attribute _ [ ParamLit l1, ParamLit l2]) = (s, litToString l2)
     where
       s = case (litToString l1) of
            ls@('{':_) -> ls
            xs         -> '{':xs ++ "}"

  customise (Attribute _ [ ParamExpr (Lit (GuidLit [s]))
                         , ParamExpr (Lit l)
                         ]) = (s, litToString l)
  customise a = error ("setCustInfo: oops - can't handle " ++ showCore (ppAttr a))
                         

\end{code}

\begin{code}
computeTypeFlags :: Id -> Word32
computeTypeFlags i = tflags
 where
  attrs = idAttributes i

  tflags =
    foldr (\ (x,val) acc -> ifSet (attrs `hasAttributeWithName` x) (fromEnum32 val) .|. acc) 0
          [ ("appobject", TYPEFLAG_FAPPOBJECT)
          , ("creatable", TYPEFLAG_FCANCREATE)
          , ("licensed",  TYPEFLAG_FLICENSED)
          , ("predecl",   TYPEFLAG_FPREDECLID) 
              -- wild&random guess at how this is done at the ODL level
              -- - exactly what is the function of that attr anyway?
          , ("hidden",    TYPEFLAG_FHIDDEN)
          , ("control",   TYPEFLAG_FCONTROL)
          , ("dual",      TYPEFLAG_FDUAL)
          , ("nonextensible", TYPEFLAG_FNONEXTENSIBLE)
          , ("oleautomation", TYPEFLAG_FOLEAUTOMATION)
          , ("restricted",    TYPEFLAG_FRESTRICTED)
          , ("aggregatable",  TYPEFLAG_FAGGREGATABLE)
          ]

computeVarFlags :: Id -> Word16
computeVarFlags i = wflags
  where
  attrs = idAttributes i

  wflags =
    foldr (\ (x,val) acc -> ifSet (attrs `hasAttributeWithName` x) (fromEnum16 val) .|. acc) 0
          [ ("readonly", VARFLAG_FREADONLY)
          , ("source",   VARFLAG_FSOURCE)
          , ("bindable", VARFLAG_FBINDABLE)
          , ("requestedit", VARFLAG_FREQUESTEDIT)
          , ("displaybind", VARFLAG_FDISPLAYBIND)
          , ("defaultbind", VARFLAG_FDEFAULTBIND)
          , ("hidden",      VARFLAG_FHIDDEN)
          , ("restricted",  VARFLAG_FRESTRICTED)
          , ("defaultcollelem", VARFLAG_FDEFAULTCOLLELEM)
          , ("uidefault",    VARFLAG_FUIDEFAULT)
          , ("nonbrowsable", VARFLAG_FNONBROWSABLE)
          , ("replaceable",   VARFLAG_FREPLACEABLE)
          , ("immediatebind", VARFLAG_FIMMEDIATEBIND)
          ]

\end{code}

\begin{code}
  END_SUPPORT_TYPELIBS -}
\end{code}
