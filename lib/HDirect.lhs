%
% (c) The Foo Project, University of Glasgow, 1998
%

Stubs for marshalling and unmarshalling primitive
types.

\begin{code}
{-# OPTIONS -#include "PointerSrc.h" #-}
module HDirect 
	(
	  module HDirect

	, Int8
	, Int16
	, Int32
	, Int64

	, Word8
	, Word16
	, Word32
	, Word64

	, Char
	, Double
	, Float
	, Bool
	
	, Ptr

	, StablePtr
	, deRefStablePtr
	, free
	
	) where

import Char
import Int  ( Int8, Int16, Int32, Int64 )
import Word ( Word8, Word16, Word32, Word64 )
--import Addr
import Monad
import Pointer
import System.IO.Unsafe ( unsafePerformIO )


import Foreign.StablePtr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.Types ( CChar )
import Foreign.C.String
import Foreign.Marshal.Alloc (mallocBytes, free)

import Bits
{- BEGIN_GHC_ONLY
import GlaExts ( Int(..), Int# )
#if __GLASGOW_HASKELL__ >= 505
import GHC.Base ( getTag )
#else
import GlaExts ( dataToTag# )
getTag :: a -> Int#
getTag x = dataToTag# x
{- WAS: x `seq` dataToTag# x
        this won't work 
	  (seq's type is a->b->b, where b isn't 'open',
           but has to be of kind *)
-}
#endif
   END_GHC_ONLY -}

infixl 5 .+.

{- BEGIN_GHC_ONLY
#if __GLASGOW_HASKELL__ >= 601
foreignPtrToPtr = unsafeForeignPtrToPtr
#endif
   END_GHC_ONLY -}
{- BEGIN_NOT_FOR_GHC -}
-- Versionitis. If your version of Hugs is troubled by this one, simply comment it out.
foreignPtrToPtr = unsafeForeignPtrToPtr
{- END_NOT_FOR_GHC -}
\end{code}

At the moment the IDL compiler will emit calls to types with identity marshallers
(i.e., by-value marshallers for primitive, FFI-recognised types), so we need
to provide stubs for these here.


Int* marshalling functions:

\begin{code}
marshallInt :: Int -> IO Int
marshallInt x = return x

unmarshallInt :: Int -> IO Int
unmarshallInt x = return x

writeInt :: Ptr Int -> Int -> IO ()
writeInt ptr v = poke ptr v

readInt :: Ptr Int -> IO Int
readInt ptr = peek ptr

--ToDo: generate host-specific versions of 
sizeofInt :: Word32
sizeofInt = fromIntegral (sizeOf (0 :: Int))

--Int8
marshallInt8 :: Int8 -> IO Int8
marshallInt8 v = return v

unmarshallInt8 :: Int8 -> IO Int8
unmarshallInt8 v = return v

writeInt8 :: Ptr Int8 -> Int8 -> IO ()
writeInt8 ptr v = poke ptr v

readInt8 :: Ptr Int8 -> IO Int8
readInt8 ptr = peek ptr

sizeofInt8 :: Word32
sizeofInt8 = fromIntegral (sizeOf (1 :: Int8))

--Int16
marshallInt16 :: Int16 -> IO Int16
marshallInt16 x = return x
unmarshallInt16 :: Int16 -> IO Int16
unmarshallInt16 x = return x

writeInt16 :: Ptr Int16 -> Int16 -> IO ()
writeInt16 ptr v = poke ptr v

readInt16 :: Ptr Int16 -> IO Int16
readInt16 ptr = peek ptr

sizeofInt16 :: Word32
sizeofInt16 = fromIntegral (sizeOf (0 :: Int16))

-- Int32
marshallInt32 :: Int32 -> IO Int32
marshallInt32 x = return x

unmarshallInt32 :: Int32 -> IO Int32
unmarshallInt32 x = return x

writeInt32 :: Ptr Int32 -> Int32 -> IO ()
writeInt32 ptr v = poke ptr v

readInt32 :: Ptr Int32 -> IO Int32
readInt32 ptr = peek ptr

sizeofInt32 :: Word32
sizeofInt32 = fromIntegral (sizeOf (0::Int32))

marshallInt64 :: Int64 -> IO Int64
marshallInt64 x = return x

unmarshallInt64 :: Int64 -> IO Int64
unmarshallInt64 x = return x

writeInt64 :: Ptr Int64 -> Int64 -> IO ()
readInt64 :: Ptr Int64 -> IO Int64
writeInt64 ptr v = poke ptr v
readInt64 ptr = peek ptr
{-
{- BEGIN_NOT_FOR_GHC -}
writeInt64 ptr v = writeI64 ptr (fromIntegral lo) (fromIntegral hi)
 where
   (hi,lo) = (fromIntegral v) `divMod` (toInteger (maxBound :: Int) + 1)
readInt64 ptr = do
  px <- readI64 ptr
  lo <- peekElemOff px 0
  hi <- peekElemOff px 1
  free ptr
  return (fromIntegral ((toInteger lo) + (toInteger hi) * (toInteger (maxBound :: Int) + 1)))
{- END_NOT_FOR_GHC -}
-}

sizeofInt64 :: Word32
sizeofInt64 = fromIntegral (sizeOf (0 :: Int64))

type Hyper   = Int64
marshallHyper :: Hyper -> IO Int64
unmarshallHyper :: Int64 -> IO Hyper
writeHyper :: Ptr Hyper -> Hyper -> IO ()
readHyper :: Ptr Hyper -> IO Hyper
sizeofHyper :: Word32

marshallHyper   = marshallInt64
unmarshallHyper = unmarshallInt64
writeHyper      = writeInt64
readHyper       = readInt64
sizeofHyper     = fromIntegral (sizeOf (0 :: Int64))

writeInteger :: Ptr Integer -> Integer -> IO ()
writeInteger ptr x = writeInt64 (castPtr ptr) (fromIntegral x)

readInteger :: Ptr Integer -> IO Integer
readInteger ptr = do
  x <- readInt64 (castPtr ptr)
  return (fromIntegral x)

marshallInteger :: Integer -> IO (Int32, Int32)
marshallInteger i =  return (fromInteger lo, fromInteger hi)
 where
   (hi,lo) = i `divMod` (toInteger (maxBound :: Int) + 1)

unmarshallInteger :: (Int32,Int32) -> IO Integer
unmarshallInteger (hi,lo) =  return ((toInteger lo) + (toInteger hi) * (toInteger (maxBound :: Int) + 1))

marshallUInteger :: Integer -> IO (Int32, Int32)
marshallUInteger = marshallInteger

unmarshallUInteger :: (Int32,Int32) -> IO Integer
unmarshallUInteger = unmarshallInteger

readUInteger :: Ptr Integer -> IO Integer
readUInteger = readInteger

writeUInteger :: Ptr Integer -> Integer -> IO ()
writeUInteger = writeInteger

\end{code}

Characters and bytes:

NOTE: we assume that Char is CChar (==an 8-bit byte.)

\begin{code}
marshallChar :: Char -> IO Char
marshallChar x = return x

unmarshallChar :: Char -> IO Char
unmarshallChar x = return x

writeChar :: Ptr Char -> Char -> IO ()
writeChar ptr v = poke ((castPtr ptr) :: Ptr CChar) (castCharToCChar v)

readChar :: Ptr Char -> IO Char
readChar ptr = peek ((castPtr ptr) :: Ptr CChar) >>= return.castCCharToChar

sizeofChar :: Word32
sizeofChar     = fromIntegral (sizeOf (undefined :: CChar))

-- wide chars.
type Wchar_t = Word16

marshallWchar_t :: Wchar_t -> IO Wchar_t
marshallWchar_t   = marshallWord16
unmarshallWchar_t :: Wchar_t -> IO Wchar_t
unmarshallWchar_t = unmarshallWord16
writeWchar_t :: Ptr Wchar_t -> Wchar_t -> IO ()
writeWchar_t      = writeWord16
readWchar_t :: Ptr Wchar_t -> IO Wchar_t
readWchar_t       = readWord16
sizeofWchar_t :: Word32
sizeofWchar_t     = fromIntegral (sizeOf (0::Word16))


type Octet   = Byte
type Byte    = Word8

marshallByte :: Byte -> IO Byte
marshallByte   = marshallWord8
unmarshallByte :: Byte -> IO Byte
unmarshallByte = unmarshallWord8
writeByte :: Ptr Byte -> Byte -> IO ()
writeByte      = writeWord8
readByte :: Ptr Byte -> IO Byte
readByte       = readWord8
sizeofByte :: Word32
sizeofByte     = fromIntegral (sizeOf (0::Word8))
\end{code}

Words:

\begin{code}
-- Word8:
marshallWord8 :: Word8 -> IO Word8
marshallWord8 x = return x

unmarshallWord8 :: Word8 -> IO Word8
unmarshallWord8 x = return x

writeWord8 :: Ptr Word8 -> Word8 -> IO ()
writeWord8 ptr v = poke ptr v

readWord8 :: Ptr Word8 -> IO Word8
readWord8 ptr = peek ptr

sizeofWord8 :: Word32
sizeofWord8 = fromIntegral (sizeOf (undefined :: Word8))

-- Word16:
marshallWord16 :: Word16 -> IO Word16
marshallWord16 x = return x

unmarshallWord16 :: Word16 -> IO Word16
unmarshallWord16 x = return x

writeWord16 :: Ptr Word16 -> Word16 -> IO ()
writeWord16 ptr v = poke ptr v

readWord16 :: Ptr Word16 -> IO Word16
readWord16 ptr = peek ptr

sizeofWord16 :: Word32
sizeofWord16 = fromIntegral (sizeOf (undefined :: Word16))

-- Word32:
marshallWord32 :: Word32 -> IO Word32
marshallWord32 x = return x

unmarshallWord32 :: Word32 -> IO Word32
unmarshallWord32 x = return x

writeWord32 :: Ptr Word32 -> Word32 -> IO ()
writeWord32 ptr v = poke ptr v

readWord32 :: Ptr Word32 -> IO Word32
readWord32 ptr = peek ptr

sizeofWord32 :: Word32
sizeofWord32 = fromIntegral (sizeOf (undefined :: Word32))

marshallWord64 :: Word64 -> IO Word64
marshallWord64 x = return x

unmarshallWord64 :: Word64 -> IO Word64
unmarshallWord64 x = return x

writeWord64 :: Ptr Word64 -> Word64 -> IO ()
readWord64 :: Ptr Word64 -> IO Word64
{- BEGIN_NOT_FOR_GHC -}
writeWord64 _ _ = undefined
readWord64 _ = undefined
{- END_NOT_FOR_GHC -}
{- BEGIN_GHC_ONLY
writeWord64 p v = poke p v
readWord64 p = peek p
   END_GHC_ONLY -}

sizeofWord64 :: Word32
sizeofWord64 = fromIntegral (sizeOf (undefined :: Word64))

\end{code}

Addr marshallers:

begin{code}
marshallAddr :: Ptr Addr -> IO Addr
marshallAddr p = return p

unmarshallAddr :: Ptr Addr -> IO Addr
unmarshallAddr p = return p

writeAddr :: Ptr Addr -> Addr -> IO ()
{- BEGIN_DEBUG
writeAddr ptr a | ptr == nullAddr = ioError (userError "writeAddr: NULL pointer")
   END_DEBUG -}
writeAddr ptr a = writeAddrOffAddr ptr 0 a

readAddr :: Ptr Addr -> IO Addr
readAddr a = readAddrOffAddr a 0

sizeofAddr :: Word32
sizeofAddr     = fromIntegral (sizeOf (undefined :: Foreign.Ptr.Ptr ()))

freeAddr :: Addr -> IO ()
freeAddr = free
end{code}

Double marshallers:

\begin{code}
marshallDouble :: Double -> IO Double
marshallDouble x = return x

unmarshallDouble :: Double -> IO Double
unmarshallDouble x = return x

writeDouble :: Ptr Double -> Double -> IO ()
writeDouble ptr x = poke ptr x

readDouble :: Ptr Double -> IO Double
readDouble ptr = peek ptr

sizeofDouble :: Word32
sizeofDouble   = fromIntegral (sizeOf (undefined :: Double))

writeFloat :: Ptr Float -> Float -> IO ()
writeFloat ptr x = poke ptr x

readFloat :: Ptr Float -> IO Float
readFloat ptr = peek ptr

sizeofFloat :: Word32
sizeofFloat   = fromIntegral (sizeOf (undefined :: Float))

\end{code}

Booleans - represented externally by a long (Int32):

\begin{code}
marshallBool :: Bool -> IO Int32
marshallBool v = marshallInt32 (if v then (-1) else 0)

unmarshallBool :: Int32 -> IO Bool
unmarshallBool v = return (v /= 0)

writeBool :: Ptr Bool -> Bool -> IO ()
writeBool ptr v = writeInt32 (castPtr ptr) (if v then (-1) else 0)

readBool :: Ptr Bool -> IO Bool
readBool ptr = do
  v <- readInt32 (castPtr ptr)
  return ( v /= 0 )

sizeofBool :: Word32
sizeofBool = fromIntegral (sizeOf (0 :: Int32))
\end{code}

\begin{code}
addNCastPtr :: Ptr a -> Word32 -> Ptr b
addNCastPtr v off = v `plusPtr` (fromIntegral off)

derefPtr :: Ptr (Ptr a) -> IO (Ptr a)
derefPtr p = peek p

indexPtr :: Ptr (Ptr a) -> Int -> IO (Ptr a)
indexPtr p off = peekElemOff p off
\end{code}

The unit of allocation is an 8-bit byte.

\begin{code}
allocOutPtr :: IO (Ptr a)
allocOutPtr = alloc 4 -- 4 bytes in a pointer. ToDo: 64-bit platform'ify.

allocBytes :: Int -> IO (Ptr a)
allocBytes len = alloc (fromIntegral len)

allocWords :: Int -> IO (Ptr a)
allocWords len = alloc (4 * fromIntegral len)


alloc :: Word32 -> IO (Ptr a)
alloc s = mallocBytes (fromIntegral s)

doThenFree ::(Ptr a -> IO ()) -> (Ptr b -> IO c) -> Ptr d -> IO c
doThenFree f act ptr = do
   v <- act (castPtr ptr)
   f (castPtr ptr)
   return v

trivialFree :: a -> IO ()
trivialFree _ = return ()

fillIn :: Int -> (Ptr a -> IO ()) -> IO (Ptr a)
fillIn sz refreemarshall = do
  ptr <- allocBytes sz
  refreemarshall ptr
  return ptr

\end{code}

[ptr]Ptr marshalling

\begin{code}
marshallPtr :: Ptr a -> IO (Ptr a)
marshallPtr v = return v

unmarshallPtr :: Ptr a -> IO (Ptr a)
unmarshallPtr v = return v

writePtr :: Ptr (Ptr a) -> Ptr a -> IO ()
writePtr a v = poke a v

readPtr :: Ptr a -> IO (Ptr b)
readPtr a = peek (castPtr a)

writefptr :: Ptr b -> ForeignPtr a -> IO ()
writefptr p v = poke (castPtr p) (foreignPtrToPtr v)
\end{code}

[unique]Ptr marshalling

\begin{code}
marshallunique :: (IO (Ptr a))
               -> (Ptr a -> a -> IO ())
	       -> Maybe a
	       -> IO (Ptr a)
marshallunique allocRef marshallInto mb = 
  case mb of
    Nothing -> return nullPtr
    Just x  -> marshallref allocRef marshallInto x

marshallMaybe :: (a -> IO b) -> b -> Maybe a -> IO b
marshallMaybe _mshall def  Nothing  = return def
marshallMaybe mshall  _def (Just x) = mshall x

writeMaybe :: (Ptr a -> a -> IO ())
           -> Ptr (Maybe a)
	   -> Maybe a
	   -> IO ()
writeMaybe _  ptr Nothing  = writePtr (castPtr ptr) nullPtr
writeMaybe wr ptr (Just x) = wr (castPtr ptr) x

readMaybe :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
readMaybe rd ptr
 | ptr == nullPtr = return Nothing
 | otherwise      = rd ptr >>= return . Just

writeunique :: IO (Ptr a)
	    -> (Ptr a -> a -> IO ())
	    -> Ptr (Maybe a)
	    -> Maybe a
	    -> IO ()
writeunique allocRef marshallInto p mb =
  case mb of
    Nothing  -> writePtr (castPtr p) nullPtr
    Just x   -> writeref allocRef marshallInto (castPtr p) x

unmarshallunique :: (Ptr a -> IO a) -> Ptr a -> IO (Maybe a)
unmarshallunique refUnmarshall ptr
 | ptr == nullPtr = return Nothing
 | otherwise      = do
    x <- unmarshallref refUnmarshall ptr
    return (Just x)


readunique :: (Ptr a -> IO a) -> Ptr b -> IO (Maybe a)
readunique refUnmarshall ptr
 | ptr == nullPtr = return Nothing
 | otherwise      = do
   v <- readPtr (castPtr ptr)
   if nullPtr == v then
      return Nothing
    else do
      x <- refUnmarshall (castPtr v)
      return (Just x)

freeunique :: (Ptr a -> IO ()) -> Ptr (Ptr a) -> IO ()
freeunique freeptee ptr
 | ptr == nullPtr = return ()
 | otherwise      = do
    ptr' <- derefPtr ptr
    freeptee ptr'
    free ptr

\end{code}

Marshalling [unique]void* pointers

\begin{code}
marshallunique_ptr :: Maybe (Ptr a) -> IO (Ptr a)
marshallunique_ptr mb = 
  case mb of
    Nothing -> marshallPtr nullPtr
    Just x  -> marshallPtr x
\end{code}


[ref]Ptr marshalling

\begin{code}
marshallref :: (IO (Ptr a)) -> (Ptr a -> a -> IO ()) -> a -> IO (Ptr a)
marshallref allocRef marshallInto v = do
   px <- allocRef
   marshallInto px v
   return px

writeref :: (IO (Ptr a)) -> (Ptr a -> a -> IO ()) -> Ptr (Ptr a) -> a -> IO ()
writeref allocRef marshallInto ptr v = do
  px <- marshallref allocRef marshallInto v
  writePtr ptr px

unmarshallref :: (Ptr a -> IO b) -> Ptr a -> IO b
unmarshallref refUnmarshall ptr = refUnmarshall ptr

readref :: (Ptr a -> IO a) -> Ptr (Ptr a) -> IO a
readref refUnmarshall ptr = do
  px <- readPtr ptr
  unmarshallref refUnmarshall (castPtr px)

freeref :: (Ptr b -> IO ()) -> Ptr a -> IO ()
freeref free_inner pptr = do
   ptr <- readPtr (castPtr pptr)
   free_inner ptr
   free pptr
\end{code}


All by-reference marshalling and unmarshalling functions
of enums can be expressed using these two stubs:

\begin{code}
writeenum16 :: (b -> IO Int16) -> Ptr Int16 -> b -> IO ()
writeenum16 marshall pv v =
  marshall v >>= \ x ->
  writeInt16 pv x

readenum16 :: (Int16 -> IO a) -> Ptr (Int16) -> IO a
readenum16 unmarshall pv = do
  v <- readInt16 pv
  unmarshall v

marshallEnum16 :: Enum a => a -> IO Int16
marshallEnum16 v = return (fromIntegral (fromEnum v))

unmarshallEnum16 :: Enum a => Int16 -> IO a
unmarshallEnum16 x = return (toEnum (fromIntegral x))

marshallEnum32 :: Enum a => a -> IO Int32
marshallEnum32 v = return (fromIntegral (fromEnum v))

unmarshallEnum32 :: Enum a => Int32 -> IO a
unmarshallEnum32 x = return (toEnum (fromIntegral x))

writeEnum32 :: Enum a => Ptr b -> a -> IO ()
writeEnum32 p v = writeInt32 (castPtr p) (fromIntegral (fromEnum v))

readEnum32 :: Enum a => Ptr b -> IO a
readEnum32 p = do
  x <- readInt32 (castPtr p)
  return (toEnum (fromIntegral x))

writeEnum16 :: Enum a => Ptr b -> a -> IO ()
writeEnum16 p v = writeInt16 (castPtr p) (fromIntegral (fromEnum v))

readEnum16 :: Enum a => Ptr b -> IO a
readEnum16 p = do
  x <- readInt16 (castPtr p)
  return (toEnum (fromIntegral x))

\end{code}

\begin{code}
marshalllist :: Word32
	     -> (Ptr a -> a -> IO ())
	     -> [a]
	     -> IO (Ptr b)
marshalllist szof writeelt ls = do
 arr <- alloc (len*szof)
 foldM writeElt (castPtr arr) ls
 return (castPtr arr)
  where
   len = fromIntegral (length ls)

   writeElt addr v = do
     writeelt addr v
     return (addNCastPtr addr szof)

unmarshalllist :: Word32 -> Word32 -> Word32 -> (Ptr any -> IO a) -> Ptr b -> IO [a]
unmarshalllist szof offset len unpack ptr = do
 let ptr0 = addNCastPtr ptr (offset*szof)
 loop ptr0 len
  where
   loop _ 0 = return []
   loop p n = do
    v  <- unpack p
    let p' = addNCastPtr p szof
    vs <- loop p' (n-1)
    return (v:vs)

unmarshallSingle :: (Ptr a -> IO a) -> Ptr a -> IO [a]
unmarshallSingle ref ptr 
 | ptr == nullPtr = return []
 | otherwise      = do
      x <- ref ptr
      return [x]

writelist :: Bool -> Word32 -> (Ptr a -> a -> IO ()) -> Ptr [a] -> [a] -> IO ()
writelist do_alloc szof writeelt pptr ls = do
 the_ptr <- 
    (if do_alloc then do
        ptr <- alloc (szof * fromIntegral len)
	writePtr (castPtr pptr) ptr
	return (castPtr ptr)
      else
        return (castPtr pptr))
 foldM writeElt the_ptr ls
 return ()
  where
   len = length ls

   writeElt addr v = do
    writeelt addr v
    return (addNCastPtr addr szof)

readlist :: Word32 -> Word32 -> (Ptr a -> IO a) -> Ptr [a] -> IO [a]
readlist szof len unpack ptr = do
 let ptr0 = castPtr ptr
 loop ptr0 len
  where
   loop _ 0 = return []
   loop p n = do
    v  <- unpack p
    let p' = addNCastPtr p szof
    vs <- loop p' (n-1)
    return (v:vs)

freelist :: Word32 -> Word32 -> (Ptr a -> IO ()) -> Ptr [a] -> IO ()
freelist szof len free_elt ptr = do
	go (castPtr ptr) len
	free ptr
  where
    go _pptr 0 = return ()
    go pptr  x = do
       p <- readPtr pptr
       free_elt p
       let pptr' = addNCastPtr pptr szof
       go pptr' (x-1)

\end{code}

Unpacking null terminated character strings:

\begin{code}
marshallString :: String -> IO (Ptr String)
marshallString str = do
 pstr  <- alloc (len*sizeofChar)
 pstr' <- foldM writeElt (castPtr pstr) str
 writeChar (castPtr pstr') '\0'
 return pstr
  where
   len = fromIntegral (length str + 1)

   writeElt addr v = do
     writeChar addr v
     return (addNCastPtr addr sizeofChar)

marshallBString :: Int -> String -> IO (Ptr String)
marshallBString len str = do
 pstr  <- alloc (len'*sizeofChar)
 pstr' <- foldM writeElt (castPtr pstr) (take len str)
 writeChar (castPtr pstr') '\0'
 return pstr
  where
   len' = fromIntegral (len + 1)

   writeElt addr v = do
     writeChar addr v
     return (addNCastPtr addr sizeofChar)

unmarshallString :: Ptr String -> IO String
unmarshallString ptr
 | ptr == nullPtr  = return ""
 | otherwise	   = do
   let ptr0 = addNCastPtr ptr 0
   loop ptr0
  where
   loop p = do
    v  <- readChar p
    if v == '\0'
     then return []
     else do
       let p' = addNCastPtr p sizeofChar
       vs <- loop p'
       return (v:vs)

-- at most len chars. or zero terminated.
unmarshallBString :: Int -> Ptr String -> IO String
unmarshallBString len ptr
 | ptr == nullPtr  = return ""
 | otherwise	   = do
   let ptr0 = addNCastPtr ptr 0
   loop ptr0 0
  where
   loop _ n | n > len = return ""
   loop p n = do
    v  <- readChar p
    if v == '\0'
     then return []
     else do
       let p' = addNCastPtr p sizeofChar
       vs <- loop p' (n+1)
       return (v:vs)

readString :: Ptr (Ptr String) -> IO String
readString pstr = do
  ptr <- readPtr pstr
  unmarshallString ptr

readBString :: Int -> Ptr (Ptr String) -> IO String
readBString len pstr = do
  ptr <- readPtr pstr
  unmarshallBString len ptr

writeString :: Bool -> Ptr String -> String -> IO ()
writeString do_alloc ppstr str = do
  pstr <-
    if not do_alloc then
       return (castPtr ppstr)
     else do
       arr <- alloc (fromIntegral string_len)
       writePtr (castPtr ppstr) arr
       return arr
  pstr' <- foldM writeElt (castPtr pstr) str
  writeChar (castPtr pstr') '\0'
 where
   string_len = length str + 1 {- terminator -}

   writeElt addr v = do
     writeChar addr v
     return (addNCastPtr addr sizeofChar)

writeBString :: Bool -> Int -> Ptr a -> String -> IO ()
writeBString do_alloc len ptr str = do
  pstr  <-
    if do_alloc then
        alloc (fromIntegral len * sizeofChar)
    else
        return (castPtr ptr)
  pstr' <- foldM writeElt pstr (take len str)
  writeChar pstr' '\0'
 where
   writeElt addr v = do
     writeChar addr v
     return (addNCastPtr addr sizeofChar)

freeString :: Ptr String -> IO ()
freeString = free
\end{code}

Sequence marshallers - i.e., R/W a sequence of
values to/from a list.

\begin{code}
marshallSequence :: (Ptr a -> a -> IO ())
		 -> (Ptr a -> IO ())
		 -> Word32
		 -> Maybe Word32
		 -> [a]
		 -> IO (Ptr a)
marshallSequence wElt wTermin szElt mbLen ls = do
   pseq  <- alloc (len*szElt) -- assume that the sequence is packed without gaps.
   pseq' <- foldM writeElt pseq the_ls
   wTermin pseq'
   return pseq'
  where
    (len, the_ls) = 
      case mbLen of
        Nothing -> (fromIntegral (length ls + 1), ls)
	Just x  -> (x + 1, take (fromIntegral x) ls)

    writeElt addr v = do
      wElt addr v
      return (addNCastPtr addr szElt)

unmarshallSequence :: ( Eq a )
		   => (Ptr (Ptr a) -> IO a)
		   -> (Ptr (Ptr a) -> IO Bool)
		   -> Word32
		   -> Maybe Word32
		   -> Ptr (Ptr a)
		   -> IO [a]
unmarshallSequence rElt termPred szElt mbLen ptr
 | ptr == nullPtr  = return []
 | otherwise	   = do
   let ptr0 = addNCastPtr ptr 0
   loop 0 ptr0
  where
   lenPred = 
     case mbLen of
       Nothing -> const False
       Just x  -> \ y -> y >= x

   loop len p = do
    flg <- termPred p
    if flg || (lenPred len)
     then return []
     else do
       v  <- rElt p
       let p' = addNCastPtr p szElt
       vs <- loop (len+1) p'
       return (v:vs)

readSequence :: ( Eq a )
	     => (Ptr (Ptr a) -> IO a)
	     -> (Ptr (Ptr a) -> IO Bool)
	     -> Word32
	     -> Maybe Word32
	     -> Ptr (Ptr a)
	     -> IO [a]
readSequence rElt termPred szElt mbLen ptr = do
  ptr' <- readPtr ptr
  unmarshallSequence rElt termPred szElt mbLen (castPtr ptr')

writeSequence :: ( Eq a )
	      => Bool
	      -> (Ptr a -> a -> IO ())
	      -> (Ptr a -> IO ())
	      -> Word32
	      -> Maybe Word32
	      -> Ptr a 
	      -> [a] 
	      -> IO ()
writeSequence do_alloc wElt wTermin szElt mbLen ppseq ls = do
  pseq <-
    if not do_alloc then
       return (castPtr ppseq)
     else do
       arr <- alloc (szElt * seq_len)
       writePtr (castPtr ppseq) arr
       return arr
  pseq' <- foldM writeElt pseq the_ls
  wTermin pseq'
 where
   (seq_len, the_ls) = 
      case mbLen of
        Nothing -> (fromIntegral (length ls + 1), ls)
	Just x  -> (x + 1, take (fromIntegral x) ls)

   writeElt addr v = do
     wElt addr v
     return (addNCastPtr addr szElt)

freeSequence :: Ptr a -> IO ()
freeSequence = free
\end{code}

\begin{code}
-- at most len elements
marshallblist :: Word32 -> Word32 -> (Ptr a -> a -> IO ()) -> [a] -> IO (Ptr [a])
marshallblist szof l writeelt ls = do
 arr <- alloc (l'*szof)
 foldM writeElt (castPtr arr) ls
 return arr
  where
   l' = atLeast l (fromIntegral (0::Int)) ls

   atLeast len  n _ | len == n = len
   atLeast _len n [] = n
   atLeast len  n (_:xs) = atLeast len (n+1) xs

   writeElt addr v = do
    writeelt addr v
    return (addNCastPtr addr szof)

writeblist :: Word32 -> Word32 -> (Ptr a -> a -> IO ()) -> Ptr [a] -> [a] -> IO ()
writeblist szof len writeelt ptr ls = do
 foldM writeElt (castPtr ptr) (take (fromIntegral len) ls)
 return ()
  where

   writeElt addr v = do
    writeelt addr v
    return (addNCastPtr addr szof)

readblist :: Word32 -> Word32 -> (Ptr a -> IO a) -> Ptr a -> IO [a]
readblist szof len unpack ptr = do
 let ptr0 = castPtr ptr
 loop ptr0 len
  where
   loop _p 0 = return []
   loop p n  = do
    v  <- unpack p
    let p' = addNCastPtr p szof
    vs <- loop p' (n-1)
    return (v:vs)

\end{code}

Misc coercion functions/shortcuts:
(ToDo: try to avoid generating them in the first place!)

\begin{code}
word16ToInt32 :: Word16 -> Int32
word16ToInt32 w = fromIntegral w -- intToInt32 (word16ToInt w)

-- This coercion is relying on no exceptions being thrown if
-- the Word32 > (maxBound::Int32).
word32ToInt32 :: Word32 -> Int32
word32ToInt32 w = fromIntegral w

-- This coercion is reling on no exceptions being thrown if w < 0.
int32ToWord32 :: Int32 -> Word32
int32ToWord32 w = fromIntegral w

int16ToWord32 :: Int16 -> Word32
int16ToWord32 w = fromIntegral w -- intToWord32 (int16ToInt w)

intToChar :: Int -> Char
intToChar = chr

charToInt32 :: Char -> Int32
charToInt32 c = fromIntegral (ord c)

word32ToChar :: Word32 -> Char
word32ToChar w = chr (fromIntegral w)

charToWord32 :: Char -> Word32
charToWord32 c = fromIntegral (ord c)

\end{code}

\begin{code}
toInt32 :: Integral a => a -> Int32
toInt32 i = fromIntegral i -- intToInt32 (toInt i)

toInt16 :: Integral a => a -> Int16
toInt16 i = fromIntegral i -- intToInt16 (toInt i)
\end{code}

ForeignPtr marshallers:

\begin{code}
marshallFO :: ForeignPtr a -> IO (ForeignPtr a)
marshallFO x = return x

unmarshallFO :: ForeignPtr a -> IO (ForeignPtr a)
unmarshallFO x = return x

writeFO :: Ptr (ForeignPtr a) -> ForeignPtr a -> IO ()
writeFO ptr fo = poke (castPtr ptr) (foreignPtrToPtr fo)

-- a C pointer, really.
sizeofForeignPtr :: Word32
sizeofForeignPtr = sizeofPtr

nullFinaliser :: FunPtr (Ptr a -> IO ())
nullFinaliser = castFunPtr (castPtrToFunPtr finalNoFree)

nullFO :: ForeignPtr a
nullFO = unsafePerformIO $ makeFO nullPtr nullFinaliser
\end{code}

\begin{code}
readStablePtr :: Ptr (StablePtr a) -> IO (StablePtr a)
readStablePtr ptr = peek ptr
\end{code}

Default by-value 'marshallers' for structs and unions.

\begin{code}
marshallStruct :: String -> a -> IO b
marshallStruct nm _ = ioError (userError (nm ++ ": Marshalling structs by value is not supported yet."))

unmarshallStruct :: String -> a -> IO c
unmarshallStruct nm _ = ioError (userError (nm ++ ": Unmarshalling structs by value is not supported yet."))

marshallUnion :: String -> a -> IO b
marshallUnion nm _ = ioError (userError (nm ++ ": Marshalling unions by value is not supported yet."))

unmarshallUnion :: String -> a -> b -> IO c
unmarshallUnion nm _ _ = ioError (userError (nm ++ ": Unmarshalling unions by value is not supported yet."))
\end{code}

Ptr marshallers:

\begin{code}
marshallPointer :: Ptr a -> IO (Ptr a)
marshallPointer p = return p

unmarshallPointer :: Ptr a -> IO (Ptr a)
unmarshallPointer ptr = return ptr --makeNoFreePtr ptr

writePointer :: Ptr (Ptr a) -> Ptr a -> IO ()
writePointer = poke

readPointer :: Ptr (Ptr a) -> IO (Ptr a)
readPointer ptr = peek ptr

sizeofPtr :: Word32
sizeofPtr  = fromIntegral (sizeOf (undefined :: Foreign.Ptr.Ptr ()))
\end{code}

\begin{code}
primInvokeIt :: (Ptr b -> Ptr a -> IO c) -> Int -> IO (Ptr a) -> IO c
primInvokeIt meth offset mk_obj_ptr = do
  obj_ptr <- mk_obj_ptr 
  lpVtbl  <- derefPtr (castPtr obj_ptr)
  methPtr <- indexPtr lpVtbl offset
  meth methPtr obj_ptr

primInvokeItFO :: (Ptr b -> Ptr a -> IO c) -> Int -> IO (ForeignPtr a) -> IO c
primInvokeItFO meth offset mk_obj_ptr = do
  obj_ptr <- mk_obj_ptr
  lpVtbl  <- peek (foreignPtrToPtr (castForeignPtr obj_ptr))
  methPtr <- indexPtr lpVtbl offset
  withForeignPtr obj_ptr (\ optr -> meth methPtr optr)
\end{code}

\begin{code}
stackStringLen :: Int -> String -> (Ptr String -> IO a) -> IO a
stackStringLen len str f
      = let slen = length str + 1 `max` len
        in stackFrame (fromIntegral slen) $ \pstr -> do 
	 writeString False pstr str
         f pstr

\end{code}

\begin{code}
{- BEGIN_GHC_ONLY

enumToFlag :: Enum a => a -> Int
enumToFlag tg = fromIntegral ((1::Word32) `shiftL` enumToInt tg)

enumToInt :: Enum a => a -> Int
enumToInt tg = I# (getTag tg)

flagToIntTag :: Int -> Int
flagToIntTag f | f < 0     = error "flagToIntTag: negative tag"
               | otherwise = go 0 f
   -- could've used Prelude.logBase, but don't need the precision it offers.
 where
    go n 0 = n
    go n 1 = n + 1
    go n x = go (n + 1) (x `div` 2)

unboxInt :: Int -> Int#
unboxInt (I# x#) = x#

toIntFlag :: Int -> Int -> Int
toIntFlag b v = fromIntegral ((1::Word32) `shiftL` (v + flagToIntTag b))

   END_GHC_ONLY -}

pow2Series :: Int -> Int32 -> [Int32]
pow2Series len start = take len (iterate double start)
 where
   double x
     | x == 0    = 1
     | otherwise = 2*x

orList :: [Int] -> Int
orList ls = fromIntegral (foldl (\ acc x -> (fromIntegral x) .|. acc) (0::Int32) ls)

orFlags :: (Num a,Flags a) => [a] -> a
orFlags ls = foldl (.+.) 0 ls

class Flags a where
  (.+.) :: a -> a -> a

\end{code}
