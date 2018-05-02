{-# LANGUAGE LambdaCase #-}

module ConstantPool
  ( Constant(..)
  , ReferenceType(..)
  , IndexType(..)
  , decodeInterfaces
  , decodeConstantPool
  , resolveNameAndType
  , resolveIndexed
  ) where

import           Data.Binary.Get
import qualified Data.ByteString       as BS
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy  as B
import           Data.List             (unfoldr)
import           Data.Word

data Constant
  = Utf8 { stringValue :: String }
  | Integer { integerValue :: Word32 }
  | FloatingPoint { floatValue :: Float }
  | Long { longValue :: Word64 }
  | Double { doubleValue :: Double }
  | Indexed { indexType :: IndexType
            , index     :: Word16 }
  | ObjectRef { referenceType    :: ReferenceType
              , classIndex       :: Word16
              , nameAndTypeIndex :: Word16 }
  | NameAndType { nameIndex :: Word16
                , typeIndex :: Word16 }
  | MethodHandle { referenceKind  :: Word8
                 , referenceIndex :: Word16 }
  | InvokeDynamic { bootstrapMethodIndex :: Word16
                  , nameAndTypeIndex     :: Word16 }
  deriving (Show, Eq)

resolveNameAndType :: [Constant] -> Constant -> (String, String)
resolveNameAndType constantPool (NameAndType nameIndex typeIndex) =
  let nameString =
        stringValue $ constantPool !! (fromIntegral (nameIndex - 1) :: Int)
      typeString =
        stringValue $ constantPool !! (fromIntegral (typeIndex - 1) :: Int)
  in (nameString, typeString)
resolveNameAndType constantPool (ObjectRef _ _ nameAndTypeIndex) =
  resolveNameAndType constantPool $
  constantPool !! (fromIntegral (nameAndTypeIndex - 1) :: Int)

resolveIndexed :: [Constant] -> Constant -> String
resolveIndexed constantPool (Indexed _ index) =
  stringValue $ constantPool !! (fromIntegral (index - 1))

data ReferenceType
  = FieldReference
  | MethodReference
  | InterfaceMethodReference
  deriving (Show, Eq)

data IndexType
  = Class
  | InterfaceClass
  | String
  | MethodType
  deriving (Show, Eq)

decodeTag :: Get Word8 -> Get Constant
decodeTag tag =
  tag >>= \case
    1 -> decodeUtf8
    3 -> decodeInteger
    4 -> decodeFloatingPoint
    5 -> decodeLong
    6 -> decodeDouble
    7 -> decodeIndexed Class
    8 -> decodeIndexed String
    9 -> decodeObjectReference FieldReference
    10 -> decodeObjectReference MethodReference
    11 -> decodeObjectReference InterfaceMethodReference
    12 -> decode32BitConstant NameAndType
    15 -> decodeMethodHandle
    16 -> decodeIndexed MethodType
    18 -> decode32BitConstant InvokeDynamic
    _ -> do
      position <- bytesRead
      error ("Unknown tag at position " ++ show position)

decodeIndexed :: IndexType -> Get Constant
decodeIndexed indexType = decode16BitConstant $ Indexed indexType

decodeObjectReference :: ReferenceType -> Get Constant
decodeObjectReference referenceType =
  decode32BitConstant $ ObjectRef referenceType

decode16BitConstant :: (Word16 -> Constant) -> Get Constant
decode16BitConstant f = getWord16be >>= return . f

decode32BitConstant :: (Word16 -> Word16 -> Constant) -> Get Constant
decode32BitConstant f =
  getWord16be >>= \x -> getWord16be >>= \y -> return $ f x y

decodeLong :: Get Constant
decodeLong = getWord64be >>= return . Long

decodeDouble :: Get Constant
decodeDouble = getDoublebe >>= return . Double

decodeMethodHandle :: Get Constant
decodeMethodHandle = do
  referenceKind <- getWord8
  referenceIndex <- getWord16be
  return $ MethodHandle referenceKind referenceIndex

decodeUtf8 :: Get Constant
decodeUtf8 = do
  str <- getWord16be >>= \x -> getByteString (fromIntegral x :: Int)
  return $ Utf8 . unpack $ str

decodeInteger :: Get Constant
decodeInteger = getWord32be >>= return . Integer

decodeFloatingPoint :: Get Constant
decodeFloatingPoint = getFloatbe >>= return . FloatingPoint

decodeInterfaces :: Word16 -> Get [Constant]
decodeInterfaces interfaceAmount =
  sequence .
  unfoldr
    (\b ->
       if b == 0
         then Nothing
         else Just (decodeIndexed InterfaceClass, b - 1)) $
  interfaceAmount

decodeConstantPool :: Word16 -> Get [Constant]
decodeConstantPool 1 = return []
decodeConstantPool constantPoolSize =
  let reservedSize :: Constant -> Word16
      reservedSize (Long _)   = 2
      reservedSize (Double _) = 2
      reservedSize _          = 1
  in do tag <- decodeTag getWord8
        next <- decodeConstantPool (constantPoolSize - reservedSize tag)
        return $ tag : next
