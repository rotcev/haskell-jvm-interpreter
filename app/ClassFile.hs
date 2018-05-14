module ClassFile
  ( parse
  , lookup
  , JavaClass(..)
  , lookupConstant
  , ClassFile.resolveNameAndType
  ) where

import           Attributes
import           ConstantPool
import           Data.Binary.Get
import           Data.Bits            ((.&.))
import qualified Data.ByteString.Lazy as LazyBS
import           Data.List            (unfoldr)
import           Data.Maybe           (catMaybes)
import           Data.Word
import           Numeric              (showHex)

data JavaClass = JavaClass
  { magic        :: Word32
  , major        :: Word16
  , minor        :: Word16
  , constantPool :: [Constant]
  , classAccess  :: [Access]
  , this         :: Word16
  , super        :: Word16
  , interfaces   :: [Constant]
  , fields       :: [Member]
  , methods      :: [Member]
  } deriving (Show)

data MemberType
  = Field
  | Method
  deriving (Show)

data Member = Member
  { memberType            :: MemberType
  , memberAccess          :: [Access]
  , memberNameIndex       :: Word16
  , memberDescriptorIndex :: Word16
  , memberAttributes      :: [Attribute]
  } deriving (Show)

data Access
  = Public
  | Private
  | Protected
  | Static
  | Final
  | Super
  | Interface
  | Abstract
  | Annotation
  | Volatile
  | Transient
  | Synthetic
  | Enumeration
  deriving Show

class ConstantLookup a where
  lookupConstant :: JavaClass -> a -> Constant

instance ConstantLookup Constant where
  lookupConstant javaClass (Indexed _ index) =
    let pool = constantPool javaClass
    in pool !! (fromIntegral (index - 1) :: Int)

instance ConstantLookup Int where
  lookupConstant javaClass index =
    let pool = constantPool javaClass
    in pool !! (index - 1)

resolveNameAndType :: JavaClass -> Constant -> (String, String)
resolveNameAndType javaClass constant =
  ConstantPool.resolveNameAndType (constantPool javaClass) constant

accessBits :: [(Word16, Access)]
accessBits =
  [ (0x0001, Public)
  , (0x0002, Private)
  , (0x0004, Protected)
  , (0x0008, Static)
  , (0x0010, Final)
  , (0x0020, Super)
  , (0x0040, Volatile)
  , (0x0080, Transient)
  , (0x0200, Interface)
  , (0x0400, Abstract)
  , (0x1000, Synthetic)
  , (0x2000, Annotation)
  , (0x4000, Enumeration)
  ]

getAccess :: Word16 -> [Access]
getAccess accessFlags =
  catMaybes .
  fmap
    (\(bit, access) ->
       if accessFlags .&. bit /= 0
         then Just access
         else Nothing) $
  accessBits

verifyMagicNumber :: Word32 -> Either String Word32
verifyMagicNumber 0xcafebabe = Right 0xcafebabe
verifyMagicNumber _          = Left "Invalid magic key"

decodeMembers :: [Constant] -> Word16 -> MemberType -> Get [Member]
decodeMembers constantPool memberAmount memberType =
  sequence .
  unfoldr
    (\b ->
       if b == 0
         then Nothing
         else Just (decodeMember, b - 1)) $
  memberAmount
  where
    decodeMember = do
      accessFlags <- getWord16be
      nameIndex <- getWord16be
      descriptorIndex <- getWord16be
      attributes <- getWord16be >>= decodeAttributes constantPool
      return
        Member
        { memberType = memberType
        , memberAccess = getAccess accessFlags
        , memberNameIndex = nameIndex
        , memberDescriptorIndex = descriptorIndex
        , memberAttributes = attributes
        }

decodeHeader :: Get (Word32, Word16, Word16, Word16)
decodeHeader = do
  key <- getWord32be
  major <- getWord16be
  minor <- getWord16be
  constantPoolSize <- getWord16be
  return (key, major, minor, constantPoolSize)

decodeBody :: Get ([Access], Word16, Word16, Word16)
decodeBody = do
  access_flags <- getWord16be
  this <- getWord16be
  super <- getWord16be
  interfaceAmount <- getWord16be
  return (getAccess access_flags, this, super, interfaceAmount)

decode :: Get (Either String JavaClass)
decode = do
  (key, major, minor, constantPoolSize) <- decodeHeader
  constantPool <- decodeConstantPool constantPoolSize
  (accessFlags, this, super, interfaceAmount) <- decodeBody
  interfaces <- decodeInterfaces interfaceAmount
  fields <- getWord16be >>= \x -> decodeMembers constantPool x Field
  methods <- getWord16be >>= \x -> decodeMembers constantPool x Method
  return
    (case verifyMagicNumber key of
       Right k ->
         Right $
         JavaClass
         { magic = key
         , major = major
         , minor = minor
         , constantPool = constantPool
         , classAccess = accessFlags
         , this = this
         , super = super
         , interfaces = interfaces
         , fields = fields
         , methods = methods
         }
       Left s -> Left s)

parse :: String -> IO (Either String JavaClass)
parse className = LazyBS.readFile className >>= return . runGet decode
