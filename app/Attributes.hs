module Attributes
  ( decodeAttributes
  , Attribute(..)
  , AttributeBody(..)
  ) where

import           ConstantPool    (Constant (..))
import           Data.Binary.Get
import           Data.Int
import           Data.List       (unfoldr)
import           Data.Word
import           Numeric         (showHex)

data Attribute = Attribute
  { attributeType :: String
  , contents      :: AttributeBody
  } deriving (Show)

data AttributeBody
  = Code { maxStack     :: Word16
         , maxLocals    :: Word16
         , instructions :: [Opcode] }
  | DefaultAttribute
  deriving (Show)

data Opcode
  = NOP
  | ACONST_NULL
  | ICONST_M1
  | ICONST_0
  | ICONNST_1
  | ICONST_2
  | ICONST_3
  | ICONST_4
  | ICONST_5
  | LCONST_0
  | LCONST_1
  | FCONST_0
  | FCONST_1
  | FCONST_2
  | DCONST_0
  | DCONST_1
  | BIPUSH
  | SIPUSH
  | LDC
  | LDC_W
  | LDC2_W
  | ILOAD
  | LLOAD
  | FLOAT
  | DLOAD
  | ILOAD_0
  | ILOAD_1
  | ILOAD_2
  | ILOAD_3
  | LLOAD_0
  | LLOAD_1
  | LLOAD_2
  | LLOAD_3
  | FLOAT_0
  | FLOAT_1
  | FLOAD_2
  | FLOAD_3
  | DLOAD_0
  | DLOAD_1
  | DLOAD_2
  | DLOAD_3
  | IALOAD
  | LALOAD
  | FALOAD
  | DALOAD
  | AALOAD
  | BALOAD
  | CALOAD
  | SALOAD
  | ISTORE
  | LSTORE
  | FSTORE
  | DSTORE
  | LOCALVARIABLE { variableInstructionType :: LocalVariableInstructionType
                  , variableIndex           :: Word8 }
  | ISTORE_0
  | ISTORE_1
  | ISTORE_2
  | ISTORE_3
  | LSTORE_0
  | LSTORE_1
  | LSTORE_2
  | LSTORE_3
  | FSTORE_0
  | FSTORE_1
  | FSTORE_2
  | FSTORE_3
  | DSTORE_0
  | DSTORE_1
  | DSTORE_2
  | DSTORE_3
  | IASTORE
  | LASTORE
  | FASTORE
  | DASTORE
  | AASTORE
  | BASTORE
  | CASTORE
  | SASTORE
  | POP
  | POP2
  | DUP
  | DUP_X1
  | DUP_X2
  | DUP2
  | DUP2_X1
  | DUP2_X2
  | SWAP
  | IADD
  | LADD
  | FADD
  | DADD
  | ISUB
  | LSUB
  | FSUB
  | DSUB
  | IMUL
  | LMUL
  | FMUL
  | DMUL
  | IDIV
  | LDIV
  | FDIV
  | DDIV
  | IREM
  | LREM
  | FREM
  | DREM
  | INEG
  | LNEG
  | FNEG
  | DNEG
  | ISHL
  | LSHL
  | ISHR
  | LSHR
  | IUSHR
  | LUSHR
  | IAND
  | LAND
  | IOR
  | LOR
  | IXOR
  | LXOR
  | IINC
  | I2L
  | I2F
  | I2D
  | L2I
  | L2F
  | L2D
  | F2I
  | F2L
  | F2D
  | D2I
  | D2L
  | D2F
  | I2B
  | I2C
  | I2S
  | LCMP
  | FCMPL
  | FCMPG
  | DCMPL
  | DCMPG
  | IFEQ
  | IFNE
  | IFLT
  | IFGE
  | IFGT
  | IFLE
  | IF_ICMPEQ
  | IF_ICMPNE
  | IF_ICMPLT
  | IF_ICMPGE
  | IF_ICMPGT
  | IF_ICMPLE
  | IF_ACMPEQ
  | IF_ACMPNE
  | GOTO
  | JSR
  | RET
  | TABLESWITCH
  | LOOKUPSWITCH
  | IRETURN
  | LRETURN
  | FRETURN
  | DRETURN
  | ARETURN
  | RETURN
  | FIELDREFERENCE { fieldReferenceType :: FieldInstructionType
                   , fieldIndex         :: Int }
  | INVOKEMETHOD { invocationType :: MethodInstructionType
                 , methodIndex    :: Int }
  | NEW { classRefIndex :: Word16 }
  | NEWARRAY
  | ANEWARRAY
  | ARRAYLENGTH
  | ATHROW
  | CHECKCAST
  | INSTANCEOF
  | MONITORENTER
  | MONITOREXIT
  | WIDE
  | MULTIANEWARRAY
  | IFNULL
  | IFNONNULL
  | GOTO_W
  | JSR_W
  | UNKNOWN { op :: String }
  deriving (Show)

data MethodInstructionType
  = INVOKESTATIC
  | INVOKEDYNAMIC
  | INVOKEVIRTUAL
  | INVOKEINTERFACE
  | INVOKESPECIAL
  deriving (Show)

data FieldInstructionType
  = GETFIELD
  | PUTFIELD
  | GETSTATIC
  | PUTSTATIC
  deriving (Show)

data LocalVariableInstructionType
  = ALOAD
  | ASTORE
  deriving (Show)

decodeAttributes :: [Constant] -> Word16 -> Get [Attribute]
decodeAttributes constantPool attributeAmount =
  sequence .
  unfoldr
    (\b ->
       if b == 0
         then Nothing
         else Just (decodeAttribute, b - 1)) $
  attributeAmount
  where
    decodeAttribute = do
      nameIndex <- getWord16be
      let attributeType =
            stringValue $ constantPool !! ((fromIntegral nameIndex) - 1)
      attributeLength <- getWord32be
      parseAttribute constantPool attributeType attributeLength

parseAttribute :: [Constant] -> String -> Word32 -> Get Attribute
parseAttribute constantPool attributeType attributeLength =
  case attributeType of
    "Code" -> do
      maxStack <- getWord16be
      maxLocals <- getWord16be
      codeLength <- getWord32be
      code <-
        (getLazyByteString (fromIntegral codeLength :: Int64)) >>=
        return . runGet (decodeCode maxStack maxLocals codeLength)
      _ <-
        getByteString
          ((fromIntegral (attributeLength - 8) :: Int) -
           (fromIntegral codeLength :: Int))
      return $ Attribute {attributeType = attributeType, contents = code}
    _ -> do
      _ <- getByteString (fromIntegral attributeLength :: Int)
      return $
        Attribute
        { attributeType = ("Unknown attribute " ++ attributeType)
        , contents = DefaultAttribute
        }

decodeCode :: Word16 -> Word16 -> Word32 -> Get AttributeBody
decodeCode maxStack maxLocals codeLength = do
  instructions <- decodeInstructions codeLength
  return $
    Code
    {maxStack = maxStack, maxLocals = maxLocals, instructions = instructions}

decodeInstructions :: Word32 -> Get [Opcode]
decodeInstructions codeLength = do
  consumed <- bytesRead
  if consumed == (fromIntegral codeLength :: Int64)
    then return []
    else do
      instruction <- decodeInstruction
      next <- decodeInstructions codeLength
      return $ instruction : next

decodeInstruction :: Get Opcode
decodeInstruction = do
  op <- getWord8
  case op of
    0x3a -> getWord8 >>= decodeLocalVariableInstruction ASTORE
    0x4b -> decodeLocalVariableInstruction ASTORE 0
    0x4c -> decodeLocalVariableInstruction ASTORE 1
    0x4d -> decodeLocalVariableInstruction ASTORE 2
    0x4e -> decodeLocalVariableInstruction ASTORE 3
    0x19 -> getWord8 >>= decodeLocalVariableInstruction ALOAD
    0x2a -> decodeLocalVariableInstruction ALOAD 0
    0x2b -> decodeLocalVariableInstruction ALOAD 1
    0x2c -> decodeLocalVariableInstruction ALOAD 2
    0x2d -> decodeLocalVariableInstruction ALOAD 3
    0xb5 -> decodeFieldInstruction PUTFIELD
    0xb4 -> decodeFieldInstruction GETFIELD
    0xb3 -> decodeFieldInstruction PUTSTATIC
    0xb2 -> decodeFieldInstruction GETSTATIC
    0xbb -> getWord16be >>= return . NEW
    0xb9 -> decodeMethodInstruction INVOKEINTERFACE
    0xb8 -> decodeMethodInstruction INVOKESTATIC
    0xb7 -> decodeMethodInstruction INVOKESPECIAL
    0xb6 -> decodeMethodInstruction INVOKEVIRTUAL
    0xb1 -> return RETURN
    0x59 -> return DUP
    _    -> return $ UNKNOWN (showHex op "")

decodeLocalVariableInstruction ::
     LocalVariableInstructionType -> Word8 -> Get Opcode
decodeLocalVariableInstruction variableInstructionType index =
  return $
  LOCALVARIABLE
  {variableInstructionType = variableInstructionType, variableIndex = index}

decodeFieldInstruction :: FieldInstructionType -> Get Opcode
decodeFieldInstruction referenceType =
  getWord16be >>= \x ->
    return $
    FIELDREFERENCE
    {fieldReferenceType = referenceType, fieldIndex = (fromIntegral x :: Int)}

decodeMethodInstruction :: MethodInstructionType -> Get Opcode
decodeMethodInstruction INVOKEINTERFACE =
  getWord32be >>= \x ->
    return $
    INVOKEMETHOD
    {invocationType = INVOKEINTERFACE, methodIndex = (fromIntegral x :: Int)}
decodeMethodInstruction invocationType =
  getWord16be >>= \x ->
    return $
    INVOKEMETHOD
    {invocationType = invocationType, methodIndex = (fromIntegral x :: Int)}
