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
  | ICONST_1
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
  | IINC
  | ARITHMETIC { operation :: ArithmeticInstructionType }
  | TYPECAST { targetType :: TypeConversionInstructionType }
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
  | SINK { returnType :: ReturnInstructionType }
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

data ReturnInstructionType
  = IRETURN
  | LRETURN
  | FRETURN
  | DRETURN
  | ARETURN
  | RETURN
  deriving (Show)

data ArithmeticInstructionType
  = IADD
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
  -- | IINC
  deriving (Show)

data TypeConversionInstructionType
  = I2L
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
    0xb1 -> decodeSink RETURN
    0xb0 -> decodeSink ARETURN
    0xaf -> decodeSink DRETURN
    0xae -> decodeSink FRETURN
    0xad -> decodeSink LRETURN
    0xac -> decodeSink IRETURN
    0x60 -> decodeArithmetic IADD
    0x61 -> decodeArithmetic LADD
    0x62 -> decodeArithmetic FADD
    0x64 -> decodeArithmetic ISUB
    0x65 -> decodeArithmetic LSUB
    0x66 -> decodeArithmetic FSUB
    0x67 -> decodeArithmetic DSUB
    0x68 -> decodeArithmetic IMUL
    0x69 -> decodeArithmetic LMUL
    0x6a -> decodeArithmetic FMUL
    0x6b -> decodeArithmetic DMUL
    0x6c -> decodeArithmetic IDIV
    0x6d -> decodeArithmetic LDIV
    0x6e -> decodeArithmetic FDIV
    0x6f -> decodeArithmetic DDIV
    0x70 -> decodeArithmetic IREM
    0x71 -> decodeArithmetic LREM
    0x72 -> decodeArithmetic FREM
    0x73 -> decodeArithmetic DREM
    0x74 -> decodeArithmetic INEG
    0x75 -> decodeArithmetic LNEG
    0x76 -> decodeArithmetic FNEG
    0x77 -> decodeArithmetic DNEG
    0x78 -> decodeArithmetic ISHL
    0x79 -> decodeArithmetic LSHL
    0x7a -> decodeArithmetic ISHR
    0x7b -> decodeArithmetic LSHR
    0x7c -> decodeArithmetic IUSHR
    0x7d -> decodeArithmetic LUSHR
    0x7e -> decodeArithmetic IAND
    0x7f -> decodeArithmetic LAND
    0x80 -> decodeArithmetic IOR
    0x81 -> decodeArithmetic LOR
    0x82 -> decodeArithmetic IXOR
    0x83 -> decodeArithmetic LXOR
    -- 0x84 -> decodeArithmetic IINC
    0x85 -> decodeTypeConversion I2L
    0x86 -> decodeTypeConversion I2F
    0x87 -> decodeTypeConversion I2D
    0x88 -> decodeTypeConversion L2I
    0x89 -> decodeTypeConversion L2F
    0x8a -> decodeTypeConversion L2D
    0x8b -> decodeTypeConversion F2I
    0x8c -> decodeTypeConversion F2L
    0x8d -> decodeTypeConversion F2D
    0x8e -> decodeTypeConversion D2I
    0x8f -> decodeTypeConversion D2L
    0x90 -> decodeTypeConversion D2F
    0x91 -> decodeTypeConversion I2B
    0x92 -> decodeTypeConversion I2C
    0x93 -> decodeTypeConversion I2S
    0x59 -> return DUP
    _    -> return $ UNKNOWN (showHex op "")

decodeSink :: ReturnInstructionType -> Get Opcode
decodeSink returnType = return . SINK $ returnType

decodeArithmetic :: ArithmeticInstructionType -> Get Opcode
decodeArithmetic operation = return . ARITHMETIC $ operation

decodeTypeConversion :: TypeConversionInstructionType -> Get Opcode
decodeTypeConversion targetType = return . TYPECAST $ targetType

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
