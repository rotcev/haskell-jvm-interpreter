module Main
  ( main
  ) where

import qualified ClassFile
import           ConstantPool
import qualified Data.Either.Utils as E

main :: IO ()
main = do
  cls <- ClassFile.parse "Test.class"
  let javaClass = E.fromRight cls
      constantPool = ClassFile.constantPool javaClass
      interfaces = ClassFile.interfaces javaClass
  print javaClass
  print . ClassFile.resolveNameAndType javaClass $
    ClassFile.lookupConstant javaClass (29 :: Int)
  print $ ClassFile.lookupConstant javaClass (26 :: Int)
