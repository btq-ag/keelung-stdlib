module Lib.U32 where

import Control.Monad
import Data.Word (Word32)
import Keelung
import Keelung.Syntax.Simplify (Elaborable (..))
import qualified Lib.Array as Array
import Lib.Bits

-- | A 32-bit unsigned integer.
newtype U32 = U32 {unU32 :: Arr Boolean} deriving (Eq)

-- | Make 'U32' a citizen of Keelung.
instance Elaborable U32 where
  convertM (U32 xs) = convertM xs

-- | Make 'U32' an instance of 'Bits'.
instance Bits U32 where
  U32 xs .&. U32 ys = U32 $ Array.zipWith And xs ys
  U32 xs .|. U32 ys = U32 $ Array.zipWith Or xs ys
  U32 xs `xor` U32 ys = U32 $ Array.zipWith Xor xs ys
  complement (U32 xs) = U32 $ Array.map neg xs
  shift (U32 xs) i = U32 $ Array.shiftAndFill false i xs
  rotate (U32 xs) i = U32 $ Array.rotate i xs
  bitSize _ = 32
  isSigned _ = False
  testBit (U32 xs) = access xs
  bit n = U32 $ toArray (replicate (31 - n) false ++ [true] ++ replicate n false)