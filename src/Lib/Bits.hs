-- | Like `Data.Bits` but with `Boolean` instead of `Bool`
module Lib.Bits where

import Data.Maybe (fromMaybe)
import Keelung

class Bits a where
  {-# MINIMAL
    (.&.),
    (.|.),
    xor,
    complement,
    (shift | (shiftL, shiftR)),
    (rotate | (rotateL, rotateR)),
    bitSize,
    isSigned,
    testBit,
    bit
    #-}

  -- | Bitwise \"and\"
  (.&.) :: a -> a -> a

  -- | Bitwise \"or\"
  (.|.) :: a -> a -> a

  -- | Bitwise \"xor\"
  xor :: a -> a -> a

  -- | Reverse all the bits in the argument
  complement :: a -> a

  -- | @'shift' x i@ shifts @x@ left by @i@ bits if @i@ is positive,
  --        or right by @-i@ bits otherwise.
  --        Right shifts perform sign extension on signed number types;
  --        i.e. they fill the top bits with 1 if the @x@ is negative
  --        and with 0 otherwise.
  --
  --        An instance can define either this unified 'shift' or 'shiftL' and
  --        'shiftR', depending on which is more convenient for the type in
  --        question.
  shift :: a -> Int -> a
  x `shift` i
    | i < 0 = x `shiftR` (-i)
    | i > 0 = x `shiftL` i
    | otherwise = x

  -- | @'rotate' x i@ rotates @x@ left by @i@ bits if @i@ is positive,
  --        or right by @-i@ bits otherwise.
  --
  --        An instance can define either this unified 'rotate' or 'rotateL' and
  --        'rotateR', depending on which is more convenient for the type in
  --        question.
  rotate :: a -> Int -> a
  x `rotate` i
    | i < 0 = x `rotateR` (-i)
    | i > 0 = x `rotateL` i
    | otherwise = x

  -- | 'zeroBits' is the value with all bits unset.
  --
  -- The following laws ought to hold (for all valid bit indices @/n/@):
  --
  --   * @'clearBit' 'zeroBits' /n/ == 'zeroBits'@
  --   * @'setBit'   'zeroBits' /n/ == 'bit' /n/@
  --   * @'testBit'  'zeroBits' /n/ == false@
  --   * @'popCount' 'zeroBits'   == 0@
  --
  -- This method uses @'clearBit' ('bit' 0) 0@ as its default
  -- implementation (which ought to be equivalent to 'zeroBits' for
  -- types which possess a 0th bit).
  zeroBits :: a
  zeroBits = clearBit (bit 0) 0

  -- | @bit /i/@ is a value with the @/i/@th bit set and all other bits clear.
  --
  -- See also 'zeroBits'.
  bit :: Int -> a

  -- | @x \`setBit\` i@ is the same as @x .|. bit i@
  setBit :: a -> Int -> a

  -- | @x \`clearBit\` i@ is the same as @x .&. complement (bit i)@
  clearBit :: a -> Int -> a

  -- | @x \`complementBit\` i@ is the same as @x \`xor\` bit i@
  complementBit :: a -> Int -> a

  -- | @x \`testBit\` i@ is the same as @x .&. bit n /= 0@
  --
  --        In other words it returns 'true' if the bit at offset @n
  --        is set.
  testBit :: a -> Int -> Boolean

  -- | Return the number of bits in the type of the argument.  The actual
  --        value of the argument is ignored.
  bitSize :: a -> Int

  -- | Return 'True' if the argument is a signed type.  The actual
  --        value of the argument is ignored
  isSigned :: a -> Bool

  {-# INLINE setBit #-}
  {-# INLINE clearBit #-}
  {-# INLINE complementBit #-}
  x `setBit` i = x .|. bit i
  x `clearBit` i = x .&. complement (bit i)
  x `complementBit` i = x `xor` bit i

  -- | Shift the argument left by the specified number of bits
  --        (which may be negative).
  --
  --        An instance can define either this and 'shiftR' or the unified
  --        'shift', depending on which is more convenient for the type in
  --        question.
  shiftL :: a -> Int -> a
  {-# INLINE shiftL #-}
  x `shiftL` i = x `shift` i

  -- | Shift the first argument right by the specified number of bits.
  --
  --        Right shifts perform sign extension on signed number types;
  --        i.e. they fill the top bits with 1 if the @x@ is negative
  --        and with 0 otherwise.
  --
  --        An instance can define either this and 'shiftL' or the unified
  --        'shift', depending on which is more convenient for the type in
  --        question.
  shiftR :: a -> Int -> a
  {-# INLINE shiftR #-}
  x `shiftR` i = x `shift` (-i)

  -- | Rotate the argument left by the specified number of bits
  --        (which may be negative).
  --
  --        An instance can define either this and 'rotateR' or the unified
  --        'rotate', depending on which is more convenient for the type in
  --        question.
  rotateL :: a -> Int -> a
  {-# INLINE rotateL #-}
  x `rotateL` i = x `rotate` i

  -- | Rotate the argument right by the specified number of bits
  --        (which may be negative).
  --
  --        An instance can define either this and 'rotateL' or the unified
  --        'rotate', depending on which is more convenient for the type in
  --        question.
  rotateR :: a -> Int -> a
  {-# INLINE rotateR #-}
  x `rotateR` i = x `rotate` (-i)

-------------------------------------------------------------------------------

-- | Infix version of 'xor'.
(.^.) :: (Bits a) => a -> a -> a
(.^.) = xor

infixl 6 .^.

-- | Infix version of 'shiftR'.
(.>>.) :: (Bits a) => a -> Int -> a
(.>>.) = shiftR

infixl 8 .>>.

-- | Infix version of 'shiftL'.
(.<<.) :: (Bits a) => a -> Int -> a
(.<<.) = shiftL

infixl 8 .<<.
