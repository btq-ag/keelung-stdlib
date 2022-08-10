{-# LANGUAGE DataKinds #-}

module Lib.W64 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word64)
import Keelung
import qualified Lib.Array as Array

type W64 = 'Arr 'Bool

fromWord64 :: Word64 -> Comp n (Val W64 n)
fromWord64 word = toArray $ map (Boolean . testBit word) [0 .. 63]

-- | Rotates right by i bits if i is positive, or right by -i bits otherwise.
rotateRight :: Int -> Val W64 n -> Comp n (Val W64 n)
rotateRight n xs = do
  result <- toArray (replicate 64 false)
  forM_ [0 .. 63] $ \i -> do
    x <- access xs i
    let i' = (i - n) `mod` 64
    update result i' x
  return result

add :: Val W64 n -> Val W64 n -> Comp n (Val W64 n)
add = fullAdder 64

xor :: Val W64 n -> Val W64 n -> Comp n (Val W64 n)
xor as bs = do
  bits <- forM [0 .. 63] $ \i -> do
    a <- access as i
    b <- access bs i
    return (a `Xor` b)
  toArray bits

complement :: Val W64 n -> Comp n (Val W64 n)
complement = Array.map neg

-- copyTo :: Val W64 n -> Val W64 n -> Comp n ()
-- copyTo src tgt = forM_ [0 .. 63] $ \i -> do
--   srcBit <- access src i
--   tgtBit <- access tgt i
--   update tgt i srcBit

equal :: Val W64 n -> Val W64 n -> Comp n (Val 'Bool n)
equal = Array.beq 64

--------------------------------------------------------------------------------

fullAdder1bit :: Val 'Bool n -> Val 'Bool n -> Val 'Bool n -> (Val 'Bool n, Val 'Bool n)
fullAdder1bit a b carry =
  let value = a `Xor` b `Xor` carry
      nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)
   in (value, nextCarry)

fullAdder :: Int -> Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
fullAdder width as bs = do
  -- allocate a new array of 64 bits for the result of the addition
  result <- toArray (replicate width false)
  -- 1-bit full adder
  foldM_
    ( \carry i -> do
        a <- access as i
        b <- access bs i
        let (value, nextCarry) = fullAdder1bit a b carry
        update result i value
        return nextCarry
    )
    false
    [0 .. width - 1]
  return result

testFullAdder :: Int -> Comp GF181 (Val 'Unit GF181)
testFullAdder width = do
  as <- inputs width
  bs <- inputs width
  cs <- inputs width
  cs' <- fullAdder width as bs

  Array.beq width cs cs' >>= assert

  return unit
