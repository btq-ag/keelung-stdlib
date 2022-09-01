{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant return" #-}

module Tutorial where

import Control.Monad
import Keelung
import qualified Lib.W8 as W8
import qualified Lib.Array as Array
import qualified BLAKE2b

-- | Outputs whether number is given.
echo :: Comp GF181 (Val 'Num GF181)
echo = do
  x <- input -- request for an input and bind it to 'x'
  return x -- return 'x'

-- | A program that expects 2 inputs and returns no output
useless :: Comp GF181 (Val 'Unit GF181)
useless = do
  x <- inputNum -- request for an input and bind it to 'x'
  y <- inputBool -- request for an input and bind it to 'y'
  return unit -- return nothing

-- | A program that expects the second input
-- to be the square of the first input
square :: Comp GF181 (Val 'Unit GF181)
square = do
  x <- input -- request for an input and bind it to 'x'
  y <- input -- request for an input and bind it to 'y'
  assert ((x * x) `Eq` y) -- assert that 'y' is the square of 'x'
  return unit -- return nothing

-- | A program that converts between Celsius and Fahrenheit degrees
tempConvert :: Comp GF181 (Val 'Num GF181)
tempConvert = do
  toFahrenheit <- input -- Bool
  degree <- input -- Num
  return $
    cond
      toFahrenheit
      (degree * 9 / 5 + 32)
      (degree - 32 * 5 / 9)

-- | Read out the 4th input from an array of 10 inputs
fourthInput :: Comp GF181 (Val 'Num GF181)
fourthInput = do
  xs <- inputs 10
  fourth <- access xs 3
  return fourth

-- | A program that asserts all 10 inputs to be 42
allBe42 :: Comp GF181 (Val 'Unit GF181)
allBe42 = do
  xs <- inputs 10
  forM_ [0 .. 9] $ \i -> do
    x <- access xs i
    assert (x `Eq` 42)
  return unit

-- | A program that sums all the 10 inputs
summation :: Comp GF181 (Val 'Num GF181)
summation = do
  xs <- inputs 10
  reduce 0 [0 .. 9] $ \acc i -> do
    x <- access xs i
    return $ acc + x

returnArray :: Comp GF181 (Val ('Arr 'Num) GF181)
returnArray = do
  x <- input
  toArray [x, x, x, x]

-- > interpret (blake2b 2 3) [1,0,0,0,0,1,1,0, 0,1,0,0,0,1,1,0]
--   Right [10110010 11101100 00100010]
--
-- means to calculate the 3-byte digest blake2b of string "ab"
-- a = 0b01100001, b = 0b01000010
-- the answer is Blake2b-24("ab") = 0x4d3744, which is
-- 0b01001101, 0b00110111, 0b1000100
blake2b :: Int -> Int -> Comp GF181 (Val ('Arr ('Arr 'Bool)) GF181)
blake2b msglen hashlen = do
  msg <- inputs2 msglen 8

  BLAKE2b.hash msg msglen hashlen
