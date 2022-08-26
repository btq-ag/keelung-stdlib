{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant return" #-}

module Tutorial where

import Control.Monad (forM_)
import Keelung

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