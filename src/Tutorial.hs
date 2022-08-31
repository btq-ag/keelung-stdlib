{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant return" #-}

module Tutorial where

import Control.Monad (forM_)
import Keelung

-- | Outputs whether number is given.
echo :: Comp (Val 'Num)
echo = do
  x <- input -- request for an input and bind it to 'x'
  return x -- return 'x'

-- | A program that expects 2 inputs and returns no output
useless :: Comp (Val 'Unit)
useless = do
  x <- inputNum -- request for an input and bind it to 'x'
  y <- inputBool -- request for an input and bind it to 'y'
  return unit -- return nothing

-- | A program that expects the second input
-- to be the square of the first input
square :: Comp (Val 'Unit)
square = do
  x <- input -- request for an input and bind it to 'x'
  y <- input -- request for an input and bind it to 'y'
  assert ((x * x) `Eq` y) -- assert that 'y' is the square of 'x'
  return unit -- return nothing

-- | A program that converts between Celsius and Fahrenheit degrees
tempConvert :: Comp (Val 'Num)
tempConvert = do
  toFahrenheit <- input -- Bool
  degree <- input -- Num
  return $
    cond
      toFahrenheit
      (degree * 9 / 5 + 32)
      (degree - 32 * 5 / 9)

-- | Read out the 4th input from an array of 10 inputs
fourthInput :: Comp (Val 'Num)
fourthInput = do
  xs <- inputs 10
  let fourth = access xs 3
  return fourth

-- | A program that asserts all 10 inputs to be 42
allBe42 :: Comp (Val 'Unit)
allBe42 = do
  xs <- inputs 10
  -- access elements of `xs` with indices
  forM_ [0 .. 9] $ \i -> do
    assert (access xs i `Eq` 42)
  -- access elements of `xs` directly 
  forM_ (fromArray xs) $ \x -> do
    assert (x `Eq` 42)
  return unit

-- | A program that sums all the 10 inputs
summation :: Comp (Val 'Num)
summation = do
  xs <- inputs 10
  return $ sum (fromArray xs)

returnArray :: Comp (Val ('Arr 'Num))
returnArray = do
  x <- input
  return $ toArray [x, x, x, x]