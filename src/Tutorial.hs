{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Tutorial where

import Keelung

-- | Outputs whether number is given.
echo :: Comp GF181 (Expr 'Num GF181)
echo = do
  x <- inputVar -- request for an input and bind it to 'x'
  return $ Var x -- return 'x'

-- | A program that expects 2 inputs and returns no output
useless :: Comp GF181 (Expr 'Unit GF181)
useless = do
  x <- inputVar -- request for an input and bind it to 'x'
  y <- inputVar -- request for an input and bind it to 'y'
  return unit -- return nothing

-- | A program that expects the second input
-- to be the square of the first input
square :: Comp GF181 (Expr 'Unit GF181)
square = do
  x <- inputVar -- request for an input and bind it to 'x'
  y <- inputVar -- request for an input and bind it to 'y'
  assert ((Var x * Var x) `Eq` Var y) -- assert that 'y' is the square of 'x'
  return unit -- return nothing

-- | A program that converts between Celsius and Fahrenheit degrees
tempConvert :: Comp GF181 (Expr 'Num GF181)
tempConvert = do
  toFahrenheit <- inputVar -- Bool
  degree <- inputVar -- Num
  return $  
    If
      (Var toFahrenheit)
      (Var degree * 9 / 5 + 32)
      (Var degree - 32 * 5 / 9)