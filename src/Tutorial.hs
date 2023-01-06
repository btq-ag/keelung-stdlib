{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant return" #-}

module Tutorial where

import Control.Monad
import qualified Hash.BLAKE2b as BLAKE2b
import qualified Hash.BLAKE2sM as BLAKE2sM
import Keelung

-- | Outputs whether number is given.
echo :: Comp Field
echo = do
  x <- input -- request for an input and bind it to 'x'
  return x -- return 'x'

-- | A program that expects 2 inputs and returns no output
useless :: Comp ()
useless = do
  _x <- inputField -- request for an input and bind it to 'x'
  _y <- inputBool -- request for an input and bind it to 'y'
  return () -- return nothing

-- | A program that expects the second input
-- to be the square of the first input
square :: Comp ()
square = do
  x <- inputField -- request for an input and bind it to 'x'
  y <- input -- request for an input and bind it to 'y'
  assert ((x * x) `eq` y) -- assert that 'y' is the square of 'x'

-- | A program that converts between Celsius and Fahrenheit degrees
tempConvert :: Comp Field
tempConvert = do
  toFahrenheit <- input -- Bool
  degree <- input -- Num
  return $
    cond
      toFahrenheit
      (degree * 9 / 5 + 32)
      (degree - 32 * 5 / 9)

-- | Read out the 4th input from an array of 10 inputs
fourthInput :: Comp Field
fourthInput = do
  xs <- inputs 10
  let fourth = access xs 3
  return fourth

-- | A program that asserts all 10 inputs to be 42
allBe42 :: Comp ()
allBe42 = do
  xs <- inputs 10
  -- access elements of `xs` with indices
  forM_ [0 .. 9] $ \i -> assert (access xs i `eq` (42 :: Field))
  -- access elements of `xs` directly
  forM_ (fromArray xs) $ \x -> assert (x `eq` 42)

-- | A program that sums all the 10 inputs
summation :: Comp Field
summation = do
  xs <- inputs 10
  return $ sum (fromArray xs)

returnArray :: Comp (Arr Field)
returnArray = do
  x <- input
  return $ toArray [x, x, x, x]

-- > interpret GF181 (blake2b 2 3) [1,0,0,0,0,1,1,0, 0,1,0,0,0,1,1,0]
--   Right [10110010 11101100 00100010]
--
-- means to calculate the 3-byte digest blake2b of string "ab"
-- a = 0b01100001, b = 0b01000010
-- the answer is Blake2b-24("ab") = 0x4d3744, which is
-- 0b01001101, 0b00110111, 0b1000100
blake2b :: Int -> Int -> Comp (ArrM (ArrM Boolean))
blake2b msglen hashlen = do
  msg <- inputs2 msglen 8 >>= thaw2
  BLAKE2b.hash msg msglen hashlen

-- > interpret GF181 (blake2s 2 3) [1,0,0,0,0,1,1,0, 0,1,0,0,0,1,1,0]
--   Right [0,1,1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0,1,1,1,0,1,1]
-- > compileO2 GF181 (blake2s 8 32)
--   R1C constraints (70126)
-- NOTE: Zcash uses 21006 constraints. See https://zips.z.cash/protocol/protocol.pdf (Appendix A.3.7)
blake2s :: Int -> Int -> Comp (ArrM (ArrM Boolean))
blake2s msglen hashlen = do
  msg <- inputs2 msglen 8 >>= thaw2
  BLAKE2sM.hash msg msglen hashlen

blake2sx :: Comp ()
blake2sx = BLAKE2sM.test

-- | Birthday voucher example
birthday :: Comp Boolean
birthday = do
  -- these inputs are private witnesses
  _hiddenYear <- inputField
  hiddenMonth <- inputField
  hiddenDate <- inputField
  -- these inputs are public inputs
  month <- input
  date <- input

  return $ (hiddenMonth `eq` month) `And` (hiddenDate `eq` date)

-- | A program that outputs the input to the 4th power (without computation reusing)
notReused :: Comp (Arr Field)
notReused = do
  x <- input
  let y = x * x * x * x
  return $ toArray [y, y]

-- | A program that outputs the input to the 4th power (with computation reusing)
reused :: Comp (Arr Field)
reused = do
  x <- input
  y <- reuse $ x * x * x * x
  return $ toArray [y, y]

packing :: Comp ()
packing = do
  x <- inputField
  x0 <- input
  x1 <- input
  x2 <- input
  x3 <- input
  x4 <- input
  assert $ x `eq` (x0 + x1 * 2 + x2 * 4 + x3 * 8 + x4 * 16)
  assert $ x0 `eq` (x0 * x0)
  assert $ x1 `eq` (x1 * x1)
  assert $ x2 `eq` (x2 * x2)
  assert $ x3 `eq` (x3 * x3)
  assert $ x4 `eq` (x4 * x4)
  return ()

packing2 :: Comp ()
packing2 = do
  a <- input
  as <- inputs 5

  let a' = foldr (\x acc -> BtoF x + 2 * acc) 0 as
  assert $ a `eq` a'
  return ()

packing3 :: Comp ()
packing3 = do
  a <- inputField
  as <- fromArray <$> inputs 5

  let a' = foldr (\(i, x) acc -> x * fromInteger (2 ^ (i :: Int)) + acc) 0 (zip [0 ..] as)
  assert $ a `eq` a'
  forM_ as $ \x -> assert (x `eq` (x * x))
  return ()
