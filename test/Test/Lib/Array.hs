{-# LANGUAGE DataKinds #-}

module Test.Lib.Array where

import Keelung
import qualified Lib.Array as Array
import Test.Util
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Immutable Array"
    [ testProperty "List shift length" propShiftLength,
      testProperty "Array shift left" propShiftL,
      testCase "Array shift left 1" $ assertUnary (Array.shiftL 1) [True, True, True, True] [True, True, True, False],
      testCase "Array shift left 2" $ assertUnary (Array.shiftL 3) [True, True, True, True] [True, False, False, False],
      testCase "Array shift left 3" $ assertUnary (Array.shiftL 0) [True, False, True, False] [True, False, True, False],
      testCase "Array shift left 4" $ assertUnary (Array.shiftL (-1)) [True, True, True, True] [False, True, True, True],
      testCase "Array shift left 5" $ assertUnary (Array.shiftL (-2)) [True, False, True, True] [False, False, True, False],
      testProperty "Array shift right" propShiftR,
      testCase "Array shift right 1" $ assertUnary (Array.shiftR 1) [True, True, True, True] [False, True, True, True],
      testCase "Array shift right 3" $ assertUnary (Array.shiftR 3) [True, True, True, True] [False, False, False, True],
      testProperty "List rotate inverse" propListRotateInverse,
      testProperty "Array rotate inverse" propArrayRotateInverse,
      testProperty "rotate left prop" propRotateL,
      testCase "rotate left 1" $ assertUnary (Array.rotateL 1) [True, False, True, False] [False, True, False, True],
      testCase "rotate left 2" $ assertUnary (Array.rotateL 3) [False, True, False] [False, True, False],
      testCase "rotate left 3" $ assertUnary (Array.rotateL 0) [True, False, True, False] [True, False, True, False],
      testCase "rotate left 4" $ assertUnary (Array.rotateL (-1)) [True, False, False, False] [False, True, False, False],
      testProperty "rotate right prop" propRotateR,
      testCase "rotate right 1" $ assertUnary (Array.rotateR 1) [True, False, True, False] [False, True, False, True],
      testCase "rotate right 2" $ assertUnary (Array.rotateR 3) [True, False, True, False] [False, True, False, True],
      testProperty "update length" propUpdateLength,
      testCase "full adder 1" $ assertBinary Array.fullAdder [False, False, False, False] [False, False, False, False] [False, False, False, False],
      testCase "full adder 2" $ assertBinary Array.fullAdder [True, False, False, False] [True, False, False, False] [False, True, False, False],
      testCase "full adder 3" $ assertBinary Array.fullAdder [True, True, True, True] [True, True, True, True] [False, True, True, True]
    ]

shift :: Int -> [Bool] -> [Bool]
shift n xs
  | n >= len = replicate len False
  | n > 0 = drop n xs <> replicate n False
  | n == 0 = xs
  | n >= (-len) = replicate (-n) False <> take (len + n) xs
  | otherwise = replicate len False
  where
    len = length xs

propShiftLength :: Int -> [Bool] -> Property
propShiftLength n xs = length (shift n xs) === length xs

rotate :: Int -> [Bool] -> [Bool]
rotate _ [] = []
rotate n xs =
  let n' = n `mod` length xs
   in drop n' xs <> take n' xs

propListRotateInverse :: Int -> [Bool] -> Property
propListRotateInverse n xs = rotate n (rotate (-n) xs) === xs

propArrayRotateInverse :: Int -> [Bool] -> Property
propArrayRotateInverse n xs = monadicIO $ do
  pre (not (null xs))

  let actual = Array.rotate n (Array.rotate (-n) (Array.map' Boolean xs))
  let expected = Array.map' Boolean xs

  actual' <- run $ interpret_ GF181 (return actual) ([] :: [GF181])
  expected' <- run $ interpret_ GF181 (return expected) ([] :: [GF181])

  Test.QuickCheck.Monadic.assert (actual' == expected')

propShiftL :: Int -> [Bool] -> Property
propShiftL n xs = monadicIO $ do
  let actual = Array.shiftL n . Array.map' Boolean $ xs
  let expect = Array.map' Boolean . shift n $ xs
  propWrap actual expect

propShiftR :: Int -> [Bool] -> Property
propShiftR n xs = monadicIO $ do
  let actual = Array.shiftR n . Array.map' Boolean $ xs
  let expect = Array.map' Boolean . shift (-n) $ xs
  propWrap actual expect

propRotateL :: Int -> [Bool] -> Property
propRotateL n xs = monadicIO $ do
  pre (not (null xs))
  let actual = Array.rotateL n . Array.map' Boolean $ xs
  let expect = Array.map' Boolean . rotate n $ xs
  propWrap actual expect

propRotateR :: Int -> [Bool] -> Property
propRotateR n xs = monadicIO $ do
  pre (not (null xs))
  let actual = Array.rotateR n . Array.map' Boolean $ xs
  let expect = Array.map' Boolean . rotate (-n) $ xs
  propWrap actual expect

propUpdateLength :: Int -> Bool -> [Bool] -> Property
propUpdateLength idx x xs = monadicIO $ do
  pre (not (null xs))
  let actual = lengthOf $ Array.update idx (Boolean x) (Array.map' Boolean xs)
  let expect = length xs
  Test.QuickCheck.Monadic.assert (actual == expect)

assertBinary :: (Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)) -> [Bool] -> [Bool] -> [Bool] -> Assertion
assertBinary f x y expect = do
    assertWrap (f (toBitArr x) (toBitArr y)) (toBitArr expect)

assertUnary :: (Val ('Arr 'Bool) -> Val ('Arr 'Bool)) -> [Bool] -> [Bool] -> Assertion
assertUnary f actual expect = assertWrap (f $ toBitArr actual) (toBitArr expect)

