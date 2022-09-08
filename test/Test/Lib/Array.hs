{-# LANGUAGE DataKinds #-}

module Test.Lib.Array where

import Data.Functor
import Keelung
import qualified Lib.Array as Array
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
      testCase "Array shift left 1" $ assertEqWrap (Array.shiftL 1) [True, True, True, True] [True, True, True, False],
      testCase "Array shift left 2" $ assertEqWrap (Array.shiftL 3) [True, True, True, True] [True, False, False, False],
      testCase "Array shift left 3" $ assertEqWrap (Array.shiftL 0) [True, False, True, False] [True, False, True, False],
      testCase "Array shift left 4" $ assertEqWrap (Array.shiftL (-1)) [True, True, True, True] [False, True, True, True],
      testCase "Array shift left 5" $ assertEqWrap (Array.shiftL (-2)) [True, False, True, True] [False, False, True, False],
      testProperty "Array shift right" propShiftR,
      testCase "Array shift right 1" $ assertEqWrap (Array.shiftR 1) [True, True, True, True] [False, True, True, True],
      testCase "Array shift right 3" $ assertEqWrap (Array.shiftR 3) [True, True, True, True] [False, False, False, True],
      testProperty "List rotate inverse" propListRotateInverse,
      testProperty "Array rotate inverse" propArrayRotateInverse,
      testProperty "rotate left prop" propRotateL,
      testCase "rotate left 1" $ assertEqWrap (Array.rotateL 1) [True, False, True, False] [False, True, False, True],
      testCase "rotate left 2" $ assertEqWrap (Array.rotateL 3) [False, True, False] [False, True, False],
      testCase "rotate left 3" $ assertEqWrap (Array.rotateL 0) [True, False, True, False] [True, False, True, False],
      testCase "rotate left 4" $ assertEqWrap (Array.rotateL (-1)) [True, False, False, False] [False, True, False, False],
      testProperty "rotate right prop" propRotateR,
      testCase "rotate right 1" $ assertEqWrap (Array.rotateR 1) [True, False, True, False] [False, True, False, True],
      testCase "rotate right 2" $ assertEqWrap (Array.rotateR 3) [True, False, True, False] [False, True, False, True]
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

  let actual = Array.rotate n (Array.rotate (-n) (f xs))
  let expected = f xs

  actual' <- run $ interpret_ GF181 (return actual) ([] :: [GF181])
  expected' <- run $ interpret_ GF181 (return expected) ([] :: [GF181])

  Test.QuickCheck.Monadic.assert (actual' == expected')
  where
    f = Array.map' Boolean

propShiftL :: Int -> [Bool] -> Property
propShiftL n xs = monadicIO $ do
  propWrap $ do
    let actual = Array.shiftL n . Array.map' Boolean $ xs
    let expect = Array.map' Boolean . shift n $ xs
    Keelung.assert $ Array.beq actual expect

propShiftR :: Int -> [Bool] -> Property
propShiftR n xs = monadicIO $ do
  propWrap $ do
    let actual = Array.shiftR n . Array.map' Boolean $ xs
    let expect = Array.map' Boolean . shift (-n) $ xs
    Keelung.assert $ Array.beq actual expect

propRotateL :: Int -> [Bool] -> Property
propRotateL n xs = monadicIO $ do
  pre (not (null xs))
  propWrap $ do
    let actual = Array.rotateL n . Array.map' Boolean $ xs
    let expect = Array.map' Boolean . rotate n $ xs
    Keelung.assert $ Array.beq actual expect

propRotateR :: Int -> [Bool] -> Property
propRotateR n xs = monadicIO $ do
  pre (not (null xs))
  propWrap $ do
    let actual = Array.rotateR n . Array.map' Boolean $ xs
    let expect = Array.map' Boolean . rotate (-n) $ xs
    Keelung.assert $ Array.beq actual expect

assertEqWrap :: (Val ('Arr 'Bool) -> Val ('Arr 'Bool)) -> [Bool] -> [Bool] -> Assertion
assertEqWrap func actual expect = do
  actual' <- interpret_ GF181 (return (func (toBitArr actual))) ([] :: [GF181])
  expect' <- interpret_ GF181 (return (toBitArr expect)) ([] :: [GF181])
  fmap (map N) actual' @?= fmap (map N) expect'
  where
    toBitArr = Array.map' Boolean

propWrap :: Comp t -> PropertyM IO ()
propWrap comp = do
  result <- run $ interpret_ GF181 (comp $> unit) ([] :: [GF181])
  Test.QuickCheck.Monadic.assert (result == Right [])
