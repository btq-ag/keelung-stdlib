{-# LANGUAGE DataKinds #-}

module Test.Lib.Array where

import Data.Functor
import Keelung
import qualified Lib.Array as Array
import qualified Lib.ArrayI as ArrayI
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Immutable Array"
    [ testProperty "shift left prop" propShiftL
    -- testCase "shift left 1" $ assertEqWrap (ArrayI.shiftL 1) [True, True, True, True] [False, True, True, True],
    -- testCase "shift left 3" $ assertEqWrap (ArrayI.shiftL 3) [True, True, True, True] [False, False, False, True]
    -- testProperty "shift right prop" prop_shiftL,
    -- testCase "shift right 1" $ assertEqWrap (ArrayI.shiftR 1) [True, True, True, True] [True, True, True, False],
    -- testCase "shift right 3" $ assertEqWrap (ArrayI.shiftR 3) [True, True, True, True] [True, False, False, False],

    -- testProperty "rotate left prop" prop_rotateL,
    -- testCase "rotate left 1" $ assertEqWrap (ArrayI.rotateL 1) [True, False, True, False] [False, True, False, True],
    -- testCase "rotate left 3" $ assertEqWrap (ArrayI.rotateL 3) [True, False, True, False] [False, True, False, True],

    -- testProperty "rotate right prop" prop_rotateR,
    -- testCase "rotate right 1" $ assertEqWrap (ArrayI.rotateR 1) [True, False, True, False] [False, True, False, True],
    -- testCase "rotate right 3" $ assertEqWrap (ArrayI.rotateR 3) [True, False, True, False] [False, True, False, True]
    ]

propShiftL :: Int -> [Bool] -> Property
propShiftL n xs = monadicIO $ do
  propWrap $ do
    let actual = Array.shiftL n . toArray . map Boolean $ xs
    let expect = toArray . map Boolean . shiftR n $ xs
    Keelung.assert $ Array.beq actual expect
  where
    shiftR :: Int -> [Bool] -> [Bool]
    shiftR n' xs' =
      if n' > 0
        then drop n' xs' <> replicate n' False
        else replicate (length xs' + n') False <> take (length xs' + n') xs'

prop_shiftR :: Int -> [Bool] -> Property
prop_shiftR n xs = monadicIO $ do
  pre (n <= length xs && n > 0)
  propWrap $ do
    let actual = ArrayI.shiftR n . toArray . map Boolean $ xs
    let expect = toArray . map Boolean . shiftL n $ xs
    Keelung.assert $ ArrayI.beq actual expect
  where
    shiftL :: Int -> [Bool] -> [Bool]
    shiftL n' xs' = drop n' xs' <> replicate (min n' (length xs')) False

prop_rotateL :: Int -> [Bool] -> Property
prop_rotateL n xs = monadicIO $ do
  pre (n <= length xs && n > 0)
  propWrap $ do
    let actual = ArrayI.rotateL n . toArray . map Boolean $ xs
    let expect = toArray . map Boolean . rotateR n $ xs
    Keelung.assert $ ArrayI.beq actual expect
  where
    rotateR :: Int -> [Bool] -> [Bool]
    rotateR n' xs' = drop (length xs' - n') xs' <> take (length xs' - n') xs'

prop_rotateR :: Int -> [Bool] -> Property
prop_rotateR n xs = monadicIO $ do
  pre (n <= length xs && n > 0)
  propWrap $ do
    let actual = ArrayI.rotateR n . toArray . map Boolean $ xs
    let expect = toArray . map Boolean . rotateL n $ xs
    Keelung.assert $ ArrayI.beq actual expect
  where
    rotateL :: Int -> [Bool] -> [Bool]
    rotateL n' xs' = drop n' xs' <> take n' xs'

assertEqWrap :: (Val ('Arr 'Bool) -> Val ('Arr 'Bool)) -> [Bool] -> [Bool] -> Assertion
assertEqWrap func actual expect = assertWrap $ do
  Keelung.assert $ ArrayI.beq (toBitArr expect) ((func . toBitArr) actual)
  where
    toBitArr = toArray . map Boolean

assertWrap :: Comp t -> Assertion
assertWrap comp = do
  result <- interpret_ GF181 (comp $> unit) ([] :: [GF181])
  result @?= Right []

propWrap :: Comp t -> PropertyM IO ()
propWrap comp = do
  result <- run $ interpret_ GF181 (comp $> unit) ([] :: [GF181])
  Test.QuickCheck.Monadic.assert (result == Right [])
