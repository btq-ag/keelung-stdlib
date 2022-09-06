{-# LANGUAGE DataKinds #-}

module Test.Lib.ArrayI where

import Data.Functor
import Keelung
import qualified Lib.ArrayI as ArrayI
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

-- import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Immutable Array"
    [ testProperty "shift left prop" prop_shiftL,
      testCase "shift left 1" $ assertEqWrap (ArrayI.shiftL 1) [True, True, True, True] [False, True, True, True],
      testCase "shift left 3" $ assertEqWrap (ArrayI.shiftL 3) [True, True, True, True] [False, False, False, True],

      testProperty "shift right prop" prop_shiftL,
      testCase "shift right 1" $ assertEqWrap (ArrayI.shiftR 1) [True, True, True, True] [True, True, True, False],
      testCase "shift right 3" $ assertEqWrap (ArrayI.shiftR 3) [True, True, True, True] [True, False, False, False],

      testProperty "rotate left prop" prop_rotateL,
      testCase "rotate left 1" $ assertEqWrap (ArrayI.rotateL 1) [True, False, True, False] [False, True, False, True],
      testCase "rotate left 3" $ assertEqWrap (ArrayI.rotateL 3) [True, False, True, False] [False, True, False, True],

      testProperty "rotate right prop" prop_rotateR,
      testCase "rotate right 1" $ assertEqWrap (ArrayI.rotateR 1) [True, False, True, False] [False, True, False, True],
      testCase "rotate right 3" $ assertEqWrap (ArrayI.rotateR 3) [True, False, True, False] [False, True, False, True]
    ]

prop_shiftL :: Int -> [Bool] -> Property
prop_shiftL n xs = monadicIO $ do
  pre (n <= length xs && n > 0)
  propWrap $ do
    actual <- ArrayI.shiftL n . toArrayI . map Boolean $ xs
    let expect = toArrayI . map Boolean . shiftR n $ xs
    Keelung.assert =<< ArrayI.beq actual expect
  where
    shiftR :: Int -> [Bool] -> [Bool]
    shiftR n' xs' = replicate (min n' (length xs')) False <> take (length xs' - n') xs'

prop_shiftR :: Int -> [Bool] -> Property
prop_shiftR n xs = monadicIO $ do
  pre (n <= length xs && n > 0)
  propWrap $ do
    actual <- ArrayI.shiftR n . toArrayI . map Boolean $ xs
    let expect = toArrayI . map Boolean . shiftL n $ xs
    Keelung.assert =<< ArrayI.beq actual expect
  where
    shiftL :: Int -> [Bool] -> [Bool]
    shiftL n' xs' = drop n' xs' <> replicate (min n' (length xs')) False

prop_rotateL :: Int -> [Bool] -> Property
prop_rotateL n xs = monadicIO $ do
    pre (n <= length xs && n > 0)
    propWrap $ do
        actual <- ArrayI.rotateL n . toArrayI . map Boolean $ xs
        let expect = toArrayI . map Boolean . rotateR n $ xs
        Keelung.assert =<< ArrayI.beq actual expect
    where
        rotateR :: Int -> [Bool] -> [Bool]
        rotateR n' xs' = drop (length xs' - n') xs' <> take (length xs' - n') xs'

prop_rotateR :: Int -> [Bool] -> Property
prop_rotateR n xs = monadicIO $ do
    pre (n <= length xs && n > 0)
    propWrap $ do
        actual <- ArrayI.rotateR n . toArrayI . map Boolean $ xs
        let expect = toArrayI . map Boolean . rotateL n $ xs
        Keelung.assert =<< ArrayI.beq actual expect
    where
        rotateL :: Int -> [Bool] -> [Bool]
        rotateL n' xs' = drop n' xs' <> take n' xs'

assertEqWrap :: (Val ('Arr 'Bool) n -> Comp GF181 (Val ('Arr 'Bool) GF181)) -> [Bool] -> [Bool] -> Assertion
assertEqWrap func actual expect = assertWrap $ do
    (func . toBitArr) actual >>= ArrayI.beq (toBitArr expect) >>= Keelung.assert
    where
        toBitArr = toArrayI . map Boolean

assertWrap :: Comp GF181 t -> Assertion
assertWrap comp = do
    result <- interpret (comp $> unit) []
    result @?= Right []

propWrap :: Comp GF181 t -> PropertyM IO ()
propWrap comp = do
  result <- run $ interpret (comp $> unit) []
  Test.QuickCheck.Monadic.assert (result == Right [])

