module Test.Lib.ArrayI where

import Data.Functor
import Keelung
import qualified Lib.ArrayI as ArrayI
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

-- import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Immutable Array"
    [ testProperty "shift left" prop_shiftL,
      testProperty "shift right" prop_shiftL,
      testProperty "rotate left" prop_rotateL,
      testProperty "rotate right" prop_rotateR
    ]

prop_shiftL :: Int -> [Bool] -> Property
prop_shiftL n xs = monadicIO $ do
  pre (n <= length xs && n > 0)
  wrap $ do
    actual <- ArrayI.shiftL n . toArrayI . map Boolean $ xs
    let expect = toArrayI . map Boolean . shiftR n $ xs
    Keelung.assert =<< ArrayI.beq actual expect
  where
    shiftR :: Int -> [Bool] -> [Bool]
    shiftR n' xs' = replicate (min n' (length xs')) False <> take (length xs' - n') xs'


prop_shiftR :: Int -> [Bool] -> Property
prop_shiftR n xs = monadicIO $ do
  pre (n <= length xs && n > 0)
  wrap $ do
    actual <- ArrayI.shiftR n . toArrayI . map Boolean $ xs
    let expect = toArrayI . map Boolean . shiftL n $ xs
    Keelung.assert =<< ArrayI.beq actual expect
  where
    shiftL :: Int -> [Bool] -> [Bool]
    shiftL n' xs' = drop n' xs' <> replicate (min n' (length xs')) False

prop_rotateL :: Int -> [Bool] -> Property
prop_rotateL n xs = monadicIO $ do
    pre (n <= length xs && n > 0)
    wrap $ do
        actual <- ArrayI.rotateL n . toArrayI . map Boolean $ xs
        let expect = toArrayI . map Boolean . rotateR n $ xs
        Keelung.assert =<< ArrayI.beq actual expect
    where
        rotateR :: Int -> [Bool] -> [Bool]
        rotateR n' xs' = drop (length xs' - n') xs' <> take (length xs' - n') xs'

prop_rotateR :: Int -> [Bool] -> Property
prop_rotateR n xs = monadicIO $ do
    pre (n <= length xs && n > 0)
    wrap $ do
        actual <- ArrayI.rotateR n . toArrayI . map Boolean $ xs
        let expect = toArrayI . map Boolean . rotateL n $ xs
        Keelung.assert =<< ArrayI.beq actual expect
    where
        rotateL :: Int -> [Bool] -> [Bool]
        rotateL n' xs' = drop n' xs' <> take n' xs'

wrap :: Comp GF181 t -> PropertyM IO ()
wrap comp = do
  result <- run $ interpret (comp $> unit) []
  Test.QuickCheck.Monadic.assert (result == Right [])

