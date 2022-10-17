{-# LANGUAGE DataKinds #-}

module Test.Lib.Array where

import Keelung hiding (run)
import Keelung.Error
import qualified Lib.Array as Array
import qualified Lib.ArrayM as ArrayM
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
      testProperty "Full adder (immutable)" propFullAdder,
      testProperty "Full adder (mutable)" propFullAdderM,
      testCase "full adder 1" $ assertBinaryM Array.fullAdder [False, False, False, False] [False, False, False, False] [False, False, False, False],
      testCase "full adder 2" $ assertBinaryM Array.fullAdder [True, False, False, False] [True, False, False, False] [False, True, False, False],
      testCase "full adder 3" $ assertBinaryM Array.fullAdder [True, True, True, True] [True, True, True, True] [False, True, True, True]
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

  let actual = Array.rotate n (Array.rotate (-n) (toArray $ map Boolean xs))
  let expected = toArray $ map Boolean xs

  actual' <- run $ interpret_ GF181 (return actual) ([] :: [GF181])
  expected' <- run $ interpret_ GF181 (return expected) ([] :: [GF181])

  Test.QuickCheck.Monadic.assert (actual' == expected')

propShiftL :: Int -> [Bool] -> Property
propShiftL n xs = monadicIO $ do
  let actual = Array.shiftL n . toArray . map Boolean $ xs
  let expect = toArray . map Boolean . shift n $ xs
  propWrap actual expect

propShiftR :: Int -> [Bool] -> Property
propShiftR n xs = monadicIO $ do
  let actual = Array.shiftR n . toArray . map Boolean $ xs
  let expect = toArray . map Boolean . shift (-n) $ xs
  propWrap actual expect

propRotateL :: Int -> [Bool] -> Property
propRotateL n xs = monadicIO $ do
  pre (not (null xs))
  let actual = Array.rotateL n . toArray . map Boolean $ xs
  let expect = toArray . map Boolean . rotate n $ xs
  propWrap actual expect

propRotateR :: Int -> [Bool] -> Property
propRotateR n xs = monadicIO $ do
  pre (not (null xs))
  let actual = Array.rotateR n . toArray . map Boolean $ xs
  let expect = toArray . map Boolean . rotate (-n) $ xs
  propWrap actual expect

propUpdateLength :: Int -> Bool -> [Bool] -> Property
propUpdateLength idx x xs = monadicIO $ do
  pre (not (null xs))
  let actual = length $ Array.update idx (Boolean x) (toArray $ map Boolean xs)
  let expect = length xs
  Test.QuickCheck.Monadic.assert (actual == expect)

assertBinary :: (Arr Boolean -> Arr Boolean -> Arr Boolean) -> [Bool] -> [Bool] -> [Bool] -> Assertion
assertBinary f x y expect = do
    assertWrap (f (toBitArr x) (toBitArr y)) (toBitArr expect)

assertUnary :: (Arr Boolean -> Arr Boolean) -> [Bool] -> [Bool] -> Assertion
assertUnary f actual expect = assertWrap (f $ toBitArr actual) (toBitArr expect)

assertBinaryM :: (Arr Boolean -> Arr Boolean -> Comp (Arr Boolean)) -> [Bool] -> [Bool] -> [Bool] -> Assertion
assertBinaryM f x y expect = do
    assertWrapM (f (toBitArr x) (toBitArr y)) (toBitArr expect)

assertUnaryM :: (Arr Boolean -> Comp (Arr Boolean)) -> [Bool] -> [Bool] -> Assertion
assertUnaryM f actual expect = assertWrapM (f $ toBitArr actual) (toBitArr expect)

-------------------------------------------------------------------------------

wrap :: Elaborable t => [GF181] -> Comp t -> PropertyM IO [Bool]
wrap ins prog = asBool $ run $ interpret_ GF181 prog ins

asBool :: PropertyM IO (Either Error [GF181]) -> PropertyM IO [Bool]
asBool f = do
  result <- f
  case result of
    Left err -> fail (show err)
    Right xs -> return (map (not . (==) 0) xs)

-------------------------------------------------------------------------------

propFullAdder :: Int -> Property
propFullAdder len = forAll (gen2BoolList len) $ \(as, bs) -> monadicIO $ do
  pre (len <= 100)
  cs <- wrap [] $ do
    Array.fullAdder (toBoolArr as) (toBoolArr bs)

  let actual = toInt cs
  let expected = toInt as + toInt bs
  let bound = 2 ^ length as

  return $
    if expected >= bound
      then actual === expected - bound
      else actual === expected

propFullAdderM :: Int -> Property
propFullAdderM len =
  forAll (gen2BoolList len) $ \(as, bs) -> monadicIO $ do
    pre (len <= 100 && len > 0)
    cs <- wrap [] $ do
      as' <- toBoolArrM as
      bs' <- toBoolArrM bs
      ArrayM.fullAdder as' bs'

    let actual = toInt cs
    let expected = toInt as + toInt bs
    let bound = 2 ^ length as

    return $
      if expected >= bound
        then actual === expected - bound
        else actual === expected

gen2BoolList :: Int -> Gen ([Bool], [Bool])
gen2BoolList len = do
  as <- vector len
  bs <- vector len
  return (as, bs)

-- | Assuming that the endianess is little endian
toInt :: [Bool] -> Integer
toInt = foldl (\acc (i, a) -> if a then acc + 2 ^ i else acc) 0 . zip [0 :: Integer ..]

toBoolArr :: [Bool] -> Arr Boolean
toBoolArr = toArray . map Boolean

toBoolArrM :: [Bool] -> Comp (ArrM Boolean)
toBoolArrM = toArrayM . map Boolean
