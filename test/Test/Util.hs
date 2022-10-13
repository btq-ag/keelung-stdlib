{-# LANGUAGE DataKinds #-}

module Test.Util
  ( tests,
    toBitArr,
    toGF181Bits,
    toWord32List,
    assertWrap,
    assertWrapM,
    propWrap,
  )
where

import Data.Bits
import Data.Word
import Keelung hiding (run)
import Lib.Array (group)
import qualified Lib.Array as Array
import qualified Lib.W32 as W32
import qualified Lib.W8 as W8
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Util"
    [ testCase "toBitArr 1" $
        testToGF181Bits
          [0x00000001]
          [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      testCase "toBitArr 2" $
        testToGF181Bits
          [0x00000011]
          [1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      testCase "toWord32List 1" $
        testToWord32List
          [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
          [0x00000001],
      testCase "toWord32List 2" $
        testToWord32List
          [1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
          [0x00000011],
      testProperty "BitConversion Property" propBitConversion,
      testCase "W8toW32" $ testStringToW8ToW32 "a" [0x61, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    ]

propBitConversion :: [Word32] -> Property
propBitConversion xs = toWord32List (toGF181Bits xs) === xs

testToGF181Bits :: [Word32] -> [GF181] -> Assertion
testToGF181Bits raw expect =
  map N (toGF181Bits raw) @?= map N expect

toGF181Bits :: [Word32] -> [GF181]
toGF181Bits = concatMap (\x -> map (f . testBit x) [0 .. 31])
  where
    f :: Bool -> GF181
    f x = if x then 1 else 0

testToWord32List :: [GF181] -> [Word32] -> Assertion
testToWord32List raw expect = toWord32List raw @?= expect

toWord32List :: [GF181] -> [Word32]
toWord32List xs =
  let xs' = group 32 . map toInteger $ xs
   in let x = zip xs' [zeroBits ..]
       in map
            ( \(bs, w) ->
                foldl
                  (\w' (b, i) -> if b == 1 then setBit w' i else clearBit w' i)
                  w
                  (zip bs [0 .. 31])
            )
            x

toBitArr :: [Bool] -> Arr Boolean
toBitArr = toArray . map Boolean

assertWrap :: Elaborable t => t -> t -> Assertion
assertWrap actual expect = do
  actual' <- interpret_ GF181 (return actual) ([] :: [GF181])
  expect' <- interpret_ GF181 (return expect) ([] :: [GF181])
  fmap (map N) actual' @?= fmap (map N) expect'

assertWrapM :: Elaborable t => Comp t -> t -> Assertion
assertWrapM actual expect = do
  actual' <- interpret_ GF181 actual ([] :: [GF181])
  expect' <- interpret_ GF181 (return expect) ([] :: [GF181])
  fmap (map N) actual' @?= fmap (map N) expect'

propWrap :: Elaborable t => t -> t -> PropertyM IO ()
propWrap actual expect = do
  actual' <- run $ interpret_ GF181 (pure actual) ([] :: [GF181])
  expect' <- run $ interpret_ GF181 (pure expect) ([] :: [GF181])
  Test.QuickCheck.Monadic.assert (fmap (map N) actual' == fmap (map N) expect')

testStringToW8ToW32 :: String -> [Word32] -> Assertion
testStringToW8ToW32 rawActual rawExpect = do
  let actual = W32.fromW8Chunks' . W8.pad' 64 . W8.fromString' $ rawActual
  let expect = W32.fromWord32List' rawExpect
  actual' <- interpret_ GF181 (return actual) ([] :: [GF181])
  expect' <- interpret_ GF181 (return expect) ([] :: [GF181])

  fmap (map N) actual' @?= fmap (map N) expect'
