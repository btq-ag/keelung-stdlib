{-# LANGUAGE DataKinds #-}

module Test.Sha256 where

import Keelung
import Sha256
import qualified Lib.W8 as W8

import Crypto.Hash (hashWith, SHA256(..))
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteArray as BA
import Test.Util
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "Sha256"
        [
            testCase "abc" $ testHash "abc",
            testCase "BE 64-bit integer" $ testFromWordNBE [True : replicate 63 False]
        ]


testHash :: String -> Assertion
testHash m = do
    expect <- interpret_ GF181 (W8.fromString . unpack . BA.pack . BA.unpack $ hashWith SHA256 . pack $ m) ([] :: [GF181])
    actual <- interpret_ GF181 (W8.fromString m >>= hash) ([] :: [GF181])
    fmap toWord32List actual @?= fmap toWord32List expect

testFromWordNBE :: [[Bool]] -> Assertion
testFromWordNBE bs = do
    let bs' = mapM (toArrayM . map Boolean) bs >>= toArrayM
    actual <- interpret_ GF181 (bs' >>= W8.fromWordNBE) ([] :: [GF181])
    expect <- interpret_ GF181 (toArrayM $ map Boolean (replicate 56 False ++ (True : replicate 7 False))) ([] :: [GF181])
    fmap (map N) actual @?= fmap (map N) expect

    
