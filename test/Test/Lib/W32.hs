{-# LANGUAGE DataKinds #-}

module Test.Lib.W32 where

-- import Keelung hiding (run)
-- import qualified Lib.W32M as W32
-- 
-- import Data.Word
-- import Test.Util
-- import Test.QuickCheck.Monadic
-- import Test.Tasty
-- import Test.Tasty.QuickCheck
-- 
-- tests :: TestTree
-- tests = testGroup "W32" [
--         testProperty "fromW32List" propFromW32List
--     ]
-- 
-- propFromW32List :: [Word32] -> Property
-- propFromW32List xs = monadicIO $ do
--     pre (not (null xs))
--     actual <- run $ W32.fromWord32List xs >>= \x -> interpret_ GF181 x ([] :: [GF181])
--     Test.QuickCheck.Monadic.assert $
--         fmap toWord32List actual == Right xs
