{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Big256 where

import Data.Proxy
import GHC.TypeNats
import Keelung hiding (Width)

-- 256 < 258 = 86*3
-- 172 = 86 * 2
-- type Limb = UInt 173 -- 2 * 86 + overflow

type Width = 86

type Limb = UInt (Width * 2 + 1) -- 2 * 86 + buffer for overflow for 3x3 school book

madd :: [Limb] -> [Limb] -> Maybe Limb -> Comp (Limb, Limb)
madd xs ys acc = do
  let z = sum (zipWith (*) xs ys)
  let z' = maybe z (+ z) acc
  z'' <- reuse z'
  return (z'' .&. UInt (2 ^ width - 1), z'' .<<. (-width))
  where
    width = fromIntegral (natVal (Proxy :: Proxy Width))

mul3x3 :: (Limb, Limb, Limb) -> (Limb, Limb, Limb) -> Comp (Limb, Limb, Limb)
mul3x3 (a, b, c) (x, y, z) = do
  (r0, acc) <- madd [a] [x] Nothing
  (r1, acc') <- madd [a, b] [y, x] (Just acc)
  (r2, _) <- madd [a, b, c] [z, y, x] (Just acc')
  return (r0, r1, r2)

f :: Comp [Limb]
f = do
  a <- inputList Private 3
  b <- inputList Private 3
  (x, y, z) <- mul3x3 (a !! 0, a !! 1, a !! 2) (b !! 0, b !! 1, b !! 2)
  return [x, y, z]
