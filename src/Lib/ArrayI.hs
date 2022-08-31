{-# LANGUAGE DataKinds #-}

module Lib.ArrayI where

import Control.Monad
import Keelung
import qualified Lib.Array as Mutable
import Prelude hiding (replicate)
import qualified Prelude

-- | See if 2 bit arrays are equal.
beq :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val 'Bool
beq as bs =
  foldl
    (\acc (a, b) -> acc `And` (a `BEq` b))
    true
    (zip (fromArray as) (fromArray bs))

-- | `map` for Keelung arrays
map :: (Val a -> Val b) -> Val ('Arr a) -> Val ('Arr b)
map f = toArray . Prelude.map f . fromArray

-- | Array concatenation
concatenate :: Val ('Arr a) -> Val ('Arr a) -> Val ('Arr a)
concatenate xs ys = toArray (fromArray xs <> fromArray ys)

cons :: Val a -> Val ('Arr a) -> Val ('Arr a)
cons x xs = toArray (x : fromArray xs)

singleton :: Val a -> Val ('Arr a)
singleton x = toArray [x]

reverse :: Val ('Arr a) -> Val ('Arr a)
reverse = toArray . Prelude.reverse . fromArray

replicate :: Int -> Val t -> Val ('Arr t)
replicate n x = toArray $ Prelude.replicate n x

zeroBits :: Int -> Val ('Arr 'Bool)
zeroBits n = replicate n false

-- | Rotate left by 'n' bits
rotate :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
rotate n xs =
  let n' = (lengthOf xs + n) `mod` lengthOf xs
   in toArray $ Prelude.drop n' (fromArray xs) <> Prelude.take n' (fromArray xs)

rotateL :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
rotateL = rotate

rotateR :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
rotateR = rotate . negate

-- | Shift left by 'n' bits (false-fill)
shift :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
shift n xs =
  toArray $
    if n > 0
      then Prelude.drop n (fromArray xs) <> Prelude.replicate n false
      else Prelude.replicate (lengthOf xs + n) false <> Prelude.take (lengthOf xs + n) (fromArray xs)

shiftL :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
shiftL = shift

shiftR :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
shiftR = shift . negate

or :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
or = bitOp Or

and :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
and = bitOp And

xor :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
xor = bitOp Xor

bitOp :: (Val 'Bool -> Val 'Bool -> Val 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
bitOp op as bs = toArray $ zipWith op (fromArray as) (fromArray bs)

or' :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
or' = bitOp Or

and' :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
and' = bitOp And

xor' :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
xor' = bitOp Xor

flatten :: Val ('Arr ('Arr t)) -> Val ('Arr t)
flatten = toArray . Prelude.concat . mapM fromArray . fromArray

cast :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
cast n xs = cast' n (fromArray xs)

-- | length xs < n
cast' :: Int -> [Val 'Bool] -> Val ('Arr 'Bool)
cast' n xs = toArray $ xs ++ Prelude.replicate (n - length xs) false

chunks :: Int -> Val ('Arr 'Bool) -> Val ('Arr ('Arr 'Bool))
chunks n = toArray . fmap toArray . group n . fromArray
  where
    group :: Int -> [a] -> [[a]]
    group _ [] = []
    group n l
      | n > 0 = take n l : group n (drop n l)
      | otherwise = error "Negative or zero n"