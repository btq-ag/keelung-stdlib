{-# LANGUAGE DataKinds #-}

module Lib.ArrayI where

import Control.Monad
import Data.Bifunctor
import Keelung hiding (update)
import Prelude hiding (drop, map, replicate, take)
import qualified Prelude

-- | See if 2 bit arrays are equal.
beq :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val 'Bool
beq as bs =
  foldl
    ( \acc i -> acc `And` (access as i `BEq` access bs i)
    )
    true
    [0 .. lengthOf as - 1]

-- | `map` for Keelung arrays
map :: (Val a -> Val b) -> Val ('Arr a) -> Val ('Arr b)
map f = toArray . Prelude.map f . fromArray

-- | Array concatenation
concatenate :: Val ('Arr a) -> Val ('Arr a) -> Val ('Arr a)
concatenate xs ys = toArray (fromArray xs <> fromArray ys)

concat :: Val ('Arr ('Arr a)) -> Val ('Arr a)
concat = toArray . concatMap fromArray . fromArray

cons :: Val a -> Val ('Arr a) -> Val ('Arr a)
cons x xs = toArray (x : fromArray xs)

singleton :: Val a -> Val ('Arr a)
singleton x = toArray [x]

reverse :: Val ('Arr a) -> Val ('Arr a)
reverse = toArray . Prelude.reverse . fromArray

replicate :: Int -> Val t -> Val ('Arr t)
replicate = curry (toArray . uncurry Prelude.replicate)

take :: Int -> Val ('Arr a) -> Val ('Arr a)
take n = toArray . Prelude.take n . fromArray

drop :: Int -> Val ('Arr a) -> Val ('Arr a)
drop n = toArray . Prelude.drop n . fromArray

zeroBits :: Int -> Val ('Arr 'Bool)
zeroBits = flip replicate false

-- | Rotate left by 'n' bits
rotate :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
rotate n xs =
  let n' = (lengthOf xs - n) `mod` lengthOf xs
   in concatenate (Lib.ArrayI.drop n' xs) (Lib.ArrayI.take n' xs)

rotateL :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
rotateL = rotate

rotateR :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
rotateR = rotate . negate

-- | Shift left by 'n' bits (false-fill)
shift :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
shift n xs =
  let l = lengthOf xs
   in if n > 0
        then concatenate (map (const false) (take n xs)) (take (l - n) xs)
        else concatenate (drop (-n) xs) (map (const false) (take (-n) xs))

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

-- | length xs <
cast :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
cast n xs = concatenate xs (replicate (n - lengthOf xs) false)

chunks :: Int -> Val ('Arr t) -> Val ('Arr ('Arr t))
chunks n = toArray . Prelude.map toArray . group n . fromArray

chunkReverse :: Int -> Val ('Arr t) -> Val ('Arr t)
chunkReverse n = toArray . concatMap Prelude.reverse . group n . fromArray

update :: Int -> Val t -> Val ('Arr t) -> Val ('Arr t)
update i x xs =
  concatenate (take i xs) (cons x (drop (i + 1) xs))

update' :: Int -> (Val t -> Val t) -> Val ('Arr t) -> Val ('Arr t)
update' i op xs = update i (op (access xs i)) xs

--------------------------------------------------------------------------------

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = Prelude.take n l : group n (Prelude.drop n l)
  | otherwise = error "Negative or zero"

fullAdder1bit :: Val 'Bool -> Val 'Bool -> Val 'Bool -> (Val 'Bool, Val 'Bool)
fullAdder1bit a b carry =
  let value = a `Xor` b `Xor` carry
   in let nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)
       in (value, nextCarry)

fullAdder :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
fullAdder as bs =
  toArray . fst $
    foldl
      ( \(result, carry) (a, b) -> do
          first (: result) $ fullAdder1bit a b carry
      )
      ([], false)
      (zip (fromArray as) (fromArray bs))
