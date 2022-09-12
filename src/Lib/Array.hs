{-# LANGUAGE DataKinds #-}

module Lib.Array where

import Keelung
import Prelude hiding (drop, map, replicate, take)
import qualified Prelude

infixr 1 >.>

-- | See if 2 bit arrays are equal.
beq :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val 'Bool
beq as bs =
  foldl
    ( \acc i -> acc `And` (access as i `BEq` access bs i)
    )
    true
    [0 .. lengthOf as - 1]

eq :: Val ('Arr ('Arr 'Bool)) -> Val ('Arr ('Arr 'Bool)) -> Val 'Bool
eq x y = beq (Lib.Array.concat x) (Lib.Array.concat y)

--------------------------------------------------------------------------------

map :: (Val a -> Val b) -> Val ('Arr a) -> Val ('Arr b)
map f = map' f . fromArray

map' :: (a -> Val b) -> [a] -> Val ('Arr b)
map' f = toArray . Prelude.map f

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
    let n' = n `mod` lengthOf xs
     in concatenate (Lib.Array.drop n' xs) (Lib.Array.take n' xs)
  -- | otherwise =
  --   let n' = n `mod` lengthOf xs
  --    in concatenate (Lib.Array.drop n' xs) (Lib.Array.take n' xs)

rotateL :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
rotateL = rotate

rotateR :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
rotateR = rotate . negate

-- | Shift left by 'n' bits (false-fill)
shift :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
shift n xs
  | n >= len = toArray $ Prelude.replicate len false
  | n > 0 = toArray $ Prelude.drop n (fromArray xs) <> Prelude.replicate n false
  | n == 0 = xs
  | n >= (-len) = toArray $ Prelude.replicate (-n) false <> Prelude.take (len + n) (fromArray xs)
  | otherwise = toArray $ Prelude.replicate len false
  where
    len = lengthOf xs

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

chunkReverse :: Int -> Val ('Arr t) -> Val ('Arr ('Arr t))
chunkReverse n = map' (toArray . Prelude.reverse) . group n . fromArray

update :: Int -> Val a -> Val ('Arr a) -> Val ('Arr a)
update idx x arr
    | idx >= lengthOf arr || idx < 0 = arr
    | otherwise = concatenate (take idx arr) (cons x (drop (idx + 1) arr))

update' :: Int -> (Val a -> Val a) -> Val ('Arr a) -> Val ('Arr a)
update' idx f arr = update idx (f (access arr idx)) arr

--------------------------------------------------------------------------------

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = Prelude.take n l : group n (Prelude.drop n l)
  | otherwise = error "Negative or zero"

fullAdder :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
fullAdder as bs =
  let zipped = zip (fromArray as) (fromArray bs)
   in toArray $ fst $ foldl f ([], false) zipped
  where
    f :: ([Val 'Bool], Val 'Bool) -> (Val 'Bool, Val 'Bool) -> ([Val 'Bool], Val 'Bool)
    f (acc, carry) (a, b) =
      let value = a `Xor` b `Xor` carry
          nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)
       in (acc ++ [value], nextCarry)

{-# INLINE (>.>) #-} --infixr 9
(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) f g = g . f

