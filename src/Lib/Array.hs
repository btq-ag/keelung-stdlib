module Lib.Array where

import Control.Monad
import Keelung hiding (rotate, shift, shiftL, shiftR)
import Prelude hiding (drop, map, replicate, take, zipWith)
import qualified Prelude

infixr 1 >.>

--------------------------------------------------------------------------------

-- | See if 2 bit arrays are equal.
beq :: Arr Boolean -> Arr Boolean -> Boolean
beq as bs =
  foldl
    ( \acc i -> acc `And` (access as i `Keelung.eq` access bs i)
    )
    true
    [0 .. length as - 1]

eq :: Arr (Arr Boolean) -> Arr (Arr Boolean) -> Boolean
eq x y = beq (Lib.Array.concat x) (Lib.Array.concat y)

--------------------------------------------------------------------------------

map :: (a -> b) -> Arr a -> Arr b
map f = toArray . Prelude.map f . fromArray

-- | Array concatenation
concatenate :: Arr a -> Arr a -> Arr a
concatenate xs ys = toArray (fromArray xs <> fromArray ys)

concat :: Arr (Arr a) -> Arr a
concat = toArray . concatMap fromArray . fromArray

cons :: a -> Arr a -> Arr a
cons x xs = toArray (x : fromArray xs)

singleton :: a -> Arr a
singleton x = toArray [x]

reverse :: Arr a -> Arr a
reverse = toArray . Prelude.reverse . fromArray

replicate :: Int -> a -> Arr a
replicate = curry (toArray . uncurry Prelude.replicate)

take :: Int -> Arr a -> Arr a
take n = toArray . Prelude.take n . fromArray

drop :: Int -> Arr a -> Arr a
drop n = toArray . Prelude.drop n . fromArray

zeroBits :: Int -> Arr Boolean
zeroBits = flip replicate false

-- | Rotate left by 'n' bits
rotate :: Int -> Arr Boolean -> Arr Boolean
rotate n xs =
  let n' = n `mod` length xs
   in concatenate (Lib.Array.drop n' xs) (Lib.Array.take n' xs)

-- otherwise =
--  let n' = n `mod` lengthOf xs
--   in concatenate (Lib.Array.drop n' xs) (Lib.Array.take n' xs)

rotateL :: Int -> Arr Boolean -> Arr Boolean
rotateL = rotate

rotateR :: Int -> Arr Boolean -> Arr Boolean
rotateR = rotate . negate

-- | Shift left by 'n' bits and fill with given value
shiftAndFill :: Boolean -> Int -> Arr Boolean -> Arr Boolean
shiftAndFill fill n xs
  | n >= len = toArray $ Prelude.replicate len fill
  | n > 0 = toArray $ Prelude.drop n (fromArray xs) <> Prelude.replicate n fill
  | n == 0 = xs
  | n >= (-len) = toArray $ Prelude.replicate (-n) fill <> Prelude.take (len + n) (fromArray xs)
  | otherwise = toArray $ Prelude.replicate len fill
  where
    len = length xs

-- | Shift left by 'n' bits (false-fill)
shift :: Int -> Arr Boolean -> Arr Boolean
shift = shiftAndFill false

shiftL :: Int -> Arr Boolean -> Arr Boolean
shiftL = shift

shiftR :: Int -> Arr Boolean -> Arr Boolean
shiftR = shift . negate

or :: Arr Boolean -> Arr Boolean -> Arr Boolean
or = zipWith Or

and :: Arr Boolean -> Arr Boolean -> Arr Boolean
and = zipWith And

xor :: Arr Boolean -> Arr Boolean -> Arr Boolean
xor = zipWith Xor

zipWith :: (Boolean -> Boolean -> Boolean) -> Arr Boolean -> Arr Boolean -> Arr Boolean
zipWith op as bs = toArray $ Prelude.zipWith op (fromArray as) (fromArray bs)

-- | length xs <
cast :: Int -> Arr Boolean -> Arr Boolean
cast n xs = concatenate xs (replicate (n - length xs) false)

chunks :: Int -> Arr a -> Arr (Arr a)
chunks n = toArray . Prelude.map toArray . group n . fromArray

chunkReverse :: Int -> Arr a -> Arr (Arr a)
chunkReverse n = toArray . Prelude.map (toArray . Prelude.reverse) . group n . fromArray

update :: Int -> a -> Arr a -> Arr a
update idx x arr
  | idx >= length arr || idx < 0 = arr
  | otherwise = concatenate (take idx arr) (cons x (drop (idx + 1) arr))

update' :: Int -> (a -> a) -> Arr a -> Arr a
update' idx f arr = update idx (f (access arr idx)) arr

--------------------------------------------------------------------------------

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = Prelude.take n l : group n (Prelude.drop n l)
  | otherwise = error "Negative or zero"

-- | Full adder without sharing
fullAdderSlow :: Arr Boolean -> Arr Boolean -> Arr Boolean
fullAdderSlow as bs =
  let zipped = zip (fromArray as) (fromArray bs)
   in toArray $ fst $ foldl f ([], false) zipped
  where
    f :: ([Boolean], Boolean) -> (Boolean, Boolean) -> ([Boolean], Boolean)
    f (acc, carry) (a, b) =
      let value = a `Xor` b `Xor` carry
          nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)
       in (acc ++ [value], nextCarry)

-- | Full adder
fullAdder :: Arr Boolean -> Arr Boolean -> Comp (Arr Boolean)
fullAdder as bs = do
  let zipped = zip (fromArray as) (fromArray bs)
  (result, _) <- foldM f ([], false) zipped
  return (toArray result)
  where
    f :: ([Boolean], Boolean) -> (Boolean, Boolean) -> Comp ([Boolean], Boolean)
    f (acc, carry) (a, b) = do
      aXORb <- reuse $ a `Xor` b
      carry' <- reuse carry
      let value = aXORb `Xor` carry'
      let nextCarry = (aXORb `And` carry') `Or` (a `And` b)
      return (acc ++ [value], nextCarry)

-- | "T" for top-level
fullAdderT :: Int -> Comp (Arr Boolean)
fullAdderT width = do
  xs <- inputs width
  ys <- inputs width
  fullAdder xs ys

{-# INLINE (>.>) #-} --infixr 9
(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) f g = g . f
