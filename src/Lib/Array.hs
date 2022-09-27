{-# LANGUAGE DataKinds #-}

module Lib.Array where

import Control.Monad
import Keelung
import Prelude hiding (drop, map, replicate, take)
import qualified Prelude

-- | See if 2 bit arrays are equal.
beq :: Arr Boolean -> Arr Boolean -> Boolean
beq as bs =
  foldl
    ( \acc i -> acc `And` (access as i `BEq` access bs i)
    )
    true
    [0 .. length as - 1]

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

-- | Shift left by 'n' bits (false-fill)
shift :: Int -> Arr Boolean -> Arr Boolean
shift n xs
  | n >= len = toArray $ Prelude.replicate len false
  | n > 0 = toArray $ Prelude.drop n (fromArray xs) <> Prelude.replicate n false
  | n == 0 = xs
  | n >= (-len) = toArray $ Prelude.replicate (-n) false <> Prelude.take (len + n) (fromArray xs)
  | otherwise = toArray $ Prelude.replicate len false
  where
    len = length xs

shiftL :: Int -> Arr Boolean -> Arr Boolean
shiftL = shift

shiftR :: Int -> Arr Boolean -> Arr Boolean
shiftR = shift . negate

or :: Arr Boolean -> Arr Boolean -> Arr Boolean
or = bitOp Or

and :: Arr Boolean -> Arr Boolean -> Arr Boolean
and = bitOp And

xor :: Arr Boolean -> Arr Boolean -> Arr Boolean
xor = bitOp Xor

bitOp :: (Boolean -> Boolean -> Boolean) -> Arr Boolean -> Arr Boolean -> Arr Boolean
bitOp op as bs = toArray $ zipWith op (fromArray as) (fromArray bs)

-- | length xs <
cast :: Int -> Arr Boolean -> Arr Boolean
cast n xs = concatenate xs (replicate (n - length xs) false)

chunks :: Int -> Arr a -> Arr (Arr a)
chunks n = toArray . Prelude.map toArray . group n . fromArray

chunkReverse :: Int -> Arr a -> Arr a
chunkReverse n = toArray . concatMap Prelude.reverse . group n . fromArray

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
      xor <- reuse $ a `Xor` b
      carry' <- reuse carry
      let value = xor `Xor` carry'
      let nextCarry = (xor `And` carry') `Or` (a `And` b)
      return (acc ++ [value], nextCarry)

-- | "T" for top-level
fullAdderT :: Int -> Comp (Arr Boolean)
fullAdderT width = do
  xs <- inputs width
  ys <- inputs width
  fullAdder xs ys