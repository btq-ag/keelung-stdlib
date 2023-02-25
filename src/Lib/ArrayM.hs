{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib.ArrayM
  ( beq,
    Lib.ArrayM.map,
    Lib.ArrayM.mapM,
    concatenate,
    cons,
    singleton,
    Lib.ArrayM.reverse,
    Lib.ArrayM.replicate,
    Lib.ArrayM.take,
    Lib.ArrayM.drop,
    zeroBits,
    rotateL,
    rotateR,
    shiftL,
    shiftR,
    Lib.ArrayM.not,
    Lib.ArrayM.or,
    Lib.ArrayM.and,
    Lib.ArrayM.xor,
    flatten,
    cast,
    chunks,
    fullAdder,
    fullAdderSlow,
    chunkReverse,
    fullAdderT
  )
where

import Control.Monad
import Keelung hiding (rotate, shift, shiftL, shiftR)
import Numeric.Natural

-- | See if 2 bit arrays of length `width` are equal.
beq :: Int -> ArrM Boolean -> ArrM Boolean -> Comp Boolean
beq width as bs =
  foldM
    ( \acc i -> do
        a <- accessM as i
        b <- accessM bs i
        return (acc `And` (a `Keelung.eq` b))
    )
    true
    [0 .. width - 1]

-- | `map` for Keelung arrays
map :: (Mutable a, Mutable b) => (a -> b) -> ArrM a -> Comp (ArrM b)
map f xs = do
  xs' <- fromArrayM xs
  toArrayM (Prelude.map f xs')

mapM :: (Mutable a, Mutable b) => (a -> Comp b) -> ArrM a -> Comp (ArrM b)
mapM f xs = do
  xs' <- fromArrayM xs
  toArrayM =<< Prelude.mapM f xs'

-- | Array concatenation
concatenate :: Mutable a => ArrM a -> ArrM a -> Comp (ArrM a)
concatenate xs ys = do
  xs' <- fromArrayM xs
  ys' <- fromArrayM ys
  toArrayM (xs' <> ys')

cons :: Mutable a => a -> ArrM a -> Comp (ArrM a)
cons x xs = do
  xs' <- fromArrayM xs
  toArrayM (x : xs')

singleton :: Mutable a => a -> Comp (ArrM a)
singleton x = toArrayM [x]

reverse :: Mutable a => ArrM a -> Comp (ArrM a)
reverse xs = do
  xs' <- fromArrayM xs
  toArrayM (Prelude.reverse xs')

replicate :: Mutable a => Int -> a -> Comp (ArrM a)
replicate n x = toArrayM $ Prelude.replicate n x

take :: Mutable a => Int -> ArrM a -> Comp (ArrM a)
take n xs = do
  xs' <- fromArrayM xs
  toArrayM (Prelude.take n xs')

drop :: Mutable a => Int -> ArrM a -> Comp (ArrM a)
drop n xs = do
  xs' <- fromArrayM xs
  toArrayM (Prelude.drop n xs')

zeroBits :: Int -> Comp (ArrM Boolean)
zeroBits n = Lib.ArrayM.replicate n false

-- | Rotate left by i bits if i is positive, or right by -i bits otherwise
rotate :: Int -> ArrM Boolean -> Comp (ArrM Boolean)
rotate n xs = do
  xs' <- fromArrayM xs
  let l = length xs'
  result <- Lib.ArrayM.replicate l false
  forM_ (zip [0 .. l - 1] xs') $ \(i, x) -> do
    let i' = (i + n) `mod` l
    updateM result i' x
  return result

rotateL :: Natural -> ArrM Boolean -> Comp (ArrM Boolean)
rotateL = rotate . fromIntegral

rotateR :: Natural -> ArrM Boolean -> Comp (ArrM Boolean)
rotateR = rotate . negate . fromIntegral

-- | Shift left by i bits if i is positive, or right by -i bits otherwise
shift :: Int -> ArrM Boolean -> Comp (ArrM Boolean)
shift n xs = do
  xs' <- fromArrayM xs
  toArrayM $
    if n > 0
      then Prelude.replicate n false <> Prelude.take (lengthOf xs - n) xs'
      else Prelude.drop (negate n) xs' <> Prelude.replicate (lengthOf xs + n) false

shiftL :: Natural -> ArrM Boolean -> Comp (ArrM Boolean)
shiftL = shift . fromIntegral

shiftR :: Natural -> ArrM Boolean -> Comp (ArrM Boolean)
shiftR = shift . negate . fromIntegral

not :: ArrM Boolean -> Comp (ArrM Boolean)
not = Lib.ArrayM.map complement

or :: ArrM Boolean -> ArrM Boolean -> Comp (ArrM Boolean)
or = bitOp Or

and :: ArrM Boolean -> ArrM Boolean -> Comp (ArrM Boolean)
and = bitOp And

xor :: ArrM Boolean -> ArrM Boolean -> Comp (ArrM Boolean)
xor = bitOp Xor

bitOp :: (Boolean -> Boolean -> Boolean) -> ArrM Boolean -> ArrM Boolean -> Comp (ArrM Boolean)
bitOp op as bs = do
  as' <- fromArrayM as
  bs' <- fromArrayM bs
  toArrayM $ zipWith op as' bs'

flatten :: Mutable a => ArrM (ArrM a) -> Comp (ArrM a)
flatten = fromArrayM >=> foldM (\ys x -> (ys <>) <$> fromArrayM x) [] >=> toArrayM

cast :: Int -> ArrM Boolean -> Comp (ArrM Boolean)
cast n xs = fromArrayM xs >>= cast' n

-- | length xs < n
cast' :: Int -> [Boolean] -> Comp (ArrM Boolean)
cast' n xs = toArrayM $ xs ++ Prelude.replicate (n - length xs) false

chunks :: Mutable t => Int -> ArrM t -> Comp (ArrM (ArrM t))
chunks n xs = do
  xs' <- group n <$> fromArrayM xs
  toArrayM =<< Prelude.mapM toArrayM xs'

--------------------------------------------------------------------------------

-- | Full adder without sharing
fullAdderSlow :: ArrM Boolean -> ArrM Boolean -> Comp (ArrM Boolean)
fullAdderSlow as bs = do
  let width = lengthOf as
  -- allocate a new array of 64 bits for the result of the addition
  result <- zeroBits width
  -- 1-bit full adder
  foldM_
    ( \carry i -> do
        a <- accessM as i
        b <- accessM bs i
        let value = a `Xor` b `Xor` carry
        let nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)
        updateM result i value
        return nextCarry
    )
    false
    [0 .. width - 1]
  return result

fullAdder :: ArrM Boolean -> ArrM Boolean -> Comp (ArrM Boolean)
fullAdder as bs = do
  let width = lengthOf as
  -- allocate an array for storing the result
  result <- zeroBits width
  foldM_
    ( \carry i -> do
        -- read out the bits at position i
        a <- accessM as i
        b <- accessM bs i
        -- store `a Xor b` and `carry` for later use
        xor <- reuse (a `Xor` b)
        carry' <- reuse carry
        -- compose the new value and carry
        let value = xor `Xor` carry'
        let nextCarry = (xor `And` carry') `Or` (a `And` b)
        -- write it back to the result array
        updateM result i value
        -- return the new carry
        return nextCarry
    )
    false
    [0 .. width - 1]
  return result

chunkReverse :: Mutable t => Int -> ArrM t -> Comp (ArrM (ArrM t))
chunkReverse n xs = do
  xs' <- group n <$> fromArrayM xs
  toArrayM =<< Prelude.mapM (toArrayM . Prelude.reverse) xs'

--------------------------------------------------------------------------------

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = Prelude.take n l : group n (Prelude.drop n l)
  | otherwise = error "Negative or zero"

-- | "T" for top-level
fullAdderT :: Int -> Comp (ArrM Boolean)
fullAdderT width = do
  xs <- inputList Private width >>= thaw
  ys <- inputList Private width >>= thaw
  fullAdder xs ys
