{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Lib.Array
  ( beq,
    Lib.Array.map,
    concatenate,
    cons,
    singleton,
    Lib.Array.reverse,
    Lib.Array.replicate,
    zeroBits,
    rotateL,
    rotateR,
    shiftL,
    shiftR,
    Lib.Array.or,
    Lib.Array.and,
    Lib.Array.xor,
    flatten,
    cast,
    chunks,
  )
where

import Control.Monad
import Numeric.Natural
import Keelung

-- | See if 2 bit arrays of length `width` are equal.
beq :: Int -> Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val 'Bool n)
beq width as bs =
  foldM
    ( \acc i -> do
        a <- access as i
        b <- access bs i
        return (acc `And` (a `BEq` b))
    )
    true
    [0 .. width - 1]

-- | `map` for Keelung arrays
map :: (Referable a, Referable b) => (Val a n -> Val b n) -> Val ('Arr a) n -> Comp n (Val ('Arr b) n)
map f xs = do
  xs' <- fromArray xs
  toArray (Prelude.map f xs')

-- | Array concatenation
concatenate :: Referable a => Val ('Arr a) n -> Val ('Arr a) n -> Comp n (Val ('Arr a) n)
concatenate xs ys = do
  xs' <- fromArray xs
  ys' <- fromArray ys
  toArray (xs' <> ys')

cons :: Referable a => Val a n -> Val ('Arr a) n -> Comp n (Val ('Arr a) n)
cons x xs = do
  xs' <- fromArray xs
  toArray (x : xs')

singleton :: Referable a => Val a n -> Comp n (Val ('Arr a) n)
singleton x = toArray [x]

reverse :: Referable a => Val ('Arr a) n -> Comp n (Val ('Arr a) n)
reverse xs = do
  xs' <- fromArray xs
  toArray (Prelude.reverse xs')

replicate :: Referable t => Int -> Val t n -> Comp n (Val ('Arr t) n)
replicate n x = toArray $ Prelude.replicate n x

zeroBits :: Int -> Comp n (Val ('Arr 'Bool) n)
zeroBits n = Lib.Array.replicate n false

-- | Rotate left by i bits if i is positive, or right by -i bits otherwise
rotate :: Int -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
rotate n xs = do
  xs' <- fromArray xs
  let l = length xs'
  result <- Lib.Array.replicate l false
  forM_ (zip [0 .. l - 1] xs')$ \(i, x) -> do
    let i' = (i + n) `mod` l
    update result i' x
  return result

rotateL :: Natural -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
rotateL = rotate . fromIntegral

rotateR :: Natural -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
rotateR = rotate . negate . fromIntegral

-- | Shift left by i bits if i is positive, or right by -i bits otherwise
shift :: Int -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
shift n xs = do
  xs' <- fromArray xs
  let l = length xs'
  result <- Lib.Array.replicate l false
  let rng =
        if n >= 0
          then [0 .. n - 1]
          else [-n .. l - 1]
  forM_ (zip rng xs') $ \(i, x) -> update result (i + n) x
  return result

shiftL :: Natural -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
shiftL = shift . fromIntegral

shiftR :: Natural -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
shiftR = shift . negate . fromIntegral

or :: Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
or = bitOp Or

and :: Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
and = bitOp And

xor :: Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
xor = bitOp Xor

bitOp :: (Val 'Bool n -> Val 'Bool n -> Val 'Bool n) -> Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
bitOp op as bs = do
  as' <- fromArray as
  bs' <- fromArray bs
  toArray $ zipWith op as' bs'

or' :: Comp n (Val ('Arr 'Bool) n) -> Comp n (Val ('Arr 'Bool) n) -> Comp n (Val ('Arr 'Bool) n)
or' = bitOp' Or

and' :: Comp n (Val ('Arr 'Bool) n) -> Comp n (Val ('Arr 'Bool) n) -> Comp n (Val ('Arr 'Bool) n)
and' = bitOp' And

xor' :: Comp n (Val ('Arr 'Bool) n) -> Comp n (Val ('Arr 'Bool) n) -> Comp n (Val ('Arr 'Bool) n)
xor' = bitOp' Xor

bitOp' :: (Val 'Bool n -> Val 'Bool n -> Val 'Bool n) -> Comp n (Val ('Arr 'Bool) n) -> Comp n (Val ('Arr 'Bool) n) -> Comp n (Val ('Arr 'Bool) n)
bitOp' op as bs = do
  as' <- fromArray =<< as
  bs' <- fromArray =<< bs
  toArray $ zipWith op as' bs'

flatten :: Referable t => Val ('Arr ('Arr t)) n -> Comp n (Val ('Arr t) n)
flatten = fromArray >=> foldM (\ys x -> (ys <>) <$> fromArray x) [] >=> toArray

cast :: Int -> Val ('Arr 'Bool) n -> Comp n (Val (Arr 'Bool) n)
cast n xs = fromArray xs >>= cast' n

-- | length xs < n
cast' :: Int -> [Val 'Bool n] -> Comp n (Val (Arr 'Bool) n)
cast' n xs = toArray $ xs ++ Prelude.replicate (n - length xs) false

chunks :: Int -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr ('Arr 'Bool)) n)
chunks n xs = fromArray xs >>= f
  where
    f :: [Val 'Bool n] -> Comp n (Val ('Arr ('Arr 'Bool)) n)
    f xs =
      let size = length xs
       in if
              | size > n -> do
                let (xs1, xs2) = splitAt n xs
                join $ cons <$> toArray xs1 <*> f xs2
              | size == n -> singleton =<< toArray xs
              | otherwise -> singleton =<< cast' n xs
