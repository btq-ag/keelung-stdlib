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
    rotate,
    shift,
    Lib.Array.or,
    Lib.Array.and,
    Lib.Array.xor,
    flatten,
    cast,
    chunks,
  )
where

import Control.Monad
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
rotate :: Int -> Val ('Arr 'Bool) n -> Int -> Comp n (Val ('Arr 'Bool) n)
rotate n xs l = do
  result <- Lib.Array.replicate l false
  forM_ [0 .. (l - 1)] $ \i -> do
    x <- access xs i
    let i' = (i - n) `mod` l
    update result i' x
  return result

-- | Shift left by i bits if i is positive, or right by -i bits otherwise
shift :: Int -> Int -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
shift n l xs = do
  result <- Lib.Array.replicate l false
  let rng =
        if n >= 0
          then [n .. (l - 1)]
          else [0 .. (l - 1 + n)]
  forM_ rng $ \i -> access xs i >>= update result (i - n)
  return result

or :: Int -> Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
or = bitOp Or

and :: Int -> Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
and = bitOp And

xor :: Int -> Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
xor = bitOp Xor

bitOp :: (Val 'Bool n -> Val 'Bool n -> Val 'Bool n) -> Int -> Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
bitOp op l as bs = do
  bits <- forM [0 .. (l - 1)] $ \i -> do
    a <- access as i
    b <- access bs i
    return (a `op` b)
  toArray bits

flatten :: Referable t => Val ('Arr ('Arr t)) n -> Comp n (Val ('Arr t) n)
flatten = fromArray >=> foldM (\ys x -> (ys <>) <$> fromArray x) [] >=> toArray

cast :: Int -> Val ('Arr 'Bool) n -> Comp n (Val (Arr 'Bool) n)
cast n xs = fromArray xs >>= cast' n

cast' :: Int -> [Val 'Bool n] -> Comp n (Val (Arr 'Bool) n)
cast' n xs = do
  result <- zeroBits n
  forM_ (zip [n - length xs .. n - 1] xs) $ \(i, x) -> do
    update result i x
  return result

chunks :: Int -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr ('Arr 'Bool)) n)
chunks n xs = fromArray xs >>= f
  where
    f :: [Val 'Bool n] -> Comp n (Val ('Arr ('Arr 'Bool)) n)
    f xs =
      let size = length xs
       in if
              | size > n -> do
                let (xs1, xs2) = splitAt (size - n) xs
                xs' <- join $ cons <$> toArray xs2 <*> f xs1
                Lib.Array.reverse xs'
              | size == n -> singleton =<< toArray xs
              | otherwise -> singleton =<< cast' n xs
