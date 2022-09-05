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
    Lib.Array.take,
    Lib.Array.drop,
    zeroBits,
    rotateL,
    rotateR,
    shiftL,
    shiftR,
    Lib.Array.or,
    Lib.Array.and,
    Lib.Array.xor,
    Lib.Array.xorOld,
    flatten,
    cast,
    chunks,
    fullAdder
  )
where

import Control.Monad
import Numeric.Natural
import Keelung

-- | See if 2 bit arrays of length `width` are equal.
beq :: Int -> Val ('ArrM 'Bool) -> Val ('ArrM 'Bool) -> Comp (Val 'Bool)
beq width as bs =
  foldM
    ( \acc i -> do
        a <- accessM as i
        b <- accessM bs i
        return (acc `And` (a `BEq` b))
    )
    true
    [0 .. width - 1]

-- | `map` for Keelung arrays
map :: (Mutable a, Mutable b) => (Val a -> Val b) -> Val ('ArrM a) -> Comp (Val ('ArrM b))
map f xs = do
  xs' <- fromArrayM xs
  toArrayM (Prelude.map f xs')

-- | Array concatenation
concatenate :: Mutable a => Val ('ArrM a) -> Val ('ArrM a) -> Comp (Val ('ArrM a))
concatenate xs ys = do
  xs' <- fromArrayM xs
  ys' <- fromArrayM ys
  toArrayM (xs' <> ys')

cons :: Mutable a => Val a -> Val ('ArrM a) -> Comp (Val ('ArrM a))
cons x xs = do
  xs' <- fromArrayM xs
  toArrayM (x : xs')

singleton :: Mutable a => Val a -> Comp (Val ('ArrM a))
singleton x = toArrayM [x]

reverse :: Mutable a => Val ('ArrM a) -> Comp (Val ('ArrM a))
reverse xs = do
  xs' <- fromArrayM xs
  toArrayM (Prelude.reverse xs')

replicate :: Mutable t => Int -> Val t -> Comp (Val ('ArrM t))
replicate n x = toArrayM $ Prelude.replicate n x

take :: Mutable a => Int -> Val ('ArrM a) -> Comp (Val ('ArrM a))
take n xs = do
  xs' <- fromArrayM xs
  toArrayM (Prelude.take n xs')

drop :: Mutable a => Int -> Val ('ArrM a) -> Comp (Val ('ArrM a))
drop n xs = do
  xs' <- fromArrayM xs
  toArrayM (Prelude.drop n xs')


zeroBits :: Int -> Comp (Val ('ArrM 'Bool))
zeroBits n = Lib.Array.replicate n false

-- | Rotate left by i bits if i is positive, or right by -i bits otherwise
rotate :: Int -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
rotate n xs = do
  xs' <- fromArrayM xs
  let l = length xs'
  result <- Lib.Array.replicate l false
  forM_ (zip [0 .. l - 1] xs')$ \(i, x) -> do
    let i' = (i + n) `mod` l
    updateM result i' x
  return result

rotateL :: Natural -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
rotateL = rotate . fromIntegral

rotateR :: Natural -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
rotateR = rotate . negate . fromIntegral

-- | Shift left by i bits if i is positive, or right by -i bits otherwise
shift :: Int -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
shift n xs = do
  xs' <- fromArrayM xs
  let l = length xs'
  result <- Lib.Array.replicate l false
  let rng =
        if n >= 0
          then [0 .. n - 1]
          else [-n .. l - 1]
  forM_ (zip rng xs') $ \(i, x) -> updateM result (i + n) x
  return result

shiftL :: Natural -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
shiftL = shift . fromIntegral

shiftR :: Natural -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
shiftR = shift . negate . fromIntegral

or :: Val ('ArrM 'Bool) -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
or = bitOp Or

and :: Val ('ArrM 'Bool) -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
and = bitOp And

xor :: Val ('ArrM 'Bool) -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
xor = bitOp Xor

bitOp :: (Val 'Bool -> Val 'Bool -> Val 'Bool) -> Val ('ArrM 'Bool) -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
bitOp op as bs = do
  as' <- fromArrayM as
  bs' <- fromArrayM bs
  toArrayM $ zipWith op as' bs'

or' :: Comp (Val ('ArrM 'Bool)) -> Comp (Val ('ArrM 'Bool)) -> Comp (Val ('ArrM 'Bool))
or' = bitOp' Or

and' :: Comp (Val ('ArrM 'Bool)) -> Comp (Val ('ArrM 'Bool)) -> Comp (Val ('ArrM 'Bool))
and' = bitOp' And

xor' :: Comp (Val ('ArrM 'Bool)) -> Comp (Val ('ArrM 'Bool)) -> Comp (Val ('ArrM 'Bool))
xor' = bitOp' Xor

bitOp' :: (Val 'Bool -> Val 'Bool -> Val 'Bool) -> Comp (Val ('ArrM 'Bool)) -> Comp (Val ('ArrM 'Bool)) -> Comp (Val ('ArrM 'Bool))
bitOp' op as bs = do
  as' <- fromArrayM =<< as
  bs' <- fromArrayM =<< bs
  toArrayM $ zipWith op as' bs'

flatten :: Mutable t => Val ('ArrM ('ArrM t)) -> Comp (Val ('ArrM t))
flatten = fromArrayM >=> foldM (\ys x -> (ys <>) <$> fromArrayM x) [] >=> toArrayM

cast :: Int -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
cast n xs = fromArrayM xs >>= cast' n

-- | length xs < n
cast' :: Int -> [Val 'Bool] -> Comp (Val ('ArrM 'Bool))
cast' n xs = toArrayM $ xs ++ Prelude.replicate (n - length xs) false

chunks :: Int -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM ('ArrM 'Bool)))
chunks n xs = fromArrayM xs >>= f
  where
    f :: [Val 'Bool] -> Comp (Val ('ArrM ('ArrM 'Bool)))
    f xs =
      let size = length xs
       in if
              | size > n -> do
                let (xs1, xs2) = splitAt n xs
                join $ cons <$> toArrayM xs1 <*> f xs2
              | size == n -> singleton =<< toArrayM xs
              | otherwise -> singleton =<< cast' n xs


--------------------------------------------------------------------------------

fullAdder1bit :: Val 'Bool -> Val 'Bool -> Val 'Bool -> (Val 'Bool, Val 'Bool)
fullAdder1bit a b carry =
  let value = a `Xor` b `Xor` carry
      nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)
   in (value, nextCarry)

fullAdder :: Int -> Val ('ArrM 'Bool) -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
fullAdder width as bs = do
  -- allocate a new array of 64 bits for the result of the addition
  result <- zeroBits width
  -- 1-bit full adder
  foldM_
    ( \carry i -> do
        a <- accessM as i
        b <- accessM bs i
        let (value, nextCarry) = fullAdder1bit a b carry
        updateM result i value
        return nextCarry
    )
    false
    [0 .. width - 1]
  return result

-- testFullAdder :: Int -> Comp (Val 'Unit)
-- testFullAdder width = do
--   as <- inputs width
--   bs <- inputs width
--   cs <- inputs width
--   cs' <- fullAdder width as bs
--
--   beq width cs cs' >>= assert
--
--   return unit
--
-----------

xorOld :: Int -> Val ('ArrM 'Bool) -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
xorOld = bitOpOld Xor

bitOpOld :: (Val 'Bool -> Val 'Bool -> Val 'Bool) -> Int -> Val ('ArrM 'Bool) -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
bitOpOld op l as bs = do
  bits <- forM [0 .. (l - 1)] $ \i -> do
    a <- accessM as i
    b <- accessM bs i
    return (a `op` b)
  toArrayM bits
