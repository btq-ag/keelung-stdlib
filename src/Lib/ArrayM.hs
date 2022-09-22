{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

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
    chunkReverse
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

mapM :: (Mutable a, Mutable b) => (Val a -> Comp (Val b)) -> Val ('ArrM a) -> Comp (Val ('ArrM b))
mapM f xs = do
    xs' <- fromArrayM xs
    toArrayM =<< Prelude.mapM f xs'

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
zeroBits n = Lib.ArrayM.replicate n false

-- | Rotate left by i bits if i is positive, or right by -i bits otherwise
rotate :: Int -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
rotate n xs = do
  xs' <- fromArrayM xs
  let l = length xs'
  result <- Lib.ArrayM.replicate l false
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
  toArrayM $
    if n > 0
        then Prelude.replicate n false <> Prelude.take (lengthOfM xs - n) xs'
        else Prelude.drop (negate n) xs' <> Prelude.replicate (lengthOfM xs + n) false

shiftL :: Natural -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
shiftL = shift . fromIntegral

shiftR :: Natural -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
shiftR = shift . negate . fromIntegral

not :: Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
not = Lib.ArrayM.map neg

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

flatten :: Mutable t => Val ('ArrM ('ArrM t)) -> Comp (Val ('ArrM t))
flatten = fromArrayM >=> foldM (\ys x -> (ys <>) <$> fromArrayM x) [] >=> toArrayM

cast :: Int -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
cast n xs = fromArrayM xs >>= cast' n

-- | length xs < n
cast' :: Int -> [Val 'Bool] -> Comp (Val ('ArrM 'Bool))
cast' n xs = toArrayM $ xs ++ Prelude.replicate (n - length xs) false

chunks :: Mutable t => Int -> Val ('ArrM t) -> Comp (Val ('ArrM ('ArrM t)))
chunks n xs = do 
    xs' <- group n <$> fromArrayM xs
    toArrayM =<< Prelude.mapM toArrayM xs'

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

chunkReverse :: Mutable t => Int -> Val ('ArrM t) -> Comp (Val ('ArrM ('ArrM t)))
chunkReverse n xs = do
    xs' <- group n <$> fromArrayM xs
    toArrayM =<< Prelude.mapM (toArrayM . Prelude.reverse) xs'

--------------------------------------------------------------------------------

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = Prelude.take n l : group n (Prelude.drop n l)
  | otherwise = error "Negative or zero"
