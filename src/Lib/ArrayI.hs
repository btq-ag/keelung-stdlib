{-# LANGUAGE DataKinds #-}

module Lib.ArrayI where

import Control.Monad
import Keelung
import qualified Lib.Array as Mutable
import Prelude hiding (replicate)
import qualified Prelude

-- | See if 2 bit arrays are equal.
beq :: Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val 'Bool n)
beq as bs =
  foldM
    ( \acc i -> do
        a <- Keelung.access as i
        b <- Keelung.access bs i
        return (acc `And` (a `BEq` b))
    )
    true
    [0 .. lengthOf as - 1]

-- | `map` for Keelung arrays
map :: (Referable a, Referable b) => (Val a n -> Val b n) -> Val ('Arr a) n -> Comp n (Val ('Arr b) n)
map f xs = do
  xs' <- fromArray xs
  return $ toArrayI (Prelude.map f xs')

-- | Array concatenation
concatenate :: Referable a => Val ('Arr a) n -> Val ('Arr a) n -> Comp n (Val ('Arr a) n)
concatenate xs ys = do
  xs' <- fromArray xs
  ys' <- fromArray ys
  return $ toArrayI (xs' <> ys')

cons :: Referable a => Val a n -> Val ('Arr a) n -> Comp n (Val ('Arr a) n)
cons x xs = do
  xs' <- fromArray xs
  return $ toArrayI (x : xs')

singleton :: Referable a => Val a n -> Val ('Arr a) n
singleton x = toArrayI [x]

reverse :: Referable a => Val ('Arr a) n -> Comp n (Val ('Arr a) n)
reverse xs = do
  xs' <- fromArray xs
  return $ toArrayI (Prelude.reverse xs')

replicate :: Referable t => Int -> Val t n -> Val ('Arr t) n
replicate n x = toArrayI $ Prelude.replicate n x

zeroBits :: Int -> Val ('Arr 'Bool) n
zeroBits n = replicate n false

-- | Rotate left by 'n' bits
rotate :: Int -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
rotate n xs = do
  xs' <- fromArray xs
  let n' = (lengthOf xs - n) `mod` lengthOf xs
  return $
    toArrayI $ Prelude.drop n' xs' <> Prelude.take n' xs'

rotateL :: Int -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
rotateL = rotate

rotateR :: Int -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
rotateR = rotate . negate

-- | Shift left by 'n' bits (false-fill)
shift :: Int -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
shift n xs = do
  xs' <- fromArray xs
  return $
    toArrayI $
      if n > 0
        then Prelude.replicate n false <> Prelude.take (lengthOf xs - n) xs'
        else Prelude.drop (negate n) xs' <> Prelude.replicate (lengthOf xs + n) false

shiftL :: Int -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
shiftL = shift

shiftR :: Int -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
shiftR = shift . negate

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
  return $ toArrayI $ zipWith op as' bs'

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
  return $ toArrayI $ zipWith op as' bs'

flatten :: Referable t => Val ('Arr ('Arr t)) n -> Comp n (Val ('Arr t) n)
flatten xss = do
  xss' <- fromArray xss
  xss'' <- mapM fromArray xss'
  return $ toArrayI $ Prelude.concat xss''

cast :: Int -> Val ('Arr 'Bool) n -> Comp n (Val (Arr 'Bool) n)
cast n xs = cast' n <$> fromArray xs

-- | length xs < n
cast' :: Int -> [Val 'Bool n] -> Val (Arr 'Bool) n
cast' n xs = toArrayI $ xs ++ Prelude.replicate (n - length xs) false

chunks :: Int -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr ('Arr 'Bool)) n)
chunks n xs = do
  xs' <- fromArray xs
  return $ toArrayI $ fmap toArrayI (group n xs')

chunkReverse :: Referable t => Int -> Val ('Arr t) n -> Comp n (Val ('Arr t) n)
chunkReverse n = fmap (toArrayI . concatMap Prelude.reverse . group n) . fromArray

update :: Referable t => Int -> Val t n -> Val ('Arr t) n -> Comp n (Val ('Arr t) n)
update i x xs = do
    xs' <- fromArray xs
    return $ toArrayI $ take i xs' <> (x : drop (i + 1) xs')

access :: Referable t => Val ('Arr t) n -> Int -> Comp n (Val t n)
access xs i = do
    xs' <- fromArray xs
    return $ xs' !! i

--------------------------------------------------------------------------------

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
    | n > 0 = take n l : group n (drop n l)
    | otherwise = error "Negative or zero n"