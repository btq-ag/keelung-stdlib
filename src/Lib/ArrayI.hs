{-# LANGUAGE DataKinds #-}

module Lib.ArrayI where

import Control.Monad
import Data.Bifunctor
import Keelung hiding (access, update)
import Prelude hiding (replicate)
import qualified Prelude

-- | See if 2 bit arrays are equal.
beq :: Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val 'Bool n)
beq as bs =
  Control.Monad.foldM
    ( \acc i -> do
        a <- access as i
        b <- access bs i
        return (acc `And` (a `BEq` b))
    )
    true
    [0 .. lengthOf as - 1]

foldM ::
  (Referable a) =>
  (Val b n -> Val a n -> Comp n (Val b n)) ->
  Val b n ->
  Val ('Arr a) n ->
  Comp n (Val b n)
foldM f y xs = fromArray xs >>= Control.Monad.foldM f y

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

take :: Referable a => Int -> Val ('Arr a) n -> Comp n (Val ('Arr a) n)
take n xs = toArrayI . Prelude.take n <$> fromArray xs

drop :: Referable a => Int -> Val ('Arr a) n -> Comp n (Val ('Arr a) n)
drop n xs = toArrayI . Prelude.drop n <$> fromArray xs

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
      let l = lengthOf xs in
      if n > 0
        then Prelude.map (const false) (take n xs') <> Prelude.take (l- n) xs'
        else Prelude.drop (-n) xs' <> Prelude.map (const false) (take (-n) xs')

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

cast :: Int -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
cast n xs = cast' n <$> fromArray xs

-- | length xs < n
cast' :: Int -> [Val 'Bool n] -> Val ('Arr 'Bool) n
cast' n xs = toArrayI $ xs ++ Prelude.replicate (n - length xs) false

chunks :: Referable t => Int -> Val ('Arr t) n -> Comp n (Val ('Arr ('Arr t)) n)
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

update' :: Referable t => Int -> (Val t n -> Comp n (Val t n)) -> Val ('Arr t) n -> Comp n (Val ('Arr t) n)
update' i op xs = access xs i >>= op >>= \x -> update i x xs

--------------------------------------------------------------------------------

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
    | n > 0 = take n l : group n (drop n l)
    | otherwise = error "Negative or zero n"

fullAdder1bit :: Val 'Bool n -> Val 'Bool n -> Val 'Bool n -> (Val 'Bool n, Val 'Bool n)
fullAdder1bit a b carry =
  let value = a `Xor` b `Xor` carry
      nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)
  in (value, nextCarry)

fullAdder :: Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
fullAdder as bs = do
  as' <- fromArray as
  bs' <- fromArray bs
  return . toArrayI . fst $
    foldl
      ( \(result, carry) (a, b) -> do
          first (: result) $ fullAdder1bit a b carry
      )
      ([], false)
      (zip as' bs')
