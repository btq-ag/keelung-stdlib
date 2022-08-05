{-# LANGUAGE DataKinds #-}

module Lib.Array where

import Control.Monad
import Keelung

-- | See if 2 bit arrays of length `width` are equal.
beq :: Int -> Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val 'Bool n)
beq width as bs =
  foldM
    ( \acc i -> do
        a <- access as i
        b <- access bs i
        return $ acc `And` (a `BEq` b)
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