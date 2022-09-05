{-# LANGUAGE DataKinds #-}

module Practice where

import Control.Monad
import Data.Field.Galois (GaloisField)
import Keelung

-- Given a number `n` and an array of numbers `xs`
--  return the sum of the first `n` numbers in `xs`
--  Input:
--      1. n  : 'Num      (0 <= n <= 4)
--      2. xs : 'Arr 'Num (of length 4)
--  Output:
--      1. 'Num
problem1 :: Comp (Val 'Num)
problem1 = undefined

answer1 :: Comp (Val 'Num)
answer1 = do
  n <- input
  xs <- inputs 4

  x0 <- accessM xs 0
  x1 <- accessM xs 1
  x2 <- accessM xs 2
  x3 <- accessM xs 3

  return $
    cond
      (n `Eq` 0)
      0
      $ cond
        (n `Eq` 1)
        x0
        $ cond
          (n `Eq` 2)
          (x0 + x1)
          $ cond
            (n `Eq` 3)
            (x0 + x1 + x2)
            (x0 + x1 + x2 + x3)
