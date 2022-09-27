{-# LANGUAGE DataKinds #-}

module Practice where

import Keelung

-- Given a number `n` and an array of numbers `xs`
--  return the sum of the first `n` numbers in `xs`
--  Input:
--      1. n  : 'Num      (0 <= n <= 4)
--      2. xs : 'Arr 'Num (of length 4)
--  Output:
--      1. 'Num
problem1 :: Comp Number
problem1 = undefined

answer1 :: Comp Number
answer1 = do
  n <- input
  xs <- inputs 4

  let x0 = access xs 0
  let x1 = access xs 1
  let x2 = access xs 2
  let x3 = access xs 3

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
