{-# LANGUAGE DataKinds #-}

module Tutorial.FullAdder where

import Control.Monad (foldM_, forM_)
import Keelung

--------------------------------------------------------------------------------

halfAdder :: Val 'Bool n -> Val 'Bool n -> (Val 'Bool n, Val 'Bool n)
halfAdder a b = (a `Xor` b, a `And` b)

testHalfAdder :: Comp GF181 (Val 'Unit GF181)
testHalfAdder = do
  a <- input
  b <- input
  let (value, carry) = halfAdder a b

  value' <- input
  carry' <- input

  assert (value `BEq` value')
  assert (carry `BEq` carry')

  return unit

--------------------------------------------------------------------------------

fullAdder :: Val 'Bool n -> Val 'Bool n -> Val 'Bool n -> (Val 'Bool n, Val 'Bool n)
fullAdder a b carry = (value, nextCarry)
  where
    value = a `Xor` b `Xor` carry
    nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)

testFullAdder :: Comp GF181 (Val 'Unit GF181)
testFullAdder = do
  a <- input
  b <- input
  c <- input

  let (value, carry) = fullAdder a b c
  value' <- input
  carry' <- input

  assert (value `BEq` value')
  assert (carry `BEq` carry')

  return unit

------------------------------------------------------------------------------

fullAdderN :: Int -> Val ('Arr 'Bool) n -> Val ('Arr 'Bool) n -> Comp n (Val ('Arr 'Bool) n)
fullAdderN width as bs = do
  -- allocate a new array of `width` bits for the result of the addition
  result <- toArray (replicate width false)
  -- 1-bit full adder
  foldM_
    ( \carry i -> do
        a <- access as i
        b <- access bs i
        let (value, nextCarry) = fullAdder a b carry
        update result i value
        return nextCarry
    )
    false
    [0 .. width - 1]

  return result

testFullAdderN :: Int -> Comp GF181 (Val 'Unit GF181)
testFullAdderN width = do
  xs <- inputs width
  ys <- inputs width
  zs <- inputs width
  zs' <- fullAdderN width xs ys

  forM_ [0 .. width - 1] $ \i -> do
    z <- access zs i
    z' <- access zs' i
    assert (z `BEq` z')

  return unit