{-# LANGUAGE DataKinds #-}

module Tutorial.FullAdder where

import Control.Monad (foldM_, forM_)
import Keelung

--------------------------------------------------------------------------------

halfAdder :: Val 'Bool -> Val 'Bool -> (Val 'Bool, Val 'Bool)
halfAdder a b = (a `Xor` b, a `And` b)

testHalfAdder :: Comp (Val 'Unit)
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

fullAdder :: Val 'Bool -> Val 'Bool -> Val 'Bool -> (Val 'Bool, Val 'Bool)
fullAdder a b carry = (value, nextCarry)
  where
    value = a `Xor` b `Xor` carry
    nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)

testFullAdder :: Comp (Val 'Unit)
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

fullAdderN :: Int -> Val ('ArrM 'Bool) -> Val ('ArrM 'Bool) -> Comp (Val ('ArrM 'Bool))
fullAdderN width as bs = do
  -- allocate a new array of `width` bits for the result of the addition
  result <- toArrayM (replicate width false)
  -- 1-bit full adder
  foldM_
    ( \carry i -> do
        a <- accessM as i
        b <- accessM bs i
        let (value, nextCarry) = fullAdder a b carry
        updateM result i value
        return nextCarry
    )
    false
    [0 .. width - 1]

  return result

-- testFullAdderN :: Int -> Comp (Val 'Unit)
-- testFullAdderN width = do
--   xs <- inputs width
--   ys <- inputs width
--   zs <- inputs width
--   zs' <- fullAdderN width xs ys
--
--   forM_ [0 .. width - 1] $ \i -> do
--     z <- accessM zs i
--     z' <- accessM zs' i
--     assert (z `BEq` z')
--
--   return unit
