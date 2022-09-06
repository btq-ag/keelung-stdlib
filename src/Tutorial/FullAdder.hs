{-# LANGUAGE DataKinds #-}

module Tutorial.FullAdder where

import Control.Monad (foldM_, forM_)
import Keelung

--------------------------------------------------------------------------------

halfAdder :: Val 'Bool -> Val 'Bool -> (Val 'Bool, Val 'Bool)
halfAdder a b = (a `Xor` b, a `And` b)

testHalfAdder :: Comp (Val ('Arr 'Bool))
testHalfAdder = do
  a <- input
  b <- input
  let (value, carry) = halfAdder a b
  return $ toArray [value, carry]

--------------------------------------------------------------------------------

fullAdder :: Val 'Bool -> Val 'Bool -> Val 'Bool -> (Val 'Bool, Val 'Bool)
fullAdder a b carry = (value, nextCarry)
  where
    value = a `Xor` b `Xor` carry
    nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)

testFullAdder :: Comp (Val ('Arr 'Bool))
testFullAdder = do
  a <- input
  b <- input
  c <- input
  let (value, carry) = fullAdder a b c
  return $ toArray [value, carry]

------------------------------------------------------------------------------

fullAdderN :: Int -> Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
fullAdderN width as bs =
  let zipped = zip (fromArray as) (fromArray bs)
   in toArray $ fst $ foldl f ([], false) zipped
  where
    f :: ([Val 'Bool], Val 'Bool) -> (Val 'Bool, Val 'Bool) -> ([Val 'Bool], Val 'Bool)
    f (acc, carry) (a, b) =
      let (value', carry') = fullAdder a b carry
       in (acc ++ [value'], carry')

-- -- allocate a new array of `width` bits for the result of the addition
-- let result = toArray (replicate width false)
-- -- 1-bit full adder
-- foldM_
--   ( \carry i -> do
--       a <- access as i
--       b <- access bs i
--       let (value, nextCarry) = fullAdder a b carry
--       update result i value
--       return nextCarry
--   )
--   false
--   [0 .. width - 1]

-- return result

testFullAdderN :: Int -> Comp (Val ('Arr 'Bool))
testFullAdderN width = do
  xs <- inputs width
  ys <- inputs width
  return $ fullAdderN width xs ys
