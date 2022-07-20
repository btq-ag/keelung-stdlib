{-# LANGUAGE DataKinds #-}

module W64 where 

import Keelung
import Control.Monad

type W64 = 'Arr 'Bool


-- | Rotates left by i bits if i is positive, or right by -i bits otherwise.
rotate :: Int -> Expr W64 n -> Comp n (Expr W64 n)
rotate n xs = do
  result <- toArray (replicate 64 false)
  forM_ [0 .. 63] $ \i -> do
    x <- access xs i
    let i' = (i - n) `mod` 64
    update result i' x
  return result


add :: Expr W64 n -> Expr W64 n -> Comp n (Expr W64 n)
add = fullAdder 64

xor :: Expr W64 n -> Expr W64 n -> Comp n (Expr W64 n)
xor as bs = do
  bits <- forM [0 .. 63] $ \i -> do
    a <- access as i
    b <- access bs i
    return (a `Xor` b)
  toArray bits

copyTo :: Expr W64 n -> Expr W64 n -> Comp n ()
copyTo src tgt = forM_ [0 .. 63] $ \i -> do
  srcBit <- access src i
  tgtBit <- access tgt i
  update tgt i srcBit

--------------------------------------------------------------------------------

fullAdder1bit :: Expr 'Bool n -> Expr 'Bool n -> Expr 'Bool n -> (Expr 'Bool n, Expr 'Bool n)
fullAdder1bit a b carry =
  let value = a `Xor` b `Xor` carry
      nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)
   in (value, nextCarry)

fullAdder :: Int -> Expr ('Arr 'Bool) n -> Expr ('Arr 'Bool) n -> Comp n (Expr ('Arr 'Bool) n)
fullAdder width as bs = do
  -- allocate a new array of 64 bits for the result of the addition
  result <- toArray (replicate width false)
  -- 1-bit full adder
  foldM_
    ( \carry i -> do
        a <- access as i
        b <- access bs i
        let (value, nextCarry) = fullAdder1bit a b carry
        update result i value
        return nextCarry
    )
    false
    [0 .. width - 1]
  return result

testFullAdder :: Int -> Comp GF181 (Expr 'Unit GF181)
testFullAdder width = do
  as <- inputs width
  bs <- inputs width
  cs <- inputs width
  cs' <- fullAdder width as bs

  forM_ [0 .. width - 1] $ \i -> do
    c <- access cs i
    c' <- access cs' i
    assert (c' `BEq` c)

  return unit
