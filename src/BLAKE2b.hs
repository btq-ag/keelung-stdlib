{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module BLAKE2b where

import Control.Monad
import Data.Bits
import Data.Int
import Data.WideWord (Int128)
import qualified Data.WideWord.Int128 as Int128
import Data.Word
import Keelung

-- | Initialization vector
iv :: [Word64]
iv =
  [ 0x6a09e667f3bcc908, -- Frac(sqrt(2))
    0xbb67ae8584caa73b, -- Frac(sqrt(3))
    0x3c6ef372fe94f82b, -- Frac(sqrt(5))
    0xa54ff53a5f1d36f1, -- Frac(sqrt(7))
    0x510e527fade682d1, -- Frac(sqrt(11))
    0x9b05688c2b3e6c1f, -- Frac(sqrt(13))
    0x1f83d9abfb41bd6b, -- Frac(sqrt(17))
    0x5be0cd19137e2179 -- Frac(sqrt(19))
  ]

-- | SIGMA has only 10 distinct entries,
--   the 11th and the 12th are the same as the 1st and the 2nd.
sigma :: [[Int]]
sigma =
  [ [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
    [14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3],
    [11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4],
    [7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8],
    [9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13],
    [2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9],
    [12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11],
    [13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10],
    [6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5],
    [10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0],
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
    [14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3]
  ]

-- hash :: Word64 -> Word64 -> Word64 -> Comp n (Expr 'Bool n)
-- hash msgLen keyLen hashLen = do
--   -- length of the message in bytes
--   msg <- inputArray2 (fromIntegral msgLen) 8
--   -- length of the key in bytes
--   key <- inputArray2 (fromIntegral keyLen) 8

--   -- rub key size and desired hash length into iv0
--   let h0 = iv0 `xor` x0101kknn

--   return true

--   where
--     -- from key size ('kk') and desired hash length ('nn')
--     -- for example, if key size = 17 bytes, desired hash length = 3
--     -- then x0101kknn = 0x010101103
--     x0101kknn = 0x01010000 + (keyLen `shiftL` 8) + hashLen

-- compress ::
--   Expr ('Arr W64) n -> -- 128 bytes of message to compress
--   Int128 -> -- count of bytes that have been compressed before
--   Bool -> -- is this the final round of compression?
--   Expr ('Arr W64) n -> -- 128 bytes of old hash value
--   Comp n ()
-- compress msg count final hash = do
--   -- 16 Word64 as local state
--   vs <- allocArray2 16 64

--   -- First 8 items are copied from old hash
--   forM_ [0 .. 7] $ \j -> do
--     h <- access hash j
--     v <- access vs j
--     copyToW64 h v

--   -- Remaining 8 items are initialized from the IV
--   forM_ [8 .. 15] $ \i -> do
--     ref <- access vs i
--     writeW64 ref (iv !! i)

--   --  Mix the 128-bit counter into V12 & V13
--   v12 <- access vs 12
--   writeW64 v12 (Int128.int128Lo64 count)
--   v13 <- access vs 13
--   writeW64 v13 (Int128.int128Hi64 count)

--   -- If this is the last block then invert all the bits in V14
--   when final $ do
--     v14 <- access vs 14
--     forM_ [0 .. 63] $ \i -> do
--       var <- access v14 i
--       update v14 i (neg (var))

--   -- 12 rounds of cryptographic message mixing
--   forM_ [0 .. 11] $ \i -> do
--     -- Select message mixing schedule for this round.
--     -- BLAKE2b uses 12 rounds, while 'sigma' has only 10 entries.
--     let indices = sigma !! i
--     mix vs 0 4 8 12 msg (indices !! 0) (indices !! 1)
--     mix vs 1 5 9 13 msg (indices !! 2) (indices !! 3)
--     mix vs 2 6 10 14 msg (indices !! 4) (indices !! 5)
--     mix vs 3 7 11 15 msg (indices !! 6) (indices !! 7)
--     mix vs 0 5 10 15 msg (indices !! 8) (indices !! 9)
--     mix vs 1 6 11 12 msg (indices !! 10) (indices !! 11)
--     mix vs 2 7 8 13 msg (indices !! 12) (indices !! 13)
--     mix vs 3 4 9 14 msg (indices !! 14) (indices !! 15)

--   -- Mix the upper and lower halves of `vs` into 'hash'
--   --  h0..7 ← h0..7 xor V0..7
--   forM_ [0 .. 7] $ \i -> do
--     h <- access hash i
--     v <- access vs i
--     x <- h `xorW64` v
--     copyToW64 x h
--   --  h0..7 ← h0..7 xor V8..15
--   forM_ [0 .. 7] $ \i -> do
--     h <- access hash i
--     v <- access vs (i + 8)
--     x <- h `xorW64` v
--     copyToW64 x h

-- mix ::
--   Expr ('Arr W64) n ->
--   Int ->
--   Int ->
--   Int ->
--   Int ->
--   Expr ('Arr W64) n ->
--   Int ->
--   Int ->
--   Comp n ()
-- mix vs ai bi ci di msg xi yi = do
--   a <- access vs ai
--   b <- access vs bi
--   c <- access vs ci
--   d <- access vs di

--   x <- access msg xi
--   y <- access msg yi

--   -- Va ← Va + Vb + x   (with input)
--   addW64 a b >>= addW64 x >>= copyToW64 a
--   -- Vd ← (Vd xor Va) rotateright 32
--   xorW64 d a >>= rotateW64 (-32) >>= copyToW64 d

--   -- Vc ← Vc + Vd       (no input)
--   addW64 c d >>= copyToW64 c
--   -- Vb ← (Vb xor Vc) rotateright 24
--   xorW64 b c >>= rotateW64 (-24) >>= copyToW64 b

--   -- Va ← Va + Vb + y   (with input)
--   addW64 a b >>= addW64 y >>= copyToW64 a
--   -- Vd ← (Vd xor Va) rotateright 16
--   xorW64 d a >>= rotateW64 (-16) >>= copyToW64 d

--   -- Vc ← Vc + Vd       (no input)
--   addW64 c d >>= copyToW64 a
--   -- Vb ← (Vb xor Vc) rotateright 63
--   xorW64 b c >>= rotateW64 (-63) >>= copyToW64 b

--   return ()

-- -- | Write a Word64 to an array of bits
-- writeW64 :: Expr W64 n -> Word64 -> Comp n ()
-- writeW64 arr w = forM_ [0 .. 63] $ \i -> do
--   let bitValue = Val (Boolean (testBit w i))
--   update arr i bitValue

type W64 = 'Arr 'Bool

-- copyToW64 :: Expr W64 n -> Expr W64 n -> Comp n ()
-- copyToW64 src tgt = forM_ [0 .. 63] $ \i -> do
--   srcBit <- access src i
--   tgtBit <- access tgt i
--   update tgt i srcBit

fullAdder1bit :: Expr 'Bool n -> Expr 'Bool n -> Expr 'Bool n -> (Expr 'Bool n, Expr 'Bool n)
fullAdder1bit a b carry =
  let value = a `Xor` b `Xor` carry
      nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)
   in (value, nextCarry)

fullAdder :: Show n => Int -> Expr ('Arr 'Bool) n -> Expr ('Arr 'Bool) n -> Comp n (Expr ('Arr 'Bool) n)
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

-- xorW64 :: Expr W64 n -> Expr W64 n -> Comp n (Expr W64 n)
-- xorW64 as bs = do
--   result <- allocArray 64
--   forM_ [0 .. 63] $ \i -> do
--     a <- access as i
--     b <- access bs i
--     update result i (a `Xor` b)
--   return result

-- -- | Rotates left by i bits if i is positive, or right by -i bits otherwise.
-- rotateW64 :: Int -> Expr W64 n -> Comp n (Expr W64 n)
-- rotateW64 n xs = do
--   result <- allocArray 64
--   forM_ [0 .. 63] $ \i -> do
--     x <- access xs i
--     let i' = (n - i) `mod` 64
--     update result i' (x)
--   return result

-- -- updateW64 ::