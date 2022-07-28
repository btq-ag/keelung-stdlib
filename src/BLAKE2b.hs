{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module BLAKE2b where

import Control.Monad
import Data.Bits
import Data.Int
-- import Data.WideWord (Int128)
-- import qualified Data.WideWord.Int128 as Int128

import Data.WideWord.Word128 (Word128)
import qualified Data.WideWord.Word128 as Word128
import Data.Word
import Keelung
import qualified Lib.Array as Array
import Lib.W64 (W64)
import qualified Lib.W64 as W64
import Lib.W8 (W8)
import qualified Lib.W8 as W8

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

-- hash ::
--   -- | Message to be hashed
--   Val ('Arr W8) n ->
--   -- | Length of the message in bytes (0..2^128)
--   Word128 ->
--   -- | Optional key (length : 0..64 bytes)
--   Val ('Arr W8) n ->
--   -- | Length of optional key in bytes (0..64)
--   Int ->
--   -- | Desired hash length in bytes (1..64)
--   Int ->
--   Comp n (Val ('Arr W64) n)
-- hash msg msgLen key keyLen hashLen = do
--   --  Initialize State vector h with IV
--   hash <- mapM W64.fromWord64 iv >>= toArray

--   -- rub key size and desired hash length into hash[0]
--   iv0 <- W64.fromWord64 (iv !! 0)
--   spice <- W64.fromWord64 (fromIntegral x0101kknn)
--   h0 <- iv0 `W64.xor` spice
--   update hash 0 h0

--   --  If there was a key supplied (i.e. `keyLen` > 0)
--   --  then pad with trailing zeros to make it 128-bytes (i.e. 16 words)
--   --  and prepend it to the message M
--   let bytesRemaining =
--         if keyLen > 0
--           then msgLen + 128
--           else msgLen

--   forM_ [0, 128 .. bytesRemaining] $ \bytesCompressed -> do
--     return ()
--   --  If there was a key supplied (i.e. cbKeyLen > 0)
--   --  then pad with trailing zeros to make it 128-bytes (i.e. 16 words)
--   --  and prepend it to the message M

--   toArray []
--   where
--     -- from key size ('kk') and desired hash length ('nn')
--     -- for example, if key size = 17 bytes, desired hash length = 3
--     -- then x0101kknn = 0x010101103
--     x0101kknn = 0x01010000 + (keyLen `shiftL` 8) + hashLen

test :: Comp GF181 (Val 'Unit GF181)
test = do
  let message = "Hello, world!"

  message' <- W8.fromString message
  actual <-
    run
      message'
      (fromIntegral (lengthOf message'))
      512
  expected <- toArray []

  forM_ [0 .. 127] $ \i -> do
    x <- access actual i
    y <- access expected i
    W64.equal x y >>= assert

  return unit

run ::
  -- | Message to be hashed
  Val ('Arr W8) n ->
  -- | Length of the message in bytes (0..2^128)
  Word128 ->
  -- | Desired hash length in bytes (1..64)
  Int ->
  Comp n (Val ('Arr W64) n)
run msg msgLen hashLen = do
  --  Initialize State vector h with IV
  hash <- mapM W64.fromWord64 iv >>= toArray

  -- rub key size and desired hash length into hash[0]
  iv0 <- W64.fromWord64 (iv !! 0)
  spice <- W64.fromWord64 (fromIntegral x0101kknn)
  h0 <- iv0 `W64.xor` spice
  update hash 0 h0

  -- let bytesRemaining = msgLen

  -- forM_ [0, 128 .. bytesRemaining] $ \bytesCompressed -> do
  --   return ()
  --  If there was a key supplied (i.e. cbKeyLen > 0)
  --  then pad with trailing zeros to make it 128-bytes (i.e. 16 words)
  --  and prepend it to the message M

  let bytesCompressed = 0

  --  Compress the final bytes

  chunk <- pad msg 128
  compress hash chunk bytesCompressed True

  return hash
  where
    -- from key size ('kk') and desired hash length ('nn')
    -- for example, if key size = 17 bytes, desired hash length = 3
    -- then x0101kknn = 0x010101103
    x0101kknn :: Int
    x0101kknn = 0x01010000 + (0 `shiftL` 8) + hashLen

-- | Padding a message with `false` to the desired length
pad :: Val ('Arr W8) n -> Int -> Comp n (Val ('Arr W8) n)
pad xs len =
  let len' = lengthOf xs
   in if len' >= len
        then return xs
        else do
          xs' <- W8.fromString (replicate (len - len') ' ')
          Array.concatenate xs xs'

compress ::
  Val ('Arr W64) n -> -- 128 bytes of old hash value
  Val ('Arr W64) n -> -- 128 bytes of message to compress
  Word128 -> -- count of bytes that have been compressed before
  Bool -> -- is this the final round of compression?
  Comp n ()
compress hash msg count final = do
  -- allocate 16 Word64 as local state
  vs <- replicateM 16 (W64.fromWord64 minBound) >>= toArray

  -- First 8 items are copied from old hash
  forM_ [0 .. 7] $ \j -> do
    h <- access hash j
    update vs j h

  -- Remaining 8 items are initialized from the IV
  forM_ [8 .. 15] $ \i -> do
    -- creates a W64 from Word64s in `iv`
    init <- W64.fromWord64 (iv !! i)
    update vs i init

  --  Mix the 128-bit counter into V12 & V13
  v12 <- W64.fromWord64 (Word128.word128Lo64 count)
  update vs 12 v12
  v13 <- W64.fromWord64 (Word128.word128Hi64 count)
  update vs 13 v13

  -- If this is the last block then invert all the bits in V14
  when final $ do
    v14 <- access vs 14
    complemented <- W64.complement v14
    update vs 14 complemented

  -- 12 rounds of cryptographic message mixing
  forM_ [0 .. 11] $ \i -> do
    -- Select message mixing schedule for this round.
    -- BLAKE2b uses 12 rounds, while 'sigma' has only 10 entries.
    let indices = sigma !! i
    mix vs 0 4 8 12 msg (indices !! 0) (indices !! 1)
    mix vs 1 5 9 13 msg (indices !! 2) (indices !! 3)
    mix vs 2 6 10 14 msg (indices !! 4) (indices !! 5)
    mix vs 3 7 11 15 msg (indices !! 6) (indices !! 7)
    mix vs 0 5 10 15 msg (indices !! 8) (indices !! 9)
    mix vs 1 6 11 12 msg (indices !! 10) (indices !! 11)
    mix vs 2 7 8 13 msg (indices !! 12) (indices !! 13)
    mix vs 3 4 9 14 msg (indices !! 14) (indices !! 15)

  -- Mix the upper and lower halves of `vs` into 'hash'
  --  h0..7 ← h0..7 xor V0..7
  forM_ [0 .. 7] $ \i -> do
    h <- access hash i
    v <- access vs i
    x <- h `W64.xor` v
    update hash i x
  --  h0..7 ← h0..7 xor V8..15
  forM_ [0 .. 7] $ \i -> do
    h <- access hash i
    v <- access vs (i + 8)
    x <- h `W64.xor` v
    update hash i x

mix ::
  Val ('Arr W64) n ->
  Int ->
  Int ->
  Int ->
  Int ->
  Val ('Arr W64) n ->
  Int ->
  Int ->
  Comp n ()
mix vs ai bi ci di msg xi yi = do
  a <- access vs ai
  b <- access vs bi
  c <- access vs ci
  d <- access vs di

  x <- access msg xi
  y <- access msg yi

  -- Va ← Va + Vb + x   (with input)
  a <- W64.add a b
  a <- W64.add x a
  -- Vd ← (Vd xor Va) rotateright 32
  d <- W64.xor d a
  d <- W64.rotate (-32) d

  -- Vc ← Vc + Vd       (no input)
  c <- W64.add c d
  -- Vb ← (Vb xor Vc) rotateright 24
  b <- W64.xor b c
  b <- W64.rotate (-24) b

  -- Va ← Va + Vb + y   (with input)
  a <- W64.add a b
  a <- W64.add y a
  -- Vd ← (Vd xor Va) rotateright 16
  d <- W64.xor d a
  d <- W64.rotate (-16) d

  -- Vc ← Vc + Vd       (no input)
  a <- W64.add c d
  -- Vb ← (Vb xor Vc) rotateright 63
  b <- W64.xor b c
  b <- W64.rotate (-63) b

  -- write back to `vs`
  update vs ai a
  update vs bi b
  update vs ci c
  update vs di d

  return ()

-- test :: Comp GF181 (Val 'Unit GF181)
-- test = do
--   xs <- inputs 3
--   ys <- inputs 3

--   let rotate n as = do
--       result <- toArray (replicate 3 false)
--       forM_ [0 .. 2] $ \i -> do
--         x <- access as i
--         let i' = (i - n) `mod` 3
--         update result i' x
--       return result

--   xs <- rotate 1 xs
--   -- xs <- rotate 1 xs

--   forM_ [0 .. 2] $ \i -> do
--     x <- access xs i
--     y <- access ys i
--     assert (x `BEq` y)

--   return unit