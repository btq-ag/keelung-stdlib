{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module Hash.BLAKE2sM where

import Control.Monad
import qualified Crypto.Hash.BLAKE2.BLAKE2s
import Data.Bits
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Word
import Keelung
import qualified Lib.ArrayM as ArrayM
import Lib.W32 (W32M)
import qualified Lib.W32 as W32
import Lib.W8 (W8M)
import qualified Lib.W8 as W8

-- | Initialization vector
iv :: [Word32]
iv =
  [ 0x6a09e667, -- Frac(sqrt(2))
    0xbb67ae85, -- Frac(sqrt(3))
    0x3c6ef372, -- Frac(sqrt(5))
    0xa54ff53a, -- Frac(sqrt(7))
    0x510e527f, -- Frac(sqrt(11))
    0x9b05688c, -- Frac(sqrt(13))
    0x1f83d9ab, -- Frac(sqrt(17))
    0x5be0cd19 -- Frac(sqrt(19))
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
    [10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0]
  ]

test :: Comp ()
test = do
  let message = concat $ replicate 200 "abc"
  let hashlen = 32 -- must <= 32
  message' <- W8.fromString message
  result <-
    hash
      message'
      (length message)
      hashlen

  let msgBS = ByteString.Char8.pack message
  let ansBS = Crypto.Hash.BLAKE2.BLAKE2s.hash hashlen ByteString.Char8.empty msgBS

  ans <- W8.fromString (ByteString.Char8.unpack ansBS)

  forM_ [0 .. hashlen - 1] $ \i -> do
    x <- accessM result i
    y <- accessM ans i
    W8.equal x y >>= assert

hash ::
  -- | Message to be hashed
  ArrM W8M ->
  -- | Length of the message in bytes (0..2^64)
  Int ->
  -- | Desired hash length in bytes (1..32)
  Int ->
  Comp (ArrM W8M)
hash msg msgLen hashLen = do
  --  Initialize State vector h with IV
  hash <- mapM W32.fromWord32 iv >>= toArrayM

  -- rub key size and desired hash length into hash[0]
  iv0 <- W32.fromWord32 (iv !! 0)
  spice <- W32.fromWord32 (fromIntegral x0101kknn)
  h0 <- iv0 `W32.xor` spice
  updateM hash 0 h0

  forM_ [0, 64 .. msgLen - 64 - 1] $ \i -> do
    chunk <- ArrayM.drop i msg
    compress hash chunk (fromIntegral (i + 64)) False

  --  Compress the final bytes
  remain <- ArrayM.drop (msgLen `div` 64 * 64) msg
  chunk <- pad remain 64
  compress hash chunk (fromIntegral msgLen) True

  -- ref <- W32.fromWord32 0x0D4D1C983FA580BA
  -- assert =<< W32.equal ref =<< accessM hash 0

  ArrayM.take hashLen =<< W32.toW8Chunks hash
  where
    -- from key size ('kk') and desired hash length ('nn')
    -- for example, if key size = 17 bytes, desired hash length = 3
    -- then x0101kknn = 0x010101103
    x0101kknn :: Int
    x0101kknn = 0x01010000 + (0 `shiftL` 8) + hashLen

-- | Padding a message with `false` to the desired length
pad :: ArrM W8M -> Int -> Comp (ArrM W8M)
pad xs len =
  let len' = lengthOf xs
   in if len' >= len
        then return xs
        else do
          xs' <- W8.zeros (len - len')
          ArrayM.concatenate xs xs'

compress ::
  ArrM W32M -> -- 128 bytes of old hash value
  ArrM W32M -> -- 128 bytes of message to compress
  Word64 -> -- count of bytes that have been compressed before
  Bool -> -- is this the final round of compression?
  Comp ()
compress hash msg count final = do
  -- allocate 16 Word32 as local state
  vs <- W32.zeros 16

  -- First 8 items are copied from old hash
  forM_ [0 .. 7] $ \j -> do
    updateM vs j =<< accessM hash j

  -- Remaining 8 items are initialized from the IV
  forM_ [8 .. 15] $ \i -> do
    -- creates a W32 from Word32s in `iv`
    init <- W32.fromWord32 (iv !! (i - 8))
    updateM vs i init

  --  Mix the 128-bit counter into V12 & V13
  v12 <- accessM vs 12
  v12 <- W32.fromWord32 (fromIntegral count) >>= (v12 `W32.xor`)
  updateM vs 12 v12
  v13 <- accessM vs 13
  v13 <- W32.fromWord32 (fromIntegral (shiftR count 32)) >>= (v13 `W32.xor`)
  updateM vs 13 v13

  -- If this is the last block then invert all the bits in V14
  when final $ do
    updateM vs 14 =<< W32.complement =<< accessM vs 14

  msg <- W32.fromW8Chunks msg

  -- ref <- W32.fromHex "6a09e667f2bdc948"
  -- pred <- accessM hash 0 >>= W32.equal ref
  -- assert pred

  -- 10 rounds of cryptographic message mixing
  forM_ [0 .. 9] $ \i -> do
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
    -- updateM hash i =<< join (W32.xor <$> accessM hash i <*> accessM vs i)
    h <- accessM hash i
    v <- accessM vs i
    x <- h `W32.xor` v
    updateM hash i x

  --  h0..7 ← h0..7 xor V8..15
  forM_ [0 .. 7] $ \i -> do
    h <- accessM hash i
    v <- accessM vs (i + 8)
    x <- h `W32.xor` v
    updateM hash i x

mix ::
  ArrM W32M ->
  Int ->
  Int ->
  Int ->
  Int ->
  ArrM W32M ->
  Int ->
  Int ->
  Comp ()
mix vs ai bi ci di msg xi yi = do
  a <- accessM vs ai
  b <- accessM vs bi
  c <- accessM vs ci
  d <- accessM vs di

  x <- accessM msg xi
  y <- accessM msg yi

  -- Va ← Va + Vb + x   (with input)
  a <- W32.add a b
  a <- W32.add x a
  -- Vd ← (Vd xor Va) rotateright 32
  d <- W32.xor d a
  d <- W32.rotateR 16 d
  -- Vc ← Vc + Vd       (no input)
  c <- W32.add c d
  -- Vb ← (Vb xor Vc) rotateright 24
  b <- W32.xor b c
  b <- W32.rotateR 12 b
  -- Va ← Va + Vb + y   (with input)
  a <- W32.add a b
  a <- W32.add y a
  -- Vd ← (Vd xor Va) rotateright 16
  d <- W32.xor d a
  d <- W32.rotateR 8 d
  -- Vc ← Vc + Vd       (no input)
  c <- W32.add c d
  -- Vb ← (Vb xor Vc) rotateright 63
  b <- W32.xor b c
  b <- W32.rotateR 7 b

  -- write back to `vs`
  updateM vs ai a
  updateM vs bi b
  updateM vs ci c
  updateM vs di d

  return ()