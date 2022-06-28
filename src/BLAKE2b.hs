{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

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

compress ::
  Ref ('A ('A ('V 'Bool))) -> -- 128 bytes of message to compress
  Int128 -> -- count of bytes that have been compressed before
  Bool -> -- is this the final round of compression?
  Ref ('A ('A ('V 'Bool))) -> -- 128 bytes of old hash value
  Comp n (Ref ('A ('A ('V 'Bool)))) -- 128 bytes of new hash value
compress msgChunk count final input = do
  -- 16 Word64 as local state
  vs <- allocArray2 16 64

  -- First 8 items are copied from old state 'input'
  forM_ [0 .. 7] $ \j -> do
    forM_ [0 .. 63] $ \i -> do
      b <- access2 (j, i) input
      update2 vs (j, i) (Var b)

  -- Remaining 8 items are initialized from the IV
  forM_ [8 .. 15] $ \i -> do
    ref <- access i vs
    writeWord64 ref (iv !! i)

  --  Mix the 128-bit counter into V12 & V13
  v12 <- access 12 vs
  writeWord64 v12 (Int128.int128Lo64 count)
  v13 <- access 13 vs
  writeWord64 v13 (Int128.int128Hi64 count)

  -- If this is the last block then invert all the bits in V14
  when final $ do
    v14 <- access 14 vs
    forM_ [0 .. 63] $ \i -> do
      var <- access i v14
      update v14 i (neg (Var var))

  -- 12 rounds of cryptographic message mixing
  forM_ [0 .. 11] $ \i -> do
    -- Select message mixing schedule for this round.
    -- BLAKE2b uses 12 rounds, while 'sigma' has only 10 entries.
    -- mix()
    return ()

  return input

-- | Write a Word64 to an array of bits
writeWord64 :: Ref ('A ('V 'Bool)) -> Word64 -> Comp n ()
writeWord64 arr w = forM_ [0 .. 63] $ \i -> do
  let bitValue = Val (Boolean (testBit w i))
  update arr i bitValue

-- copyArray :: Ref ('A ('V 'Bool)) -> Ref ('A ('V 'Bool)) -> Comp n ()
-- copyArray source target = do
--   forM_ [0 .. 15] $ \i -> do
--     forM_ [0 .. 63] $ \j -> do
--       b <- access2 (i, j) source
--       update2 target (i, j) (Var b)