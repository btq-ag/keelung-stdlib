{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module WIP.BLAKE2b (hash) where

-- import Control.Monad
-- import qualified Crypto.Hash.BLAKE2.BLAKE2b
-- import Data.Bits
-- import qualified Data.ByteString.Char8 as ByteString.Char8
-- import Data.WideWord.Word128 (Word128)
-- import qualified Data.WideWord.Word128 as Word128
-- import Data.Word

import Data.Bits qualified
import Data.Vector (Vector, (!), (//))
import Data.Vector qualified as Vec
import Data.WideWord (Word128)
import Data.WideWord qualified as Word128
import Keelung

-- import Keelung
-- import qualified Lib.ArrayM as ArrayM
-- import Lib.W64 (W64M)
-- import qualified Lib.W64 as W64
-- import Lib.W8 (W8M)
-- import qualified Lib.W8 as W8

type U64 = UInt 64

type Byte = UInt 8

-- | Initialization vector
iv :: Vector U64
iv =
  Vec.fromList
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

byteToU64 :: (Byte, Byte, Byte, Byte, Byte, Byte, Byte, Byte) -> U64
byteToU64 (b0, b1, b2, b3, b4, b5, b6, b7) = foldl (\x byte -> foldl (\x' index -> setBit x' index (byte !!! index)) x [0 .. 7]) 0 [b0, b1, b2, b3, b4, b5, b6, b7]

bytesToU64s :: Vector Byte -> Vector U64
bytesToU64s xs =
  let size = Vec.length xs `div` 8
   in Vec.generate size $ \i ->
        byteToU64
          ( xs ! (i * 8),
            xs ! (i * 8 + 1),
            xs ! (i * 8 + 2),
            xs ! (i * 8 + 3),
            xs ! (i * 8 + 4),
            xs ! (i * 8 + 5),
            xs ! (i * 8 + 6),
            xs ! (i * 8 + 7)
          )

u64sToBytes :: Vector U64 -> Vector Byte
u64sToBytes = Vec.concatMap u64ToBytes

u64ToBytes :: U64 -> Vector Byte
u64ToBytes u64 =
  Vec.fromList
    [ foldl (\x index -> setBit x index (u64 !!! index)) 0 [0 .. 7],
      foldl (\x index -> setBit x index (u64 !!! (index + 8))) 0 [0 .. 7],
      foldl (\x index -> setBit x index (u64 !!! (index + 16))) 0 [0 .. 7],
      foldl (\x index -> setBit x index (u64 !!! (index + 24))) 0 [0 .. 7],
      foldl (\x index -> setBit x index (u64 !!! (index + 32))) 0 [0 .. 7],
      foldl (\x index -> setBit x index (u64 !!! (index + 40))) 0 [0 .. 7],
      foldl (\x index -> setBit x index (u64 !!! (index + 48))) 0 [0 .. 7],
      foldl (\x index -> setBit x index (u64 !!! (index + 56))) 0 [0 .. 7]
    ]

-- test :: Comp ()
-- test = do
--   let message = concat $ replicate 200 "abc"
--   let hashlen = 64 -- must <= 64
--   message' <- W8.fromString message
--   result <-
--     hash
--       message'
--       (length message)
--       hashlen

--   let msgBS = ByteString.Char8.pack message
--   let ansBS = Crypto.Hash.BLAKE2.BLAKE2b.hash hashlen ByteString.Char8.empty msgBS

--   ans <- W8.fromString (ByteString.Char8.unpack ansBS)

--   forM_ [0 .. hashlen - 1] $ \i -> do
--     x <- accessM result i
--     y <- accessM ans i
--     W8.equal x y >>= assert

-- hash ::
--   -- | Message to be hashed
--   ArrM W8M ->
--   -- | Length of the message in bytes (0..2^128)
--   Int ->
--   -- | Desired hash length in bytes (1..64)
--   Int ->
--   Comp (ArrM W8M)
-- hash msg msgLen hashLen = do
--   --  Initialize State vector h with IV
--   state <- mapM W64.fromWord64 iv >>= toArrayM

--   -- rub key size and desired hash length into hash[0]
--   iv0 <- W64.fromWord64 (iv !! 0)
--   spice <- W64.fromWord64 (fromIntegral x0101kknn)
--   h0 <- iv0 `W64.xor` spice
--   updateM state 0 h0

--   forM_ [0, 128 .. msgLen - 128 - 1] $ \i -> do
--     chunk <- ArrayM.drop i msg
--     compress state chunk (fromIntegral (i + 128)) False

--   --  Compress the final bytes
--   remain <- ArrayM.drop (msgLen `div` 128 * 128) msg
--   chunk <- pad remain 128
--   compress state chunk (fromIntegral msgLen) True

--   -- ref <- W64.fromWord64 0x0D4D1C983FA580BA
--   -- assert =<< W64.equal ref =<< accessM hash 0

--   ArrayM.take hashLen =<< W64.toW8Chunks state
--   where
--     -- from key size ('kk') and desired hash length ('nn')
--     -- for example, if key size = 17 bytes, desired hash length = 3
--     -- then x0101kknn = 0x010101103
--     x0101kknn :: Int
--     x0101kknn = 0x01010000 + (0 `shiftL` 8) + hashLen

-- | NO KEY SUPPLIED
hash ::
  -- | Message to be hashed
  Vector U64 ->
  -- | Length of the message in bytes (0..2^128)
  Word128 ->
  -- | Desired hash length in bytes (1..64)
  Int ->
  Vector U64
hash msg msgLen hashLen =
  let --  Initialize State vector h with IV
      state = iv
      -- rub key size and desired hash length into hash[0]
      spice = fromIntegral x0101kknn
      state' = state // [(0, state ! 0 .^. spice)]
      -- compress whole 128-byte chunks of the message, except the last chunk
      state'' =
        foldl
          ( \st bytesCompressed ->
              let bytesRemaining = msgLen - bytesCompressed
                  isLastChunk = bytesRemaining <= 128
                  chunk =
                    if isLastChunk
                      then pad (Vec.drop (fromIntegral bytesCompressed) (u64sToBytes st)) 128
                      else Vec.slice (fromIntegral bytesCompressed) 128 (u64sToBytes st)
               in if isLastChunk
                    then compress st (bytesToU64s chunk) msgLen True
                    else compress st (bytesToU64s chunk) bytesCompressed False
          )
          state'
          [0, 128 .. msgLen - 128 - 1]
   in -- mix the upper and lower halves of V into ongoing state vector h
      Vec.zipWith (.^.) (Vec.drop 8 state'') $ Vec.zipWith (.^.) (Vec.take 8 state'') msg
  where
    -- from key size ('kk') and desired hash length ('nn')
    -- for example, if key size = 17 bytes, desired hash length = 3
    -- then x0101kknn = 0x010101103
    x0101kknn :: Int
    x0101kknn = 0x01010000 + (0 `Data.Bits.shiftL` 8) + hashLen

-- | Padding a message with trailing zeros to the desired length
pad :: Vector Byte -> Int -> Vector Byte
pad xs len =
  let lenDiff = len - Vec.length xs
   in if lenDiff > 0
        then xs <> Vec.replicate lenDiff 0
        else xs -- no padding needed

-- | The Compress function takes a full 128-byte chunk of the input message and mixes it into the ongoing state array
compress ::
  Vector U64 -> -- 128 bytes of old hash value
  Vector U64 -> -- 128 bytes of message to compress
  Word128 -> -- count of bytes that have been compressed before
  Bool -> -- is this the final round of compression?
  Vector U64
compress h chunk t isLastBlock =
  let -- setup local work vector 'v'
      -- first 8 items are copied from persistent state vector 'h'
      -- remaining 8 items are initialized from the 'iv'
      v =
        Vec.take 8 h
          <> Vec.fromList
            [ iv ! 0,
              iv ! 1,
              iv ! 2,
              iv ! 3,
              iv ! 4 .^. fromIntegral (Word128.word128Lo64 t), -- Lo 64-bits of Word128 't'
              iv ! 5 .^. fromIntegral (Word128.word128Hi64 t), -- Hi 64-bits of Word128 't'
              if isLastBlock then iv ! 6 .^. 0xFFFFFFFFFFFFFFFF else iv ! 6, -- if this is the last block then invert all the bits in 'v14'
              iv ! 7
            ]
      -- work vector after 12 rounds of mixing
      mixedV :: Vector U64
      mixedV = foldl (mixRound chunk) v [0 .. 11]
   in -- mix the upper and lower halves of 'mixedV' into onpicking state vector 'h'
      Vec.zipWith (.^.) (Vec.drop 8 mixedV) $ Vec.zipWith (.^.) (Vec.take 8 mixedV) h

-- round of cryptographic message mixing
mixRound :: Vector U64 -> Vector U64 -> Int -> Vector U64
mixRound chunk vector index =
  -- select message mixing schedule for this round.
  -- BLAKE2b uses 12 rounds, while 'sigma' has only 10 entries.
  let indices = sigma !! index
      -- select which element of the work vector to mix
      pick (ai, bi, ci, di) (xi, yi) xs =
        let (a, b, c, d) = mix (xs ! ai, xs ! bi, xs ! ci, xs ! di) (chunk ! (indices !! xi), chunk ! (indices !! yi))
         in xs // [(ai, a), (bi, b), (ci, c), (di, d)]
      go =
        pick (3, 4, 9, 14) (14, 15)
          . pick (2, 7, 8, 13) (12, 13)
          . pick (1, 6, 11, 12) (10, 11)
          . pick (0, 5, 10, 15) (8, 9)
          . pick (3, 7, 11, 15) (6, 7)
          . pick (2, 6, 10, 14) (4, 5)
          . pick (1, 5, 9, 13) (2, 3)
          . pick (0, 4, 8, 12) (0, 1)
   in go vector

mix ::
  (U64, U64, U64, U64) -> -- four U64 entries from the work vector V
  (U64, U64) -> -- two U64 entries from padded message m
  (U64, U64, U64, U64) -- the modified four U64 entries from the work vector V
mix (a, b, c, d) (x, y) =
  let a1 = a + b + x -- with input
      d1 = (d .^. a1) `rotate` (-32)
      c1 = c + d1 -- no input
      b1 = (b .^. c1) `rotate` (-24)
      a2 = a1 + b1 + y -- with input
      d2 = (d1 .^. a2) `rotate` (-16)
      c2 = c1 + d2 -- no input
      b2 = (b1 .^. c2) `rotate` (-63)
   in (a2, b2, c2, d2)