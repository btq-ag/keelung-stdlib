{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module Cipher.AES where

import Cipher.AES.Constant
import Cipher.AES.Types
import Control.Monad (forM_)
import Keelung

-- references
-- https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf
-- https://opensource.apple.com/source/CommonCrypto/CommonCrypto-55010/Source/AESedp/AES.c.auto.html

-- -- | subBytes: applies a substitution table (S-box) to each byte
-- subBytes :: (WordBlock, WordBlock, WordBlock, WordBlock) -> Comp (WordBlock, WordBlock, WordBlock, WordBlock)
-- subBytes (a0, a1, a2, a3) = do
--   b0 <- subWordBlock a0
--   b1 <- subWordBlock a1
--   b2 <- subWordBlock a2
--   b3 <- subWordBlock a3
--   return (b0, b1, b2, b3)
--   where
--     subWordBlock :: WordBlock -> Comp WordBlock
--     subWordBlock (x0, x1, x2, x3) = do
--       y0 <- sBox x0
--       y1 <- sBox x1
--       y2 <- sBox x2
--       y3 <- sBox x3
--       return (y0, y1, y2, y3)

-- shiftRows :: (WordBlock, WordBlock, WordBlock, WordBlock) -> (WordBlock, WordBlock, WordBlock, WordBlock)
-- shiftRows (r0, r1, r2, r3) = (r0, shiftWordBlock1 r1, shiftWordBlock2 r2, shiftWordBlock3 r3)
--   where
--     shiftWordBlock1 :: WordBlock -> WordBlock
--     shiftWordBlock1 (c0, c1, c2, c3) = (c1, c2, c3, c0)

--     shiftWordBlock2 :: WordBlock -> WordBlock
--     shiftWordBlock2 (c0, c1, c2, c3) = (c2, c3, c0, c1)

--     shiftWordBlock3 :: WordBlock -> WordBlock
--     shiftWordBlock3 (c0, c1, c2, c3) = (c3, c0, c1, c2)

-- -- | KeyExpansion – round keys are derived from the cipher key using the AES key schedule.
-- --   AES requires a separate 128-bit round key block for each round plus one more.
-- -- expandKey = undefined
-- mixColumns :: WordBlock -> WordBlock -> WordBlock
-- mixColumns (a0, a1, a2, a3) (b0, b1, b2, b3) = (d0, d1, d2, d3)
--   where
--     d0 = a0 * b0 + a3 * b1 + a2 * b2 + a1 * b3
--     d1 = a1 * b0 + a0 * b1 + a3 * b2 + a2 * b3
--     d2 = a2 * b0 + a1 * b1 + a0 * b2 + a3 * b3
--     d3 = a3 * b0 + a2 * b1 + a1 * b2 + a0 * b3

-- -- | SBox but implemented in a more "analytical" way
-- --      (b * 31 mod 257) + 99
-- sBox2 :: Field -> Comp Field
-- sBox2 b = do
--   -- inverse of `b`
--   --    if b == 0
--   --        then 0
--   --        else b ^ (-1)
--   let inverse = cond (b `eq` 0) 0 (pow b 254)

--   uint <- toUInt 8 (inverse * 31) :: Comp Byte
--   (_, modulo) <- performDivMod uint 257
--   field <- toField modulo
--   return $ field + 99

-- | SBox
sBox :: Byte -> Comp Byte
sBox b = do
  -- inverse of `b`
  --    if b == 0
  --        then 0
  --        else b ^ (-1)
  field <- toField b
  let inverse = cond (b `eq` 0) 0 (pow field 254)
  -- convert the inverse to Byte so that we can manipulate the bits
  b' <- toUInt 8 inverse :: Comp Byte

  let b0 = b' !!! 0 .^. b' !!! 4 .^. b' !!! 5 .^. b' !!! 6 .^. b' !!! 7 .^. true
  let b1 = b' !!! 1 .^. b' !!! 5 .^. b' !!! 6 .^. b' !!! 7 .^. b' !!! 0 .^. true
  let b2 = b' !!! 2 .^. b' !!! 6 .^. b' !!! 7 .^. b' !!! 0 .^. b' !!! 1 .^. false
  let b3 = b' !!! 3 .^. b' !!! 7 .^. b' !!! 0 .^. b' !!! 1 .^. b' !!! 2 .^. false
  let b4 = b' !!! 4 .^. b' !!! 0 .^. b' !!! 1 .^. b' !!! 2 .^. b' !!! 3 .^. false
  let b5 = b' !!! 5 .^. b' !!! 1 .^. b' !!! 2 .^. b' !!! 3 .^. b' !!! 4 .^. true
  let b6 = b' !!! 6 .^. b' !!! 2 .^. b' !!! 3 .^. b' !!! 4 .^. b' !!! 5 .^. true
  let b7 = b' !!! 7 .^. b' !!! 3 .^. b' !!! 4 .^. b' !!! 5 .^. b' !!! 6 .^. false

  let result =
        set (0, b0) $
          set (1, b1) $
            set (2, b2) $
              set (3, b3) $
                set (4, b4) $
                  set (5, b5) $
                    set (6, b6) $
                      set (7, b7) 0

  return result
  where
    set :: (Int, Boolean) -> Byte -> Byte
    set (index, value) byte = setBit byte index value

-- | SBox but implemented in a more "analytical" way
--      (b * 31 mod 257) + 99
-- sBox3 :: Byte -> Comp Byte
-- sBox3 b = do
--   -- higher 4 bits of b
--   let x = b !!! 4 .^. b !!! 5 .^. b !!! 6 .^. b !!! 7

--   (_, modulo) <- performDivMod (b * 31) 257
--   return $ modulo + 99

-- | Choose from the given list of 16 bytes based on the lower 4 bits of the given byte
genSBoxRow :: [Byte] -> Byte -> Byte
genSBoxRow c x =
  cond
    (x !!! 3)
    ( cond
        (x !!! 2)
        (cond (x !!! 1) (cond (x !!! 0) (c !! 15) (c !! 14)) (cond (x !!! 0) (c !! 13) (c !! 12))) -- 0b11__
        (cond (x !!! 1) (cond (x !!! 0) (c !! 11) (c !! 10)) (cond (x !!! 0) (c !! 9) (c !! 8))) -- 0b10__
    )
    ( cond
        (x !!! 2)
        (cond (x !!! 1) (cond (x !!! 0) (c !! 7) (c !! 6)) (cond (x !!! 0) (c !! 5) (c !! 4))) -- 0b01__
        (cond (x !!! 1) (cond (x !!! 0) (c !! 3) (c !! 2)) (cond (x !!! 0) (c !! 1) (c !! 0))) -- 0b00__
    )

-- | SBox, using Table 4 of the AES specification (page 14)
-- | Choose from the given list of 16 16-bytes list based on the higher 4 bits of the given byte
sBoxTabulation :: Byte -> Byte
sBoxTabulation x =
  cond
    (x !!! 7)
    ( cond
        (x !!! 6)
        ( cond
            (x !!! 5)
            (cond (x !!! 4) (genSBoxRow (sBoxTable !! 15) x) (genSBoxRow (sBoxTable !! 14) x)) -- 0b111_
            (cond (x !!! 4) (genSBoxRow (sBoxTable !! 13) x) (genSBoxRow (sBoxTable !! 12) x)) -- 0b110_
        )
        ( cond
            (x !!! 5)
            (cond (x !!! 4) (genSBoxRow (sBoxTable !! 11) x) (genSBoxRow (sBoxTable !! 10) x)) -- 0b101_
            (cond (x !!! 4) (genSBoxRow (sBoxTable !! 9) x) (genSBoxRow (sBoxTable !! 8) x)) -- 0b100_
        )
    )
    ( cond
        (x !!! 6)
        ( cond
            (x !!! 5)
            (cond (x !!! 4) (genSBoxRow (sBoxTable !! 7) x) (genSBoxRow (sBoxTable !! 6) x)) -- 0b011_
            (cond (x !!! 4) (genSBoxRow (sBoxTable !! 5) x) (genSBoxRow (sBoxTable !! 4) x)) -- 0b010_
        )
        ( cond
            (x !!! 5)
            (cond (x !!! 4) (genSBoxRow (sBoxTable !! 3) x) (genSBoxRow (sBoxTable !! 2) x)) -- 0b001_
            (cond (x !!! 4) (genSBoxRow (sBoxTable !! 1) x) (genSBoxRow (sBoxTable !! 0) x)) -- 0b000_
        )
    )

-- addRoundKey :: WordBlock -> WordBlock -> WordBlock
-- addRoundKey (a0, a1, a2, a3) (b0, b1, b2, b3) = (a0 ^ b0, a1 ^ b1, a2 ^ b2, a3 ^ b3)

-- | KeyExpansion for AES-128
keyExpansion128 :: (UIntWord, UIntWord, UIntWord, UIntWord) -> Comp [Byte]
keyExpansion128 (k0, k1, k2, k3) = do
  -- constants
  let nk = nk128 -- 4
  let nr = nr128 -- 10

  -- create a mutable array of `4 * (nr + 1)` words
  w <- thaw (replicate (4 * (nr + 1)) (0 :: Byte))
  -- the first `Nk` words are the key itself
  updateUIntWord w k0 0
  updateUIntWord w k1 1
  updateUIntWord w k2 2
  updateUIntWord w k3 3

  -- followed by a loop over i from 4 to 43
  forM_ [4 .. 4 * nr + 3] $ \i -> do
    temp <- accessUIntWord w (i - 1)
    temp1 <-
      if i `mod` nk == 0
        then do
          -- temp ← SUBWORD(ROTWORD(temp)) ⊕ Rcon[i/Nk]
          temp' <- subUIntWord (rotWord temp)
          return $ temp' `xorUIntWord` (rCon !! (i `div` nk - 1))
        else
          if nk > 6 && i `mod` nk == 4
            then do
              -- temp ← SUBWORD(temp)
              subUIntWord temp
            else do
              return temp
    -- w[i] ← w[i−Nk]⊕temp
    wink <- accessUIntWord w (i - nk)
    updateUIntWord w (temp1 `xorUIntWord` wink) i

  freeze w

-- | Convert a FieldWord to a FieldWord
toUIntWod :: FieldWord -> Comp UIntWord
toUIntWod (a0, a1, a2, a3) = do
  a0' <- toUInt 8 a0
  a1' <- toUInt 8 a1
  a2' <- toUInt 8 a2
  a3' <- toUInt 8 a3
  return (a0', a1', a2', a3')

-- | Convert a UIntWord to a FieldWord
fromUIntWod :: UIntWord -> Comp FieldWord
fromUIntWod (a0, a1, a2, a3) = do
  a0' <- toField a0
  a1' <- toField a1
  a2' <- toField a2
  a3' <- toField a3
  return (a0', a1', a2', a3')

-- | XOR on UIntWords
xorUIntWord :: UIntWord -> UIntWord -> UIntWord
xorUIntWord (a0, a1, a2, a3) (b0, b1, b2, b3) = (a0 .^. b0, a1 .^. b1, a2 .^. b2, a3 .^. b3)

-- | Accessing a UIntWord from the State (array of Bytes)
accessUIntWord :: ArrM Byte -> Int -> Comp UIntWord
accessUIntWord w i = do
  c0 <- accessM w (4 * i)
  c1 <- accessM w (4 * i + 1)
  c2 <- accessM w (4 * i + 2)
  c3 <- accessM w (4 * i + 3)
  return (c0, c1, c2, c3)

-- | Updating a UIntWord in the State (array of Bytes)
updateUIntWord :: ArrM Byte -> UIntWord -> Int -> Comp ()
updateUIntWord w (c0, c1, c2, c3) i = do
  updateM w (4 * i) c0
  updateM w (4 * i + 1) c1
  updateM w (4 * i + 2) c2
  updateM w (4 * i + 3) c3

-- | SUBWORD
subUIntWord :: UIntWord -> Comp UIntWord
subUIntWord (a0, a1, a2, a3) = do
  b0 <- sBox a0
  b1 <- sBox a1
  b2 <- sBox a2
  b3 <- sBox a3
  return (b0, b1, b2, b3)

-- | ROTWORD
rotWord :: UIntWord -> UIntWord
rotWord (a0, a1, a2, a3) = (a1, a2, a3, a0)
