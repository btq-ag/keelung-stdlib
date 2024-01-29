{-# LANGUAGE DataKinds #-}

module Cipher.AES where

import Keelung

type Byte = UInt 8

type WordBlock = (Field, Field, Field, Field)

-- references
-- https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf
-- https://opensource.apple.com/source/CommonCrypto/CommonCrypto-55010/Source/AESedp/AES.c.auto.html

-- | Constants

-- | Number of columns (32-bit words) comprising the State. For this standard, Nb = 4.
nb :: Int
nb = 4

nk :: Int
nk = 4

nr :: Int
nr = 10

-- | KeyExpansion – round keys are derived from the cipher key using the AES key schedule.
--   AES requires a separate 128-bit round key block for each round plus one more.
-- expandKey = undefined
mixColumns :: WordBlock -> WordBlock -> WordBlock
mixColumns (a0, a1, a2, a3) (b0, b1, b2, b3) = (d0, d1, d2, d3)
  where
    d0 = a0 * b0 + a3 * b1 + a2 * b2 + a1 * b3
    d1 = a1 * b0 + a0 * b1 + a3 * b2 + a2 * b3
    d2 = a2 * b0 + a1 * b1 + a0 * b2 + a3 * b3
    d3 = a3 * b0 + a2 * b1 + a1 * b2 + a0 * b3

-- | subBytes: applies a substitution table (S-box) to each byte
subBytes :: (WordBlock, WordBlock, WordBlock, WordBlock) -> Comp (WordBlock, WordBlock, WordBlock, WordBlock)
subBytes (a0, a1, a2, a3) = do
  b0 <- subWordBlock a0
  b1 <- subWordBlock a1
  b2 <- subWordBlock a2
  b3 <- subWordBlock a3
  return (b0, b1, b2, b3)
  where
    subWordBlock :: WordBlock -> Comp WordBlock
    subWordBlock (x0, x1, x2, x3) = do
      y0 <- sBox x0
      y1 <- sBox x1
      y2 <- sBox x2
      y3 <- sBox x3
      return (y0, y1, y2, y3)

-- | SBox
sBox :: Field -> Comp Field
sBox b = do
  -- inverse of `b`
  --    if b == 0
  --        then 0
  --        else b ^ (-1)
  let inverse = cond (b `eq` 0) 0 (pow b 254)
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

  toField result
  where
    set :: (Int, Boolean) -> Byte -> Byte
    set (index, value) byte = setBit byte index value

-- | SBox but implemented in a more "analytical" way
--      (b * 31 mod 257) + 99
sBox2 :: Field -> Comp Field
sBox2 b = do
  -- inverse of `b`
  --    if b == 0
  --        then 0
  --        else b ^ (-1)
  let inverse = cond (b `eq` 0) 0 (pow b 254)

  uint <- toUInt 8 (inverse * 31) :: Comp Byte
  (_, modulo) <- performDivMod uint 257
  field <- toField modulo
  return $ field + 99

shiftRows :: (WordBlock, WordBlock, WordBlock, WordBlock) -> (WordBlock, WordBlock, WordBlock, WordBlock)
shiftRows (r0, r1, r2, r3) = (r0, shiftWordBlock1 r1, shiftWordBlock2 r2, shiftWordBlock3 r3) 
  where 
    shiftWordBlock1 :: WordBlock -> WordBlock
    shiftWordBlock1 (c0, c1, c2, c3) = (c1, c2, c3, c0)

    shiftWordBlock2 :: WordBlock -> WordBlock
    shiftWordBlock2 (c0, c1, c2, c3) = (c2, c3, c0, c1)

    shiftWordBlock3 :: WordBlock -> WordBlock
    shiftWordBlock3 (c0, c1, c2, c3) = (c3, c0, c1, c2)

-- mixColumns :: 