{-# LANGUAGE DataKinds #-}

module Cipher.AES where

import Keelung

type Byte = UInt 8

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
mixColumns :: (Field, Field, Field, Field) -> (Field, Field, Field, Field) -> (Field, Field, Field, Field)
mixColumns (a0, a1, a2, a3) (b0, b1, b2, b3) = (d0, d1, d2, d3)
  where
    d0 = a0 * b0 + a3 * b1 + a2 * b2 + a1 * b3
    d1 = a1 * b0 + a0 * b1 + a3 * b2 + a2 * b3
    d2 = a2 * b0 + a1 * b1 + a0 * b2 + a3 * b3
    d3 = a3 * b0 + a2 * b1 + a1 * b2 + a0 * b3

-- | SubFields: applies a substitution table (S-box) to each byte
-- sBox :: Field -> Field
-- sBox 0x00 = 0x63
-- sBox 0x01 = 0x7c
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

-- testSBox :: Comp Field
-- testSBox = do 
--   input Public >>= sBox
