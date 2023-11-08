{-# LANGUAGE DataKinds #-}

module Cipher.AES where

import Keelung

-- type Field = Field

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
sBox :: (Field, Field, Field, Field, Field, Field, Field, Field) -> (Field, Field, Field, Field, Field, Field, Field, Field)
sBox (b0, b1, b2, b3, b4, b5, b6, b7) = (b0', b1', b2', b3', b4', b5', b6', b7')
    where 
        b0' = inv b0 + inv b4 + inv b5 + inv b6 + inv b7 + 1
        b1' = inv b0 + inv b1 + inv b5 + inv b6 + inv b7 + 1
        b2' = inv b0 + inv b1 + inv b2 + inv b6 + inv b7 + 0
        b3' = inv b0 + inv b1 + inv b2 + inv b3 + inv b7 + 0
        b4' = inv b0 + inv b1 + inv b2 + inv b3 + inv b4 + 0
        b5' = inv b1 + inv b2 + inv b3 + inv b4 + inv b5 + 1
        b6' = inv b2 + inv b3 + inv b4 + inv b5 + inv b6 + 1
        b7' = inv b3 + inv b4 + inv b5 + inv b6 + inv b7 + 0

        inv :: Field -> Field
        inv 0 = 0
        inv b = pow b 254 -- b ^ 254 = b ^ (-1)