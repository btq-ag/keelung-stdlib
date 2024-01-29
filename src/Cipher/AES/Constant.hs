-- | Constants of AES.
module Cipher.AES.Constant where

import Cipher.AES.Types

-- | Key length of AES-128
nk128 :: Int
nk128 = 4

-- | Key length of AES-192
nk192 :: Int
nk192 = 6

-- | Key length of AES-256
nk256 :: Int
nk256 = 8

-- | Block size of AES-128, AES-192, and AES-256
nb :: Int
nb = 4

-- | Number of rounds of AES-128
nr128 :: Int
nr128 = 10

-- | Number of rounds of AES-192
nr192 :: Int
nr192 = 12

-- | Number of rounds of AES-256
nr256 :: Int
nr256 = 14

-- | Round constants.
rCon :: [UIntWord]
rCon =
  [ (0x01, 0x00, 0x00, 0x00),
    (0x02, 0x00, 0x00, 0x00),
    (0x04, 0x00, 0x00, 0x00),
    (0x08, 0x00, 0x00, 0x00),
    (0x10, 0x00, 0x00, 0x00),
    (0x20, 0x00, 0x00, 0x00),
    (0x40, 0x00, 0x00, 0x00),
    (0x80, 0x00, 0x00, 0x00),
    (0x1b, 0x00, 0x00, 0x00),
    (0x36, 0x00, 0x00, 0x00)
  ]