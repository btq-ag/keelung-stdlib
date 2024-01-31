{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module Cipher.AES (cipher128, runCipher128) where

import Cipher.AES.Constant
import Cipher.AES.Types
import Control.Monad (foldM)
import Data.Array
import Keelung
import Keelung.Constraint.R1CS
import Keelung.Syntax.Counters
import Prelude hiding (round)

-- references
-- https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197-upd1.pdf
-- https://opensource.apple.com/source/CommonCrypto/CommonCrypto-55010/Source/AESedp/AES.c.auto.html

-- | subBytes: applies a substitution table (S-box) to each byte
subBytes :: Tuple UIntWord -> Comp (Tuple UIntWord)
subBytes (a0, a1, a2, a3) = do
  b0 <- subFieldWord a0
  b1 <- subFieldWord a1
  b2 <- subFieldWord a2
  b3 <- subFieldWord a3
  return (b0, b1, b2, b3)
  where
    subFieldWord :: UIntWord -> Comp UIntWord
    subFieldWord (x0, x1, x2, x3) = do
      y0 <- sBox x0
      y1 <- sBox x1
      y2 <- sBox x2
      y3 <- sBox x3
      return (y0, y1, y2, y3)

-- | ShiftRows: cyclically shifts the last three rows of the state by different offsets
shiftRows :: Tuple (Tuple a) -> Tuple (Tuple a)
shiftRows (c0, c1, c2, c3) =
  let (s00, s10, s20, s30) = c0
      (s01, s11, s21, s31) = c1
      (s02, s12, s22, s32) = c2
      (s03, s13, s23, s33) = c3
   in ((s00, s11, s22, s33), (s01, s12, s23, s30), (s02, s13, s20, s31), (s03, s10, s21, s32))

mixSingleColumn :: FieldWord -> FieldWord
mixSingleColumn (x0, x1, x2, x3) =
  ( 0x02 * x0 + 0x03 * x1 + x2 + x3,
    x0 + 0x02 * x1 + 0x03 * x2 + x3,
    x0 + x1 + 0x02 * x2 + 0x03 * x3,
    0x03 * x0 + x1 + x2 + 0x02 * x3
  )

-- | MixColumns: each column of the state is multiplied with a fixed polynomial
mixColumns :: Tuple FieldWord -> Tuple FieldWord
mixColumns (c0, c1, c2, c3) = (mixSingleColumn c0, mixSingleColumn c1, mixSingleColumn c2, mixSingleColumn c3)

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
    set (i, value) byte = setBit byte i value

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
_sBoxTabulation :: Byte -> Byte
_sBoxTabulation x =
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

updateUIntWord :: Array Int UIntWord -> UIntWord -> Int -> Comp (Array Int UIntWord)
updateUIntWord arr (c0, c1, c2, c3) i = do
  c0' <- reuse c0
  c1' <- reuse c1
  c2' <- reuse c2
  c3' <- reuse c3
  return $ arr // [(i, (c0', c1', c2', c3'))]

keyExpansion128 :: Tuple UIntWord -> Comp [UIntWord]
keyExpansion128 (k0, k1, k2, k3) = do
  -- constants
  let nk = nk128 -- 4
  let nr = nr128 -- 10
  -- create a array of `4 * (nr + 1)` words
  let arr0 = listArray (0, 4 * (nr + 1) - 1) (replicate (4 * (nr + 1)) ((0, 0, 0, 0) :: UIntWord))
  -- the first `Nk` words are the key itself
  arr1 <- updateUIntWord arr0 k0 0
  arr2 <- updateUIntWord arr1 k1 1
  arr3 <- updateUIntWord arr2 k2 2
  arr4 <- updateUIntWord arr3 k3 3
  -- followed by a loop over i from 4 to 43
  arr5 <-
    foldM
      ( \arr i -> do
          let temp = arr ! (i - 1)
          temp1 <-
            if i `mod` nk == 0
              then do
                -- temp ← SUBWORD(ROTWORD(temp)) ⊕ Rcon[i/Nk]
                temp' <- subUIntWord (rotWord temp)
                return $ temp' `xorUIntWord` (rCon !! ((i `div` nk) - 1))
              else
                if nk > 6 && i `mod` nk == 4
                  then do
                    -- temp ← SUBWORD(temp)
                    subUIntWord temp
                  else do
                    return temp
          -- w[i] ← w[i−Nk]⊕temp
          let wink = arr ! (i - nk)
          updateUIntWord arr (temp1 `xorUIntWord` wink) i
      )
      arr4
      [4 .. 4 * nr + 3]
  return $ elems arr5

-- | Convert a FieldWord to a FieldWord
toUIntWord :: FieldWord -> Comp UIntWord
toUIntWord = mapTupleM (toUInt 8)

-- | Convert a UIntWord to a FieldWord
fromUIntWord :: UIntWord -> Comp FieldWord
fromUIntWord = mapTupleM toField

mapTupleM :: (a -> Comp b) -> Tuple a -> Comp (Tuple b)
mapTupleM f (a0, a1, a2, a3) = do
  b0 <- f a0
  b1 <- f a1
  b2 <- f a2
  b3 <- f a3
  return (b0, b1, b2, b3)

-- | XOR on UIntWords
xorUIntWord :: UIntWord -> UIntWord -> UIntWord
xorUIntWord (a0, a1, a2, a3) (b0, b1, b2, b3) = (a0 .^. b0, a1 .^. b1, a2 .^. b2, a3 .^. b3)

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

-- | ADDROUNDKEY
addRoundKey :: [UIntWord] -> Int -> Tuple UIntWord -> Tuple UIntWord
addRoundKey w round (c0, c1, c2, c3) =
  let l = 4 * round
   in (c0 `xorUIntWord` (w !! l), c1 `xorUIntWord` (w !! (l + 1)), c2 `xorUIntWord` (w !! (l + 2)), c3 `xorUIntWord` (w !! (l + 3)))

-- | AES
--    input: a tuple of 16 bytes (columns of rows of bytes)
--    output: a tuple of 16 bytes (columns of rows of bytes)
cipher128 :: Tuple UIntWord -> Comp (Tuple UIntWord)
cipher128 plaintext = do
  let nr = nr128
  keys <- keyExpansion128 plaintext
  let state = addRoundKey keys 0 plaintext
  state' <-
    foldM
      ( \st round -> do
          st' <- subBytes st
          st'' <- mapTupleM fromUIntWord (shiftRows st')
          st''' <- mapTupleM toUIntWord (mixColumns st'')
          return $ addRoundKey keys round st'''
      )
      state
      [1 .. nr - 1]
  state'' <- subBytes state'
  let state''' = shiftRows state''
  return $ addRoundKey keys nr state'''

runCipher128 :: Comp [Byte]
runCipher128 = do
  inputs <- inputList Public 16
  ((output0, output1, output2, output3), (output4, output5, output6, output7), (output8, output9, output10, output11), (output12, output13, output14, output15)) <- cipher128 ((inputs !! 0, inputs !! 1, inputs !! 2, inputs !! 3), (inputs !! 4, inputs !! 5, inputs !! 6, inputs !! 7), (inputs !! 8, inputs !! 9, inputs !! 10, inputs !! 11), (inputs !! 12, inputs !! 13, inputs !! 14, inputs !! 15))
  return [output0, output1, output2, output3, output4, output5, output6, output7, output8, output9, output10, output11, output12, output13, output14, output15]

_compileAndReportNumbers :: (Encode a) => FieldType -> Comp a -> IO (Maybe (Int, Counters))
_compileAndReportNumbers field program = do
  result <- compile field program
  case result of
    Left _ -> return Nothing
    Right r1cs -> return $ Just (length (r1csConstraints r1cs), r1csCounters r1cs)
