{-# LANGUAGE DataKinds #-}

module Lib.W8 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word8)
import Keelung
import qualified Lib.Array as Arr
import qualified Lib.ArrayI as ArrI
import Numeric (readHex)

type W8M = 'ArrM 'Bool
type W8 = 'Arr 'Bool

-- | Construct a W8 from a Word8
fromWord8 :: Word8 -> Comp (Val W8M)
fromWord8 word = toArrayM $ Prelude.map (Boolean . testBit word) [0 .. 7]

-- | Construct a W8 from a Char
fromChar :: Char -> Comp (Val W8M)
fromChar = fromWord8 . toEnum . fromEnum

-- | Construct an array of W8s from a String
fromString :: String -> Comp (Val ('ArrM W8M))
fromString xs = mapM fromChar xs >>= toArrayM

-- | Equality on W8
equal :: Val W8M -> Val W8M -> Comp (Val 'Bool)
equal = Arr.beq 8

zero :: Comp (Val W8M)
zero = Arr.zeroBits 8

zeros :: Int -> Comp (Val ('ArrM W8M))
zeros n = zero >>= Arr.replicate n

toW8Chunks :: Val ('ArrM ('ArrM 'Bool)) -> Comp (Val ('ArrM W8M))
toW8Chunks = Arr.flatten >=> Arr.chunks 8

----
zero' :: Val W8
zero' = ArrI.zeroBits 8

zeros' :: Int -> Val ('Arr W8)
zeros' n = ArrI.replicate n zero'

-- | `fromWord8` implemented with immutable arrays
fromWord8' :: Word8 -> Val W8
fromWord8' word = toArray $ Prelude.map (Boolean . testBit word) [0 .. 7]

-- | `fromChar` implemented with immutable arrays
fromChar' :: Char -> Val W8
fromChar' = fromWord8' . toEnum . fromEnum

-- | `fromString` implemented with immutable arrays
fromString' :: String -> Val ('Arr W8)
fromString' = toArray . map fromChar'

-- [A, B, C, D, ...] -> [[D C B A], ...]
toWordNBE' :: Int -> Val ('Arr W8) -> Val ('Arr ('Arr 'Bool))
toWordNBE' n = ArrI.chunkReverse (n `div` 8) . ArrI.concat . ArrI.chunks n

-- [[D C B A], ...] -> [A, B, C, D, ...]
fromWordNBE' :: Val ('Arr ('Arr 'Bool)) -> Val ('Arr W8)
fromWordNBE' xs =
    let n = lengthOf xs in
    let xs' = ArrI.chunks 8 (ArrI.concat xs) in
    ArrI.chunkReverse (n `div` 8) xs'

toW8Chunks' :: Val ('Arr ('Arr 'Bool)) -> Val ('Arr W8)
toW8Chunks' = ArrI.chunks 8 . ArrI.concat

---
pad' :: Val ('Arr W8) -> Int -> Val ('Arr W8)
pad' xs len =
    let len' = lengthOf xs in
    let p = zeros' (len - len' `mod` len) in
    ArrI.concatenate xs p

equal' :: Val W8 -> Val W8 -> Val 'Bool
equal' = ArrI.beq

add' :: Val W8 -> Val W8 -> Val W8
add' = ArrI.fullAdder
