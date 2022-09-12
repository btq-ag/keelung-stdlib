{-# LANGUAGE DataKinds #-}

module Lib.W8 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word8)
import Keelung
import qualified Lib.ArrayM as ArrayM
import qualified Lib.Array as Array


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
equal = ArrayM.beq 8

zero :: Comp (Val W8M)
zero = ArrayM.zeroBits 8

zeros :: Int -> Comp (Val ('ArrM W8M))
zeros n = zero >>= ArrayM.replicate n

toW8Chunks :: Val ('ArrM ('ArrM 'Bool)) -> Comp (Val ('ArrM W8M))
toW8Chunks = ArrayM.flatten >=> ArrayM.chunks 8

----
zero' :: Val W8
zero' = Array.zeroBits 8

zeros' :: Int -> Val ('Arr W8)
zeros' n = Array.replicate n zero'

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
toWordNBE' n = Array.map Array.concat . Array.chunkReverse (n `div` 8)

-- [[D C B A], ...] -> [A, B, C, D, ...]
fromWordNBE' :: Val ('Arr ('Arr 'Bool)) -> Val ('Arr W8)
fromWordNBE' xs =
    let n = lengthOf xs in
    let xs' = Array.chunks 8 (Array.concat xs) in
    Array.concat $ Array.chunkReverse (n `div` 8) xs'

toW8Chunks' :: Val ('Arr ('Arr 'Bool)) -> Val ('Arr W8)
toW8Chunks' = Array.chunks 8 . Array.concat

---
pad' :: Int -> Val ('Arr W8) -> Val ('Arr W8)
pad' len xs =
    let len' = lengthOf xs in
    let p = zeros' (len - len' `mod` len) in
    Array.concatenate xs p

equal' :: Val W8 -> Val W8 -> Val 'Bool
equal' = Array.beq

add' :: Val W8 -> Val W8 -> Val W8
add' = Array.fullAdder
