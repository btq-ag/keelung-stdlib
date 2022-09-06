{-# LANGUAGE DataKinds #-}

module Lib.W8 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word8)
import Keelung
import qualified Lib.Array as Arr
import qualified Lib.ArrayI as ArrI
import Numeric (readHex)

type W8 = 'ArrM 'Bool

type W8' = 'Arr 'Bool

-- | Construct a W8 from a Word8
fromWord8 :: Word8 -> Comp (Val W8)
fromWord8 word = toArrayM $ Prelude.map (Boolean . testBit word) [0 .. 7]

-- | Construct a W8 from a Char
fromChar :: Char -> Comp (Val W8)
fromChar = fromWord8 . toEnum . fromEnum

-- | Construct an array of W8s from a String
fromString :: String -> Comp (Val ('ArrM W8))
fromString xs = mapM fromChar xs >>= toArrayM

-- | Equality on W8
equal :: Val W8 -> Val W8 -> Comp (Val 'Bool)
equal = Arr.beq 8

zero :: Comp (Val W8)
zero = Arr.zeroBits 8

zeros :: Int -> Comp (Val ('ArrM W8))
zeros n = zero >>= Arr.replicate n

----

-- | `fromWord8` implemented with immutable arrays
fromWord8' :: Word8 -> Val ('Arr 'Bool)
fromWord8' word = toArray $ Prelude.map (Boolean . testBit word) [0 .. 7]

-- | `fromChar` implemented with immutable arrays
fromChar' :: Char -> Val ('Arr 'Bool)
fromChar' = fromWord8' . toEnum . fromEnum

-- | `fromString` implemented with immutable arrays
fromString' :: String -> Val ('Arr ('Arr 'Bool))
fromString' = toArray . map fromChar'

-- [A, B, C, D, ...] -> [[D C B A], ...]
toWordNBE :: Int -> Val ('Arr ('Arr 'Bool)) -> Val ('Arr ('Arr 'Bool))
toWordNBE n = ArrI.chunks n . ArrI.flatten . ArrI.chunkReverse (n `div` 8)

-- [[D C B A], ...] -> [A, B, C, D, ...]
fromWordNBE :: Val ('Arr ('Arr 'Bool)) -> Val ('Arr ('Arr 'Bool))
fromWordNBE xs =
  let n = lengthOf $ head (fromArray xs)
      xs' = ArrI.chunks 8 (ArrI.flatten xs)
   in ArrI.chunkReverse (n `div` 8) xs'

-- ---

equal' :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val 'Bool
equal' = ArrI.beq

add' :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
add' = ArrI.fullAdder