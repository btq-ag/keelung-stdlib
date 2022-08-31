{-# LANGUAGE DataKinds #-}

module Lib.W8 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word8)
import Keelung
import qualified Lib.Array as Arr
import Numeric (readHex)

type W8 = 'ArrM 'Bool

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

