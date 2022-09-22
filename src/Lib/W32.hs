{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Lib.W32 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word32)
import Keelung
import qualified Lib.ArrayM as ArrayM
import Lib.W8 (W8, W8M)
import qualified Lib.W8 as W8

type W32M = 'ArrM 'Bool
type W32 = 'Arr 'Bool

fromWord32 :: Word32 -> Comp (Val W32M)
fromWord32 word = toArrayM $ map (Boolean . testBit word) [0 .. 31]

equal :: Val W32M -> Val W32M -> Comp (Val 'Bool)
equal = ArrayM.beq 32

zero :: Comp (Val W32M)
zero = ArrayM.zeroBits 32

zeros :: Int -> Comp (Val ('ArrM W32M))
zeros n = zero >>= ArrayM.replicate n

fromW8 :: Val W8M -> Comp (Val W32M)
fromW8 = ArrayM.cast 32

fromW8Chunks :: Val ('ArrM W8M) -> Comp (Val ('ArrM W32M))
fromW8Chunks = ArrayM.flatten >=> ArrayM.chunks 32

-- fromW8Chunks' :: Val ('Arr W8) -> Val ('Arr W32)
-- fromW8Chunks' = W8.toWordNBE' 32

-- fromWord32' :: Word32 -> Val W32
-- fromWord32' w = toArray $ Prelude.map (Boolean . testBit w) [0 .. 31]

-- fromWord32List' :: [Word32] -> Val ('Arr W32)
-- fromWord32List' = toArray . map fromWord32'
