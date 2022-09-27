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

type W32M = ArrM Boolean

type W32 = Arr Boolean

fromWord32 :: Word32 -> Comp W32M
fromWord32 word = toArrayM $ map (Boolean . testBit word) [0 .. 31]

equal :: W32M -> W32M -> Comp Boolean
equal = ArrayM.beq 32

zero :: Comp W32M
zero = ArrayM.zeroBits 32

zeros :: Int -> Comp (ArrM W32M)
zeros n = zero >>= ArrayM.replicate n

fromW8 :: W8M -> Comp W32M
fromW8 = ArrayM.cast 32

fromW8Chunks :: ArrM W8M -> Comp (ArrM W32M)
fromW8Chunks = ArrayM.flatten >=> ArrayM.chunks 32

-- fromW8Chunks' :: ('Arr W8) -> ('Arr W32)
-- fromW8Chunks' = W8.toWordNBE' 32

-- fromWord32' :: Word32 -> W32
-- fromWord32' w = toArray $ Prelude.map (Boolean . testBit w) [0 .. 31]

-- fromWord32List' :: [Word32] -> ('Arr W32)
-- fromWord32List' = toArray . map fromWord32'
