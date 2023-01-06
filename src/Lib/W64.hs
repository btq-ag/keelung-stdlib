{-# LANGUAGE DataKinds #-}

module Lib.W64 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word64)
import GHC.Natural
import Keelung
import qualified Lib.ArrayM as ArrayM
import Lib.W8 (W8M)
type W64M = ArrM Boolean

type W64 = Arr Boolean

fromWord64 :: Word64 -> Comp W64M
fromWord64 word = toArrayM $ map (Boolean . testBit word) [0 .. 63]

zero :: Comp W64M
zero = ArrayM.zeroBits 64

zeros :: Int -> Comp (ArrM W64M)
zeros n = zero >>= ArrayM.replicate n

-- | Rotates right by i bits if i is positive, or right by -i bits otherwise.
rotateR :: Natural -> W64M -> Comp W64M
rotateR = ArrayM.rotateR

add :: W64M -> W64M -> Comp W64M
add = ArrayM.fullAdder

xor :: W64M -> W64M -> Comp W64M
xor = ArrayM.xor

complement :: W64M -> Comp W64M
complement = ArrayM.map Keelung.complement

equal :: W64M -> W64M -> Comp Boolean
equal = ArrayM.beq 64

fromW8Chunks :: ArrM W8M -> Comp (ArrM W64M)
fromW8Chunks = ArrayM.flatten >=> ArrayM.chunks 64

toW8Chunks :: ArrM W64M -> Comp (ArrM W8M)
toW8Chunks = ArrayM.flatten >=> ArrayM.chunks 8
