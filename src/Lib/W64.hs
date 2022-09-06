{-# LANGUAGE DataKinds #-}

module Lib.W64 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word64)
import Keelung
import qualified Lib.Array as Arr

import Lib.W8 (W8)
import GHC.Natural


type W64 = 'ArrM 'Bool

fromWord64 :: Word64 -> Comp (Val W64)
fromWord64 word = toArrayM $ map (Boolean . testBit word) [0 .. 63]

zero :: Comp (Val W64)
zero = Arr.zeroBits 64

zeros :: Int -> Comp (Val ('ArrM W64))
zeros n = zero >>= Arr.replicate n

-- | Rotates right by i bits if i is positive, or right by -i bits otherwise.
rotateR :: Natural -> Val W64 -> Comp (Val W64)
rotateR = Arr.rotateR

add :: Val W64 -> Val W64 -> Comp (Val W64)
add = Arr.fullAdder 64

xor :: Val W64 -> Val W64 -> Comp (Val W64)
xor = Arr.xorOld 64

complement :: Val W64 -> Comp (Val W64)
complement = Arr.map neg

equal :: Val W64 -> Val W64 -> Comp (Val 'Bool)
equal = Arr.beq 64

fromW8Chunks :: Val ('ArrM W8) -> Comp (Val ('ArrM W64))
fromW8Chunks = Arr.flatten >=> Arr.chunks 64

toW8Chunks :: Val ('ArrM W64) -> Comp (Val ('ArrM W8))
toW8Chunks = Arr.flatten >=> Arr.chunks 8
