{-# LANGUAGE DataKinds #-}

module Lib.W64 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word64)
import Keelung
import qualified Lib.Array as Arr

import Lib.W8 (W8)
import GHC.Natural


type W64 = 'Arr 'Bool

fromWord64 :: Word64 -> Comp n (Val W64 n)
fromWord64 word = toArray $ map (Boolean . testBit word) [0 .. 63]

zero :: Comp n (Val W64 n)
zero = Arr.zeroBits 64

zeros :: Int -> Comp n (Val ('Arr W64) n)
zeros n = zero >>= Arr.replicate n

-- | Rotates right by i bits if i is positive, or right by -i bits otherwise.
rotateR :: Natural -> Val W64 n -> Comp n (Val W64 n)
rotateR = Arr.rotateR

add :: Val W64 n -> Val W64 n -> Comp n (Val W64 n)
add = Arr.fullAdder 64

xor :: Val W64 n -> Val W64 n -> Comp n (Val W64 n)
xor = Arr.xorOld 64

complement :: Val W64 n -> Comp n (Val W64 n)
complement = Arr.map neg

equal :: Val W64 n -> Val W64 n -> Comp n (Val 'Bool n)
equal = Arr.beq 64

fromW8Chunks :: Val ('Arr W8) n -> Comp n (Val ('Arr W64) n)
fromW8Chunks = Arr.flatten >=> Arr.chunks 64

toW8Chunks :: Val ('Arr W64) n -> Comp n (Val ('Arr W8) n)
toW8Chunks = Arr.flatten >=> Arr.chunks 8
