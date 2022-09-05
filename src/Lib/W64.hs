{-# LANGUAGE DataKinds #-}

module Lib.W64 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word64)
import Keelung
import qualified Lib.Array as Arr

import Lib.W8 (W8, W8M)
import GHC.Natural


type W64M = 'ArrM 'Bool
type W64 = 'Arr 'Bool

fromWord64 :: Word64 -> Comp (Val W64M)
fromWord64 word = toArrayM $ map (Boolean . testBit word) [0 .. 63]

zero :: Comp (Val W64M)
zero = Arr.zeroBits 64

zeros :: Int -> Comp (Val ('ArrM W64M))
zeros n = zero >>= Arr.replicate n

-- | Rotates right by i bits if i is positive, or right by -i bits otherwise.
rotateR :: Natural -> Val W64M -> Comp (Val W64M)
rotateR = Arr.rotateR

add :: Val W64M -> Val W64M -> Comp (Val W64M)
add = Arr.fullAdder 64

xor :: Val W64M -> Val W64M -> Comp (Val W64M)
xor = Arr.xorOld 64

complement :: Val W64M -> Comp (Val W64M)
complement = Arr.map neg

equal :: Val W64M -> Val W64M -> Comp (Val 'Bool)
equal = Arr.beq 64

fromW8Chunks :: Val ('ArrM W8M) -> Comp (Val ('ArrM W64M))
fromW8Chunks = Arr.flatten >=> Arr.chunks 64

toW8Chunks :: Val ('ArrM W64M) -> Comp (Val ('ArrM W8M))
toW8Chunks = Arr.flatten >=> Arr.chunks 8
