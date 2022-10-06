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
import GHC.Natural

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

toW8Chunks :: ArrM W32M -> Comp (ArrM W8M)
toW8Chunks = ArrayM.flatten >=> ArrayM.chunks 8

-- | Rotates right by i bits if i is positive, or right by -i bits otherwise.
rotateR :: Natural -> W32M -> Comp W32M
rotateR = ArrayM.rotateR

add :: W32M -> W32M -> Comp W32M
add = ArrayM.fullAdder

xor :: W32M -> W32M -> Comp W32M
xor = ArrayM.xorOld 32

complement :: W32M -> Comp W32M
complement = ArrayM.map neg