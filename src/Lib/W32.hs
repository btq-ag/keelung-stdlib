{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Lib.W32 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word32)
import Keelung
import qualified Lib.Array as Arr
import Lib.W8 (W8)

type W32 = 'ArrM 'Bool

fromWord32 :: Word32 -> Comp (Val W32)
fromWord32 word = toArrayM $ map (Boolean . testBit word) [0 .. 31]

zero :: Comp (Val W32)
zero = Arr.zeroBits 32

zeros :: Int -> Comp (Val ('ArrM W32))
zeros n = zero >>= Arr.replicate n

fromW8 :: Val W8 -> Comp (Val W32)
fromW8 = Arr.cast 32

fromW8Chunks :: Val ('ArrM W8) -> Comp (Val ('ArrM W32))
fromW8Chunks = Arr.flatten >=> Arr.chunks 32
