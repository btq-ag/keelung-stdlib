{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Lib.W32 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word32)
import Keelung
import qualified Lib.Array as Arr
import Lib.W8 (W8)

type W32 = 'Arr 'Bool

fromWord32 :: Word32 -> Comp n (Val W32 n)
fromWord32 word = toArray $ map (Boolean . testBit word) [0 .. 31]

zero :: Comp n (Val W32 n)
zero = Arr.zeroBits 32

zeros :: Int -> Comp n (Val ('Arr W32) n)
zeros n = zero >>= Arr.replicate n

fromW8 :: Val W8 n -> Comp n (Val W32 n)
fromW8 = Arr.cast 32

fromW8Chunks :: Val ('Arr W8) n -> Comp n (Val ('Arr W32) n)
fromW8Chunks = Arr.flatten >=> Arr.chunks 32

equal :: Val W32 n -> Val W32 n -> Comp n (Val 'Bool n)
equal = Arr.beq 32

complement :: Val W32 n -> Comp n (Val W32 n)
complement = Arr.map neg