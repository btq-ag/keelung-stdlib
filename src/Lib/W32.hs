{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Lib.W32 where

import Control.Monad
import Data.Bits (Bits (testBit), (.|.), shiftL)
import Data.Word (Word8, Word32)
import Data.List (foldl')
import Keelung hiding (shiftL, (.|.))
import qualified Lib.ArrayM as ArrayM
import qualified Lib.Array as Array
import Lib.W8 (W8, W8M)
import GHC.Natural

type W32M = ArrM Boolean

type W32 = Arr Boolean

fromWord32 :: Word32 -> Comp W32M
fromWord32 word = toArrayM $ map (Boolean . testBit word) [0 .. 31]

fromWord32List :: [Word32] -> Comp (ArrM W32M)
fromWord32List ws = mapM fromWord32 ws >>= toArrayM

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

-- --------------------------------------------------------------

fromW8Chunks' :: Arr W8 -> Arr W32
fromW8Chunks' = Array.chunks 32 . Array.concat

fromWord32' :: Word32 -> W32
fromWord32' w = toArray $ Prelude.map (Boolean . testBit w) [0 .. 31]

fromString' :: String -> Arr W32
fromString' =
    toArray . map (fromWord32' . fromOctets) . Array.group 4 . map (toEnum . fromEnum)

fromWord32List' :: [Word32] -> Arr W32
fromWord32List' = toArray . Prelude.map fromWord32'

---------------------------------------------------------

fromOctets :: [Word8] -> Word32
fromOctets = foldl' accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o

toW8Chunks :: ArrM W32M -> Comp (ArrM W8M)
toW8Chunks = ArrayM.flatten >=> ArrayM.chunks 8

-- | Rotates right by i bits if i is positive, or right by -i bits otherwise.
rotateR :: Natural -> W32M -> Comp W32M
rotateR = ArrayM.rotateR

add :: W32M -> W32M -> Comp W32M
add = ArrayM.fullAdder

xor :: W32M -> W32M -> Comp W32M
xor = ArrayM.xor

complement :: W32M -> Comp W32M
complement = ArrayM.map Keelung.complement
