{-# LANGUAGE DataKinds #-}

module W8 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word8)
import Keelung

type W8 = 'Arr 'Bool

fromWord8 :: Word8 -> Comp n (Expr W8 n)
fromWord8 word = toArray $ map (Val . Boolean . testBit word) [0 .. 8]
