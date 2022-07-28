{-# LANGUAGE DataKinds #-}

module Lib.W8 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word8)
import Keelung
import qualified Lib.Array as Array

type W8 = 'Arr 'Bool

-- | Construct a W8 from a Word8
fromWord8 :: Word8 -> Comp n (Val W8 n)
fromWord8 word = toArray $ Prelude.map (Boolean . testBit word) [0 .. 8]

-- | Construct a W8 from a Char
fromChar :: Char -> Comp n (Val W8 n)
fromChar = fromWord8 . toEnum . fromEnum

-- | Construct an array of W8s from a String  
fromString :: String -> Comp n (Val ('Arr W8) n)
fromString xs = mapM fromChar xs >>= toArray

-- | Equality on W8 
equal :: Val W8 n -> Val W8 n -> Comp n (Val 'Bool n)
equal = Array.beq 8
