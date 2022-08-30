{-# LANGUAGE DataKinds #-}

module Lib.Test where

import Keelung
import qualified Lib.W8 as W8
import qualified Lib.ArrayI as ArrayI
import qualified Lib.W32 as W32

testBE :: Comp GF181 (Val 'Unit GF181)
testBE = do
  let message = "abcdefgh"
  message' <- W8.fromString message

  m <- W8.toWordNBE 32 message'

  ref <- W32.fromWord32 0x61626364
  assert =<< W32.equal ref =<< access m 0

  ref <- W32.fromWord32 0x65666768
  assert =<< W32.equal ref =<< access m 1

  m' <- W8.fromWordNBE m

  ref <- W8.fromWord8 0x61
  assert =<< W8.equal ref =<< access m' 0

  ref <- W8.fromWord8 0x62
  assert =<< W8.equal ref =<< access m' 1

  ref <- W8.fromWord8 0x63
  assert =<< W8.equal ref =<< access m' 2

  ref <- W8.fromWord8 0x64
  assert =<< W8.equal ref =<< access m' 3

  ref <- W8.fromWord8 0x65
  assert =<< W8.equal ref =<< access m' 4

  ref <- W8.fromWord8 0x66
  assert =<< W8.equal ref =<< access m' 5

  ref <- W8.fromWord8 0x67
  assert =<< W8.equal ref =<< access m' 6

  ref <- W8.fromWord8 0x68
  assert =<< W8.equal ref =<< access m' 7

  return unit