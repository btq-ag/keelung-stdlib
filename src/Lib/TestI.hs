{-# LANGUAGE DataKinds #-}
module Lib.TestI where

import Keelung hiding (access, update)
import qualified Lib.Array as Array
import Lib.W8 as W8

testAdder :: Comp (Val 'Unit)
testAdder = do
    let x = W8.fromWord8' 0xa4
        y = W8.fromWord8' 0x89

    let ref = W8.fromWord8' (0xa4+0x89)

    assert $ W8.equal' ref (W8.add' x y)

    return unit
