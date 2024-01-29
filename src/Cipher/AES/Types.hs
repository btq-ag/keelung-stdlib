{-# LANGUAGE DataKinds #-}

module Cipher.AES.Types where

import Keelung

type Byte = UInt 8

type State = (FieldWord, FieldWord, FieldWord, FieldWord)

type Tuple a = (a, a, a, a)

-- | Word made of 4 Field elements
type FieldWord = Tuple Field

-- | Word made of 4 Bytes (UInt 8)
type UIntWord = Tuple Byte
