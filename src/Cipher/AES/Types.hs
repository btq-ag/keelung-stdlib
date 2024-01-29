{-# LANGUAGE DataKinds #-}

module Cipher.AES.Types where

import Keelung

type Byte = UInt 8

type State = (FieldWord, FieldWord, FieldWord, FieldWord)


-- | Word made of 4 Field elements
type FieldWord = (Field, Field, Field, Field)

-- | Word made of 4 Bytes (UInt 8)
type UIntWord = (Byte, Byte, Byte, Byte)
