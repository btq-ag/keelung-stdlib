{-# LANGUAGE DataKinds #-}

module Lib.W8 where

import Control.Monad
import Data.Bits (Bits (testBit))
import Data.Word (Word8)
import Keelung
import qualified Lib.Array as Arr
import qualified Lib.ArrayI as ArrI
import Numeric (readHex)

type W8 = 'Arr 'Bool

-- | Construct a W8 from a Word8
fromWord8 :: Word8 -> Comp n (Val W8 n)
fromWord8 word = toArray $ Prelude.map (Boolean . testBit word) [0 .. 7]

-- | Construct a W8 from a Char
fromChar :: Char -> Comp n (Val W8 n)
fromChar = fromWord8 . toEnum . fromEnum

-- | Construct an array of W8s from a String
fromString :: String -> Comp n (Val ('Arr W8) n)
fromString xs = mapM fromChar xs >>= toArray

-- | Equality on W8
equal :: Val W8 n -> Val W8 n -> Comp n (Val 'Bool n)
equal = Arr.beq 8

zero :: Comp n (Val W8 n)
zero = Arr.zeroBits 8

zeros :: Int -> Comp n (Val ('Arr W8) n)
zeros n = zero >>= Arr.replicate n

----

-- | `fromWord8` implemented with immutable arrays
fromWord8' :: Word8 -> Val W8 n
fromWord8' word = toArrayI $ Prelude.map (Boolean . testBit word) [0 .. 7]

-- | `fromChar` implemented with immutable arrays
fromChar' :: Char -> Val W8 n
fromChar' = fromWord8' . toEnum . fromEnum

-- | `fromString` implemented with immutable arrays
fromString' :: String -> Val ('Arr W8) n
fromString' = toArrayI . map fromChar'

----

-- [A, B, C, D, ...] -> [[D C B A], ...]
toWordNBE' :: Int -> Val ('Arr W8) n -> Comp n (Val ('Arr ('Arr 'Bool)) n)
toWordNBE' n = ArrI.chunkReverse (n `div` 8) >=> ArrI.flatten >=> ArrI.chunks n

-- [[D C B A], ...] -> [A, B, C, D, ...]
fromWordNBE' :: Val ('Arr ('Arr 'Bool)) n -> Comp n (Val ('Arr W8) n)
fromWordNBE' xs = do
    n <- lengthOf . head <$> fromArray xs
    xs' <- ArrI.flatten xs >>= ArrI.chunks 8
    ArrI.chunkReverse (n `div` 8) xs'

-- fromWordNBE' === fromWordNBE
-- toWordNBE' (n*4) === toWordNBE' n

fromWordNBE :: Val ('Arr ('Arr 'Bool)) n -> Comp n (Val ('Arr W8) n)
fromWordNBE xs = do
    xs' <- fromArray xs
    tt <- mapM toWord8LE xs'
    ttt <- mapM toArray (concat tt)
    toArray ttt
    where
        toWord8LE :: Val ('Arr 'Bool) n -> Comp n [[Val 'Bool n]]
        toWord8LE = fmap (Prelude.reverse . group 8) . fromArray

toWordNBE :: Int -> Val ('Arr W8) n -> Comp n (Val ('Arr ('Arr 'Bool)) n)
toWordNBE n xs = do
    xs' <- group (n `div` 8) <$> fromArray xs
    let t = Prelude.map Prelude.reverse xs'
    toArray =<< mapM aggregate t
  where
    aggregate :: [Val W8 n] -> Comp n (Val ('Arr 'Bool) n)
    aggregate xs = do
      xss <- mapM fromArray xs
      toArray $ concat xss

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"