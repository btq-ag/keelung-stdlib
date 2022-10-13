{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module BLAKE2s where

import Control.Monad
import Data.Bits
import Data.Function ((&))
import Data.Word
import Keelung
import Lib.Array ((>.>))
import qualified Lib.Array as Array
import Lib.W32 (W32)
import qualified Lib.W32 as W32
import Lib.W8 (W8)
import qualified Lib.W8 as W8

--               | BLAKE2b          | BLAKE2s          |
-- --------------+------------------+------------------+
--  Bits in word | w = 64           | w = 32           |
--  Rounds in F  | r = 12           | r = 10           |
--  Block bytes  | bb = 128         | bb = 64          |
--  Hash bytes   | 1 <= nn <= 64    | 1 <= nn <= 32    |
--  Key bytes    | 0 <= kk <= 64    | 0 <= kk <= 32    |
--  Input bytes  | 0 <= ll < 2**128 | 0 <= ll < 2**64  |
-- --------------+------------------+------------------+
--  G Rotation   | (R1, R2, R3, R4) | (R1, R2, R3, R4) |
--   constants = | (32, 24, 16, 63) | (16, 12,  8,  7) |
-- --------------+------------------+------------------+

iv :: [Word32]
iv =
  [ 0x6a09e667,
    0xbb67ae85,
    0x3c6ef372,
    0xa54ff53a,
    0x510e527f,
    0x9b05688c,
    0x1f83d9ab,
    0x5be0cd19
  ]

sigma :: [[Int]]
sigma =
  [ [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
    [14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3],
    [11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4],
    [7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8],
    [9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13],
    [2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9],
    [12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11],
    [13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10],
    [6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5],
    [10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0],
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
    [14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3]
  ]

bb :: Int
bb = 64

hash :: Arr W8 -> Word64 -> Word32 -> Word32 -> Comp (Arr W8)
hash xs ll kk nn =
  let xs' = (Array.chunks 16 . W32.fromW8Chunks' . W8.pad' 64) xs
   in blake2 xs' ll kk nn

blake2 :: Arr (Arr W32) -> Word64 -> Word32 -> Word32 -> Comp (Arr W8)
blake2 d ll kk nn = do
  let dd = length d
  let d' = fromArray d

  h <-
    applyWhenM
      (dd > 1)
      ( \h' ->
          foldM
            (\h' (di, i) -> compress di (fromIntegral (i + 1) * fromIntegral bb) False h')
            h'
            (zip d' [0 .. dd - 2])
      )
      >=> ( if kk == 0
              then compress (last d') ll True
              else compress (last d') (ll + fromIntegral bb) True
          )
      $ ih kk nn

  return $ Array.take (fromIntegral nn) (W8.toW8Chunks' h)

-- init state
ih :: Word32 -> Word32 -> Arr W32
ih kk nn =
  W32.fromWord32List'
    >.> Array.update' 0 (Array.xor . W32.fromWord32' $ 0x01010000 `xor` shiftL kk 8 `xor` nn)
    $ take 8 iv

compress :: Arr W32 -> Word64 -> Bool -> Arr W32 -> Comp (Arr W32)
compress m t f h = do
  v <- compress' 10 m t f h

  return $
    foldl
      ( \h' i -> do
          let vi = access v i
          let vj = access v (i + 8)
          Array.update' i (Array.xor vi . Array.xor vj) h'
      )
      h
      [0 .. 7]

compress' :: Int -> Arr W32 -> Word64 -> Bool -> Arr W32 -> Comp (Arr W32)
compress' r m t f h = do
  let v =
        Array.concatenate (Array.take 8 h) . W32.fromWord32List'
          >.> (Array.update' 12 . Array.xor . W32.fromWord32' . word64lo32) t
          >.> (Array.update' 13 . Array.xor . W32.fromWord32' . word64hi32) t
          >.> applyWhen f (Array.update' 14 . Array.xor . W32.fromWord32' $ 0xFFFFFFFF)
          $ take 8 iv

  foldM
    ( \v' i ->
        let si = sigma !! i
         in mix 0 4 8 12 m (si !! 0) (si !! 1)
              >=> mix 1 5 9 13 m (si !! 2) (si !! 3)
              >=> mix 2 6 10 14 m (si !! 4) (si !! 5)
              >=> mix 3 7 11 15 m (si !! 6) (si !! 7)
              >=> mix 0 5 10 15 m (si !! 8) (si !! 9)
              >=> mix 1 6 11 12 m (si !! 10) (si !! 11)
              >=> mix 2 7 8 13 m (si !! 12) (si !! 13)
              >=> mix 3 4 9 14 m (si !! 14) (si !! 15)
              $ v'
    )
    v
    [0 .. r - 1]

mix :: Int -> Int -> Int -> Int -> Arr W32 -> Int -> Int -> Arr W32 -> Comp (Arr W32)
mix a b c d msg xi yi v = do
  let va = access v a
  let vb = access v b
  let vc = access v c
  let vd = access v d

  let x = access msg xi
  let y = access msg yi

  va' <- va `Array.fullAdder` vb >>= Array.fullAdder x
  let vd' = vd `Array.xor` va' & Array.rotateR r1
  vc' <- vc `Array.fullAdder` vd'
  let vb' = vb `Array.xor` vc' & Array.rotateR r2

  va'' <- va' `Array.fullAdder` vb' >>= Array.fullAdder y
  let vd'' = vd' `Array.xor` va'' & Array.rotateR r3
  vc'' <- vc' `Array.fullAdder` vd''
  let vb'' = vb' `Array.xor` vc'' & Array.rotateR r4

  return $
    Array.update a va''
      >.> Array.update b vb''
      >.> Array.update c vc''
      >.> Array.update d vd''
      $ v
  where
    (r1, r2, r3, r4) = (16, 12, 8, 7)

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen t f x = if t then f x else x

applyWhenM :: Applicative m => Bool -> (a -> m a) -> a -> m a
applyWhenM t f x = if t then f x else pure x

word64hi32 :: Word64 -> Word32
word64hi32 x = fromIntegral $ shift x (-32)

word64lo32 :: Word64 -> Word32
word64lo32 = fromIntegral
