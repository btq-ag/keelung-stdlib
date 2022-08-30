{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module SHA256 where

import Control.Monad

import qualified Crypto.Hash.SHA256
import Data.Bits
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Char
import Data.Int
import Data.Word
import Debug.Trace
import qualified GHC.Generics as W8
import Keelung
import Lib.Array (beq)
import qualified Lib.Array as Array
import Lib.W64 (W64)
import qualified Lib.W64 as W64
import Lib.W8 (W8)
import qualified Lib.W8 as W8
import qualified Keelung.Constraint.Polynomial as W8
import qualified Lib.W32 as W32
import Lib.W32 (W32)

-- | Initialization vector
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

k :: [Word32]
k =
  [ 0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
    0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
    0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
    0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
    0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
    0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
    0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
    0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2
  ]

-- SHA-256 wants to do add/rot/shift in BE, but we implement W8/W32/W64 in LE,
-- we can either
-- 1. make everything BE, write something like W8BE, W32BE, W64BE
-- 2. use the convention and design helpers:
--      W8/W32/W64 itself is in LE
--      we need to convert [W32/64 in LE to W8 in LE] in BE
--      meaning that W32LE(0xaabbccdd) <-> [W8LE(0xaa), W8LE(0xbb), w8LE(0xcc), W8LE(0xdd)]
--
-- We chose 2. Results: W8.fromWordNBE, W8.fromWordNBE m

test :: Comp GF181 (Val 'Unit GF181)
test = do
  let message = concat $ replicate 55 "a"
  message' <- W8.fromString message

  -- ref <- W8.fromWord8 0x61
  -- assert =<< W8.equal ref =<< access mmm 0

  result <- hash message'

  let msgBS = ByteString.Char8.pack message
  let ansBS = Crypto.Hash.SHA256.hash msgBS

  ans <- W8.fromString (ByteString.Char8.unpack ansBS)

  forM_ [0 .. 32 - 1] $ \i -> do
    x <- access result i
    y <- access ans i
    W8.equal x y >>= assert

  return unit

hash :: Val ('Arr W8) n -> Comp n (Val ('Arr W8) n)
hash xs = do
  xs <- pad xs

  s <- mapM W32.fromWord32 iv >>= toArray

  forM_ [0,64 .. lengthOf xs - 1] $ \i -> do
    chunk <- Array.drop i xs
    compress s chunk

  W8.fromWordNBE s

pad :: Val ('Arr W8) n -> Comp n (Val ('Arr W8) n)
pad xs = do
  let datalen = lengthOf xs
  xs <- Array.concatenate xs =<< Array.singleton =<< W8.fromWord8 0x80

  let curlen = lengthOf xs
  let padlen = (56 - curlen) `mod` 64
  xs <- if padlen == 0 then return xs
    else Array.concatenate xs =<< W8.zeros padlen

  lenpad <- W64.fromWord64 (fromIntegral datalen * 8)
  Array.concatenate xs =<< W64.toW8BE lenpad

compress :: Val ('Arr W32) n -> Val ('Arr W8) n -> Comp n ()
compress s xs = do
  m <- W8.toWordNBE 32 xs

  m <- Array.concatenate m =<< W32.zeros (64-16)

  forM_ [16 .. 63] $ \i -> do
    mi2 <- access m (i-2)
    mi7 <- access m (i-7)
    mi15 <- access m (i-15)
    mi16 <- access m (i-16)

    t <- sig1 mi2 >>= W32.add mi7
    t <- sig0 mi15 >>= W32.add t >>= W32.add mi16
    update m i t

  a' <- access s 0
  b' <- access s 1
  c' <- access s 2
  d' <- access s 3
  e' <- access s 4
  f' <- access s 5
  g' <- access s 6
  h' <- access s 7

  forM_ [0 .. 63] $ \i -> do
    a <- access s 0
    b <- access s 1
    c <- access s 2
    d <- access s 3
    e <- access s 4
    f <- access s 5
    g <- access s 6
    h <- access s 7

    t1 <- ep1 e >>= W32.add h
    t1 <- ch e f g >>= W32.add t1
    t1 <- W32.fromWord32 (k !! i) >>= W32.add t1
    t1 <- access m i >>= W32.add t1

    tmp <- maj a b c
    t2 <- ep0 a >>= W32.add tmp

    let h = g
    let g = f
    let f = e
    e <- W32.add d t1
    let d = c
    let c = b
    let b = a
    a <- W32.add t1 t2

    update s 0 a
    update s 1 b
    update s 2 c
    update s 3 d
    update s 4 e
    update s 5 f
    update s 6 g
    update s 7 h

    return ()

  update s 0 =<< W32.add a' =<< access s 0
  update s 1 =<< W32.add b' =<< access s 1
  update s 2 =<< W32.add c' =<< access s 2
  update s 3 =<< W32.add d' =<< access s 3
  update s 4 =<< W32.add e' =<< access s 4
  update s 5 =<< W32.add f' =<< access s 5
  update s 6 =<< W32.add g' =<< access s 6
  update s 7 =<< W32.add h' =<< access s 7

  return ()


xor3 :: Val W32 n -> Val W32 n -> Val W32 n -> Comp n (Val W32 n)
ch :: Val W32 n -> Val W32 n -> Val W32 n -> Comp n (Val W32 n)
maj :: Val W32 n -> Val W32 n -> Val W32 n -> Comp n (Val W32 n)
ep0 :: Val W32 n -> Comp n (Val W32 n)
ep1 :: Val W32 n -> Comp n (Val W32 n)
sig0 :: Val W32 n -> Comp n (Val W32 n)
sig1 :: Val W32 n -> Comp n (Val W32 n)

xor3 x y z = Array.xor x =<< Array.xor y z

ch x y z = do
  s <- Array.and x y
  t <- Array.and z =<< W32.complement x
  Array.xor s t

maj x y z = do
  s <- Array.and x y
  t <- Array.and x z
  u <- Array.and z y
  xor3 s t u

ep0 x = do
  s <- Array.rotateR 2 x
  t <- Array.rotateR 13 x
  u <- Array.rotateR 22 x
  xor3 s t u

ep1 x = do
  s <- Array.rotateR 6 x
  t <- Array.rotateR 11 x
  u <- Array.rotateR 25 x
  xor3 s t u

sig0 x = do
  s <- Array.rotateR 7 x
  t <- Array.rotateR 18 x
  u <- Array.shiftR 3 x
  xor3 s t u

sig1 x = do
  s <- Array.rotateR 17 x
  t <- Array.rotateR 19 x
  u <- Array.shiftR 10 x
  xor3 s t u