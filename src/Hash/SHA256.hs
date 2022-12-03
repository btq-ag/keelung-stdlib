{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Hash.SHA256 where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Word
import Keelung
import qualified Lib.ArrayM as ArrayM
import qualified Lib.W64 as W64
import Lib.W32 (W32M)
import qualified Lib.W32 as W32
import Lib.W8 (W8M)
import qualified Lib.W8 as W8

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
  [ 0x428a2f98,
    0x71374491,
    0xb5c0fbcf,
    0xe9b5dba5,
    0x3956c25b,
    0x59f111f1,
    0x923f82a4,
    0xab1c5ed5,
    0xd807aa98,
    0x12835b01,
    0x243185be,
    0x550c7dc3,
    0x72be5d74,
    0x80deb1fe,
    0x9bdc06a7,
    0xc19bf174,
    0xe49b69c1,
    0xefbe4786,
    0x0fc19dc6,
    0x240ca1cc,
    0x2de92c6f,
    0x4a7484aa,
    0x5cb0a9dc,
    0x76f988da,
    0x983e5152,
    0xa831c66d,
    0xb00327c8,
    0xbf597fc7,
    0xc6e00bf3,
    0xd5a79147,
    0x06ca6351,
    0x14292967,
    0x27b70a85,
    0x2e1b2138,
    0x4d2c6dfc,
    0x53380d13,
    0x650a7354,
    0x766a0abb,
    0x81c2c92e,
    0x92722c85,
    0xa2bfe8a1,
    0xa81a664b,
    0xc24b8b70,
    0xc76c51a3,
    0xd192e819,
    0xd6990624,
    0xf40e3585,
    0x106aa070,
    0x19a4c116,
    0x1e376c08,
    0x2748774c,
    0x34b0bcb5,
    0x391c0cb3,
    0x4ed8aa4a,
    0x5b9cca4f,
    0x682e6ff3,
    0x748f82ee,
    0x78a5636f,
    0x84c87814,
    0x8cc70208,
    0x90befffa,
    0xa4506ceb,
    0xbef9a3f7,
    0xc67178f2
  ]

type HV = StateT (ArrM W32M) Comp

hash :: ArrM W8M -> Comp (ArrM W8M)
hash msg = do
    hs <- W32.fromWord32List iv

    msg' <- pad msg
    chunks <- W8.toWordNBE 32 msg' >>= ArrayM.chunks 16 >>= fromArrayM

    hs' <- execStateT (forM_ chunks compression) hs

    W8.fromWordNBE =<< toArrayM =<< forM [0..7] (\i -> join $ addM <$> accessM hs i <*> accessM hs' i)

pad :: ArrM W8M -> Comp (ArrM W8M)
pad xs = do
    let l = lengthOf xs * 8
    let k' = 512 - (l + 64 + 1 + 7) `mod` 512
    xs <- ArrayM.concatenate xs =<< join (ArrayM.cons <$> W8.fromWord8 0x80 <*> (ArrayM.chunks 8 =<< ArrayM.zeroBits k'))
    ArrayM.concatenate xs =<< W8.fromWordNBE =<< ArrayM.singleton =<< W64.fromWord64 (fromIntegral l)

compression :: ArrM W32M -> HV ()
compression chunk = do
  w <- lift $ do
    chunk' <- ArrayM.take 16 chunk
    w <- ArrayM.concatenate chunk' =<< W32.zeros 48
    forM_ [16 .. 63] $ \i -> do
        w2 <- accessM w (i - 2)
        w7 <- accessM w (i - 7)
        w15 <- accessM w (i - 15)
        w16 <- accessM w (i - 16)

        s0 <- ArrayM.rotateR 7 w15 `xorM` ArrayM.rotateR 18 w15 `xorM` ArrayM.shiftR 3 w15
        s1 <- ArrayM.rotateR 17 w2 `xorM` ArrayM.rotateR 19 w2 `xorM` ArrayM.shiftR 10 w2
        wi <- w16 `addM` s0 >>= addM w7 >>= addM s1
        updateM w i wi

    return w

  mapM_ (compression' w) [0..63]

compression' :: ArrM W32M -> Int -> HV ()
compression' w i = do
    hs <- get
    hs' <- lift $ do
        a <- accessM hs 0
        b <- accessM hs 1
        c <- accessM hs 2
        d <- accessM hs 3
        e <- accessM hs 4
        f <- accessM hs 5
        g <- accessM hs 6
        h <- accessM hs 7

        s1 <- ArrayM.rotateR 6 e `xorM` ArrayM.rotateR 11 e `xorM` ArrayM.rotateR 25 e
        ch <- ArrayM.and e f `xorM` (ArrayM.not e >>= ArrayM.and g)
        ki <- W32.fromWord32 (k !! i)
        wi <- accessM w i

        tmp1 <- h `addM` s1 >>= addM ch >>= addM ki >>= addM wi
        s0 <- ArrayM.rotateR 2 a `xorM` ArrayM.rotateR 13 a `xorM` ArrayM.rotateR 22 a
        maj <- ArrayM.and a b `xorM` ArrayM.and a c `xorM` ArrayM.and b c
        tmp2 <- s0 `addM` maj

        e' <- d `addM` tmp1
        a' <- tmp1 `addM` tmp2

        toArrayM [a', a, b, c, e', e, f, g]
    put hs'


xorM :: Comp (ArrM Boolean) -> Comp (ArrM Boolean) -> Comp (ArrM Boolean)
xorM x y = join $ ArrayM.xor <$> x <*> y

addM :: W32M -> W32M -> Comp W32M
addM = ArrayM.fullAdder

