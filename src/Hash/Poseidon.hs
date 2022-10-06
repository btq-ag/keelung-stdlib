module Hash.Poseidon (hash, hashSlow) where

import Control.Monad (when)
import Data.Foldable (foldlM, toList)
import Data.Traversable (mapAccumL)
import Data.Vector (Vector, (!))
import qualified Hash.Poseidon.Constant as Constant
import Keelung
import Prelude hiding (round)

-- | Map with index, basically 'mapi' in OCaml.
mapI :: Traversable f => (Int -> a -> b) -> f a -> f b
mapI f = snd . mapAccumL (\i x -> (i + 1, f i x)) 0

-- | "AddRoundConstants"
arc :: Vector Number -> Int -> Arr Number -> Arr Number
arc c it = mapI $ \i x -> x + c ! (it + i)

-- | "SubWords"
sbox :: Int -> Int -> Int -> Arr Number -> Arr Number
sbox f p r = mapI go
  where
    go 0 = fullSBox
    go _ = if r < f `div` 2 || r >= f `div` 2 + p then fullSBox else id
    -- Full S-box of x⁵
    fullSBox x = x * x * x * x * x

-- | "MixLayer"
mix :: Vector (Vector Number) -> Arr Number -> Arr Number
mix m state =
  toArray $
    map
      (\i -> sum (mapI (\j x -> x * (m ! i ! j)) state))
      [0 .. length state - 1]

-- | Same as 'hash' but without 'reuse`
hashSlow :: Arr Number -> Number
hashSlow msg =
  if null msg || length msg > 6 -- check message length
    then error "Invalid message length"
    else
      let t = length msg + 1
          roundsP = [56, 57, 56, 60, 60, 63, 64, 63]

          f = 8
          p = roundsP !! (t - 2)
          -- Constants are padded with zeroes to the maximum value calculated by
          -- t * (f + p) = 497, where `t` (number of inputs + 1) is a max of 7.
          c = Constant.c ! (t - 2)
          m = Constant.m ! (t - 2)

          -- initialize state with the first element as 0 and the rest as the message
          state = toArray $ 0 : toList msg
          -- the round function consists of 3 components
          round r = mix m . sbox f p r . arc c r

          result = foldl (flip round) state [0 .. f + p - 1]
       in access result 0

-- | 'hashSlow' with 'reuse'
hash :: Arr Number -> Comp Number
hash msg = do
  -- check message length
  when
    (null msg || length msg > 6)
    (error "Invalid message length")

  let t = length msg + 1
  let roundsP = [56, 57, 56, 60, 60, 63, 64, 63]

  let f = 8
  let p = roundsP !! (t - 2)
  -- Constants are padded with zeroes to the maximum value calculated by
  -- t * (f + p) = 497, where `t` (number of inputs + 1) is a max of 7.
  let c = Constant.c ! (t - 2)
  let m = Constant.m ! (t - 2)

  -- initialize state with the first element as 0 and the rest as the message
  let initState = toArray $ 0 : toList msg
  -- the round function consists of 3 components
  let round r = mix m . sbox f p r . arc c (r * t)

  result <- foldlM (\state r -> reuse (round r state)) initState [0 .. f + p - 1]
  return $ access result 0