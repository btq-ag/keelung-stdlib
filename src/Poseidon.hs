module Poseidon where

import Control.Monad
import Data.Foldable (toList)
import Data.Traversable (for, mapAccumL)
import Data.Vector (Vector, (!))
import Data.Word
import Keelung
import qualified Poseidon.Constant as Constant

-- | Map with index
mapI :: Traversable f => (Int -> a -> b) -> f a -> f b
mapI f = snd . mapAccumL (\i x -> (i + 1, f i x)) 0

arc :: Vector Number -> Word32 -> Arr Number -> Arr Number
arc c it = mapI $ \i x -> x + c ! (fromIntegral it + i)

sbox :: Word32 -> Word32 -> Word32 -> Arr Number -> Arr Number
sbox f p r = mapI go
  where
    go 0 = exp5
    go _ = if r < f `div` 2 || r >= f `div` 2 + p then exp5 else id

    exp5 x = x * x * x * x * x

mix :: Vector (Vector Number) -> Arr Number -> Arr Number
mix m state =
  toArray $
    map
      (\i -> sum (mapI (\j x -> x * (m ! i ! j)) state))
      [0 .. length state - 1]

hash :: Arr Number -> Number
hash msg =
  if null msg || length msg > 6 -- check message length
    then error "Invalid message length"
    else
      let t = length msg + 1
          roundsP = [56, 57, 56, 60, 60, 63, 64, 63]

          f = 8
          p = roundsP !! t - 2
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