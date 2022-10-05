module Poseidon where

import Data.Traversable (mapAccumL)
import Data.Vector (Vector, (!))
import Data.Word
import Keelung
import qualified Poseidon.Constant as Constant

-- | Map with index
mapI :: (Traversable f) => (Int -> a -> b) -> f a -> f b
mapI f = snd . mapAccumL (\i x -> (i + 1, f i x)) 0

ark :: Vector Number -> Word32 -> Arr Number -> Arr Number
ark c it = mapI $ \i x -> x + c ! (fromIntegral it + i)