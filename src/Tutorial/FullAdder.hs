{-# LANGUAGE DataKinds #-}

module Tutorial.FullAdder where

import Keelung

------------------------------------------------------------------------------

fullAdder :: Arr Boolean -> Arr Boolean -> Arr Boolean
fullAdder as bs =
  let zipped = zip (fromArray as) (fromArray bs)
   in toArray $ fst $ foldl f ([], false) zipped
  where
    f :: ([Boolean], Boolean) -> (Boolean, Boolean) -> ([Boolean], Boolean)
    f (acc, carry) (a, b) =
      let value = a `Xor` b `Xor` carry
          nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)
       in (acc ++ [value], nextCarry)

-- | "T" for top-level
fullAdderT :: Int -> Comp (Arr Boolean)
fullAdderT width = do
  xs <- inputs width
  ys <- inputs width
  return $ fullAdder xs ys
