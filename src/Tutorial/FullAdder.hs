{-# LANGUAGE DataKinds #-}

module Tutorial.FullAdder where

import Keelung

------------------------------------------------------------------------------

fullAdder :: [Boolean]-> [Boolean]-> [Boolean]
fullAdder as bs =
  let zipped = zip as bs
   in fst $ foldl f ([], false) zipped
  where
    f :: ([Boolean], Boolean) -> (Boolean, Boolean) -> ([Boolean], Boolean)
    f (acc, carry) (a, b) =
      let value = a `Xor` b `Xor` carry
          nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)
       in (acc ++ [value], nextCarry)

-- | "T" for top-level
fullAdderT :: Int -> Comp [Boolean]
fullAdderT width = do
  xs <- inputList width
  ys <- inputList width
  return $ fullAdder xs ys
