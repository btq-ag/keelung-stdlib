{-# LANGUAGE DataKinds #-}

module Tutorial.FullAdder where

import Keelung

------------------------------------------------------------------------------

fullAdder :: Val ('Arr 'Bool) -> Val ('Arr 'Bool) -> Val ('Arr 'Bool)
fullAdder as bs =
  let zipped = zip (fromArray as) (fromArray bs)
   in toArray $ fst $ foldl f ([], false) zipped
  where
    f :: ([Val 'Bool], Val 'Bool) -> (Val 'Bool, Val 'Bool) -> ([Val 'Bool], Val 'Bool)
    f (acc, carry) (a, b) =
      let value = a `Xor` b `Xor` carry 
          nextCarry = (a `Xor` b `And` carry) `Or` (a `And` b)
       in (acc ++ [value], nextCarry)

-- | "T" for top-level
fullAdderT :: Int -> Comp (Val ('Arr 'Bool))
fullAdderT width = do
  xs <- inputs width
  ys <- inputs width
  return $ fullAdder xs ys
