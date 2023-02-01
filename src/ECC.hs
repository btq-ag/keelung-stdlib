{-# LANGUAGE DataKinds #-}
module ECC where
import Keelung

type EC = (Field, Field)
type Point = (EC, Field, Field)

run :: Int -> Comp (Field, Field)
run n = do
    a <- inputField
    b <- inputField
    x <- inputField
    y <- inputField
    assert ((y * y) `eq` ((x * x * x) + (x * a) + b))
    let (_, x', y') = ((a, b), x, y) `times` n
    return (x', y')
  where
    times :: Point -> Int -> Point
    times point 1 = point
    times point number = 
        if even number then double (point `times` (number `div` 2))
                       else (point `times` (pred number)) `add` point
    add :: Point -> Point -> Point
    add p0@(ec, x0, y0) p1@(_, x1, y1)
      | p0 == p1 = double p0
      | x0 == x1 = (ec, 0, 0)
      | otherwise = (ec, x2, y2)
      where
        slope = (y1 - y0) / (x1 - x0)
        x2 = slope * slope - (x0 + x1)
        y2 = (x0 - x2) * slope - y1
    double :: Point -> Point
    double ((a, b), x, y)
      | y == 0 = ((a, b), 0, 0)
      | otherwise = ((a, b), x0, y0)
        where
          slope = (x * x * 3 + a) / (y + y)
          x0 = slope * slope - x - x
          y0 = (x - x0) * slope - y