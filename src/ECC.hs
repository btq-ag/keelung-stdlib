{-# LANGUAGE DataKinds #-}

module ECC where

import Keelung

type EC = (Field, Field)

newtype Point = Point (EC, Field, Field)
  deriving (Eq, Show)

instance Reusable Point where
  reuse (Point ((a, b), x, y)) = do
    a' <- reuse a
    b' <- reuse b
    x' <- reuse x
    y' <- reuse y
    return $ Point ((a', b'), x', y')

instance Cmp Point where
  eq (Point (_, x0, y0)) (Point (_, x1, y1)) = (x0 `eq` x1) .&. (y0 `eq` y1)
  neq x y = Not (x `eq` y)

genPoint' :: Int -> Comp (Field, Field)
genPoint' n = do
  a <- inputField Private
  b <- inputField Private
  x <- inputField Private
  y <- inputField Private
  genPoint n (Point ((a, b), x, y))

genPoint :: Int -> Point -> Comp (Field, Field)
genPoint n (Point ((a, b), x, y)) = do
  assert $ ((y * y) `eq` ((x * x * x) + (x * a) + b)) .|. (x `eq` 0 .&. y `eq` 0)
  Point (_, x', y') <- Point ((a, b), x, y) `times` n
  return (x', y')
  where
    times :: Point -> Int -> Comp Point
    times _ 0 = return (Point ((a, b), 0, 0))
    times point 1 = return point
    times point number = do
      if even number
        then (point `times` (number `div` 2)) >>= reuse >>= \p -> return $ add p p
        else (point `times` pred number) >>= reuse >>= \p -> return $ p `add` point

condPoint :: Boolean -> Point -> Point -> Point
condPoint b (Point (ec, x0, y0)) (Point (_, x1, y1)) =
  Point (ec, cond b x0 x1, cond b y0 y1)

add :: Point -> Point -> Point
add p0@(Point (ec@(a, _), x0, y0)) p1@(Point (_, x1, y1)) =
  condPoint (p0 `eq` zero) p1 $ -- if p0 = O, return p1
    condPoint (p1 `eq` zero) p0 $ -- if p1 = O, return p0
      condPoint
        ((x0 `eq` x1) .&. (y0 `eq` negate y1)) -- if x0 = x1 and y0 = −y1, return O
        zero
        (Point (ec, x2, y2))
  where
    zero = Point (ec, 0, 0)
    slope = cond (p0 `eq` p1) ((x0 * x0 * 3 + a) / (y0 + y0)) ((y1 - y0) / (x1 - x0))
    x2 = slope * slope - (x0 + x1)
    y2 = (x0 - x2) * slope - y0

-- gf181 testFixedScalarMultAll [] []
testFixedScalarMultAll :: Comp ()
testFixedScalarMultAll = do
  testFixedScalarMult
  testFixedScalarMult1
  testFixedScalarMult2
  testFixedScalarMult3
  testFixedScalarMult4

testFixedScalarMult :: Comp ()
testFixedScalarMult = do
  (x', y') <- genPoint 71 (Point ((a, b), x, y))
  assert $ x' `eq` 1462206297875531203695911991939774104835886193121241862
  assert $ y' `eq` 575361663117637504098133183537141500890013945643887968
  where
    (a, b) = (3, 5)
    x = 625314423133545616470255033484595795489252976466585708
    y = 531545639388128122741209816467026673686376804877818139

-- O * 321 == O
testFixedScalarMult1 :: Comp ()
testFixedScalarMult1 = do
  (x', y') <- genPoint 321 (Point ((a, b), 0, 0))
  assert $ x' `eq` 0
  assert $ y' `eq` 0
  where
    (a, b) = (3, 5)

-- (x,y)*151 == O
testFixedScalarMult2 :: Comp ()
testFixedScalarMult2 = do
  (x', y') <- genPoint 151 (Point ((a, b), x, y))
  assert $ x' `eq` 0
  assert $ y' `eq` 0
  where
    (a, b) = (3, 5)
    x = 625314423133545616470255033484595795489252976466585708
    y = 531545639388128122741209816467026673686376804877818139

-- (x,y)*152 == (x,y)
testFixedScalarMult3 :: Comp ()
testFixedScalarMult3 = do
  (x', y') <- genPoint 152 (Point ((a, b), x, y))
  assert $ x' `eq` x
  assert $ y' `eq` y
  where
    (a, b) = (3, 5)
    x = 625314423133545616470255033484595795489252976466585708
    y = 531545639388128122741209816467026673686376804877818139

-- (x,y)*0 == O
testFixedScalarMult4 :: Comp ()
testFixedScalarMult4 = do
  (x', y') <- genPoint 0 (Point ((a, b), x, y))
  assert $ x' `eq` 0
  assert $ y' `eq` 0
  where
    (a, b) = (3, 5)
    x = 625314423133545616470255033484595795489252976466585708
    y = 531545639388128122741209816467026673686376804877818139
