{-# LANGUAGE DataKinds #-}

module ECCTiny where

import Control.Monad (foldM)
import Keelung

-- Choice of bit width:
-- In current implementation, the number can be largest when calculating slopeDbl.
-- slopeDbl < (p*p*3+a)*p < 2^36
type F = UInt 36

p, a, b :: F
p = 2833
a = 1
b = 1

-- | Performs eager modulo operation with respect to p.
-- It is important to be mindful of potential overflows (exceeding the bit-width) and underflows (during subtraction),
-- and to perform modular reductions in a timely manner.
modP :: F -> Comp F
modP n = snd <$> performDivMod n p

-- | Find a^-1 mod p with Fermat's Little Theorem. Use eager reduction.
inverse :: F -> Comp F
inverse num = foldM f 1 pm2_bitsrev
  where
    pm2_bitsrev = reverse $ map ((p - 2) !!!) [0 .. widthOf p - 1]
    f n bit = do
      r <- modP (n * n)
      r' <- modP (r * num)
      reuse $ cond bit r' r

-- | As a rule of thumb, the coordinates of any encapsulated points should be fully reduced.
newtype Point = Point (F, F)
  deriving (Eq, Show)

instance Reusable Point where
  reuse (Point (x, y)) = do
    x' <- reuse x
    y' <- reuse y
    return $ Point (x', y')

instance Cmp Point where
  eq (Point (x0, y0)) (Point (x1, y1)) = (x0 `eq` x1) .&. (y0 `eq` y1)
  neq x y = Not (x `eq` y)

onCurve :: Point -> Comp Boolean
onCurve (Point (x, y)) = do
  r0 <- modP $ y * y
  r1 <- modP $ (x * x * x) + (x * a) + b
  return $ (r0 `eq` r1) .|. (x `eq` 0 .&. y `eq` 0)

smult' :: Int -> Comp (F, F)
smult' n = do
  x <- input Public
  y <- input Public
  smult n (Point (x, y))

smult :: Int -> Point -> Comp (F, F)
smult n pp@(Point (x, y)) = do
  assert =<< onCurve pp
  Point (x', y') <- Point (x, y) `times` n
  return (x', y')
  where
    times :: Point -> Int -> Comp Point
    times _ 0 = return (Point (0, 0))
    times point 1 = return point
    times point number = do
      if even number
        then (point `times` (number `div` 2)) >>= reuse >>= \pt -> add pt pt
        else (point `times` pred number) >>= reuse >>= add point

condPoint :: Boolean -> Point -> Point -> Point
condPoint c (Point (x0, y0)) (Point (x1, y1)) =
  Point (cond c x0 x1, cond c y0 y1)

condPointM :: Boolean -> Point -> Comp Point -> Comp Point
condPointM c p0 p1 = condPoint c p0 <$> p1

-- Comparisons can be solely dependent on the x-coordinate if we exercise caution.
-- By ensuring that the scalar value is greater than zero and less than the order of E,
-- we can eliminate all edge cases. (It may require range proofs.)
-- By "caution" I mean that constraints must be appropriate enough so that adversaries
-- won't be able to generate fake proofs by exploiting under-constrained programs.
handleCornerCases :: Point -> Point -> Comp Point -> Comp Point
handleCornerCases p0@(Point (x0, y0)) p1@(Point (x1, y1)) fallover =
  condPointM (p0 `eq` zero) p1 $
    condPointM (p1 `eq` zero) p0 $
      condPointM
        ((x0 `eq` x1) .&. (((y0 + y1) `eq` 0) .|. ((y0 + y1) `eq` p)))
        zero
        fallover
  where
    zero = Point (0, 0)

add :: Point -> Point -> Comp Point
add p0@(Point (x0, y0)) p1@(Point (x1, y1)) =
  handleCornerCases p0 p1 $ do
    slopeDbl <- (* (x0 * x0 * 3 + a)) <$> inverse (y0 + y0)
    slopeAdd <- (* (p + y1 - y0)) <$> inverse (p + x1 - x0)
    slope <- modP $ cond (p0 `eq` p1) slopeDbl slopeAdd
    x2 <- modP $ slope * slope + 2 * p - (x0 + x1)
    y2 <- modP $ (x0 + p - x2) * slope - y0
    return (Point (x2, y2))

add' :: Comp (F, F)
add' = do
  x <- input Public
  y <- input Public
  Point (x', y') <- add (Point (x, y)) (Point (x, y))
  return (x', y')

testScalarMult0 :: Comp ()
testScalarMult0 = do
  (x', y') <- smult 123456 (Point (1341, 854))
  assert $ x' `eq` 1341
  assert $ y' `eq` 854
