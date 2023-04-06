{-# LANGUAGE DataKinds #-}

module ECCTiny where

import Control.Monad (foldM)
import Keelung

-- Choice of bit width:
-- In current implementation, the number can be largest when calculating slopeDbl.
-- slopeDbl < (p*p*3+a)*p < 2^36
type F = UInt 36

-- The programmer is responsible for ensuring that the following parameters constitute a valid ECDSA parameter.
__p :: Integer
__p = 2833

_p, _a, _b, _n :: F
_g :: (F, F)
_p = UInt __p
_a = 1
_b = 1
_g = (1341, 854) -- Generator
_n = 131         -- Order of g

-- | Performs eager modulo operation with respect to p.
-- It is important to be mindful of potential overflows (exceeding the bit-width) and underflows (during subtraction),
-- and to perform modular reductions in a timely manner.
modP :: F -> Comp F
modP n = snd <$> performDivMod n _p

inverse :: F -> F
inverse n = modInv n __p

-- | Find a^-1 mod p with Fermat's Little Theorem. Use eager reduction.
inverseFermat :: F -> Comp F
inverseFermat num = foldM f 1 pm2_bitsrev
  where
    pm2_bitsrev = reverse $ map ((_p - 2) !!!) [0 .. widthOf _p - 1]
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
  r1 <- modP $ (x * x * x) + (x * _a) + _b
  return $ (r0 `eq` r1) .|. (x `eq` 0 .&. y `eq` 0)

smult' :: Int -> Comp (F, F)
smult' n = do
  x <- input Public
  y <- input Public
  smult n (Point (x, y))

smult :: Int -> Point -> Comp (F, F)
smult n p@(Point (x, y)) = do
  assert =<< onCurve p
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
        ((x0 `eq` x1) .&. (((y0 + y1) `eq` 0) .|. ((y0 + y1) `eq` _p)))
        zero
        fallover
  where
    zero = Point (0, 0)

inverseGuarded :: F -> F
inverseGuarded n = cond ((n `eq` 0) .|. (n `eq` _p)) 1 (inverse n)

add :: Point -> Point -> Comp Point
add p0@(Point (x0, y0)) p1@(Point (x1, y1)) =
  handleCornerCases p0 p1 $ do
    -- We need to handle the case of possible zero input for `inverse` because
    -- the else branch of condPointM is a `Comp`, which is eagerly evaluated
    -- regardless of the truth value of the predicate (during `intrepret` only?)
    let slopeDbl = (x0 * x0 * 3 + _a) * inverseGuarded (y0 + y0)
        slopeAdd = (_p + y1 - y0) * inverseGuarded (_p + x1 - x0)
    slope <- modP $ cond (p0 `eq` p1) slopeDbl slopeAdd
    x2 <- modP $ slope * slope + 2 * _p - (x0 + x1)
    y2 <- modP $ (x0 + _p - x2) * slope - y0
    return (Point (x2, y2))

smultVar :: F -> Point -> Comp (F, F)
smultVar n p = do
  assert =<< onCurve p
  Point p' <- p `times` n
  return p'
  where
    times :: Point -> F -> Comp Point
    times point@(Point (_, _)) s = do
      let bitsrev = reverse $ map (s !!!) [0 .. widthOf s - 1]
      let f pt bit = do
            p2 <- reuse =<< add pt pt
            reuse =<< condPointM (complement bit) p2 (add p2 point)
      foldM f (Point (0, 0)) bitsrev

testScalarMult0 :: Comp ()
testScalarMult0 = do
  (x', y') <- smult 123456 (Point (1341, 854))
  assert $ x' `eq` 2560
  assert $ y' `eq` 380

testScalarMult1 :: Comp ()
testScalarMult1 = do
  (x', y') <- smultVar 123456 (Point (1341, 854))
  assert $ x' `eq` 2560
  assert $ y' `eq` 380
