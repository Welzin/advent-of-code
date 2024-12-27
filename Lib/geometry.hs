module Lib.Geometry where

import Prelude hiding (cos, sin)
import qualified Prelude as P (cos, sin)

-- A small geometry library for the problems of the advent of code.

-- # General definitions

-- todo: Vec2D with a parameterized type so that we can use integers (maybe)
type Point a = (a, a)
type Vec2D a = (a, a)
type Line a = (Point a, Vec2D a)

north :: Num a => Vec2D a
north = (-1, 0)

south :: Num a => Vec2D a
south = (1, 0)

west :: Num a => Vec2D a
west = (0, -1)

east :: Num a => Vec2D a
east = (0, 1)

nswe :: Num a => [Vec2D a]
nswe = [north, south, west, east]

-- # Distances
-- TODO: implement this distance for vectors [define a suitable type class]
manhattan :: Num a => a -> a -> a
manhattan x y = abs x + abs y

-- # Vectors 

-- mkVec st ed makes a vector from st to ed.
mkVec :: Num a => Point a -> Point a -> Vec2D a
mkVec (a, b) (x, y) = (x - a, y - b)

-- Map a function to the elements of a vector.
mapVec :: (a -> b) -> Vec2D a -> Vec2D b
mapVec f (x, y) = (f x, f y)

-- The L1-norm of a vector is the sum of its absolute values.
norm :: Num a => Vec2D a -> a
norm (x, y) = abs x + abs y

-- A unitary vector is one such that its L1-norm is 1.
unitarize :: Fractional a => Vec2D a -> Vec2D a
unitarize (x, y) = (x / n, y / n)
  where n = norm (x, y)

-- direction st ed gives a vector from st to ed that is unitary.
direction :: Fractional a => Point a -> Point a -> Vec2D a
direction st ed = unitarize $ mkVec st ed

-- Dot product, i.e., for projection of a vector on another vector.
(·) :: Num a => Vec2D a -> Vec2D a -> a
(a, b) · (x, y) = x * a + y * b

-- Two vectors u, v are orthogonal when uᵗv = 0
(⊥) :: (Eq a, Num a) => Vec2D a -> Vec2D a -> Bool
u ⊥ v = u · v == 0

(×) :: (Num a) => Vec2D a -> Vec2D a -> a
(a, b) × (x, y) = a * y - b * x

-- Two vectors u, v are collinear if their cross product is zero.
collinear :: (Eq a, Num a) => Vec2D a -> Vec2D a -> Bool
collinear u v = u × v == 0

-- We define a class that allows rotations (i.e., a class of nums with cos and sin)
class Num a => TrigNum a where
  cos :: Float -> a
  sin :: Float -> a

instance TrigNum Float where
  cos = P.cos
  sin = P.sin

instance TrigNum Int where
  cos = round . P.cos 
  sin = round . P.sin

-- Rotates the vector by the given (trigonometric) angle. Use as infix if possible.
rotateBy :: TrigNum a => Vec2D a -> Float -> Vec2D a
rotateBy (x, y) θ = (x * cos θ - y * sin θ, x * sin θ + y * cos θ)
