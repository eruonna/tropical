module Data.Tropical.PlaneCurve where

import Prelude hiding ((*), (+), negate, subtract, (-), recip, foldr,
                       (^), sum, product, replicate, concat)
import qualified Prelude
import Data.Tropical
import Data.Vec
import Numeric.Algebra hiding ((<), (/))

-- |A tropical plane curve is built of lines, rays, and segments. We may want
-- to generalize this to polyhedral complexes in some way to represent other
-- tropical varieties. The type a should be a rational type when representing
-- tropical curves.
data Edge a = Line (Vec2 a) (Vec2 a)
            | Ray (Vec2 a) (Vec2 a)
            | Segment (Vec2 a) (Vec2 a) deriving (Eq, Show)

-- | (a :. b :. c) corresponds to ax + by + c < 0
type Constraint a = Vec3 a

satisfies :: (Num a, Ord a) => Vec2 a -> Constraint a -> Bool
satisfies (x :. y :. ()) (a :. b :. c :. ()) = a*x + b*y + c < 0

-- |Add a constraint to an edge. With no constraints, an edge is a line. With
-- one, it may become a ray. And so on. If no points on the edge satisfy the
-- constraint, return Nothing.
addConstraint :: (Fractional a, Ord a) => Edge a -> Constraint a -> Maybe (Edge a)
addConstraint l@(Line pt@(x :. y :. ()) v@(vx :. vy :. ())) con@(a :. b :. c :. ())
  = if det == 0 then
      if base < 0 then Just l else Nothing
    else
      Just $ Ray intersect (if det < 0 then v else (negate vx :. negate vy :. ()))
  where
    det = a*vx + b*vy 
    base = a*x + b*y + c
    intersect = ((b * base - vx * c) / det) :. ((negate a * base - vy * c) / det) :. ()
addConstraint r@(Ray pt@(x :. y :. ()) v@(vx :. vy :. ())) con@(a :. b :. c :. ())
  = case compare det 0 of
      EQ -> if base < 0 then Just r else Nothing
      LT -> if base < 0 then Just r else Just $ Ray intersect v
      GT -> if base < 0 then Just $ Segment pt intersect else Nothing
  where
    det = a*vx + b*vy
    base = a*x + b*y + c
    intersect = ((b * base - vx * c) / det) :. ((negate a * base - vy * c) / det) :. ()
addConstraint s@(Segment pt1@(x1 :. y1 :. ()) pt2@(x2 :. y2 :. ())) con@(a :. b :. c :. ())
  = case (base < 0, pt2 `satisfies` con) of
      (True, True) -> Just s
      (True, False) -> Just $ Segment pt1 intersect
      (False, True) -> Just $ Segment intersect pt2
      (False, False) -> Nothing
  where
    vx = x2 - x1
    vy = y2 - y1
    det = a*vx + b*vy
    base = a*x1 + b*y1 + c
    intersect = ((b * base - vx * c) / det) :. ((negate a * base - vy * c) / det) :. ()
