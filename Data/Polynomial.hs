{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Polynomial where

import Prelude hiding ((*), (+), negate, subtract, (-), recip, (/), foldr,
                       sum, product, replicate, concat)
import Data.List (intercalate)
import qualified Data.Map as M
import Numeric.Algebra

newtype Var = V Integer deriving (Eq, Ord, Show)

newtype Monomial v = Monomial { powers :: M.Map v Natural } deriving (Eq, Ord)

instance Show v => Show (Monomial v) where
  show = intercalate "*" . fmap doVar . M.toList . powers . normalizeMonomial
    where doVar (v, p) | p == 1 = show v
                       | otherwise = show v ++ "^" ++ show p

normalizeMonomial :: Monomial v -> Monomial v
normalizeMonomial = Monomial . M.filter (/= 0) . powers

degree :: Whole n => Monomial v -> n
degree = fromIntegral . M.size . powers

var :: v -> Monomial v
var v = Monomial $ M.singleton v 1

instance (Semiring r, Monoidal r, Ord v) => Algebra r (Monomial v) where
  mult f m = mult (\ a b -> f (Monomial a) (Monomial b)) $ powers m
instance (Semiring r, Monoidal r, Ord v) => UnitalAlgebra r (Monomial v) where
  unit r m = if degree m == (0 :: Natural) then r else zero
instance (Commutative r, Semiring r, Monoidal r, Ord v) => CommutativeAlgebra r (Monomial v) where

instance (Ord v) => Multiplicative (Monomial v) where
  (Monomial m) * (Monomial n) = Monomial $ M.unionWith (+) m n
  pow1p (Monomial m) n = Monomial . fmap (sinnum1p n) $ m
instance (Ord v) => Commutative (Monomial v) where
instance (Ord v) => Unital (Monomial v) where
  one = Monomial M.empty

newtype Polynomial r v = Polynomial { terms :: M.Map (Monomial v) r }

instance (Show r, Show v, Eq r, Monoidal r) => Show (Polynomial r v) where
  show = intercalate " + " . fmap doTerm . M.toList . terms . normalize
    where doTerm (m, r) | M.size (powers m) == 0 = show r
                        | otherwise = show r ++ "*" ++ show m

instance (Ord v, Additive r) => Additive (Polynomial r v) where
  p + q = Polynomial $ M.unionWith (+) (terms p) (terms q)
instance (Ord v, Abelian r) => Abelian (Polynomial r v) where
instance (Ord v, Idempotent r) => Idempotent (Polynomial r v) where
instance (Ord v, LeftModule n r) => LeftModule n (Polynomial r v) where
  (.*) n = Polynomial . fmap (n .*) . terms
instance (Ord v, RightModule n r) => RightModule n (Polynomial r v) where
  p *. n = Polynomial . fmap (*. n) . terms $ p
instance (Ord v, Monoidal r) => Monoidal (Polynomial r v) where
  zero = Polynomial M.empty
instance (Ord v, Group r) => Group (Polynomial r v) where
  negate = Polynomial . fmap negate . terms
  m - n = m + negate n
  subtract m n = n - m
  times n = Polynomial . fmap (times n) . terms
-- Ideally, we'd have: 
--  instance (Ord a, Algebra r a) => Multiplicative (M.Map a r) where
-- but the Algebra instance doesn't let us deduce that the product of finite
-- sums is a finite sum. But in the case of a monoid algebra we have
instance (Ord v, Semiring r) => Multiplicative (Polynomial r v) where
  p * q = Polynomial $ M.fromListWith (+) [(k * k', r * r')
                               | (k, r) <- M.toList . terms $ p,
                                 (k', r') <- M.toList . terms $ q]
instance (Ord v, Semiring r, Commutative r) => Commutative (Polynomial r v) where
instance (Ord v, Semiring r, Unital r) => Unital (Polynomial r v) where
  one = Polynomial $ M.singleton one one

normalize :: (Eq r, Monoidal r) => Polynomial r v -> Polynomial r v
normalize = Polynomial . M.filter (/= zero) . terms

support :: (Eq r, Monoidal r) => Polynomial r v -> [Monomial v]
support = M.keys . terms . normalize

monomial :: (Unital r) => Monomial v => Polynomial r v
monomial m = Polynomial $ M.singleton m one

evalMonomial :: (Unital r) => (v -> r) -> Monomial v -> r
evalMonomial f = product . fmap (\ (v, n) -> pow (f v) n) . M.toList . powers

eval :: (Rig r) => (v -> r) -> Polynomial r v -> r
eval f = sum . fmap (\ (m, r) -> r * evalMonomial f m) . M.toList . terms

vals :: [a] -> Var -> a
vals vs (V i) = vs !! Prelude.fromInteger i
