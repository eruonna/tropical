{- Copyright 2014 eruonna, see LICENSE -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Tropical where

import Prelude hiding ((*), (+), negate, subtract, (-), recip, (/), foldr,
                       sum, product, replicate, concat)
import qualified Prelude
import Numeric.Algebra hiding ((<), (>))
import Control.Applicative
import Data.Functor.Identity
import Data.Monoid

instance Show a => Show (Identity a) where
  show = show . runIdentity
instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity a) (Identity b) = Identity $ a <> b

-- |A generic tropical value. The idea is that @v@ represents some optimizable
--structure using a 'Monoid' instance to build, with @a@ as the weight. The
--functor @f@ describes how to treat equally weighted values. The common
--expected use case is @AugmentedTropical [] v a@, tracking all minimal values.
data AugmentedTropical f v a = Infinity | AT (f v) a deriving Show

-- The algebra package also has concepts of equality and ordering. Perhaps
-- those should be used instead?
instance (Eq a) => Eq (AugmentedTropical f v a) where
  Infinity == Infinity = True
  Infinity == _ = False
  _ == Infinity = False
  (AT _ w1) == (AT _ w2) = w1 == w2

instance (Ord a) => Ord (AugmentedTropical f v a) where
  compare Infinity Infinity = EQ
  compare Infinity _ = GT
  compare _ Infinity = LT
  compare (AT _ w1) (AT _ w2) = compare w1 w2

instance (Ord a, Monoid (f v)) => Additive (AugmentedTropical f v a) where
  Infinity + a = a
  a + Infinity = a
  a1@(AT v1 w1) + a2@(AT v2 w2) | w1 <  w2 = a1
                                | w1 >  w2 = a2
                                | w1 == w2 = AT (v1 <> v2) w1
instance (Ord a, Monoid (f v)) => Abelian (AugmentedTropical f v a) where
instance (Ord a, Monoid (f v)) => LeftModule Natural (AugmentedTropical f v a) where
  0 .* _ = Infinity
  _ .* a = a
instance (Ord a, Monoid (f v)) => RightModule Natural (AugmentedTropical f v a) where
  _ *. 0 = Infinity
  a *. _ = a
instance (Ord a, Monoid (f v)) => Monoidal (AugmentedTropical f v a) where
  zero = Infinity
instance (Num a, Monoid v, Applicative f) => Multiplicative (AugmentedTropical f v a) where
  Infinity * _ = Infinity
  _ * Infinity = Infinity
  (AT v1 w1) * (AT v2 w2) = AT (liftA2 (<>) v1 v2) (w1 Prelude.+ w2)
instance (Ord a, Num a, Monoid v, Applicative f, Monoid (f v)) => Semiring (AugmentedTropical f v a) where
instance (Num a, Monoid v, Applicative f) => Unital (AugmentedTropical f v a) where
  one = AT (pure mempty) 0
instance (Ord a, Num a, Monoid v, Applicative f, Monoid (f v)) => Rig (AugmentedTropical f v a) where
  fromNatural 0 = zero
  fromNatural _ = one

-- |A simple tropical weight not attached to a structure.
type Tropical a = AugmentedTropical [] () a

-- |Tag a value with a weight.
weightedVal :: (Applicative f) => v -> a -> AugmentedTropical f v a
weightedVal = AT . pure

-- |A simple weight with no value.
weight :: a -> Tropical a
weight = weightedVal ()
