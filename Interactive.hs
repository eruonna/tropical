module Interactive where

import Prelude hiding ((*), (+), negate, subtract, (-), recip, (/), foldr,
                       (^), sum, product, replicate, concat)
import Data.Tropical
import Data.Polynomial
import qualified Data.Map as M
import Numeric.Algebra

x, y, z :: Polynomial (Tropical Integer) Var
x = monomial $ var $ V 0
y = monomial $ var $ V 1
z = monomial $ var $ V 2

