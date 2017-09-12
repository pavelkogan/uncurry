{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Uncurry where

import qualified Data.Tuple
import           GHC.TypeLits
import           Prelude      hiding (uncurry)

-- | @uncurry@ (equivalently @($$)@) converts a curried function to a function on tuples.
--
-- The function can be partially applied, for example:
--
-- > maybe $$ (0, (*2)) :: Maybe Int -> Int
class Uncurry f t where
  {-# MINIMAL uncurry | ($$) #-}
  uncurry, ($$) :: f -> t -> Consume (TupleSize t) f
  uncurry = ($$)
  ($$) = uncurry

instance (f ~ (a -> b -> Consume 2 f)) => Uncurry f (a, b) where
  uncurry = Data.Tuple.uncurry

instance (f ~ (a -> b -> c -> Consume 3 f)) => Uncurry f (a, b, c) where
  f $$ (a, b, c) = f a b c

instance (f ~ (a -> b -> c -> d -> Consume 4 f)) => Uncurry f (a, b, c, d) where
  f $$ (a, b, c, d) = f a b c d

instance (f ~ (a -> b -> c -> d -> e -> Consume 5 f)) => Uncurry f (a, b, c, d, e) where
  f $$ (a, b, c, d, e) = f a b c d e

instance (f ~ (a -> b -> c -> d -> e -> x -> Consume 6 f)) => Uncurry f (a, b, c, d, e, x) where
  f $$ (a, b, c, d, e, x) = f a b c d e x

instance (f ~ (a -> b -> c -> d -> e -> x -> y -> Consume 7 f)) => Uncurry f (a, b, c, d, e, x, y) where
  f $$ (a, b, c, d, e, x, y) = f a b c d e x y

instance (f ~ (a -> b -> c -> d -> e -> x -> y -> z -> Consume 8 f)) => Uncurry f (a, b, c, d, e, x, y, z) where
  f $$ (a, b, c, d, e, x, y, z) = f a b c d e x y z

-- | Removes @n@ parameters from the function type @f@.
type family Consume n f where
  Consume 0 f = f
  Consume n (a -> b) = Consume (n - 1) b
  Consume n r = TypeError ('Text "Cannot consume more parameters than arity of function")

-- | Returns count of elements of a tuple.
type family TupleSize a :: Nat
type instance TupleSize (_,_) = 2
type instance TupleSize (_,_,_) = 3
type instance TupleSize (_,_,_,_) = 4
type instance TupleSize (_,_,_,_,_) = 5
type instance TupleSize (_,_,_,_,_,_) = 6
type instance TupleSize (_,_,_,_,_,_,_) = 7
type instance TupleSize (_,_,_,_,_,_,_,_) = 8
