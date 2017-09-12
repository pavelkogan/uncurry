{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Control.Uncurry where

-- | @uncurry@ (equivalently @($$)@) converts a curried function to a function on tuples.
--
-- The function can be partially applied, for example:
--
-- > maybe $$ (0, (*2)) :: Maybe Int -> Int
class Uncurry t r where
  type Uncurried t r

  {-# MINIMAL uncurry | ($$) #-}
  uncurry, ($$) :: Uncurried t r -> t -> r
  uncurry = ($$)
  ($$) = uncurry

instance Uncurry (a, b) r where
  type Uncurried (a, b) r = a -> b -> r
  f $$ (a, b) = f a b

instance Uncurry (a, b, c) r where
  type Uncurried (a, b, c) r = a -> b -> c -> r
  f $$ (a, b, c) = f a b c

instance Uncurry (a, b, c, d) r where
  type Uncurried (a, b, c, d) r = a -> b -> c -> d -> r
  f $$ (a, b, c, d) = f a b c d

instance Uncurry (a, b, c, d, e) r where
  type Uncurried (a, b, c, d, e) r = a -> b -> c -> d -> e -> r
  f $$ (a, b, c, d, e) = f a b c d e

instance Uncurry (a, b, c, d, e, x) r where
  type Uncurried (a, b, c, d, e, x) r = a -> b -> c -> d -> e -> x -> r
  f $$ (a, b, c, d, e, x) = f a b c d e x

instance Uncurry (a, b, c, d, e, x, y) r where
  type Uncurried (a, b, c, d, e, x, y) r = a -> b -> c -> d -> e -> x -> y -> r
  f $$ (a, b, c, d, e, x, y) = f a b c d e x y

instance Uncurry (a, b, c, d, e, x, y, z) r where
  type Uncurried (a, b, c, d, e, x, y, z) r = a -> b -> c -> d -> e -> x -> y -> z -> r
  f $$ (a, b, c, d, e, x, y, z) = f a b c d e x y z
