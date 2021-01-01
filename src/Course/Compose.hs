{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.Compose where

import Course.Applicative
import Course.Contravariant
import Course.Core
import Course.Functor
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a = Compose (f (g a))
  deriving (Show, Eq)

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  -- (<$>) :: (a -> b) -> k a -> k b
  (<$>) h (Compose fg) = Compose ((h <$>) <$> fg)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  -- Implement the pure function for an Applicative instance for Compose
  -- pure :: a -> k a
  pure = Compose . pure . pure

  -- Implement the (<*>) function for an Applicative instance for Compose
  -- (<*>) :: k (a -> b) -> k a -> k b
  (<*>) (Compose f) (Compose x) = Compose (lift2 (<*>) f x)

instance (Monad f, Monad g) => Monad (Compose f g) where
  -- Implement the (=<<) function for a Monad instance for Compose
  -- (=<<) :: (a -> k b) -> k a -> k b
  (=<<) = error "impossible"

-- There seems to be no way to join this `f (Compose f g b)`
-- (=<<) f (Compose fg) = (f <$>) =<< fg

-- Note that the inner g is Contravariant but the outer f is
-- Functor. We would not be able to write an instance if both were
-- Contravariant; why not?

-- Answer: (>$<) needs a `ga` but we have a `gb`
-- instance (Contravariant f, Contravariant g) => Contravariant (Compose f g) where
--   (>$<) :: (b -> a) -> k a -> k b
--   (>$<) ba (Compose fga) = Compose ((\gb -> _t) >$< fga)

-- I also tried for other combinations of Functor and Contravariant and
-- the types never line up.

instance (Functor f, Contravariant g) => Contravariant (Compose f g) where
  -- Implement the (>$<) function for a Contravariant instance for Compose
  -- (>$<) :: (b -> a) -> k a -> k b
  (>$<) ba (Compose fga) = Compose ((ba >$<) <$> fga)
