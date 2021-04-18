{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.Compose where

import Course.Applicative
import Course.Contravariant
import Course.Core
import Course.Functor
import Course.List
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a = Compose (f (g a))
  deriving (Show, Eq)

runCompose :: Compose f g a -> f (g a)
runCompose (Compose fga) = fga

-- (<$>) :: (a -> b) -> k a -> k b
-- k ~ Compose f g
-- `~` means "unified types"

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  (<$>) :: (a -> b) -> Compose f g a -> Compose f g b
  (<$>) ab (Compose fga) = Compose ((ab <$>) <$> fga)

-- fmap x y = x <$> y
-- (<$>) ab fga = Compose $ (fmap . fmap $ ab) (runCompose fga)

-- x = pure 1 :: Compose Optional (Compose List Optional) Int
-- (+ 1) <$> x -- STILL ONLY NEED 1 FMAP! whoa

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  -- Implement the pure function for an Applicative instance for Compose
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  -- example using the above `pure
  -- doubleListify :: Int -> Compose List List Int
  -- doubleListify = pure

  -- (<*>) :: k (a -> b) -> k a -> k b
  -- lift2 :: Applicative k => (a -> b -> c) -> k a -> k b -> k c
  -- lift2 f ka kb = f <$> ka <*> kb
  -- k ~ Compose f g
  -- lift2 :: Applicative k => (x -> y -> z) -> Compose f g x -> Compose f g y -> Compose f g z

  -- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  -- (<*>) (Compose fgab) (Compose fga) = Compose $ (<*>) <$> fgab <*> fga
  -- (<*>) (Compose fgab) (Compose fga) = Compose $ pure (<*>) <*> fgab <*> fga
  (<*>) (Compose fgab) (Compose fga) = Compose $ lift2 (<*>) fgab fga

-- f :: Compose List List (Int -> Int)
-- f = pure (+3) :: Compose List List (Int -> Int)
-- x :: Compose List List Int
-- x = pure (+1) :: Compose List List (Int -> Int)
-- f <*> x
-- Compose [[4]]

-- lift2 $ lift2 (<*>) :: k1 (k2 (k3 (a -> b))) -> k1 (k2 (k3 a)) -> k1 (k2 (k3 b))

instance (Monad f, Monad g) => Monad (Compose f g) where
  -- Implement the (=<<) function for a Monad instance for Compose
  (=<<) :: (a -> Compose f g b) -> Compose f g a -> Compose f g b
  (=<<) = error "inconceivable"

-- Compose $ fga >>= \ga -> _todo

-- https://stackoverflow.com/questions/7040844/applicatives-compose-monads-dont

-- join :: Compose f g (Compose f g a) -> Compose f g a
-- join = (id =<<)

-- Note that the inner g is Contravariant but the outer f is
-- Functor. We would not be able to write an instance if both were
-- Contravariant; why not?
instance (Functor f, Contravariant g) => Contravariant (Compose f g) where
  -- Implement the (>$<) function for a Contravariant instance for Compose
  (>$<) :: (b -> a) -> Compose f g a -> Compose f g b
  (>$<) f (Compose fga) = Compose $ (f >$<) <$> fga
