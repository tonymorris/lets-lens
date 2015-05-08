{-# LANGUAGE RankNTypes #-}

module Lets.OpticLens where

data Lens a b =
  Lens
    (forall f. Functor f => (b -> f b) -> a -> f a)

-- | The get/set law of lenses. This function should always return @True@.
getsetLaw ::
  Eq a =>
  Lens a b
  -> a
  -> Bool
getsetLaw (Lens r) =
  undefined

{-

-- | The get/set law of lenses. This function should always return @True@.
getsetLaw ::
  Eq a =>
  Lens a b
  -> a
  -> Bool
getsetLaw (Lens s g) =
  \a -> s a (g a) == a
-}

data Const a b = Const a

getC ::
  Const a b
  -> a
getC (Const a) =
  a

instance Functor (Const a) where
  fmap _ (Const a) =
    Const a

data Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) =
    Identity (f a)
getI ::
  Identity a
  -> a
getI (Identity a) =
  a

get ::
  Lens a b
  -> a
  -> b
get (Lens r) =
  getC . r Const

set ::
  Lens a b
  -> a
  -> b
  -> a
set (Lens r) a b =
  getI (r (const (Identity b)) a)
