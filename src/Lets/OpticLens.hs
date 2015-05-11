{-# LANGUAGE RankNTypes #-}

module Lets.OpticLens where

data Lens a b =
  Lens
    (forall f. Functor f => (b -> f b) -> a -> f a)

data Const a b =
  Const {
    getConst :: 
      a
  }

instance Functor (Const a) where
  fmap _ (Const a) =
    Const a

data Identity a =
  Identity {
    getIdentity ::
      a
  }

instance Functor Identity where
  fmap f (Identity a) =
    Identity (f a)

get ::
  Lens a b
  -> a
  -> b
get (Lens r) =
  getConst . r Const

set ::
  Lens a b
  -> a
  -> b
  -> a
set (Lens r) a b =
  getIdentity (r (const (Identity b)) a)

-- | The get/set law of lenses. This function should always return @True@.
getsetLaw ::
  Eq a =>
  Lens a b
  -> a
  -> Bool
getsetLaw l =
  \a -> set l a (get l a) == a
  
-- | The set/get law of lenses. This function should always return @True@.
setgetLaw ::
  Eq b =>
  Lens a b
  -> a
  -> b
  -> Bool
setgetLaw l a b =
  get l (set l a b) == b
  
-- | The set/set law of lenses. This function should always return @True@.
setsetLaw ::
  Eq a =>
  Lens a b
  -> a
  -> b
  -> b
  -> Bool
setsetLaw l a b1 b2 =
  set l (set l a b1) b2 == set l a b2
