{-# LANGUAGE ScopedTypeVariables #-}

module Lets.GetSetLens(
  Lens(..)
) where


data Lens a b =
  Lens
    (a -> b -> a)
    (a -> b)
    
get ::
  Lens a b
  -> a
  -> b
get (Lens _ g) =
  g

set ::
  Lens a b
  -> a 
  -> b
  -> a
set (Lens s _) =
  s

modify ::
  Lens a b
  -> (b -> b)
  -> a
  -> a
modify (Lens s g) f a =
  s a (f (g a))

(.=) ::
  Lens a b
  -> b
  -> a
  -> a
(.=) l =
  modify l . const
