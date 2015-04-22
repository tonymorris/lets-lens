module Lets.GetSetLens(
  Lens(..)
) where

data Lens a b =
  Lens
    (a -> b)
    (a -> b -> a)
  