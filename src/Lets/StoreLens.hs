module Lets.StoreLens(
  Lens(..)
) where

data Store s a =
  Store
    (s -> a)
    s

data Lens a b =
  Lens
    (a -> Store b a)
    