{-# LANGUAGE ScopedTypeVariables #-}

module Lets.GetSetLens(
  Lens(..)
) where

import Control.Applicative((<*>))
import Data.Bool(bool)
import Data.Map(Map)
import qualified Data.Map as Map(insert, delete, lookup)
import Data.Maybe(maybe)
import Data.Set(Set)
import qualified Data.Set as Set(insert, delete, member)
import Prelude

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

fmodify ::
  Functor f =>
  Lens a b
  -> (b -> f b)
  -> a
  -> f a
fmodify (Lens s g) f a =
  fmap (s a) (f (g a))

(|=) ::
  Functor f =>
  Lens a b
  -> f b
  -> a
  -> f a
(|=) l =
  fmodify l . const

fstL ::
  Lens (x, y) x
fstL =
  Lens
    (\(_, y) x -> (x, y))
    (\(x, _) -> x)

sndL ::
  Lens (x, y) y
sndL =
  Lens
    (\(x, _) y -> (x, y))
    (\(_, y) -> y)

mapL ::
  Ord k =>
  k
  -> Lens (Map k v) (Maybe v)
mapL k =
  Lens
    (maybe . Map.delete k <*> (flip (Map.insert k)))
    (Map.lookup k)

setL ::
  Ord k =>
  k
  -> Lens (Set k) Bool
setL k =
  Lens
    (bool . Set.delete k <*> Set.insert k)
    (Set.member k)

{-

laws

-}
