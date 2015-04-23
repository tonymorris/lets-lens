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

data Lens a b =
  Lens
    (a -> b -> a)
    (a -> b)

-- | The get/set law of lenses. This function should always return @True@.
getsetLaw ::
  Eq a =>
  Lens a b
  -> a
  -> Bool
getsetLaw (Lens s g) =
  \a -> s a (g a) == a

-- | The set/get law of lenses. This function should always return @True@.
setgetLaw ::
  Eq b =>
  Lens a b
  -> a
  -> b
  -> Bool
setgetLaw (Lens s g) a b =
  g (s a b) == b

-- | The set/set law of lenses. This function should always return @True@.
setsetLaw ::
  Eq a =>
  Lens a b
  -> a
  -> b
  -> b
  -> Bool
setsetLaw (Lens s _) a b1 b2 =
  s (s a b1) b2 == s a b2

-- |
--
-- >>> get fstL (0 :: Int, "abc")
-- 0
--
-- >>> get sndL ("abc", 0 :: Int)
-- 0
--
-- prop> let types = (x :: Int, y :: String) in get fstL (x, y) == x
--
-- prop> let types = (x :: Int, y :: String) in get sndL (x, y) == y
get ::
  Lens a b
  -> a
  -> b
get (Lens _ g) =
  g

-- |
--
-- >>> set fstL (0 :: Int, "abc") 1
-- (1,"abc")
--
-- >>> set sndL ("abc", 0 :: Int) 1
-- ("abc",1)
--
-- prop> let types = (x :: Int, y :: String) in set fstL (x, y) z == (z, y)
--
-- prop> let types = (x :: Int, y :: String) in set sndL (x, y) z == (x, z)
set ::
  Lens a b
  -> a 
  -> b
  -> a
set (Lens s _) a =
  s a

-- |
--
-- >>> modify fstL (+1) (0 :: Int, "abc")
-- (1,"abc")
--
-- >>> modify sndL (+1) ("abc", 0 :: Int)
-- ("abc",1)
--
-- prop> let types = (x :: Int, y :: String) in modify fstL id (x, y) == (x, y)
--
-- prop> let types = (x :: Int, y :: String) in modify sndL id (x, y) == (x, y)
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

-- |
--
-- prop> let types = (x :: Int, y :: String) in getsetLaw fstL (x, y)
--
-- prop> let types = (x :: Int, y :: String) in setgetLaw fstL (x, y) z
--
-- prop> let types = (x :: Int, y :: String) in setsetLaw fstL (x, y) z
fstL ::
  Lens (x, y) x
fstL =
  Lens
    (\(_, y) x -> (x, y))
    (\(x, _) -> x)

-- |
--
-- prop> let types = (x :: Int, y :: String) in getsetLaw sndL (x, y)
--
-- prop> let types = (x :: Int, y :: String) in setgetLaw sndL (x, y) z
--
-- prop> let types = (x :: Int, y :: String) in setsetLaw sndL (x, y) z
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

compose ::
  Lens b c
  -> Lens a b
  -> Lens a c
compose (Lens s1 g1) (Lens s2 g2) =
  Lens
    (\a -> s2 a . s1 (g2 a))
    (g1 . g2)

product ::
  Lens a b
  -> Lens c d
  -> Lens (a, c) (b, d)
product (Lens s1 g1) (Lens s2 g2) =
  Lens
    (\(a, c) (b, d) -> (s1 a b, s2 c d))
    (\(a, c) -> (g1 a, g2 c))

choice ::
  Lens a x
  -> Lens b x
  -> Lens (Either a b) x
choice (Lens s1 g1) (Lens s2 g2) =
  Lens
    (\e x -> either (\a -> Left (s1 a x)) (\b -> Right (s2 b x)) e)
    (either g1 g2)

{-

enforce laws
usage example

-}
