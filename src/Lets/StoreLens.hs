{-# LANGUAGE ScopedTypeVariables #-}

module Lets.StoreLens {-(
  Lens(..)
, Store(..)
) -} where

import Control.Applicative((<*>))
import Data.Bool(bool)
import Data.Functor((<$>))
import Data.Map(Map)
import qualified Data.Map as Map(insert, delete, lookup)
import Data.Maybe(maybe)
import Data.Set(Set)
import qualified Data.Set as Set(insert, delete, member)

data Store s a =
  Store
    (s -> a)
    s

setS ::
  Store s a
  -> s -> a
setS (Store s _) =
  s

getS ::
  Store s a
  -> s
getS (Store _ g) =
  g

-- todo; functor, comonad

data Lens a b =
  Lens
    (a -> Store b a)

-- | The get/set law of lenses. This function should always return @True@.
getsetLaw ::
  Eq a =>
  Lens a b
  -> a
  -> Bool
getsetLaw (Lens r) =
  \a -> 
    let Store s g = r a 
    in s g == a
  
-- | The set/get law of lenses. This function should always return @True@.
setgetLaw ::
  Eq b =>
  Lens a b
  -> a
  -> b
  -> Bool
setgetLaw (Lens r) a b =
  getS (r (setS (r a) b)) == b

-- | The set/set law of lenses. This function should always return @True@.
setsetLaw ::
  Eq a =>
  Lens a b
  -> a
  -> b
  -> b
  -> Bool
setsetLaw (Lens r) a b1 b2 =
  let s = setS (r a)
  in setS (r (s b1)) b2 == s b2

get ::
  Lens a b
  -> a
  -> b
get (Lens r) =
  getS . r

set ::
  Lens a b
  -> a 
  -> b
  -> a
set (Lens r) =
  setS . r

modify ::
  Lens a b
  -> (b -> b)
  -> a
  -> a
modify (Lens r) f a =
  let Store s g = r a
  in s (f g)

-- | An alias for @modify@.
(%~) ::
  Lens a b
  -> (b -> b)
  -> a
  -> a
(%~) =
  modify

infixr 4 %~

(.~) ::
  Lens a b
  -> b
  -> a
  -> a
(.~) l =
  modify l . const

infixl 5 .~

fmodify ::
  Functor f =>
  Lens a b
  -> (b -> f b)
  -> a
  -> f a
fmodify (Lens r) f a =
  let Store s g = r a
  in fmap s (f g)
  
(|=) ::
  Functor f =>
  Lens a b
  -> f b
  -> a
  -> f a
(|=) l =
  fmodify l . const

infixl 5 |=

fstL ::
  Lens (x, y) x
fstL =
  Lens
    (\(x, y) -> Store (\x' -> (x', y)) x)

sndL ::
  Lens (x, y) y
sndL =
  Lens
    (\(x, y) -> Store (\y' -> (x, y')) y)

mapL ::
  Ord k =>
  k
  -> Lens (Map k v) (Maybe v)
mapL k =
  Lens
    (Store <$> (maybe . Map.delete k <*> (flip (Map.insert k))) <*> Map.lookup k)

setL ::
  Ord k =>
  k
  -> Lens (Set k) Bool
setL k =
  Lens
    (Store <$> (bool . Set.delete k <*> Set.insert k) <*> Set.member k)

compose ::
  Lens b c
  -> Lens a b
  -> Lens a c
compose (Lens r1) (Lens r2) =
  Lens
    (\a -> let q = r2 a in Store undefined undefined)
  {-
  Lens
    (\a -> s2 a . s1 (g2 a))
    (\a x -> s2 a (s1 (g2 a) x)))
    (g1 . g2)
    -}


