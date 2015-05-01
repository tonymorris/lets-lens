{-# LANGUAGE ScopedTypeVariables #-}

module Lets.StoreLens(
  Lens(..)
, Store(..)
) where

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
  
