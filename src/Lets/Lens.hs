module Lets.Lens where

import Data.Monoid(Monoid(..))
import Data.Traversable(Traversable(..))
import Control.Applicative(Applicative(..))

-- Let's remind ourselves of Traversable, noting Foldable and Functor.
--
-- class (Foldable t, Functor t) => Traversable t where
--   traverse ::
--     Applicative f => 
--     (a -> f b)
--     -> t a
--     -> f (t b)

data Const a b =
  Const {
    getConst :: 
      a
  }

instance Functor (Const a) where
  fmap _ (Const a) =
    Const a

instance Monoid a => Applicative (Const a) where
  pure _ =
    Const mempty
  Const f <*> Const a =
    Const (f `mappend` a)

data Identity a =
  Identity {
    getIdentity ::
      a
  }

instance Functor Identity where
  fmap f (Identity a) =
    Identity (f a)

instance Applicative Identity where
  pure =
    Identity
  Identity f <*> Identity a =
    Identity (f a)

-- | Observe that @fmap@ can be recovered from @traverse@ using @Identity@.
--
-- /Reminder:/ fmap :: Functor t => (a -> b) -> t a -> t b
fmapT ::
  Traversable t =>
  (a -> b)
  -> t a
  -> t b
fmapT f =
  getIdentity . traverse (Identity . f)

-- | Observe that @fmap@ can be recovered from @traverse@ using @Identity@.
--
-- /Reminder:/ foldMap :: (Foldable t, Monoid b) => (a -> b) -> t a -> b
foldMapT ::
  (Traversable t, Monoid b) =>
  (a -> b)
  -> t a
  -> b
foldMapT f =
  getConst . traverse (Const . f)
