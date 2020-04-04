{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

module Music.Time.Duration
  ( module Music.Time.Transform,

    -- * The HasDuration class
    HasDuration (..),

    -- * Absolute duration
    duration,
    stretchToD,
  )
where

import Control.Lens hiding
  ( (<|),
    Indexable,
    Level,
    below,
    index,
    inside,
    parts,
    reversed,
    transform,
    (|>),
  )
import Control.Lens (Getter)
import Data.Functor.Contravariant (Op (..))
import Data.NumInstances ()
import Data.Semigroup hiding ()
import Data.VectorSpace hiding (Sum (..))
import Music.Time.Transform

-- |
-- Class of values that have a duration.
--
-- For any type that is also 'Transformable', you should ensure that:
--
-- Laws:
-- @
-- _duration (transform s x) = transform s (_duration x)
-- @
class HasDuration a where

  -- | Return the duration of a value.
  _duration :: a -> Duration

  {-# MINIMAL _duration #-}

{-
"Standard" values has duration 1 and are non-transformable, to allow storage in recursive containers.
-}

instance HasDuration () where
  _duration _ = 1

instance HasDuration Char where
  _duration _ = 1

instance HasDuration Int where
  _duration _ = 1

instance HasDuration Double where
  _duration _ = 1

instance HasDuration Duration where
  _duration = id

instance HasDuration Span where
  _duration = snd . view onsetAndDuration

--
-- By convention, we treat pairs and triplets as having the form
-- (t,x), (d,x) and (t,d,x) where t has a position and d has a
-- duration. This makes it convenient to represent simple event
-- lists as [(Time, Duration, a)] without needing any special
-- structure.
--

instance HasDuration a => HasDuration (a, b) where
  _duration (d, _) = _duration d

instance HasDuration b => HasDuration (a, b, c) where
  _duration (_, d, _) = _duration d

instance HasDuration a => HasDuration (Product a) where
  _duration (Product x) = _duration x

instance HasDuration a => HasDuration (Sum a) where
  _duration (Sum x) = _duration x

instance HasDuration a => HasDuration (Min a) where
  _duration (Min x) = _duration x

instance HasDuration a => HasDuration (Max a) where
  _duration (Max x) = _duration x

-- For HasDuration [a] we assume parallel composition and
-- use the HasPosition instance, see Music.Time.Position.

instance (HasDuration a, HasDuration b) => HasDuration (Either a b) where
  _duration (Left x) = _duration x
  _duration (Right x) = _duration x

-- |
-- Stretch a value to have the given duration.
stretchToD :: (Transformable a, HasDuration a) => Duration -> a -> a
stretchToD d x = (d ^/ _duration x) `stretch` x

-- |
-- Access the duration.
duration :: (Transformable a, HasDuration a) => Getter a Duration
duration = to _duration
{-# INLINE duration #-}
{-# DEPRECATED duration "Use _duration" #-}
