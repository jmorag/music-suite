-- |
-- Monadic composition.
--
-- Based on an operator found in /Composing Monads/ by Jones and Duponcheel (1993).
--
-- Should be moved to a separate package `control-monad-compose`.
module Control.Monad.Compose
  ( mjoin,
    mbind,
  )
where

import Control.Monad
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Typeable

-- Intution:
--
--  Starts off with                      m (n (m (n a)))
--  Sequences inner structure to get     m (m (n (n a)))
--  Folds outer level to get             m (n (n a))
--  Folds inner level to get             m (n a)

mjoin :: (Monad m, Monad n, Functor m, Traversable n) => m (n (m (n a))) -> m (n a)
mjoin = fmap join . join . fmap sequence

mbind :: (Monad m, Monad n, Functor m, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
mbind = (join .) . fmap . (fmap join .) . traverse
