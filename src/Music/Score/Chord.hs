
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides a representation for chords.
--
-------------------------------------------------------------------------------------


module Music.Score.Chord (
        -- * Chord representation
        HasChord(..),
        ChordT(..),      
        
        -- * Chord transformations
        renderChord,
        simultaneous,
  ) where

import Data.Ratio
import Data.Foldable (Foldable(..))
import Data.Typeable
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Control.Monad.Plus       
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.List as List

import Music.Score.Voice
import Music.Score.Score
import Music.Time
import Music.Score.Part
import Music.Score.Pitch
import Music.Score.Combinators

class HasChord a where
    type ChordNote a :: *
    getChord :: a -> [ChordNote a]
    -- Score a -> Score a
    -- modifyChord :: (ChordNote a -> ChordNote a) -> a -> a

instance HasChord [a] where
    type ChordNote [a] = a
    getChord = id

-- Actually we should use NonEmpty here
-- Empty chords will cause error with HasPitch, among others
newtype ChordT a = ChordT { getChordT :: [a] }
    deriving (Eq, Show, Ord, Monad, Functor, Foldable, Typeable)

-- instance HasChord 

-- Score a -> Score (ChordT a)

-- Note:                                                    
--
-- The HasChord instance (for other transformer types) takes care to transform strucuture *above* the chord representation
--      In particular, getChord will extract the chord from below and transform each note (or only the first etc) 
--      as appropriate for the given type.
-- The ChordT instances (of other transformer classes) transforms structure *below* the chord representation
--      For example, it allow us to use functions such as up, down, legato etc on chords.

-- |
-- Render all chords of a given score into singular notes composed in parallel.
--
renderChord :: (MonadPlus m, HasChord a) => m a -> m (ChordNote a)
renderChord = mscatter . fmap' getChord
    where fmap' = liftM



-- Score [a] -> Score [a]


-- |
-- Move all simultaneous events into chords.
--
-- Two events are considered simultaneous iff their onset and offset are the same.
--
simultaneous :: Score a -> Score (ChordT a)
simultaneous = fmap ChordT . simultaneous'

simultaneous' :: Score a -> Score [a]
simultaneous' sc = compose vs
    where
        -- es :: [Era]
        -- evs :: [[a]]
        -- vs :: [(TimeT, DurationT, [a])]
        es  = eras sc
        evs = fmap (`events` sc) es
        vs  = zipWith (\(t,d) a -> (t,d,a)) es evs

eras :: Score a -> [Era]
eras sc = fmap getEra . perform $ sc

events :: Era -> Score a -> [a]
events era sc = fmap getValue . filter (\ev -> getEra ev == era) . perform $ sc




getValue :: (TimeT, DurationT, a) -> a
getValue (t,d,a) = a

getEra :: (TimeT, DurationT, a) -> Era
getEra (t,d,a) = (t,d)        

type Era = (TimeT, DurationT)

-- Fold over all consecutive events

-- consec (\a b -> if a == b then [a] else [a,b]) [1,1,1,2,3] = [1,2,3]

-- consec :: (a -> a -> [a]) -> [a] -> [a]           

-- consec f as = List.mapAccumL (\(prev, elems) current -> ( (Just x, []) , x)) (Nothing, []) as






-- consec f = go
--     where
--         go []       = []
--         go [a]      = [a]
--         go (a:b:cs) = f a b : go cs
--         
        



