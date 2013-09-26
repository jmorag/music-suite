
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,
    DeriveDataTypeable,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    GeneralizedNewtypeDeriving #-}

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
-- Provides functions for manipulating ornaments (and miscellaneous stuff to be
-- given its own module soon...).
--
-------------------------------------------------------------------------------------


module Music.Score.Ornaments (
        -- * Tremolo
        HasTremolo(..),
        TremoloT(..),
        tremolo,

        -- * Text
        HasText(..),
        TextT(..),
        text,

        -- * Harmonics
        HasHarmonic(..),
        HarmonicT(..),
        harmonic,
        artificial,

        -- * Slides and glissando
        HasSlide(..),
        SlideT(..),
        slide,
        glissando,
  ) where

import Data.Ratio
import Data.Foldable
import Data.Semigroup
import Data.Typeable

import Music.Time
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Part
import Music.Score.Combinators

class HasTremolo a where
    setTrem :: Int -> a -> a

newtype TremoloT a = TremoloT { getTremoloT :: (Int, a) }
    deriving (Eq, Show, Ord, Functor{-, Foldable-}, Typeable)

instance HasTremolo (TremoloT a) where
    setTrem      n (TremoloT (_,x))                 = TremoloT (n,x)

instance HasTremolo b => HasTremolo (a, b) where
    setTrem n = fmap (setTrem n)





class HasText a where
    addText :: String -> a -> a

newtype TextT a = TextT { getTextT :: ([String], a) }
    deriving (Eq, Show, Ord, Functor{-, Foldable-}, Typeable)

instance HasText (TextT a) where
    addText      s (TextT (t,x))                    = TextT (t ++ [s],x)





-- 0 for none, positive for natural, negative for artificial
class HasHarmonic a where
    setHarmonic :: Int -> a -> a

newtype HarmonicT a = HarmonicT { getHarmonicT :: (Int, a) }
    deriving (Eq, Show, Ord, Functor{-, Foldable-}, Typeable)

instance HasHarmonic (HarmonicT a) where
    setHarmonic   n (HarmonicT (_,x))               = HarmonicT (n,x)

instance HasHarmonic a => HasHarmonic (b, a) where
    setHarmonic   n                                 = fmap (setHarmonic n)
instance HasSlide a => HasSlide (b, a) where
    setBeginGliss n                                 = fmap (setBeginGliss n)
    setBeginSlide n                                 = fmap (setBeginSlide n)
    setEndGliss   n                                 = fmap (setEndGliss n)
    setEndSlide   n                                 = fmap (setEndSlide n)
instance HasText a => HasText (b, a) where
    addText       s                                 = fmap (addText s)


class HasSlide a where
    setBeginGliss :: Bool -> a -> a
    setBeginSlide :: Bool -> a -> a
    setEndGliss   :: Bool -> a -> a
    setEndSlide   :: Bool -> a -> a

newtype SlideT a = SlideT { getSlideT :: (Bool, Bool, a, Bool, Bool) }
    deriving (Eq, Show, Ord, Functor{-, Foldable-}, Typeable)

instance HasSlide (SlideT a) where
    setBeginGliss bg (SlideT (eg,es,a,_,bs))       = SlideT (eg,es,a,bg,bs)
    setBeginSlide bs (SlideT (eg,es,a,bg,_))       = SlideT (eg,es,a,bg,bs)
    setEndGliss   eg (SlideT (_,es,a,bg,bs))       = SlideT (eg,es,a,bg,bs)
    setEndSlide   es (SlideT (eg,_,a,bg,bs))       = SlideT (eg,es,a,bg,bs)


-- |
-- Set the number of tremolo divisions for all notes in the score.
--
tremolo :: (Functor s, HasTremolo b) => Int -> s b -> s b
tremolo n = fmap (setTrem n)

-- |
-- Attach the given text to the first note in the score.
--
text :: (HasPart' a, HasText a) => String -> Score a -> Score a
text s = mapPhrase (addText s) id id

-- |
-- Add a slide between the first and the last note.
--
slide :: (HasPart' a, HasSlide a) => Score a -> Score a
slide = mapPhrase (setBeginSlide True) id (setEndSlide True)

-- |
-- Add a glissando between the first and the last note.
--
glissando :: (HasPart' a, HasSlide a) => Score a -> Score a
glissando = mapPhrase (setBeginGliss True) id (setEndGliss True)

-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
--
harmonic :: (Functor s, HasHarmonic a) => Int -> s a -> s a
harmonic n = fmap (setHarmonic n)

-- |
-- Make all notes natural harmonics on the given overtone (1 for octave, 2 for fifth etc).
-- Sounding pitch is unaffected, but notated output is transposed automatically.
--
artificial :: (Functor s, HasHarmonic a) => s a -> s a
artificial = fmap f where f = setHarmonic (-4)

