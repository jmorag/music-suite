{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints #-}

------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------

-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
module Music.Pitch.Alterable
  ( -- * Alterable class
    Alterable (..),
    alter,
  )
where

import Data.Functor.Couple
import Data.Ratio

-- |
-- Class of things that can be altered.
--
-- ==== Laws
--
-- [/inverse/]
--
--    @sharpen . flatten = id = flatten . sharpen@
class Alterable a where

  -- |
  -- Increase the given pitch by one.
  sharpen :: a -> a

  -- |
  -- Decrease the given pitch by one.
  flatten :: a -> a

instance Alterable a => Alterable (b -> a) where

  sharpen = fmap sharpen

  flatten = fmap flatten

instance Alterable a => Alterable (Maybe a) where

  sharpen = fmap sharpen

  flatten = fmap flatten

instance Alterable Double where

  sharpen = (+ 1)

  flatten = (subtract 1)

instance Alterable Integer where

  sharpen = (+ 1)

  flatten = (subtract 1)

instance Integral a => Alterable (Ratio a) where

  sharpen = (+ 1)

  flatten = (subtract 1)

instance Alterable a => Alterable [a] where

  sharpen = fmap sharpen

  flatten = fmap flatten

instance Alterable a => Alterable (b, a) where

  sharpen = fmap sharpen

  flatten = fmap flatten

deriving instance (Alterable a) => Alterable (Couple b a)

{-
sharpened :: Alterable a => Iso' a a
sharpened = iso sharpen flatten

flattened :: Alterable a => Iso' a a
flattened = iso flatten sharpen
-}

alter :: Alterable a => Int -> a -> a
alter n x
  | n < 0 = iterate flatten x !! (abs n)
  | n == 0 = x
  | n > 0 = iterate sharpen x !! n
  | otherwise = error "impossible"
