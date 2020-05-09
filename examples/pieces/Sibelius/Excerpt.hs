{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

-- import Control.Lens
import Music.Prelude

topVoice :: Music
topVoice = pseq ([eb'', d'' |* 2, c'', c'', bb' |* 2, a'] |/ 8)

bottomVoice :: Music
bottomVoice = pseq ([eb', fs', eb', c', fs', c'] |/ 6)

-- music :: Score StandardNote
music = inspectableToMusic $ ppar [topVoice, bottomVoice]

main = defaultMain (timeSignature (4 / 4) music)

