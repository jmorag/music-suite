{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Concurrent
import Control.Lens ((^..))
import Control.Monad.Random
import Control.Monad.Reader
import Data.Char
import Data.String
import Data.Time.Clock.System
import Music.Prelude
import Music.Score.Export.StandardNotation (LilypondLayout (..), LilypondOptions (..))
import System.Environment
import System.Exit
import System.IO
import System.Process.Typed
import Text.Printf

{-
An exploration of using the computer to generate row rules instead of picking
them myself
-}

chaos :: Row a -> Row a -> Row a
chaos original permuted = do
  rng <- getRandomR (0, 100)
  prob <- ask
  if prob <= rng then original else permuted

type Row = ReaderT Double (Rand StdGen)

row1, row2, row3, row4, row5, row6, row7 :: [Music]
row1 = [f, fs, b, bb, a, d', c', cs', e'', g, ab, eb']
row2 = [e', f', fs', cs', b, a, c', bb, d', eb', g', ab']
row3 = [d', eb', f', e', c', b, a, bb, db', ab, g, fs]
row4 = [b_, ds, fs, c, e, g, d, f, a, as, cs', e']
row5 = [cs, gs, e', d, a, f', eb, bb, fs', e, b, g']
row6 = [f, c', ab', fs, cs', a', g, d', bb', gs, ds', b']
row7 = [ds, b_, bb_, d, f, a, fs, cs, e, c', g, ab]

rows :: [[Music]]
rows = [row1, row2, row3, row4, row5, row6, row7]

transpose_ :: [Music] -> Row [Music]
transpose_ = \case
  [] -> pure []
  [n] -> pure [n]
  (x : y : ns) ->
    chaos
      (fmap (x :) (transpose_ (y : ns)))
      (fmap (y :) (transpose_ (x : ns)))

cycle_ :: Int -> [Music] -> Row [Music]
cycle_ len row = do
  n <- getRandomR (0 :: Int, len)
  chaos (pure row) (pure . take len . drop n $ cycle row)

displace_ :: [Music] -> Row [Music]
displace_ = mapM displaceSingle
  where
    displaceSingle :: Music -> Row Music
    displaceSingle n = do
      nOctaves <- getRandomR (-2, 2)
      chaos (pure n) (pure $ over pitches' (displacePitch nOctaves) n)
    displacePitch nOctaves p =
      let p' = octavesUp nOctaves p
       in if inAmbitus (comfortableRange violin) p' then p' else p

combine_ :: [Music] -> Row [Music]
combine_ = \case
  [] -> pure []
  [n] -> pure [n]
  (x : y : ns) -> do
    rng <- getRandomR (0, 100)
    prob <- ask
    let [p1, p2] = map (fromPitch @Int) $ (x <> y) ^.. pitches'
    if and [rng < prob, abs (p2 - p1) <= 15, not ((p1 < d) && (p2 < d))]
      then fmap ((x <> y) :) (combine_ ns)
      else fmap (x :) (combine_ (y : ns))

random_effs :: Double -> [Music] -> Rand StdGen [Music]
random_effs prob mus =
  runReaderT
    ((displace_ >=> transpose_ >=> cycle_ 12 >=> combine_) mus)
    prob

main :: IO ()
main =
  getArgs >>= \case
    [] -> putStrLn "Enter the length of the piece in seconds"
    arg : _ -> do
      hSetBuffering stdin NoBuffering
      keypress <- newEmptyMVar
      let len = read arg
          mu = fromIntegral len / 2
          sigma = fromIntegral len / 6
          normal' = normal mu sigma
      t_0 <- systemSeconds <$> getSystemTime
      forever $ do
        n <- randomRIO (0, 6)
        let row = rows !! n
            base = set parts' (solo violin) (compress 2 row)
        -- Listen for keypress
        tid <- forkIO (getChar >>= putMVar keypress)
        input <- takeMVar keypress
        if ord input == 27 -- ESC key
          then exitSuccess
          else do
            t <- systemSeconds <$> getSystemTime
            loop (normal' (fromIntegral (t - t_0))) n base
            killThread tid
            threadDelay (3 * 10 ^ 6)

loop :: Double -> Int -> [Music] -> IO ()
loop prob rowNum base = do
  music <- evalRandIO (pseq <$> random_effs prob base)
  let title_ = printf "Row: %d, Chaos: %.2f" (rowNum + 1) prob
      allRows = music <> rcat (map pseq rows) |/ 2
  exportLy
    (LilypondOptions LilypondBigScore)
    (title (fromString title_) $ allRows)
    "rows.ly"
  runProcess_ (shell "lilypond rows.ly")

-- scaled to lie in (0, 100)
normal :: Floating a => a -> a -> a -> a
normal mu sigma t = 100 * exp (- ((t - mu) * (t - mu)) / (2 * sigma * sigma))
