{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Concurrent
import           Control.Lens                   ( (^..) )
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.Char
import           Data.String
import           Data.Time.Clock.System
import           Music.Prelude           hiding ( option )
import           Music.Score.Export.StandardNotation
                                                ( LilypondLayout(..)
                                                , LilypondOptions(..)
                                                )
import           System.Exit
import           System.IO
import           System.Process.Typed
import           Text.Printf
import           Text.Read
import           Options.Applicative

{-
An exploration of using the computer to generate row rules instead of picking
them myself
-}

chaotically :: (a -> a) -> a -> Row a
chaotically fn row = do
  rng  <- getRandomR (0, 100)
  prob <- ask
  if prob <= rng then pure row else pure (fn row)

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
  []           -> pure []
  [n         ] -> pure [n]
  (x : y : ns) -> do
    rng  <- getRandomR (0, 100)
    prob <- ask
    if prob <= rng
      then fmap (x :) (transpose_ (y : ns))
      else fmap (y :) (transpose_ (x : ns))

cycle_ :: [Music] -> Row [Music]
cycle_ row = do
  n <- getRandomR (0 :: Int, 12)
  chaotically (take 12 . drop n . cycle) row

displace_ :: [Music] -> Row [Music]
displace_ = mapM displaceSingle
 where
  displaceSingle :: Music -> Row Music
  displaceSingle n = do
    nOctaves <- getRandomR (-2, 2)
    chaotically (over pitches' (displacePitch nOctaves)) n
  displacePitch nOctaves p =
    let p' = octavesUp nOctaves p
    in  if inAmbitus (comfortableRange violin) p' then p' else p

combine_ :: [Music] -> Row [Music]
combine_ = \case
  []           -> pure []
  [n         ] -> pure [n]
  (x : y : ns) -> do
    rng  <- getRandomR (0, 100)
    prob <- ask
    let [p1, p2] = map (fromPitch @Int) $ (x <> y) ^.. pitches'
    if and [rng < prob, abs (p2 - p1) <= 15, not ((p1 < d) && (p2 < d))]
      then fmap ((x <> y) :) (combine_ ns)
      else fmap (x :) (combine_ (y : ns))

random_effs :: Double -> [Music] -> Rand StdGen [Music]
random_effs prob mus =
  runReaderT ((displace_ >=> transpose_ >=> cycle_ >=> combine_) mus) prob

data Options
  = Options
      { -- | Seconds
        pieceLen :: Int,
        -- | Number of complete rows cycles to output at a time
        phraseLen :: Int,
        -- | Which rows to use
        rowNums :: [Int]
      }
  deriving (Show, Eq)

parseOpts :: Parser Options
parseOpts =
  Options
    <$> option auto                       lengthOpts
    <*> option auto                       phraseOpts
    <*> option (eitherReader readNumList) rowsOpts
 where
  readNumList = traverse (inBounds <=< (fmap pred . readEither)) . words
  inBounds n | n >= 0 && n < length rows = Right n
             | otherwise = Left ("Out of bounds row " ++ show (n + 1))
  lengthOpts =
    long "seconds"
      <> short 's'
      <> metavar "SECONDS"
      <> help "length of piece"
      <> value 300
  phraseOpts =
    long "phrase"
      <> short 'p'
      <> metavar "INT"
      <> help "number of row permutations to output at once"
      <> value 1
  rowsOpts =
    long "rows"
      <> metavar "[INT]"
      <> help "which rows to use (space separated numbers 1 through 7)"
      <> value [0 .. 6]


main :: IO ()
main = do
  Options {..} <- execParser $ info (parseOpts <**> helper) fullDesc
  hSetBuffering stdin NoBuffering
  keypress <- newEmptyMVar
  let len     = pieceLen
      mu      = fromIntegral len / 2
      sigma   = fromIntegral len / 6
      normal' = normal mu sigma
  t_0 <- systemSeconds <$> getSystemTime
  forever $ do
    -- Listen for keypress
    tid   <- forkIO (getChar >>= putMVar keypress)
    input <- takeMVar keypress
    if ord input == 27 -- ESC key
      then exitSuccess
      else do
        t <- systemSeconds <$> getSystemTime
        let prob      = normal' (fromIntegral (t - t_0))
            phraseLen = ord input - 48
        ns <- replicateM phraseLen $ randomRIO (0, length rowNums - 1)
        let rows' = map (rows !!) (map (rowNums !!) ns)
            prob' = if prob == 0 then 0 else if prob < 10 then 10 else prob
        music <- evalRandIO (traverse (random_effs prob') rows')
        let
          title_ = printf "Row - Chaos: %.2f" prob
          allRows =
            (set parts'
                 (solo violin)
                 (compress (fromIntegral phraseLen) (pseq (concat music)))
              )
              <> rcat (map (pseq . (rows !!)) rowNums)
        exportLy (LilypondOptions LilypondBigScore)
                 (title (fromString title_) $ allRows)
                 "rows.ly"
        runProcess_ (shell "lilypond rows.ly")
        killThread tid
        threadDelay (3 * 10 ^ 6)



-- scaled to lie in (0, 100)
normal :: Floating a => a -> a -> a -> a
normal mu sigma t = 100 * exp (-((t - mu) * (t - mu)) / (2 * sigma * sigma))
