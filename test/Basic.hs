
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies
  , QuasiQuotes, DataKinds, MultiParamTypeClasses #-}

import Data.Functor.Identity(Identity(..))
import Control.Lens(set)

import Music.Prelude
import qualified Music.Pitch
import qualified Music.Parts
import qualified Music.Dynamics
import qualified Music.Articulation
import qualified Music.Score
import Music.Score.Export.StandardNotation(fromAspects, Work, PureExportM, runPureExportMNoLog, umts_export)

import Record(r, l)
import Record.Types -- TODO import list
-- Pitch and interval literals

{-

semitones (2:: ChromaticSteps)  == 2
semitones (9:: ChromaticSteps)  == 9
semitones (-3:: ChromaticSteps) == -3


c .+ m3     ==    eb
c' .+ m3'   ==    eb'
g' .-. c    ==    (_P5 ^+^ _P8)

-- TODO can not see these instances in Haddocks
instance HasSemitones Music.Pitch.Interval
instance HasNumber Music.Pitch.Interval
instance HasQuality Music.Pitch.Interval

-- TODO Diatonic/ChromaticSteps Show instance
-- TODO move or rename Music.Pitch.Common.Number diatonicSteps

semitones (m3 :: Music.Pitch.Interval)    == (3 :: Semitones)
semitones (m3 :: Music.Pitch.Interval)    == 3
semitones (_A4 :: Music.Pitch.Interval)   == tritone
semitones (d5 :: Music.Pitch.Interval)    == tritone
isTritone (d5 :: Music.Pitch.Interval)    == True

((_A2 :: Music.Pitch.Interval) /= m3)   == True
((_A2 :: Music.Pitch.Interval) =:= m3)  == True
((_A2 :: Music.Pitch.Interval) == m3)   == False
((_A2 :: Music.Pitch.Interval) /:= m3)  == False

expectedQualityType 3 == MajorMinorType
True
expectedQualityType 2 == MajorMinorType
True
expectedQualityType (-2) == MajorMinorType
True
expectedQualityType (-9) == MajorMinorType
True
expectedQualityType (-11) == PerfectType
True
expectedQualityType (-4) == PerfectType
True
expectedQualityType (-5) == PerfectType
True
quality (m3 :: Music.Pitch.Interval) == Music.Pitch.Minor
True
quality (_A4 :: Music.Pitch.Interval) == Music.Pitch.Augmented 1
True
quality (m3 :: Music.Pitch.Interval) == Music.Pitch.Minor
True
quality (_A4 :: Music.Pitch.Interval) == Music.Pitch.Augmented 1
True
qualityToAlteration Upward PerfectType Music.Pitch.Minor
Nothing
qualityToAlteration Upward MajorMinorType  Music.Pitch.Minor
Just (-1)
qualityToAlteration Downward MajorMinorType  Music.Pitch.Minor
Just 0
qualityToAlteration Downward MajorMinorType  (Music.Pitch.Augmented 1)
Just (-2)
qualityToAlteration Downward PerfectType   (Music.Pitch.Augmented 1)
Just (-1)

-- TODO the Isos inteval/interval'/interval'' are really confusing
-- TODO swap/change names of _number/number, _quality/quality, steps, alteration

-- TODO test Music.Pitch(fifth, augmened...etc)
-- TODO test Music.Pitch(natural, flat ...etc)
-- TODO test Music.Pitch(perfect, major, minor, augmented, diminished, doublyAugmented, doublyDiminished)
-- TODO test Music.Pitch(isNegative, isPositive, isNonNegative, isStep, isLeap)
-- TODO test Music.Pitch(isSimple, isCompound, separate, simple, octaves)
-- TODO unify the two fifths/octaves types
-- TODO test/doc Music.Pitch.invert better

-- TODO better example use of the HasBasis interval
-- Nicer relation to convertBasis/convertBasisFloat/intervalDiv

-- TODO nicer alternative to mkPitch (also including octaves)
-- TODO nicer alternative to upDiatonicP et al
  -- Interval instance (w.o. need to provide origin)
  -- Maybe provide tonic/origin in a reader monad?

-- Nicer spelling/normalization API, including function to remove all
-- overflow spellings (this should be used by default by backends)

-- Rename the Music.Pitch.Equal(Equal) to something better
-- Doc Music.Pitch.Clef(positionPitch ...)

-- TODO generic ambitus/chord (w o time)/scale
-- Solve chord naming dilemma


-- TODO basic rhythm library (use tree representation)






-- TODO swap/change names of _solo, _subpart, _instrument
-- TODO is the subpart definition really sound?
-- I.e. how do we handle colliding notes when working with arbitrary string
-- division, for instance?






-- TODO fix problems with phrase traversals





-- TODO test/fix meta-data









-- TODO new TT imple (based on MTL derivation)
  -- Proper HasMeta inst for notes?




-- TODO fix type of voice zips (use real n-tuples!)
-- TODO fix/remove valuesV/durationsV in Time.Voice (what is the alternative?)





-- TODO transformations back/forth betwe Score and more restricted types
-- Laws governing this







-- TODO what to do with negative durations/spans?
Reasoning:
  We allow negative semitones/intervals
  For all affine (point-like) types such as pitch, dynamics, articulation etc "negative" is kind of arbitrary anyway (as the origin/zero is arbitrary)

  For vector-like types (interval, duration etc), negativity is OK, but note that we often want to talk about magnitude and direction separately
  (as in "a downward minor third").

  Is there a process to simply "turning a vector around" if it is negative? What is it called?

















Show instances



Aligned - OK but use showsPrec
> aligned 2 0 ()
aligned (2) (0) (())
> aligned 2 0 EQ
aligned (2) (0) (EQ)

> (1<->2, GT)^.event
(1 <-> 2,GT)^.event
> (3,GT)^.note
(3,GT)^.note
> (3,GT)^.placed
(3,GT)^.placed

-- TODO AddMeta
-- We should just use pure here, as meta-data can be ignored AFA Show/Eq/Ord is concerned
-- (I.e. they are defined up to meta-data, or up to meta-data modification).

> pure GT :: AddMeta Ordering
AddMeta {getAddMeta = Twain {getTwain = ({ meta },GT)}}
-- TODO Show Reactive?
> [(3,GT)^.note]^.voice
[(3,GT)^.note]^.voice
> [(1<->2, GT)^.event]^.score
[(1 <-> 2,GT)^.event]^.score
-- TODO Track
? [(3,GT)^.placed]^.track
Track {getTrack = [(3,GT)^.placed]}

> 2 :: Time
2

> 2 :: Duration
2
> 2 <-> 3
2 <-> 3


-- TODO Dynamics
> ff :: Music.Dynamics.Dynamics
Average {getAverage = [2.5]}


-- TODO Articulation
> mempty :: Music.Articulation.Articulation
(Average {getAverage = []},Average {getAverage = []})

-- TODO Part
> mempty :: Music.Parts.Part
Piano

> cs :: Music.Pitch.Pitch
cs

> m3 :: Music.Pitch.Interval
m3


-- TODO Fifths/Cents
music-suite> 3 :: Music.Pitch.Fifths
Fifths {getFifths = 3.0 Hz}
music-suite> 3 :: Music.Pitch.Cents
Cents {getCents = 3.0 Hz}

-- TODO DiatonicSteps/ChromaticSteps
> 3  :: DiatonicSteps
DiatonicSteps {getDiatonicSteps = 3}
>
> 3 :: Chromatic

<interactive>:277:6:
    Not in scope: type constructor or class ‘Chromatic’
    A data constructor of that name is in scope; did you mean DataKinds?
> 3 :: ChromaticSteps
ChromaticSteps {getChromaticSteps = 3}
> 3 :: Music.Pitch.Octaves
3
> 3 :: Music.Pitch.Semitones
ChromaticSteps {getChromaticSteps = 3}
> Augment
Augmentable  Augmented
> Augmented 2
Augmented 2
> third
3
> 3 :: Number
3
>
> C :: Name
C


-- TODO showsPrec for sharp
> sharpen doubleSharp
sharp * 3
> sharp * 3
sharp * 3

-- TODO Bounded instance for Pitch.Name

-- TODO Show instances for meta-types (unnecessary?)



-}


-- Rhythms
-- Percussion instruments
-- Scales
-- Chords
-- Bass lines
-- Chord sequences
-- (Hierachical) melodies
-- Orchestration patterns


-- TODO comprehensive export tests, i.e. piano/orchestral scores/quartets/vocal/pop/unusual stuff

-- "infinite" rhytmical and harmonic patterns
-- Misc 20th century harmony tehcniques

-- Voice separation, part extraction
-- I.e. string divisions (a la Mist)

-- Hierarchical/inexact melody, variation

-- Counterpoint

-- Converting scores to more restricted forms and back
-- Using the monoid instances of score/voice

-- music-suite/test/legacy-music-files/articulation_all_accents.music
articulation_all_accents :: Music
articulation_all_accents =
  accent (pseq [c..g]|/8)
      </>
  marcato (pseq [c..g]|/8)


-- music-suite/test/legacy-music-files/articulation_all_separations.music
articulation_all_separations :: Music
articulation_all_separations =
  legato (pseq [c..g]|/8)
      </>
  staccato (pseq [c..g]|/8)
      </>
  portato (pseq [c..g]|/8)
      </>
  tenuto (pseq [c..g]|/8)
      </>
  separated (pseq [c..g]|/8)
      </>
  spiccato (pseq [c..g]|/8)


-- music-suite/test/legacy-music-files/articulation_legato.music
articulation_legato :: Music
articulation_legato =
  legato (pseq [c..g]|/8)


-- music-suite/test/legacy-music-files/articulation_portato.music
articulation_portato :: Music
articulation_portato =
  portato (pseq [c..g]|/8)


-- music-suite/test/legacy-music-files/articulation_staccato.music
articulation_staccato :: Music
articulation_staccato =
  staccato (pseq [c..g]|/8)
-- TODO articulation, more high-level combinators (a la photoshop)


-- music-suite/test/legacy-music-files/decl_style1.music
-- decl_style1 =
--
--   data Foo = Foo | Bar
--
--   scale Foo = pseq [c,d,e,f,g,a,g,f]|/8
--   scale Bar = scale Foo
--
--   triad a = a <> up _M3 a <> up _P5 a
--
--   example = up _P8 (scale Foo) </> (triad c)|/2 |> (triad g_)|/2



-- music-suite/test/legacy-music-files/dynamics_constant.music
-- dynamics_test :: Music
-- dynamics_test =
--   pseq $ zipWith level [fff,ff,_f,mf,mp,_p,pp,ppp] [c..]

dynamics_test2 :: Music
dynamics_test2 =
  pseq $ louder 1 $ zipWith level [pp,ff,pp] [c,d,e]

dynamics_test3 :: Music
dynamics_test3 =
  pseq $ softer 1 $ zipWith level [pp,ff,pp] [c,d,e]

dynamics_test4 :: Music
dynamics_test4 =
  pseq $ softer (ff-pp) $ zipWith level [pp,ff,pp] [c,d,e]

-- TODO more basic dynamics stuff
-- TODO music-part
-- TODO music-pitch



-- TODO more dynamics (fadeIn, fadeOut, alternate fade curves, compress up/down)

-- TODO ties
-- We probably need to retain this for internal purposes, but can we trim
-- the API?

-- TODO color
-- Should be moved to meta

-- music-suite/test/legacy-music-files/melody_chords.music
melody_chords :: Music
melody_chords =
  let
      scale = pseq [c,d,e,f,g,a,g,f] |/ 8
      triad a = a <> up _M3 a <> up _P5 a
  in up _P8 scale </> (triad c)|/2 |> (triad g_)|/2


-- music-suite/test/legacy-music-files/meta_annotations.music
meta_annotations :: Music
meta_annotations =
  showAnnotations $ annotate "First note" c |> d |> annotate "Last note" d

meta_annotations2 :: Music
meta_annotations2 =
  showAnnotations $ annotate "First note" $ pseq [c,d,e]

meta_annotations3 :: Music
meta_annotations3 =
  showAnnotations $ annotateSpan (1 <-> 2) "First note" $ pseq [c,d,e]

meta_barlines :: Music
meta_barlines = pseq [c{-, barline-}, d{-, doubleBarline-}, e, f {-, finalBarline-}]

-- music-suite/test/legacy-music-files/meta_composer.music
meta_attribution :: Music
meta_attribution =
  composer "Anonymous" $ pseq [c,d,e,c]

meta_attribution2 :: Music
meta_attribution2 =
  lyricist "Anonymous" $ pseq [c,d,e,c]

meta_attribution3 :: Music
meta_attribution3 =
  arrangerDuring (0 <-> 1) "Anonymous I" $
  arrangerDuring (1 <-> 2) "Anonymous II" $
    pseq [c,d,e,c]

-- music-suite/test/legacy-music-files/meta_clef1.music
meta_clef1 :: Music
meta_clef1 =
  let
      part1 = clef f $ staccato $ pseq [c_,g_,c,g_]
      part2 = clef c $ staccato $ pseq [ab_,eb,d,a]
      part3 = clef g $ staccato $ accentLast $ pseq [g,fs,e,d]
  in compress 8 $ part1 |> part2 |> part3
  -- TODO need a better API here, integrated with Music.Pitch.Clef
  -- This should only be a hint, as clefs should be automatically inferred


-- meta_fermata :: Music
-- meta_fermata = pseq [c, d, fermata StandardFermata e]
-- TODO does not work (Fermata /~ FermataType)
-- TODO just saying "fermata" should yield a standard fermata

-- meta_fermata2 :: Music
-- meta_fermata2 = pseq [c, d, fermata LongFermata e]

-- meta_fermata3 :: Music
-- meta_fermata3 = fermataAt 2 $ pseq [c, d, e]
-- TODO does not work (Fermata /~ FermataType)
-- TODO remove fermataDuring, add fermataAt (fermatas attach to points, not spans)


meta_key_signature :: Music
meta_key_signature =
  keySignature (key 1 False) $ pseq [c,d,e,f,g]
  -- TODO should really be (keySignature g major) or similar
  -- Integrate with music-pitch

-- meta_rehearsal_mark :: Music
-- meta_rehearsal_mark =
  -- rehearsalMark $ pseq [c,d,e,f,g]
  -- TODO

-- music-suite/test/legacy-music-files/meta_time_signature.music
meta_time_signature :: Music
meta_time_signature =
  compress 4 $ timeSignature (4/4) (pseq [c,d,e,c,d,e,f,d,g,d]) |> timeSignature (3/4) (pseq [a,g,f,g,f,e])

-- music-suite/test/legacy-music-files/meta_time_signature.music
meta_time_signature2 :: Music
meta_time_signature2 =
  compress 16 $ timeSignature ((3+2)/16) $ pseq [c,d,e,f,g]

meta_tempo :: Music
meta_tempo = pseq
  [ tempo presto $ pseq [c,d,e,f,g]
  , tempo allegretto $ pseq [c,d,e,f,g]
  , tempo (metronome (1/4) 48) $ pseq [c,d,e,f,g]
  ]
  -- TODO custom tempo names

-- music-suite/test/legacy-music-files/meta_title.music
meta_title :: Music
meta_title =
  title "Piece" $ pseq [c,d,e,c]

meta_title2 :: Music
meta_title2 =
  subtitle "I" $ pseq [c,d,e,c]
  -- TODO alternative for indexing movements by number etc

-- music-suite/test/legacy-music-files/misc_counterpoint.music
misc_counterpoint :: Music
misc_counterpoint =
  let
      subj = pseq $ pseq [ [c],       [d],        [f],          [e]           ]
      cs1  = pseq $ pseq [ [g,f,e,g], [f,a,g,d'], [c',b,c',d'], [e',g',f',e'] ]
  in compress 4 cs1 </> subj


-- music-suite/test/legacy-music-files/octaves.music
octaves :: Music
octaves =
  c__ |> c_ |> c |> c' |> c''


-- music-suite/test/legacy-music-files/overlay_chords.music
overlay_chords :: Music
overlay_chords =
  ppar [c,e,g] |> ppar [d,f,a] |> ppar [e,g,b] |> ppar [c,e,g]


-- music-suite/test/legacy-music-files/overlay_voices.music
overlay_voices :: Music
overlay_voices =
  pseq [c,d,e,c] <> pseq [e,f,g,e] <> pseq [g,a,b,g]

voice1 :: Voice Pitch
voice1 = a -- mconcat [a,a,b,b,b,b,c,c]
  where
    a = [(1,c)^.note, (1,d)^.note, (2,e)^.note]^.voice
    -- b = [(1,d)^.note]^.voice
    -- c = [(2,c)^.note]^.voice

-- music-suite/test/legacy-music-files/pitch_inv.music
-- pitch_inv :: Music
-- pitch_inv =
--   (pseq [c..g]|*(2/5))
--       </>
--   (invertPitches c $ pseq [c..g]|*(2/5))
--       </>
--   (invertPitches e $ pseq [c..g]|*(2/5))


-- music-suite/test/legacy-music-files/sharpen.music
-- sharpen' :: Music
-- sharpen' =
--   sharpen c
--       </>
--   (sharpen . sharpen) c


-- music-suite/test/legacy-music-files/simple_figure.music
simple_figure :: Music
simple_figure =
  (c |> d |> e |> c |> d|*2 |> d|*2)|/16


-- music-suite/test/legacy-music-files/simple_start_later.music
simple_start_later :: Music
simple_start_later =
  up _P8 . compress 2 . delay 3 $ c


-- music-suite/test/legacy-music-files/single_note.music
single_note :: Music
single_note =
  c

-- music-suite/test/legacy-music-files/special_gliss.music
special_gliss :: Music
special_gliss =
  glissando $ pseq [c,d]|/2
-- TODO slide/gliss
-- This should be moved to pitch using Behavior or similar
-- How?

-- music-suite/test/legacy-music-files/special_harmonics.music
-- special_harmonics :: Music
-- special_harmonics =
--   (harmonic 1 $ c|/2)
--       </>
--   (harmonic 2 $ c|/2)
--       </>
--   (harmonic 3 $ c|/2)
-- TODO should be moved to techniques
-- Nicer way of distinguishing artificial/natural (for instruments where this
-- makes sense).

-- music-suite/test/legacy-music-files/special_text.music
special_text :: Music
special_text =
  text "pizz." $ c|/2
-- TODO text
-- Should be split up into expressive marks (lento, dolce etc) and what else
-- Lyrics should be separate
-- Arguably all technical instructions (pizz etc) are better represented as
-- part of the instrument/technique tuple.


-- music-suite/test/legacy-music-files/special_tremolo.music
special_tremolo :: Music
special_tremolo =
  tremolo 2 $ times 2 $ (c |> d)|/2
-- TODO should be moved to techniques
-- Would not mind retaining this top-level combinator
-- What about unmeasured tremolo?


-- music-suite/test/legacy-music-files/stretch_single_note1.music
stretch_single_note1 :: Music
stretch_single_note1 =
  stretch (1/2) c


-- music-suite/test/legacy-music-files/stretch_single_note2.music
stretch_single_note2 :: Music
stretch_single_note2 =
  stretch (1/2) c


-- music-suite/test/legacy-music-files/stretch_single_note3.music
stretch_single_note3 :: Music
stretch_single_note3 =
  stretch (4+1/2) c


-- music-suite/test/legacy-music-files/times.music
times' :: Music
times' =
  let
      melody = legato $ pseq [c,d,e,cs,ds,es]|/16
  in times 4 $ melody

-- TODO Aligned
-- TODO Behavior/Reactive
-- TODO HasDuration/HasPosition
-- TODO Transformable
-- TODO basic time types (Time, Duration, Span)
-- TODO Event?Note/Placed
-- TODO AddMeta?
-- TODO basic time combinators
-- TODO Rest (remove?)
-- TODO Reverse
-- TODO Split
-- TODO Score
-- TODO Track (remove?)
-- TODO Voice
-- TODO Quantization (basic)

-- music-suite/test/legacy-music-files/track_single.music
track_single :: Music
track_single =
  let
      x = [ (0, c)^.placed, (1, d)^.placed, (2, e)^.placed ]^.track
      y = join $ [ (0, x)^.placed,
                  (1.5,  up _P5 x)^.placed,
                  (3.25, up _P8 x)^.placed ]^.track

      trackToScore d = view score . map (view event . (\(t,x) -> (t >-> d,x)) . (view $ from placed)) . view placeds

  in trackToScore (1/8) y
-- TODO can we do without/rename track
-- TODO note vs event
--   Irritating that we can not call the things are score is made up of "notes"
--   Maybe use a parameterized type (data family?) such as (Note Voice :: * -> *)
-- (Note Score :: * -> *), i.e. Note has kind ((* -> *) -> * -> *) etc.

string_quartet :: Music
string_quartet = mainCanon2
  where
    mainCanon2 = (palindrome mainCanon <> celloEntry) |> tremCanon

    celloEntry = set parts' cellos e''|*(25*5/8)

    mainCanon = timeSignature (time 6 8) $ asScore $
        (set parts' violins1 $ harmonic 2 $ times 50 $ legato $ accentLast $
            octavesUp 2 $ pseq [a_,e,a,cs',cs',a,e,a_]|/8)
            <>
        (set parts' violins2 $ harmonic 2 $ times 50 $ legato $ accentLast $
            octavesUp 2 $ pseq [d,g,b,b,g,d]|/8)|*(3/2)
            <>
        (set parts' violas $ harmonic 2 $ times 50 $ legato $ accentLast $
            octavesUp 2 $ pseq [a,d,a,a,d,a]|/8)|*(3*2/2)
            <>
        set parts' cellos a'|*(25*5/8)

    tremCanon = compress 4 $
        (delay 124 $ set parts' violins1 $ subjs|*1)
            <>
        (delay 120 $ set parts' violins2 $ subjs|*1)
            <>
        (delay 4 $ set parts' violas $ subjs|*2)
            <>
        (delay 0 $ set parts' cellos  $ subjs|*2)
        where
          subjs = pseq $ map (\n -> palindrome $ rev $ subj n) [1..40::Int]
          subj n
              | n < 8     = a_|*2  |> e|*1   |> a|*1
              | n < 16    = a_|*2  |> e|*1   |> a|*1   |> e|*1   |> a|*1
              | n < 24    = a_|*2  |> e|*0.5 |> a|*0.5 |> e|*0.5 |> a|*0.5
              | otherwise = e|*0.5 |> a|*0.5

bartok_mikrokosmos :: Music
bartok_mikrokosmos = let
    meta = id
      . title "Mikrokosmos (excerpt)"
      . composer "Bela Bartok"
      . timeSignature (2/4)
      . timeSignatureDuring ((2/4) >-> (5/4)) (3/4)

    left = (level pp . legato)
         (pseq [a,g,f,e] |> d|*2)
      |> {-(level ((mp |> mp `cresc` mf |> mf)|*8) . legato)-}id
         (pseq [g,f,e,d] |> c |> (d |> e)|/2 |> f |> e |> d|*8)
    --
    right = up _P4 . delay 2 $
         (level pp . legato)
         (pseq [a,g,f,e] |> d|*2)
      |> (level mp . legato)
         (pseq [g,f,e,d] |> c |> (d |> e)|/2 |> f |> e |> d|*8)

  in meta $ compress 8 $ left <> set parts' cellos (down _P8 right)

chopin_etude :: Music
chopin_etude = music
  where
    rh :: Music
    rh = [((1/2) <-> (3/4),e)^.event,((3/4) <-> (15/16),cs')^.event,((15/16) <-> 1,d')^.event,(1 <-> (5/4),d)^.event,(1 <->
      (5/4),gs)^.event,(1 <-> (5/4),b)^.event,((5/4) <-> (3/2),d)^.event,((5/4) <-> (3/2),gs)^.event,((5/4) <->
      (3/2),b)^.event,((3/2) <-> 2,d)^.event,((3/2) <-> 2,gs)^.event,((3/2) <-> 2,b)^.event,(2 <-> (9/4),d')^.event,(2 <->
      (9/4),fs')^.event,((9/4) <-> (39/16),bs)^.event,((9/4) <-> (39/16),ds')^.event,((39/16) <-> (5/2),cs')^.event,((39/16) <->
      (5/2),e')^.event,((5/2) <-> (11/4),cs')^.event,((5/2) <-> (11/4),a')^.event,((11/4) <-> 3,cs')^.event,((11/4) <->
      3,a')^.event,(3 <-> (7/2),cs')^.event,(3 <-> (7/2),a')^.event,((7/2) <-> (15/4),e)^.event,((7/2) <->
      (15/4),cs')^.event,((15/4) <-> (63/16),cs)^.event,((15/4) <-> (63/16),as)^.event,((63/16) <-> 4,d)^.event,((63/16) <->
      4,b)^.event,(4 <-> (17/4),fs)^.event,(4 <-> (17/4),d')^.event,((17/4) <-> (9/2),fs)^.event,((17/4) <->
      (9/2),d')^.event,((9/2) <-> 5,fs)^.event,((9/2) <-> 5,d')^.event,(5 <-> (21/4),d)^.event,(5 <-> (21/4),gs)^.event,((21/4)
      <-> (87/16),d)^.event,((21/4) <-> (87/16),gs)^.event,((87/16) <-> (11/2),cs)^.event,((87/16) <-> (11/2),a)^.event,((11/2)
      <-> (23/4),cs)^.event,((11/2) <-> (23/4),cs')^.event,((23/4) <-> 6,cs)^.event,((23/4) <-> 6,cs')^.event,(6 <->
      (13/2),cs)^.event,(6 <-> (13/2),cs')^.event]^.score

    lh :: Music
    lh = [((3/4) <-> 1,e__)^.event,(1 <-> (5/4),e_)^.event,(1 <-> (5/4),e)^.event,((5/4) <-> (3/2),e_)^.event,((5/4) <->
      (3/2),e)^.event,((3/2) <-> 2,e_)^.event,((3/2) <-> 2,e)^.event,((9/4) <-> (5/2),a__)^.event,((5/2) <->
      (11/4),a_)^.event,((5/2) <-> (11/4),e)^.event,((11/4) <-> 3,a_)^.event,((11/4) <-> 3,e)^.event,(3 <-> (7/2),a_)^.event,(3
      <-> (7/2),e)^.event,((15/4) <-> 4,e__)^.event,(4 <-> (17/4),e_)^.event,(4 <-> (17/4),b_)^.event,((17/4) <->
      (9/2),e_)^.event,((17/4) <-> (9/2),b_)^.event,((9/2) <-> 5,e_)^.event,((9/2) <-> 5,b_)^.event,((21/4) <->
      (11/2),a___)^.event,((11/2) <-> (23/4),e_)^.event,((11/2) <-> (23/4),a_)^.event,((11/2) <-> (23/4),e)^.event,((23/4) <->
      6,e_)^.event,((23/4) <-> 6,a_)^.event,((23/4) <-> 6,e)^.event,(6 <-> (13/2),e_)^.event,(6 <-> (13/2),a_)^.event,(6 <->
      (13/2),e)^.event]^.score

    music = timeSignature (3/4) $ lh <> rh

-- aspects_basic


type PitchOf a          = Music.Score.Pitch a
type P a                = Music.Score.Pitch a
type PartOf a           = Music.Score.Part a
type R a                = Music.Score.Part a
type DynamicOf a        = Music.Score.Dynamic a
type D a                = Music.Score.Dynamic a
type ArticulationOf a   = Music.Score.Articulation a
type A a                = Music.Score.Articulation a


toAspects ::
  ( IsPitch b
  , HasPitches a a
  , HasPart a a
  , HasPart b b
  , HasArticulation a a
  , HasArticulation b b
  , HasDynamic a a
  , HasDynamic b b
  , PartOf b         ~ PartOf a
  , ArticulationOf b ~ ArticulationOf a
  , DynamicOf b      ~ DynamicOf a
  , PitchOf a        ~ Pitch
  , Functor f
  ) =>
    f a -> f b
toAspects = fmap toAspects1

toAspects1 ::
  ( IsPitch b
  , HasPitches a a
  , HasPart a a
  , HasPart b b
  , HasArticulation a a
  , HasArticulation b b
  , HasDynamic a a
  , HasDynamic b b
  , PartOf b         ~ PartOf a
  , ArticulationOf b ~ ArticulationOf a
  , DynamicOf b      ~ DynamicOf a
  , PitchOf a        ~ Pitch
  ) =>
     a -> b
toAspects1 x = set part' r $ set articulation' a $ set dynamic' d $ fromPitch p
  where
    p = x^?!pitches
    d = x^.dynamic
    a = x^.articulation
    r = x^.part



toStandardNotation ::
  ( HasPitches a a
  , HasPart a a
  , HasArticulation a a
  , HasDynamic a a
  , PartOf a         ~ Part
  , ArticulationOf a ~ Articulation
  , DynamicOf a      ~ Dynamics
  , PitchOf a        ~ Pitch
  -- TODO suitable restriction on f (need @f a -> Score a@), i.e. @Score a@, @Voice a@, @Identity a@.
  , HasScore f
  ) =>
    f a -> PureExportM Work
toStandardNotation = fromAspects  . toAspects . toScore

class (Functor f) => HasScore f where
  toScore :: f a -> Score a
instance HasScore Score where
  toScore = id
instance HasScore Voice where
  toScore = voiceToScore
instance HasScore Note where
  toScore = voiceToScore . noteToVoice
instance HasScore Event where
  toScore = eventToScore
instance HasScore Identity where
  toScore (Identity x) = return x

voiceToScore :: Voice a -> Score a
voiceToScore = renderAlignedVoice . aligned 0 0 :: Voice a -> Score a
noteToVoice :: Note a -> Voice a
noteToVoice = view voice . pure
eventToScore :: Event a -> Score a
eventToScore = view score . pure



type PitchDynamicArticulationPart = [r|
  { pitch        :: Pitch
  , dynamic      :: Dynamics
  , articulation :: Articulation
  , part         :: Part
  }|]
-- type Mus = Record4
--   "articulation" Articulation
--   "dynamic"      Dynamics
--   "part"         Part
--   "pitch"        Pitch

{-
The following instances should look in alphabetical order, i.e.

(articulation, dynamic, part, pitch)

(dynamic, part, pitch)
(articulation, part, pitch)
(articulation, dynamic, pitch)
(articulation, dynamic, part)

(part, pitch)
(dynamic, pitch)
(dynamic, part)
(articulation, pitch)
(articulation, part)
(articulation, dynamic)

-- TODO all in all this forces us to define
  (12+12+8)*2=64 HasT instances AND
  (12+12+8)*2=64 T/SetT instances

Below example defines the 4-tuple case and part of the 3-tuple case (ca 20 out of 128 instances)
-}

-- TOOD allow both dynamic and articulation in 1st pos
type instance
  Music.Score.Articulation        (Record3 "articulation" v1 n2 v2 n3 v3) = v1
type instance
  Music.Score.SetArticulation  v1' (Record3 "articulation" v1 n2 v2 n3 v3) = (Record3 "articulation" v1' n2 v2 n3 v3)
instance (Transformable v1, Transformable v1', n1 ~ "articulation") => HasArticulations
  (Record3 n1 v1 n2 v2 n3 v3)
  (Record3 n1 v1' n2 v2 n3 v3) where
  articulations = [l|articulation|]

type instance
  Music.Score.Dynamic        (Record3 "dynamic" v1 n2 v2 n3 v3) = v1
type instance
  Music.Score.SetDynamic  v1' (Record3 "dynamic" v1 n2 v2 n3 v3) = (Record3 "dynamic" v1' n2 v2 n3 v3)
instance (Transformable v1, Transformable v1', n1 ~ "dynamic") => HasDynamics
  (Record3 n1 v1 n2 v2 n3 v3)
  (Record3 n1 v1' n2 v2 n3 v3) where
  dynamics = [l|dynamic|]



-- TODO qualify to assure fields in correct order
type instance
  Music.Score.Articulation        (Record4 "articulation" v1 n2 v2 n3 v3 n4 v4) = v1
type instance
  Music.Score.Dynamic      (Record4 n1 v1 n2 v2 n3 v3 n4 v4) = v2
type instance
  Music.Score.Part (Record4 n1 v1 n2 v2 n3 v3 n4 v4) = v3
type instance
  Music.Score.Pitch         (Record4 n1 v1 n2 v2 n3 v3 n4 v4) = v4
type instance
  Music.Score.SetArticulation  v1' (Record4 n1 v1 n2 v2 n3 v3 n4 v4) = (Record4 n1 v1' n2 v2 n3 v3 n4 v4)
type instance
  Music.Score.SetDynamic  v2' (Record4 n1 v1 n2 v2 n3 v3 n4 v4) = (Record4 n1 v1 n2 v2' n3 v3 n4 v4)
type instance
  Music.Score.SetPart  v3' (Record4 n1 v1 n2 v2 n3 v3 n4 v4) = (Record4 n1 v1 n2 v2 n3 v3' n4 v4)
type instance
  Music.Score.SetPitch  v4' (Record4 n1 v1 n2 v2 n3 v3 n4 v4) = (Record4 n1 v1 n2 v2 n3 v3 n4 v4')

instance (Transformable v1, Transformable v1', n1 ~ "articulation") => HasArticulations
  (Record4 n1 v1 n2 v2 n3 v3 n4 v4)
  (Record4 n1 v1' n2 v2 n3 v3 n4 v4) where
    articulations = [l|articulation|]
instance (Transformable v2, Transformable v2', n2 ~ "dynamic") => HasDynamics
  (Record4 n1 v1 n2 v2 n3 v3 n4 v4)
  (Record4 n1 v1 n2 v2' n3 v3 n4 v4) where
  dynamics = [l|dynamic|]
instance (Transformable v3, Transformable v3', n3 ~ "part") => HasParts
  (Record4 n1 v1 n2 v2 n3 v3 n4 v4)
  (Record4 n1 v1 n2 v2 n3 v3' n4 v4) where
  parts = [l|part|]
instance (Transformable v4, Transformable v4', n4 ~ "pitch") => HasPitches
  (Record4 n1 v1 n2 v2 n3 v3 n4 v4)
  (Record4 n1 v1 n2 v2 n3 v3 n4 v4') where
    pitches = [l|pitch|]

instance (Transformable v1, Transformable v1', n1 ~ "articulation") => HasArticulation
  (Record4 n1 v1 n2 v2 n3 v3 n4 v4)
  (Record4 n1 v1' n2 v2 n3 v3 n4 v4) where
  articulation = [l|articulation|]
instance (Transformable v2, Transformable v2', n2 ~ "dynamic") => HasDynamic
  (Record4 n1 v1 n2 v2 n3 v3 n4 v4)
  (Record4 n1 v1 n2 v2' n3 v3 n4 v4) where
  dynamic = [l|dynamic|]
instance (Transformable v3, Transformable v3', n3 ~ "part") => HasPart
  (Record4 n1 v1 n2 v2 n3 v3 n4 v4)
  (Record4 n1 v1 n2 v2 n3 v3' n4 v4) where
  part = [l|part|]
instance (Transformable v4, Transformable v4', n4 ~ "pitch") => HasPitch
  (Record4 n1 v1 n2 v2 n3 v3 n4 v4)
  (Record4 n1 v1 n2 v2 n3 v3 n4 v4') where
  pitch = [l|pitch|]

instance (IsPitch v4, Monoid v2, Monoid v3, Monoid v1) => IsPitch (Record4  n1 v1 n2 v2 n3 v3 n4 v4)
  where
    fromPitch p = Record4 mempty mempty mempty (fromPitch p)

testAll = do
  pure $ runPureExportMNoLog $ toStandardNotation (c :: Voice StandardNote)
  pure $ runPureExportMNoLog $ toStandardNotation (c :: Note StandardNote)
  pure $ runPureExportMNoLog $ toStandardNotation (c :: Score StandardNote)
  pure $ runPureExportMNoLog $ toStandardNotation (c :: Score PitchDynamicArticulationPart)

  print $ runPureExportMNoLog $ toStandardNotation $
    [ (0 <-> 2, [r|{ pitch = c, dynamic = pp, articulation = mempty, part = violins}|] )^.event
    , (0 <-> 2, [r|{ pitch = e, dynamic = pp, articulation = mempty, part = violins}|] )^.event
    , (0 <-> 2, [r|{ pitch = g, dynamic = pp, articulation = mempty, part = violins}|] )^.event
    , (0 <-> 2, [r|{ pitch = d, dynamic = pp, articulation = mempty, part = violins}|] )^.event
    , (0 <-> 2, [r|{ pitch = f, dynamic = pp, articulation = mempty, part = violins}|] )^.event
    , (2 <-> 4, [r|{ pitch = a, dynamic = pp, articulation = mempty, part = violins}|] )^.event

    , (1 <-> 2, [r|{ pitch = b, dynamic = pp, articulation = mempty, part = violas}|] )^.event
    ]^.score

  -- TODO something like this still not possible
  -- TODO test this with records too!
  -- Probably requires FlexibleInstances (as in the Inspectable module)

main = do
  testAll
-- main = return ()
