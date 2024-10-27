{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TupleSections     #-}

module Euterpea.Music where

import           Data.Bifunctor  (first)
import           Data.List       (uncons)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           GHC.Generics    (Generic)

infixr 5 :+:, :=:

type AbsPitch = Int
type Octave = Int
type Pitch = (PitchClass, Octave)
type Dur   = Rational

data PitchClass  =  Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds
                 |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
                 |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
                 |  Bf | Ass | B | Bs | Bss
     deriving (Show, Eq, Ord, Read, Enum, Bounded)

data Primitive a where
  Note :: Dur -> a -> Primitive a
  Rest :: Dur -> Primitive a
  deriving (Show, Eq, Ord, Functor)

data Music a where
  Prim :: (Primitive a) -> Music a
  (:+:) :: Music a -> Music a -> Music a
  (:=:) :: Music a -> Music a -> Music a
  Modify :: Control -> (Music a) -> Music a
  deriving (Show, Eq, Ord, Functor)

data Control where
  Tempo :: Rational -> Control
  Transpose :: AbsPitch -> Control
  Instrument :: InstrumentName -> Control
  Phrase :: [PhraseAttribute] -> Control
  KeySig :: PitchClass -> Mode -> Control
  Custom :: String -> Control
  deriving (Show, Eq, Ord)

data Mode = Major | Minor |
            Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian |
            CustomMode String
  deriving (Show, Eq, Ord)


data PhraseAttribute  =  Dyn Dynamic
                      |  Tmp Tempo
                      |  Art Articulation
                      |  Orn Ornament
     deriving (Show, Eq, Ord)

data Dynamic where
  Accent :: Rational -> Dynamic
  Crescendo :: Rational -> Dynamic
  Diminuendo :: Rational -> Dynamic
  StdLoudness :: StdLoudness -> Dynamic
  Loudness :: Rational -> Dynamic
  deriving (Show, Eq, Ord)

data StdLoudness = PPP | PP | P | MP | SF | MF | NF | FF | FFF
     deriving (Show, Eq, Ord, Enum, Bounded, Read)

data Tempo = Ritardando Rational | Accelerando Rational
     deriving (Show, Eq, Ord)

data Articulation  =  Staccato Rational | Legato Rational | Slurred Rational
                   |  Tenuto | Marcato | Pedal | Fermata | FermataDown | Breath
                   |  DownBow | UpBow | Harmonic | Pizzicato | LeftPizz
                   |  BartokPizz | Swell | Wedge | Thumb | Stopped
     deriving (Show, Eq, Ord)

data Ornament  =  Trill | Mordent | InvMordent | DoubleMordent
               |  Turn | TrilledTurn | ShortTrill
               |  Arpeggio | ArpeggioUp | ArpeggioDown
               |  Instruction String | Head NoteHead
               |  DiatonicTrans Int
     deriving (Show, Eq, Ord)

data NoteHead  =  DiamondHead | SquareHead | XHead | TriangleHead
               |  TremoloHead | SlashHead | ArtHarmonic | NoHead
     deriving (Show, Eq, Ord)

type Volume = Int

-- addVolume    :: Volume -> Music Pitch -> Music (Pitch,Volume)
-- addVolume v  = mMap (\p -> (p,v))

addVolume :: Volume -> Music Pitch -> Music (Pitch, Volume)
addVolume v = fmap (, v)



data NoteAttribute
    = Volume {-# UNPACK #-} !Int -- Utilize Int for efficiency
    | Fingering {-# UNPACK #-} !Int
    | Dynamics !String
    | Params ![Double] -- Using Vector instead of list for potential performance gains
    deriving (Eq, Show, Generic)

type Note1   = (Pitch, [NoteAttribute])
type Music1  = Music Note1

class ToMusic1 a where
    toMusic1 :: Music a -> Music1

instance ToMusic1 Pitch where
    toMusic1 :: Music Pitch -> Music1
    toMusic1 = fmap (, [])


instance ToMusic1 (Pitch, Volume) where
    toMusic1 :: Music (Pitch, Volume) -> Music1
    toMusic1  = fmap (\(p, v) -> (p, [Volume v]))

instance ToMusic1 Note1 where
    toMusic1 :: Music Note1 -> Music1
    toMusic1 = id

instance ToMusic1 AbsPitch where
    toMusic1 :: Music AbsPitch -> Music1
    toMusic1 = fmap (\x -> (pitch x, []))

instance ToMusic1 (AbsPitch, Volume) where
    toMusic1 :: Music (AbsPitch, Volume) -> Music1
    toMusic1 = fmap (\(p,v) -> (pitch p, [Volume v]))

note            :: Dur -> a -> Music a
note dur_ p        = Prim (Note dur_ p)

rest            :: Dur -> Music a
rest dur_          = Prim (Rest dur_)

tempo           :: Dur -> Music a -> Music a
tempo r = Modify (Tempo r)

transpose       :: AbsPitch -> Music a -> Music a
transpose i = Modify (Transpose i)

instrument      :: InstrumentName -> Music a -> Music a
instrument i = Modify (Instrument i)

phrase          :: [PhraseAttribute] -> Music a -> Music a
phrase pa = Modify (Phrase pa)

keysig          :: PitchClass -> Mode -> Music a -> Music a
keysig pc mo = Modify (KeySig pc mo)

cff,cf,c,cs,css,dff,df,d,ds,dss,eff,ef,e,es,ess,fff,ff,f,
  fs,fss,gff,gf,g,gs,gss,aff,af,a,as,ass,bff,bf,b,bs,bss ::
    Octave -> Dur -> Music Pitch

cff  o dur_ = note dur_ (Cff,  o);  cf   o dur_ = note dur_ (Cf,   o)
c    o dur_ = note dur_ (C,    o);  cs   o dur_ = note dur_ (Cs,   o)
css  o dur_ = note dur_ (Css,  o);  dff  o dur_ = note dur_ (Dff,  o)
df   o dur_ = note dur_ (Df,   o);  d    o dur_ = note dur_ (D,    o)
ds   o dur_ = note dur_ (Ds,   o);  dss  o dur_ = note dur_ (Dss,  o)
eff  o dur_ = note dur_ (Eff,  o);  ef   o dur_ = note dur_ (Ef,   o)
e    o dur_ = note dur_ (E,    o);  es   o dur_ = note dur_ (Es,   o)
ess  o dur_ = note dur_ (Ess,  o);  fff  o dur_ = note dur_ (Fff,  o)
ff   o dur_ = note dur_ (Ff,   o);  f    o dur_ = note dur_ (F,    o)
fs   o dur_ = note dur_ (Fs,   o);  fss  o dur_ = note dur_ (Fss,  o)
gff  o dur_ = note dur_ (Gff,  o);  gf   o dur_ = note dur_ (Gf,   o)
g    o dur_ = note dur_ (G,    o);  gs   o dur_ = note dur_ (Gs,   o)
gss  o dur_ = note dur_ (Gss,  o);  aff  o dur_ = note dur_ (Aff,  o)
af   o dur_ = note dur_ (Af,   o);  a    o dur_ = note dur_ (A,    o)
as   o dur_ = note dur_ (As,   o);  ass  o dur_ = note dur_ (Ass,  o)
bff  o dur_ = note dur_ (Bff,  o);  bf   o dur_ = note dur_ (Bf,   o)
b    o dur_ = note dur_ (B,    o);  bs   o dur_ = note dur_ (Bs,   o)
bss  o dur_ = note dur_ (Bss,  o)

bn, wn, hn, qn, en, sn, tn, sfn, dwn, dhn,
    dqn, den, dsn, dtn, ddhn, ddqn, dden :: Dur

bnr, wnr, hnr, qnr, enr, snr, tnr, sfnr, dwnr, dhnr,
     dqnr, denr, dsnr, dtnr, ddhnr, ddqnr, ddenr :: Music Pitch

bn    = 2;     bnr    = rest bn    --  brevis rest
wn    = 1;     wnr    = rest wn    --  whole note rest
hn    = 1/2;   hnr    = rest hn    --  half note rest
qn    = 1/4;   qnr    = rest qn    --  quarter note rest
en    = 1/8;   enr    = rest en    --  eighth note rest
sn    = 1/16;  snr    = rest sn    --  sixteenth note rest
tn    = 1/32;  tnr    = rest tn    --  thirty-second note rest
sfn   = 1/64;  sfnr   = rest sfn   --  sixty-fourth note rest

dwn   = 3/2;   dwnr   = rest dwn   --  dotted whole note rest
dhn   = 3/4;   dhnr   = rest dhn   --  dotted half note rest
dqn   = 3/8;   dqnr   = rest dqn   --  dotted quarter note rest
den   = 3/16;  denr   = rest den   --  dotted eighth note rest
dsn   = 3/32;  dsnr   = rest dsn   --  dotted sixteenth note rest
dtn   = 3/64;  dtnr   = rest dtn   --  dotted thirty-second note rest

ddhn  = 7/8;   ddhnr  = rest ddhn  --  double-dotted half note rest
ddqn  = 7/16;  ddqnr  = rest ddqn  --  double-dotted quarter note rest
dden  = 7/32;  ddenr  = rest dden  --  double-dotted eighth note rest

absPitch           :: Pitch -> AbsPitch
absPitch (pc,oct)  = 12*(oct+1) + pcToInt pc

pcToInt     :: PitchClass -> Int
pcToInt pc  = case pc of
  Cff  -> -2;  Cf  -> -1;  C  -> 0;   Cs  -> 1;   Css  -> 2;
  Dff  -> 0;   Df  -> 1;   D  -> 2;   Ds  -> 3;   Dss  -> 4;
  Eff  -> 2;   Ef  -> 3;   E  -> 4;   Es  -> 5;   Ess  -> 6;
  Fff  -> 3;   Ff  -> 4;   F  -> 5;   Fs  -> 6;   Fss  -> 7;
  Gff  -> 5;   Gf  -> 6;   G  -> 7;   Gs  -> 8;   Gss  -> 9;
  Aff  -> 7;   Af  -> 8;   A  -> 9;   As  -> 10;  Ass  -> 11;
  Bff  -> 9;   Bf  -> 10;  B  -> 11;  Bs  -> 12;  Bss  -> 13

pitch     :: AbsPitch -> Pitch
pitch ap  =
    let (oct, n) = divMod ap 12
    in  ([C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] !! n, oct-1)

trans      :: Int -> Pitch -> Pitch
trans i p  = pitch (absPitch p + i)

line, chord :: [Music a] -> Music a
line   = foldr (:+:) (rest 0)
chord  = foldr (:=:) (rest 0)

line1, chord1 :: [Music a] -> Music a
line1  = foldr1 (:+:)
chord1 = foldr1 (:=:)

offset      :: Dur -> Music a -> Music a
offset dur_ m  = rest dur_ :+: m

times      :: Int -> Music a -> Music a
times 0 _ = rest 0
times n m = m :+: times (n-1) m

forever    :: Music a -> Music a
forever m  = m :+: forever m

lineToList                    :: Music a -> [Music a]
lineToList (Prim (Rest 0))    = []
lineToList (n :+: ns)         = n : lineToList ns
lineToList _                  =
    error "lineToList: argument not created by function line"

invertAt :: Pitch -> Music Pitch -> Music Pitch
invertAt pRef = fmap (\p -> pitch (2 * absPitch pRef - absPitch p))

invertAt1 :: Pitch -> Music (Pitch, a) -> Music (Pitch, a)
invertAt1 pRef = fmap (\(p,x) -> (pitch (2 * absPitch pRef - absPitch p),x))

-- invert :: Music Pitch -> Music Pitch
-- invert m =
--     let pRef = mFold pFun (++) (++) (flip const) m
--     in  if null pRef then m -- no pitches in the structure!
--         else invertAt (head pRef) m
--     where pFun (Note d p) = [p]
--           pFun _          = []
invert :: Music Pitch -> Music Pitch
invert m =
    let pRef = mFold pFun (++) (++) (\_ x -> x) m
    in case uncons pRef of
         Nothing     -> m -- no pitches in the structure!
         Just (p, _) -> invertAt p m
    where pFun (Note _ p) = [p]
          pFun _          = []

invert1 :: Music (Pitch, a) -> Music (Pitch, a)
invert1 m =
    let pRef = mFold pFun (++) (++) (\_ x -> x) m
    in case uncons pRef of
         Nothing     -> m -- no pitches!
         Just (p, _) -> invertAt1 p m
    where pFun (Note _ (p,_)) = [p]
          pFun _              = []

-- invert1 :: Music (Pitch, a) -> Music (Pitch, a)
-- invert1 m =
--     let pRef = mFold pFun (++) (++) (\_ x -> x) m
--     in if null pRef then m -- no pitches!
--        else invertAt1 (head pRef) m
--     where pFun (Note _ (p,_)) = [p]
--           pFun _              = []

-- invert1 :: Music (Pitch,a) -> Music (Pitch,a)
-- invert1 m =
--     let pRef = mFold pFun (++) (++) (flip const) m
--     in  if null pRef then m -- no pitches!
--         else invertAt1 (head pRef) m
--     where pFun (Note d (p,x)) = [p]
--           pFun _              = []

retro               :: Music a -> Music a
retro n@(Prim _)    = n
retro (Modify c m)  = Modify c (retro m)
retro (m1 :+: m2)   = retro m2 :+: retro m1
retro (m1 :=: m2)   =
   let  d1 = dur m1
        d2 = dur m2
   in if d1>d2  then retro m1 :=: (rest (d1-d2) :+: retro m2)
                else (rest (d2-d1) :+: retro m1) :=: retro m2

retroInvert, invertRetro :: Music Pitch -> Music Pitch
retroInvert  = retro  . invert
invertRetro  = invert . retro

dur                       :: Music a -> Dur
dur (Prim (Note dur_ _)) = dur_
dur (Prim (Rest dur_))   = dur_
dur (m1 :+: m2)          = dur m1   +   dur m2
dur (m1 :=: m2)          = dur m1 `max` dur m2
dur (Modify (Tempo r) m) = dur m / r
dur (Modify _ m)         = dur m



-- | The 'cut' function trims a 'Music' structure to a specified duration 'd'.
--   It effectively limits the music piece's playtime to the provided duration.
--
--   This function processes different musical constructs:
--   - Simple notes and rests.
--   - Parallel and sequential compositions.
--   - Modifications such as tempo changes.
--
--   Special Cases:
--   - If 'd' <= 0, the result is a rest with zero duration.
--   - For parallel (:=:) structures, each component is independently cut to duration 'd'.
--   - In sequential (:|:) structures, the first part is cut to 'd', and then, if needed,
--     the remaining duration is applied to subsequent parts.
--   - Tempo modifications adjust 'd' proportionally when cutting.
--
--   Returns:
--   A new 'Music' object that plays for only up to the requested duration.
--
-- >>> cut 2 (note 1 (C,4) :+: note 2 (D,4))
-- NOW Prim (Note (1 % 1) (C,4)) :+: Prim (Note (1 % 1) (D,4))
--
-- >>> cut 1 (note 2 (C,4) :+: note 1 (D,4))
-- Prim (Note (1 % 1) (C,4)) :+: Prim (Rest (0 % 1))
cut :: Dur -> Music a -> Music a
cut dur_ m | dur_ <= 0         = rest 0
cut dur_ (Prim (Note oldD p))  =  let d' = max (min oldD dur_) 0
                               in if d'>0 then note d' p else rest 0
cut dur_ (Prim (Rest oldD))    = rest (max (min oldD dur_) 0)
cut dur_ (m1 :=: m2)           = cut dur_ m1 :=: cut dur_ m2
cut dur_ (m1 :+: m2)           =  let  m'1  = cut dur_ m1
                                       m'2  = cut (dur_ - dur m'1) m2
                               in   m'1 :+: m'2
cut dur_ (Modify (Tempo r) m)  = tempo r (cut (dur_*r) m)
cut dur_ (Modify c m)          = Modify c (cut dur_ m)


-- | The 'remove' function traverses a 'Music' structure and subtracts a given duration 'd'
--   from it. It effectively removes the specified amount of duration from notes and rests
--   in the music piece. If 'd' is greater than or equal to the duration of a note or rest,
--   that element is shortened or potentially removed.
--
--   This function handles different music constructs:
--   - Simple notes and rests.
--   - Parallel and sequential compositions.
--   - Modifications like tempo changes.
--
--   Special Cases:
--   - If 'd' <= 0, the entire music piece remains unchanged.
--   - When removing from parallel (:=:) structures, 'd' is applied to both components.
--   - In sequential (:|:) structures, 'd' is reduced by the duration of each segment before
--     being applied to subsequent segments.
--   - Tempo modification is factored into the removal process by adjusting 'd' proportionally.
--
--   Returns:
--   A new 'Music' object with the requested duration removed.
-- >>> remove 0.5 (note 2 (C,4) :+: note 1 (D,4))
-- NOW Prim (Note (3 % 2) (C,4)) :+: Prim (Note (1 % 1) (D,4))
-- >>> remove 1 (note 2 (C,4) :+: note 1 (D,4))
-- Prim (Note (1 % 1) (C,4)) :+: Prim (Note (1 % 1) (D,4))
remove :: Dur -> Music a -> Music a
remove dur_ m | dur_ <= 0            = m
remove dur_ (Prim (Note oldD p))  =  let d' = max (oldD-dur_) 0
                                  in  if d'>0 then note d' p else rest 0
remove dur_ (Prim (Rest oldD))    = rest (max (oldD-dur_) 0)
remove dur_ (m1 :=: m2)           = remove dur_ m1 :=: remove dur_ m2
remove dur_ (m1 :+: m2)           =  let  m'1  = remove dur_ m1
                                          m'2  = remove (dur_ - dur m1) m2
                                  in   m'1 :+: m'2
remove dur_ (Modify (Tempo r) m)  = tempo r (remove (dur_*r) m)
remove dur_ (Modify c m)          = Modify c (remove dur_ m)

removeZeros :: Music a -> Music a
removeZeros (Prim p)      = Prim p
removeZeros (m1 :+: m2)   =
  let  m'1  = removeZeros m1
       m'2  = removeZeros m2
  in case (m'1,m'2) of
       (Prim (Note 0 _), m) -> m
       (Prim (Rest 0  ), m) -> m
       (m, Prim (Note 0 _)) -> m
       (m, Prim (Rest 0  )) -> m
       (m1, m2)             -> m1 :+: m2
removeZeros (m1 :=: m2)   =
  let  m'1  = removeZeros m1
       m'2  = removeZeros m2
  in case (m'1,m'2) of
       (Prim (Note 0 _), m) -> m
       (Prim (Rest 0  ), m) -> m
       (m, Prim (Note 0 _)) -> m
       (m, Prim (Rest 0  )) -> m
       (m1, m2)             -> m1 :=: m2
removeZeros (Modify c m)  = Modify c (removeZeros m)

type LazyDur = [Dur]
durL :: Music a -> LazyDur
durL m@(Prim _)            =  [dur m]
durL (m1 :+: m2)           =  let d1 = durL m1
                              in d1 ++ map (+last d1) (durL m2)
durL (m1 :=: m2)           =  mergeLD (durL m1) (durL m2)
durL (Modify (Tempo r) m)  =  map (/r) (durL m)
durL (Modify _ m)          =  durL m

mergeLD :: LazyDur -> LazyDur -> LazyDur
mergeLD [] ld = ld
mergeLD ld [] = ld
mergeLD ld1@(d1:ds1) ld2@(d2:ds2) =
  if d1<d2  then  d1 : mergeLD ds1 ld2
            else  d2 : mergeLD ld1 ds2

minL :: LazyDur -> Dur -> Dur
minL []      d' = d'
minL [d]     d' = min d d'
minL (d:ds)  d' = if d < d' then minL ds d' else d'

cutL :: LazyDur -> Music a -> Music a
cutL [] m                     = rest 0
cutL (d:ds) m | d <= 0        = cutL ds m
cutL ld (Prim (Note oldD p))  = note (minL ld oldD) p
cutL ld (Prim (Rest oldD))    = rest (minL ld oldD)
cutL ld (m1 :=: m2)           = cutL ld m1 :=: cutL ld m2
cutL ld (m1 :+: m2)           =
   let  m'1 = cutL ld m1
        m'2 = cutL (map (\d -> d - dur m'1) ld) m2
   in m'1 :+: m'2
cutL ld (Modify (Tempo r) m)  = tempo r (cutL (map (*r) ld) m)
cutL ld (Modify c m)          = Modify c (cutL ld m)

(/=:)      :: Music a -> Music a -> Music a
m1 /=: m2  = cutL (durL m2) m1 :=: cutL (durL m1) m2

data PercussionSound =
        AcousticBassDrum  --  MIDI Key 35
     |  BassDrum1         --  MIDI Key 36
     |  SideStick         --  ...
     |  AcousticSnare  | HandClap      | ElectricSnare  | LowFloorTom
     |  ClosedHiHat    | HighFloorTom  | PedalHiHat     | LowTom
     |  OpenHiHat      | LowMidTom     | HiMidTom       | CrashCymbal1
     |  HighTom        | RideCymbal1   | ChineseCymbal  | RideBell
     |  Tambourine     | SplashCymbal  | Cowbell        | CrashCymbal2
     |  Vibraslap      | RideCymbal2   | HiBongo        | LowBongo
     |  MuteHiConga    | OpenHiConga   | LowConga       | HighTimbale
     |  LowTimbale     | HighAgogo     | LowAgogo       | Cabasa
     |  Maracas        | ShortWhistle  | LongWhistle    | ShortGuiro
     |  LongGuiro      | Claves        | HiWoodBlock    | LowWoodBlock
     |  MuteCuica      | OpenCuica     | MuteTriangle
     |  OpenTriangle      --  MIDI Key 82
   deriving (Show,Eq,Ord,Enum)

perc :: PercussionSound -> Dur -> Music Pitch
perc ps dur = instrument Percussion $ note dur (pitch (fromEnum ps + 35))

pMap               :: (a -> b) -> Primitive a -> Primitive b
pMap f (Note dur_ x) = Note dur_ (f x)
pMap _ (Rest dur_)   = Rest dur_

mMap                 :: (a -> b) -> Music a -> Music b
mMap f (Prim p)     = Prim (pMap f p)
mMap f (m1 :+: m2)  = fmap f m1 :+: fmap f m2
mMap f (m1 :=: m2)  = fmap f m1 :=: fmap f m2
mMap f (Modify c m) = Modify c (fmap f m)

-- instance Functor Primitive where
--     fmap = pMap

-- instance Functor Music where
--     fmap = mMap

mFold ::  (Primitive a -> b) -> (b->b->b) -> (b->b->b) ->
          (Control -> b -> b) -> Music a -> b
mFold f (+:) (=:) g m =
  let rec = mFold f (+:) (=:) g
  in case m of
       Prim p     -> f p
       m1 :+: m2  -> rec m1 +: rec m2
       m1 :=: m2  -> rec m1 =: rec m2
       Modify c m -> g c (rec m)

shiftPitches :: AbsPitch -> Music Pitch -> Music Pitch
shiftPitches k = fmap (trans k)

-- shiftPitches1 :: AbsPitch -> Music (Pitch, b) -> Music (Pitch, b)
-- shiftPitches1 k = mMap (\(p,xs) -> (trans k p, xs))


shiftPitches1 :: AbsPitch -> Music (Pitch, b) -> Music (Pitch, b)
shiftPitches1 k = fmap (first (trans k))

scaleDurations :: Rational -> Music a -> Music a
scaleDurations r (Prim (Note dur_ p)) = note (dur_/r) p
scaleDurations r (Prim (Rest dur_))   = rest (dur_/r)
scaleDurations r (m1 :+: m2)       = scaleDurations r m1 :+: scaleDurations r m2
scaleDurations r (m1 :=: m2)       = scaleDurations r m1 :=: scaleDurations r m2
scaleDurations r (Modify c m)      = Modify c (scaleDurations r m)

changeInstrument :: InstrumentName -> Music a -> Music a
changeInstrument i m = Modify (Instrument i) $ removeInstruments m

removeInstruments :: Music a -> Music a
removeInstruments (Modify (Instrument i) m) = removeInstruments m
removeInstruments (Modify c m) = Modify c $ removeInstruments m
removeInstruments (m1 :+: m2) = removeInstruments m1 :+: removeInstruments m2
removeInstruments (m1 :=: m2) = removeInstruments m1 :=: removeInstruments m2
removeInstruments m = m



data InstrumentName =
     AcousticGrandPiano     | BrightAcousticPiano    | ElectricGrandPiano
  |  HonkyTonkPiano         | RhodesPiano            | ChorusedPiano
  |  Harpsichord            | Clavinet               | Celesta
  |  Glockenspiel           | MusicBox               | Vibraphone
  |  Marimba                | Xylophone              | TubularBells
  |  Dulcimer               | HammondOrgan           | PercussiveOrgan
  |  RockOrgan              | ChurchOrgan            | ReedOrgan
  |  Accordion              | Harmonica              | TangoAccordion
  |  AcousticGuitarNylon    | AcousticGuitarSteel    | ElectricGuitarJazz
  |  ElectricGuitarClean    | ElectricGuitarMuted    | OverdrivenGuitar
  |  DistortionGuitar       | GuitarHarmonics        | AcousticBass
  |  ElectricBassFingered   | ElectricBassPicked     | FretlessBass
  |  SlapBass1              | SlapBass2              | SynthBass1
  |  SynthBass2             | Violin                 | Viola
  |  Cello                  | Contrabass             | TremoloStrings
  |  PizzicatoStrings       | OrchestralHarp         | Timpani
  |  StringEnsemble1        | StringEnsemble2        | SynthStrings1
  |  SynthStrings2          | ChoirAahs              | VoiceOohs
  |  SynthVoice             | OrchestraHit           | Trumpet
  |  Trombone               | Tuba                   | MutedTrumpet
  |  FrenchHorn             | BrassSection           | SynthBrass1
  |  SynthBrass2            | SopranoSax             | AltoSax
  |  TenorSax               | BaritoneSax            | Oboe
  |  Bassoon                | EnglishHorn            | Clarinet
  |  Piccolo                | Flute                  | Recorder
  |  PanFlute               | BlownBottle            | Shakuhachi
  |  Whistle                | Ocarina                | Lead1Square
  |  Lead2Sawtooth          | Lead3Calliope          | Lead4Chiff
  |  Lead5Charang           | Lead6Voice             | Lead7Fifths
  |  Lead8BassLead          | Pad1NewAge             | Pad2Warm
  |  Pad3Polysynth          | Pad4Choir              | Pad5Bowed
  |  Pad6Metallic           | Pad7Halo               | Pad8Sweep
  |  FX1Train               | FX2Soundtrack          | FX3Crystal
  |  FX4Atmosphere          | FX5Brightness          | FX6Goblins
  |  FX7Echoes              | FX8SciFi               | Sitar
  |  Banjo                  | Shamisen               | Koto
  |  Kalimba                | Bagpipe                | Fiddle
  |  Shanai                 | TinkleBell             | Agogo
  |  SteelDrums             | Woodblock              | TaikoDrum
  |  MelodicDrum            | SynthDrum              | ReverseCymbal
  |  GuitarFretNoise        | BreathNoise            | Seashore
  |  BirdTweet              | TelephoneRing          | Helicopter
  |  Applause               | Gunshot                | Percussion
  |  CustomInstrument String
  deriving (Show, Eq, Ord)



  {-

-- | PitchClass represents musical notes including quarter tones
-- Each note can be:
-- Double flat (ff), Flat-and-a-quarter (fq), Flat (f), Quarter-flat (qf)
-- Natural (n), Quarter-sharp (qs), Sharp (s), Sharp-and-a-quarter (sq), Double sharp (ss)
data PitchClass =
    Cff  | Cfq  | Cf   | Cqf  | C    | Cqs  | Cs   | Csq  | Css  |
    Dff  | Dfq  | Df   | Dqf  | D    | Dqs  | Ds   | Dsq  | Dss  |
    Eff  | Efq  | Ef   | Eqf  | E    | Eqs  | Es   | Esq  | Ess  |
    Fff  | Ffq  | Ff   | Fqf  | F    | Fqs  | Fs   | Fsq  | Fss  |
    Gff  | Gfq  | Gf   | Gqf  | G    | Gqs  | Gs   | Gsq  | Gss  |
    Aff  | Afq  | Af   | Aqf  | A    | Aqs  | As   | Asq  | Ass  |
    Bff  | Bfq  | Bf   | Bqf  | B    | Bqs  | Bs   | Bsq  | Bss
    deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- Updated pitch conversion map to handle quarter tones
-- Each quarter step is represented by 0.5 in the conversion
pcToIntMap :: Map.Map PitchClass Double
pcToIntMap = Map.fromList [
    -- C and variants (from double-flat to double-sharp)
    (Cff, -2.0), (Cfq, -1.75), (Cf, -1.5), (Cqf, -1.25), (C, 0.0),
    (Cqs, 0.25), (Cs, 0.5),    (Csq, 0.75), (Css, 1.0),

    -- D and variants
    (Dff, 0.0),  (Dfq, 0.25),  (Df, 0.5),   (Dqf, 0.75), (D, 1.0),
    (Dqs, 1.25), (Ds, 1.5),    (Dsq, 1.75), (Dss, 2.0),

    -- E and variants
    (Eff, 2.0),  (Efq, 2.25),  (Ef, 2.5),   (Eqf, 2.75), (E, 3.0),
    (Eqs, 3.25), (Es, 3.5),    (Esq, 3.75), (Ess, 4.0),

    -- F and variants
    (Fff, 3.0),  (Ffq, 3.25),  (Ff, 3.5),   (Fqf, 3.75), (F, 4.0),
    (Fqs, 4.25), (Fs, 4.5),    (Fsq, 4.75), (Fss, 5.0),

    -- G and variants
    (Gff, 5.0),  (Gfq, 5.25),  (Gf, 5.5),   (Gqf, 5.75), (G, 6.0),
    (Gqs, 6.25), (Gs, 6.5),    (Gsq, 6.75), (Gss, 7.0),

    -- A and variants
    (Aff, 7.0),  (Afq, 7.25),  (Af, 7.5),   (Aqf, 7.75), (A, 8.0),
    (Aqs, 8.25), (As, 8.5),    (Asq, 8.75), (Ass, 9.0),

    -- B and variants
    (Bff, 9.0),  (Bfq, 9.25),  (Bf, 9.5),   (Bqf, 9.75), (B, 10.0),
    (Bqs, 10.25), (Bs, 10.5),  (Bsq, 10.75), (Bss, 11.0)
  ]

-- Quarter tone helper functions
cff, cfq, cf, cqf, c, cqs, cs, csq, css,
dff, dfq, df, dqf, d, dqs, ds, dsq, dss,
eff, efq, ef, eqf, e, eqs, es, esq, ess,
fff, ffq, ff, fqf, f, fqs, fs, fsq, fss,
gff, gfq, gf, gqf, g, gqs, gs, gsq, gss,
aff, afq, af, aqf, a, aqs, as, asq, ass,
bff, bfq, bf, bqf, b, bqs, bs, bsq, bss :: Octave -> Dur -> Music Pitch

-- C family
cff o d = note d (Cff, o); cfq o d = note d (Cfq, o)
cf  o d = note d (Cf,  o); cqf o d = note d (Cqf, o)
c   o d = note d (C,   o); cqs o d = note d (Cqs, o)
cs  o d = note d (Cs,  o); csq o d = note d (Csq, o)
css o d = note d (Css, o)

-- D family
dff o d = note d (Dff, o); dfq o d = note d (Dfq, o)
df  o d = note d (Df,  o); dqf o d = note d (Dqf, o)
d   o d = note d (D,   o); dqs o d = note d (Dqs, o)
ds  o d = note d (Ds,  o); dsq o d = note d (Dsq, o)
dss o d = note d (Dss, o)

-- E family
eff o d = note d (Eff, o); efq o d = note d (Efq, o)
ef  o d = note d (Ef,  o); eqf o d = note d (Eqf, o)
e   o d = note d (E,   o); eqs o d = note d (Eqs, o)
es  o d = note d (Es,  o); esq o d = note d (Esq, o)
ess o d = note d (Ess, o)

-- F family
fff o d = note d (Fff, o); ffq o d = note d (Ffq, o)
ff  o d = note d (Ff,  o); fqf o d = note d (Fqf, o)
f   o d = note d (F,   o); fqs o d = note d (Fqs, o)
fs  o d = note d (Fs,  o); fsq o d = note d (Fsq, o)
fss o d = note d (Fss, o)

-- G family
gff o d = note d (Gff, o); gfq o d = note d (Gfq, o)
gf  o d = note d (Gf,  o); gqf o d = note d (Gqf, o)
g   o d = note d (G,   o); gqs o d = note d (Gqs, o)
gs  o d = note d (Gs,  o); gsq o d = note d (Gsq, o)
gss o d = note d (Gss, o)

-- A family
aff o d = note d (Aff, o); afq o d = note d (Afq, o)
af  o d = note d (Af,  o); aqf o d = note d (Aqf, o)
a   o d = note d (A,   o); aqs o d = note d (Aqs, o)
as  o d = note d (As,  o); asq o d = note d (Asq, o)
ass o d = note d (Ass, o)

-- B family
bff o d = note d (Bff, o); bfq o d = note d (Bfq, o)
bf  o d = note d (Bf,  o); bqf o d = note d (Bqf, o)
b   o d = note d (B,   o); bqs o d = note d (Bqs, o)
bs  o d = note d (Bs,  o); bsq o d = note d (Bsq, o)
bss o d = note d (Bss, o)

-- Updated pitch conversion for quarter tones
-- Now using Double for more precise pitch calculations
type AbsPitch = Double

absPitch :: Pitch -> AbsPitch
absPitch (pc, oct) = 12 * fromIntegral (oct + 1) + fromMaybe 0 (Map.lookup pc pcToIntMap)

-- Convert absolute pitch back to Pitch, handling quarter tones
pitch :: AbsPitch -> Pitch
pitch ap =
    let (oct, remainder) = properFraction (ap / 12)
        quartertoneVal = remainder * 12
        pc = findClosestPitch quartertoneVal
    in (pc, oct - 1)
  where
    findClosestPitch val =
        fst $ minimumBy (comparing (abs . subtract val . snd)) (Map.toList pcToIntMap)

-- Updated transposition to handle quarter tones
trans :: Double -> Pitch -> Pitch
trans i p = pitch (absPitch p + i)

  -}
