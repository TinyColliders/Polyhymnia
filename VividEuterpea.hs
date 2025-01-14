{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- module based on code by DonyaQuick
module VividEuterpea where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception      (onException)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Euterpea
import           System.Info
import           Vivid

type Params = [Double]
type VInstr = Dur -> AbsPitch -> Volume -> Params -> SynthDef '["gate", "fadeSecs"]

data ReleaseType =
    FixedDuration  -- sound duration unaffected by note duration (percussive, frees itself)
  | Internal       -- sound duration handled within synth def with envelopes (frees itself)
  | External       -- fade out and free expected to be handled externally to synth def
  deriving (Eq, Show, Ord, Enum)

data SynthInfo = SynthInfo {
    synthDef    :: VInstr,
    releaseTime :: Dur,
    releaseType :: ReleaseType
}

toSynth :: SynthInfo -> AbsPitch -> Volume -> Params -> SynthDef '["gate", "fadeSecs"]
toSynth e ap v p = (synthDef e) (releaseTime e) ap v p

type SynthTable = [(InstrumentName, SynthInfo)]

defaultSound :: VInstr
defaultSound _ ap _ _ = sd (1 ::I "gate", 0 ::I "fadeSecs") $ do
  s <- 0.3 ~* sinOsc (freq_ $ midiCPS ap)
  e <- envGen (env 1.0 [(0.0, 0.25)] Curve_Linear) FreeEnclosing
  out 0 [s ~* e, s ~* e]

defaultSynth :: SynthInfo
defaultSynth = SynthInfo defaultSound 0.25 FixedDuration

playV :: (ToMusic1 a) => SynthTable -> Music a -> IO ()
playV t m = onException (playMEvs t 0 $ perform m) cmdPeriod

instance NFData MEvent where
  rnf (MEvent t i ap d v params) =
      rnf t `seq` rnf i `seq` rnf ap `seq` rnf d `seq` rnf v `seq` rnf params

playVC :: (ToMusic1 a) => SynthTable -> PlayParams -> Music a -> IO ()
playVC t pp m =
  let x = (perfAlg pp . toMusic1) m
  in  if strict pp then deepseq x $ onException (playMEvs t 0 x) cmdPeriod
      else onException (playMEvs t 0 x) cmdPeriod

playVS :: (ToMusic1 a) => SynthTable -> Music a -> IO ()
playVS t = playVC t defParams {strict=True}

playEvent :: VividAction m => SynthTable -> MEvent -> m ()
playEvent insts me = do
  let eSyn = maybe defaultSynth id (lookup (eInst me) insts)
      sd   = toSynth eSyn (ePitch me) (eVol me) (eParams me)
      waitTime = case releaseType eSyn of
          Internal -> eDur me + releaseTime eSyn
          External -> eDur me
          _        -> releaseTime eSyn
  case releaseType eSyn of
    External -> do
      s0 <- synth sd ()
      wait $ fromRational waitTime
      set s0 (fromRational (releaseTime eSyn) :: I "fadeSecs")
      release s0
    _ -> do
      s0 <- synth sd ()
      wait (fromRational waitTime)

playMEvs :: VividAction m => SynthTable -> PTime -> [MEvent] -> m ()
playMEvs _ _ [] = return ()
playMEvs insts cTime (me:mes) = do
  wait $ fromRational (eTime me - cTime)
  fork $ playEvent insts me
  playMEvs insts (eTime me) mes

writeWavV :: (ToMusic1 a) => FilePath -> SynthTable -> Music a -> IO ()
writeWavV outFile t m = writeNRT outFile (playMEvs t 0 $ perform m)
