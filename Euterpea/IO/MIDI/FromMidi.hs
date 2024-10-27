
module Euterpea.IO.MIDI.FromMidi (fromMidi) where
import Euterpea.Music
import Euterpea.IO.MIDI.ToMidi
import Euterpea.IO.MIDI.GeneralMidi
import Data.List
import Codec.Midi

data NEvent = On | Off
  deriving (Eq, Show, Ord)

data SimpleMsg = SE (Rational, AbsPitch, Volume, Int, NEvent) |
              T (Rational, Rational)
  deriving (Eq, Show)
instance Ord (SimpleMsg) where
    compare (SE(t,p,v,i,e)) (SE(t',p',v',i',e')) = 
        if t<t' then LT else if t>t' then GT else EQ
    compare (T(t,x)) (SE(t',p',v',i',e')) = 
        if t<t' then LT else if t>t' then GT else EQ
    compare (SE(t,p,v,i,e)) (T(t',x)) = 
        if t<t' then LT else if t>t' then GT else EQ
    compare (T(t,x)) (T(t',x')) =
        if t<t' then LT else if t>t' then GT else EQ

addTrackTicks :: Int -> [(Ticks, a)] -> [(Ticks, a)]
addTrackTicks sum [] = []
addTrackTicks sum ((t,x):ts) = (t+sum,x) : addTrackTicks (t+sum) ts

applyTD :: TimeDiv -> SimpleMsg -> SimpleMsg
applyTD tdw x = 
    case x of T(t,i) -> T(fixT tdw t, i) 
              SE(t,p,v,i,e) -> SE(fixT tdw t, p, v, i, e) where

fixT tdw t = 
    case tdw of TicksPerBeat td -> t / (fromIntegral td * 4)
                TicksPerSecond fps tpf -> t / fromIntegral (fps * tpf)

midiToEvents :: Midi -> [[SimpleMsg]]
midiToEvents m = 
    let ts = map (simplifyTrack 0) $ map (addTrackTicks 0) (tracks m) 
    in  distributeTempos $ map (map (applyTD $ timeDiv m)) ts where 
  simplifyTrack :: Int -> [(Ticks, Message)] -> [SimpleMsg]
  simplifyTrack icur [] = []
  simplifyTrack icur ((t,m):ts) = 
    case m of (NoteOn c p v) -> 
                  SE (fromIntegral t, p, v, icur, On) : simplifyTrack icur ts
              (NoteOff c p v) -> 
                  SE (fromIntegral t, p, v, icur, Off) : simplifyTrack icur ts
              (ProgramChange c p) -> simplifyTrack (if c==9 then (-1) else p) ts 
              (TempoChange x) -> T (fromIntegral t, fromIntegral x) : simplifyTrack icur ts
              _ -> simplifyTrack icur ts 

distributeTempos :: [[SimpleMsg]] -> [[SimpleMsg]]
distributeTempos tracks = 
    if length tracks > 1 then map (sort . (head tracks ++)) (tail tracks)
    else tracks -- must be a single-track file with embedded tempo changes.

eventsToMusic :: [[SimpleMsg]] -> Music (Pitch, Volume)
eventsToMusic tracks = 
    let tracks' = splitByInstruments tracks -- handle any mid-track program changes
        is = map toInstr $ map getInstrument $ filter (not.null) tracks' -- instruments
        tDef = 500000 -- current tempo, 120bpm as microseconds per qn
    in  chord $ zipWith instrument is $ map (seToMusic tDef) tracks' where
  
  toInstr :: Int -> InstrumentName
  toInstr i = if i<0 then Percussion else toEnum i 

  seToMusic :: Rational -> [SimpleMsg] -> Music (Pitch, Volume)
  seToMusic tCurr [] = rest 0
  seToMusic tCurr (e1@(SE(t,p,v,ins,On)):es) = 
    let piMatch (SE(t1,p1,v1,ins1,e1)) = (p1==p && ins1==ins) && e1==Off
        piMatch (T(t1,x)) = False
        is = findIndices piMatch es -- find mactching note-offs
        SE(t1,p1,v1,ins1, e) = es !! (is !! 0) -- pick the first matching note-off
        n = (rest t :+: note (t1-t) (pitch p,v)) -- create a Music note
    in  if v > 0 then -- a zero volume note is silence
             if length is > 0 then n :=: seToMusic tCurr es -- found an off
             else seToMusic tCurr ((e1:es)++[correctOff e1 es]) -- missing off case
        else seToMusic tCurr es
  seToMusic tCurr (e1@(T (t,newTempo)):es) = 
    let t2 = getTime $ head es -- find time of next event after tempo change
        tfact = tCurr / newTempo -- calculate tempo change factor
        es' = map (changeTime (subtract t)) es -- adjust start times
        m = rest t :+: tempo tfact (seToMusic newTempo es')  
    in  if null es then rest 0 else m where
        changeTime f (SE (t,p,v,i,e)) = SE (f t,p,v,i,e)
        changeTime f (T (t,x)) = T (f t, x)
  seToMusic tCurr (_:es) = seToMusic tCurr es -- ignore note-offs (already handled)

getTime (SE(t,p,v,i,e)) = t
getTime (T (t,x)) = t

getInstrument ((SE(t,p,v,i,e)):xs) = i
getInstrument ((T x) : xs) = getInstrument xs
getInstrument [] = -1 -- No instrument assigned

splitByInstruments :: [[SimpleMsg]] -> [[SimpleMsg]] 
splitByInstruments [] = []
splitByInstruments (t:ts) = 
    let i = getInstrument t
        (t',t'') = splitByI i t
        ts' = if or $ map isSE t'' then splitByInstruments (t'':ts) 
              else splitByInstruments ts
    in  if or $ map isSE t' then t' : ts' else ts'

isSE :: SimpleMsg -> Bool
isSE (SE xs) = True
isSE (T i) = False

splitByI :: Int -> [SimpleMsg] -> ([SimpleMsg],[SimpleMsg])
splitByI i0 [] = ([],[])
splitByI i0 (x:xs) = 
    let (ts,fs) = splitByI i0 xs
        f (SE(_,_,_,i1,_)) = i0 == i1
        f _ = False
    in  case x of SE x' -> if f x then (x:ts,fs) else (ts,x:fs)
                  T i -> (x:ts, x:fs) -- add tempos to both streams

correctOff (SE(t,p,v,ins,e)) [] = SE(t,p,v,ins,Off)
correctOff (SE(t,p,v,ins,e)) es = 
    let SE(t1,p1,v1,ins1,e1) = last $ filter isSE es
    in  SE(t1,p,v,ins,Off) 

fromMidi :: Midi -> Music1
fromMidi m = 
    let seList = midiToEvents m
        iNums = filter (>0) $ map getInstrument seList
        upm = makeUPM $ map toEnum iNums
    in  mMap (\(p,v) -> (p, [Volume v])) $ eventsToMusic seList

makeUPM :: [InstrumentName] -> UserPatchMap
makeUPM is = 
    case findIndex (==Percussion) is of 
        Nothing -> zip is ([0..8]++[10..]) -- no percussion
        Just i -> (is !! i, 9) : 
                  zip (take i is ++ drop (i+1) is) ([0..8]++[10..])
