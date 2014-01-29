module ZMidi.Skyline.MelFind where -- ( findMelody ) where

import ZMidi.Score

import Data.List          ( sortBy, groupBy, partition )
import Data.Function      ( on )
import Data.Ord           ( comparing )
import Control.Arrow      ( (***), second )

--------------------------------------------------------------------------------
-- Melody Finding
--------------------------------------------------------------------------------

-- | Representing monophonic 'Voice's. A 'MonoVoice' only contains NoteEvents
-- and no two NoteEvents have the same onset timestamp.
-- newtype MonoVoice = MonoVoice { monoVoice :: Voice }

type MelFind = Voice -> (Voice, Voice)

-- TODO: output QMidiFile instead of Voice

-- | Applies 'findMelodyQuant' to a 'MidiScore' but returns the score instead
-- of just the 'Voice'.
filterMelodyQuant :: ShortestNote -> MidiScore -> MidiScore
filterMelodyQuant q ms = ms {getVoices = [findMelodyQuant q ms]}

-- | Merges all 'Voices' and returns the melody using the skyline algorithm
-- with a lower limit at the middle C. The melody is quantised by
-- 'ShortestNote' and the overlapping notes are cut off. [more..?]
findMelodyQuant :: ShortestNote ->  MidiScore -> Voice
findMelodyQuant q = removeOverlap . head . getVoices -- get the first voice 
                                  . sepHand skyLineLLDipDetect -- melody finding
                                  . qMidiScore . quantise q . mergeTracks   

-- | The complement of 'findMelodyQuant', returning only the accompaniment 
-- without the melody in a single 'Voice'. 
getAccompQuant  :: ShortestNote ->  MidiScore -> Voice
getAccompQuant q = (!! 1) . getVoices          -- get the second voice
                 . sepHand skyLineLLDipDetect  -- melody separation
                 . qMidiScore . quantise q . mergeTracks 
                                  
-- | Returns the melody 'Voice', if there the 'MidiScore' has exactly 2 voices
-- This function assumes that the first track is the melody.
getMelody :: MidiScore -> Voice 
getMelody ms = case getVoices ms of
  [r,_l] -> r
  _   -> error "getMelody: Found a MIDI file with more or less than 2 tracks"

-- | Merges all tracks into one track. This track is guaranteed to be sorted.
mergeTracks :: MidiScore -> MidiScore
mergeTracks ms = let vs = [sortVoice . setChans 0 . concat . getVoices $ ms]
                     -- the minimum duration might have changed
                     md = gcIOId . buildTickMap $ vs
                 in  ms {getVoices = vs, minDur = md} 
 
-- TODO: adapt channel number
-- | A hand separation function that takes a separation function and applies
-- this to a 'MidiScore'. sepHand throws an error the number of tracks is not 1
sepHand :: MelFind -> MidiScore -> MidiScore
sepHand f mf = case getVoices mf of
  [x] -> let (r,l) = f x in mf {getVoices = [r,l]}
  _   -> error "sepHand: more or less than 1 voice!"

-- | Skyline melody finding with a lower limit at the middle C and a dip
-- detection tolerance of 9 semitones.
skyLineLLDipDetect :: Voice -> (Voice, Voice)
skyLineLLDipDetect = combine (skyLineLowLim (Pitch (0,0))) (dipDetect (-9) 9)
  
-- | An implementation of the "skyline algorithm" that picks the highest note  
-- in a melody (high, low)
skyLine :: Voice -> (Voice, Voice)  -- MelFind
skyLine = skyLineLowLim (Pitch (-5,0))
    
-- | An implementation of the "skyline algorithm" that picks the highest note
-- in a melody (high, low), and has a lower limit. All notes below a 'Pitch' 
-- /x/ belong to the accompaniment
skyLineLowLim :: Pitch -> Voice -> (Voice, Voice)
skyLineLowLim p = (f 0 *** f 1) . unzip . map (pickHigh p) 
                                        . groupBy ((==) `on` onset)
  where f c = setChans c . concat

-- | Picks the highest notes and separates them from the rest (high,low)
pickHigh :: Pitch -> [Timed ScoreEvent] 
         -> ([Timed ScoreEvent],[Timed ScoreEvent])
pickHigh _ [ ] = error "pickHigh: empty list"
pickHigh p l | getPitch h >= p = ([h], t)
             | otherwise       = ([ ], l) 
                 where (h:t)   = reverse . sortBy (comparing getPitch) $ l 


--------------------------------------------------------------------------------
-- Dip detection
--------------------------------------------------------------------------------

-- | Takes an 'Interval' down and up, respectively, and performs dip detection
-- on a 'Voice', returning the melody without dips and the dips. N.B. a 
-- monophonic melody is assumed.
dipDetect :: Interval -> Interval -> Voice -> (Voice, Voice)
dipDetect dwn up v = 
  (f *** f) . partition ((Dip /= ) . snd) . markDip dwn up $ v where f = map fst
       
data IntClass = Up | Down | OK | Dip deriving (Eq, Show)

-- | Marks notes of they represent a significant dip
markDip :: Interval -> Interval -> [Timed ScoreEvent] 
        -> [(Timed ScoreEvent, IntClass)]
markDip dwn up v = foldr mark [] . foldr classInter [] $ v where

  -- Given a note and its interval classes, dips are marked
  mark :: (Timed ScoreEvent, IntClass) -> [(Timed ScoreEvent , IntClass)]
         -> [(Timed ScoreEvent, IntClass)]
  mark x []    = [x]
  mark s (h:t) = case (snd s, snd h) of
    (Down, Up) -> s : second (const Dip) h : t -- mark Down-Up patterns
    _          -> s : h                    : t -- otherwise continue

  -- classifies an interval between to notes. A note is coupled with the 
  -- interval class between the previous note and the note itself
  classInter :: Timed ScoreEvent -> [(Timed ScoreEvent, IntClass)]
           -> [(Timed ScoreEvent, IntClass)]
  classInter a []            = [(a, OK  )]
  classInter a l | i >= up   =  (a, Up  ) : l
                 | i <= dwn  =  (a, Down) : l
                 | otherwise =  (a, OK  ) : l
    where i = getInterval (getPitch a) (getPitch . fst . head $ l)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | combines to 'MelFind' melody finding functions.
combine :: MelFind -> MelFind -> (Voice -> (Voice, Voice))
combine f s v = let (melf, accf) = f v
                    (mels, accs) = s melf
                in  (mels, sortVoice (accs ++ accf))

-- | Sorts a 'Voice' based on the onset times of the 'ScoreEvents'
sortVoice :: Voice -> Voice 
sortVoice = sortBy (comparing onset)

setChans :: Channel -> [Timed ScoreEvent] -> [Timed ScoreEvent]
setChans c = map (setChan c)

setChan :: Channel -> Timed ScoreEvent -> Timed ScoreEvent
setChan c tse = fmap f tse where f ne = ne {chan = c}

-- | Returns the number of different channels used in NoteEvents within a track
countChan :: Voice -> Int
countChan = length . groupBy ((==) `on` f) . sortBy (comparing f)
  where f = chan . getEvent

     