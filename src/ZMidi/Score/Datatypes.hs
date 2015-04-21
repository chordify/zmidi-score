{-# OPTIONS_GHC -Wall                   #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
-- |
-- Module      :  ZMidi.Score.Datatypes
-- Copyright   :  (c) 2012--2014, Utrecht University 
-- License     :  LGPL-3
--
-- Maintainer  :  W. Bas de Haas <w.b.dehaas@uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: a simple score representation derived from a MidiFile (as parsed
-- by the ZMidi.Core library: <https://hackage.haskell.org/package/zmidi-core>
module ZMidi.Score.Datatypes ( -- * Score representation of a MidiFile
                               MidiScore (..)
                             , Key (..)
                             , TimeSig (..)
                             , MeterKind (..)
                             , Voice 
                             , Channel (..)
                             , Pitch (..)
                             , Octave (..)
                             , PitchClass (..)
                             , Interval (..)
                             , Velocity (..)
                             , Timed (..)
                             , Time (..)
                             , Bar (..)
                             , Beat (..)
                             , BeatRat (..)
                             , BarRat (..)
                             , TPB (..)
                             , ScoreEvent (..)
                             , HasMeter (..)
                             ) where

import ZMidi.Score.Internal
import ZMidi.Core                 ( MidiFormat (..), MidiScaleType (..) )
import Data.Ratio                 ( Ratio, numerator, denominator, (%) )
import Data.Word                  ( Word8 )
import Data.Int                   ( Int8 )
import Data.Char                  ( toLower )
import Text.Printf                ( PrintfArg )
import Data.Aeson                 ( ToJSON (..), FromJSON (..)
                                  , (.=), (.:), Value (..), object)
import Data.Text                  ( pack, unpack )
import Data.Binary                ( Binary, Get )
import qualified Data.Binary as B ( get, put )
import GHC.Generics               ( Generic )
import Control.DeepSeq            ( NFData (..) )
import Control.DeepSeq.Generics   ( genericRnf)
import Control.Applicative        ( (<$>), (<*>), (<|>) )
import Control.Monad              ( mzero )


--------------------------------------------------------------------------------                                   
-- A less low-level MIDI data representation
--------------------------------------------------------------------------------

-- | Stores the main elements of a musical score that can be derived from a 
-- midifile
data MidiScore  = MidiScore     { -- | The 'Key's of the piece with time stamps
                                  getKey     :: [Timed Key]
                                  -- | The 'TimeSig'natures of the piece with time stamps
                                , getTimeSig :: [Timed TimeSig]
                                  -- | The number of MIDI-ticks-per-beat
                                , ticksPerBeat :: TPB  
                                  -- | The kind of midi file that created this score
                                , midiFormat :: MidiFormat
                                  -- | The microseconds per quarter note
                                , tempo      :: [Timed Time]         
                                  -- TODO the usage of the newtype Time is 
                                  -- confusing as it does not represent ticks, 
                                  -- we should use a separate
                                  -- newtype 'Tempo' or the the like
                                  -- | The minimum note length found.
                                , minDur     :: Time
                                  -- | The midi 'Voice's
                                , getVoices  :: [Voice]
                                } deriving (Eq, Show, Generic)

-- | Represents a musical key
data Key        = Key           { keyRoot    :: Int8
                                , keyMode    :: MidiScaleType
                                } 
                | NoKey           deriving (Eq, Ord, Generic)
               
-- | A 'TimeSig'nature has a fraction, e.g. 4\/4, 3\/4, or 6\/8.
data TimeSig    = TimeSig       { tsNum      :: Int 
                                , tsDen      :: Int
                                , metronome  :: Word8
                                , nr32ndNotes:: Word8
                                }
                | NoTimeSig       deriving (Generic)

-- | Describes the kind of meter in a piece. 
data MeterKind  = -- | A meter with a numerator that is dividable by two
                  Duple 
                  -- | A meter with a numerator dividable by three
                | Triple 
                  -- | an ambiguous meter with a numerator dividable both by
                  -- two and three, e.g. a 12\/8 meter
                | Both
                  -- | An odd meter, with a numerator that is dividable 
                  -- by two or three
                | Odd deriving (Show, Eq, Ord, Generic)
                
                
-- Note: we could consider adding a Voice label, like there is a track label
-- | A 'Voice' is a list of 'ScoreEvent's that have time stamps.
type Voice      = [Timed ScoreEvent]

-- | The MIDI Channel as stored in the MIDI file
newtype Channel = Channel {channel :: Word8 }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, NFData )

-- | Pitch is represented by a 'PitchClass' and an 'Octave' 
newtype Pitch   = Pitch   ( Octave, PitchClass ) 
                    deriving ( Eq, Ord, Binary, NFData, Generic )
                    
-- | Represents a musical octave
newtype Octave   = Octave { octave :: Int }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, PrintfArg, NFData, Generic )
-- | A Pitch class representation (there is no check for values > 11)
newtype PitchClass = PitchClass { pitchclass :: Int }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, PrintfArg, NFData, Generic )

-- | Represents a musical interval
newtype Interval   = Interval { interval :: Int }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, PrintfArg, NFData, Generic )
                    
-- | Represents MIDI velocity
newtype Velocity = Velocity { velocity :: Word8 }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, NFData, Generic )

-- TODO: should be renamed into Tick, which is more clear
-- | Represents MIDI time in ticks
newtype Time    = Time { time :: Int } 
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, PrintfArg, NFData, Generic )

-- | A Bar counter used to interpret a MIDI 'Time' stamp
newtype Bar     = Bar  { bar  :: Int } 
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, PrintfArg, NFData, Generic )
                    
-- | A counter for musical beats
newtype Beat    = Beat { beat :: Int } 
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, PrintfArg, NFData, Generic )

-- | Representing time within a 'Beat' as a 'Ratio'
newtype BeatRat = BeatRat { beatRat  :: Ratio Int } 
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Binary, NFData, Generic )                    

-- | Representing time within a 'Bar' as a 'Ratio'
newtype BarRat  = BarRat  { barRat  :: Ratio Int } 
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Binary, NFData, Generic )  

-- | The MIDI ticks in 'Time' per 'Beat'
newtype TPB     = TPB { tpb :: Int } 
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Integral, Binary, PrintfArg, NFData, Generic )
                    
-- | Adds MIDI 'Time' information to a datatype
data Timed a    = Timed         { onset       :: Time 
                                , getEvent    :: a
                                } deriving (Functor, Eq, Ord, Generic)

-- | Within ZMidi.Score we represent four score events: a note, a key change, 
-- a time signature or a tempo change
data ScoreEvent = NoteEvent     { chan        :: Channel
                                , pitch       :: Pitch
                                , velo        :: Velocity
                                -- No, this should this not be in Timed 
                                -- because only a NoteEvent has a duration
                                -- (or we should give Key and TimeSig changes 
                                -- also a duration)
                                , duration    :: Time 
                                } 
                | KeyChange     { keyChange   :: Key
                                } 
                | TimeSigChange { tsChange    :: TimeSig
                                } 
                | TempoChange   { tempChange  :: Time
                                } deriving (Eq, Ord, Show, Generic)

class (Show a, Eq a) => HasMeter a where
  fromTimeSig   :: TimeSig -> a
  -- | Determines the 'MeterKind' based on a 'TimeSig'nature
  toMeterKind   :: a -> MeterKind
  -- | Determines how many 'Beat's are in a 'Bar', if it's possible to
  -- determine this.
  toBarDivision :: a -> Maybe Int
                                                              
--------------------------------------------------------------------------------
-- Some ad-hoc instances
--------------------------------------------------------------------------------

instance Show a => Show (Timed a) where
  show (Timed t a) = show a ++ " @ " ++ show (time t)

instance Eq TimeSig where
  (TimeSig a1 b1 _ _) == (TimeSig a2 b2 _ _) = a1 == a2 && b1 == b2
  NoTimeSig           == NoTimeSig           = True
  _                   == _                   = False

instance Ord TimeSig where
  compare _         NoTimeSig                     = GT
  compare NoTimeSig _                             = LT
  compare (TimeSig a1 b1 _ _) (TimeSig a2 b2 _ _) = 
    case compare b1 b2 of 
      EQ -> compare a1 a2
      c  -> c

instance Show TimeSig where
  show (TimeSig n d _ _) = show n ++ '/' : show d
  show NoTimeSig         = "NoTimeSig"  

instance Read TimeSig where 
  readsPrec _ = error "Read TimeSig: implement me"

instance Read MeterKind where 
  readsPrec _ = error "Read MeterKind: implement me"

instance HasMeter TimeSig where
  fromTimeSig = id
  toMeterKind = toMeterKind'

  toBarDivision NoTimeSig = error "toBarDivision applied to NoTimeSig"
  toBarDivision a = Just . tsNum $ a

instance HasMeter MeterKind where
  fromTimeSig     = toMeterKind'
  toMeterKind     = id
  toBarDivision x = case x of Duple  -> Just 2 
                              Triple -> Just 3
                              _      -> Nothing
  
toMeterKind' :: TimeSig -> MeterKind
toMeterKind' NoTimeSig = error "toMeterKind applied to NoTimeSig"
toMeterKind' ts = let n = tsNum ts 
                 in case (n `mod` 2, n `mod` 3) of 
                      (0,0) -> Both
                      (0,_) -> Duple
                      (_,0) -> Triple
                      _     -> Odd    
  
instance Show Key where
  show NoKey      = "NoKey"
  show (Key rt m) = showRoot rt ++ ' ' : (map toLower . show $ m) where
      
    showRoot :: Int8 -> String
    showRoot i = let r = fromIntegral i in case compare r 0 of
      LT -> replicate (abs r) 'b'
      EQ -> "0"
      GT -> replicate r '#'

instance Show Pitch where
  show (Pitch (oct, p)) = showOct oct ++ showPitch p where
  
    showOct :: Octave -> String
    showOct (Octave i) | i < 0     = show i
                       | otherwise = ' ' : show i
  
-- shows the Midi number in a musical way
-- N.B. ignoring all pitch spelling, at the moment
showPitch :: PitchClass -> String
showPitch p = case pitchclass p of
                0  -> "C "
                1  -> "C#"
                2  -> "D "
                3  -> "D#"
                4  -> "E "
                5  -> "F "
                6  -> "F#"
                7  -> "G "
                8  -> "G#"
                9  -> "A "
                10 -> "Bb"
                11 -> "B "
                n  -> invalidMidiNumberError n

-- Binary instances
instance Binary MidiScore
instance Binary TimeSig
instance Binary MeterKind
instance Binary ScoreEvent
instance Binary Key  
instance (Binary a) => Binary (Timed a)
instance Binary MidiScaleType where
  put MAJOR           =    B.put (0 :: Word8)
  put MINOR           =    B.put (1 :: Word8)
  put (SCALE_OTHER i) = do B.put (2 :: Word8)
                           B.put i
                           
  get = do t <- B.get :: Get Word8
           case t of 
             0 -> return MAJOR
             1 -> return MINOR
             2 -> do i <- B.get
                     return (SCALE_OTHER i)
             _ -> error "invalid binary encoding of MidiScaleType"
      
instance Binary MidiFormat where
  put MF0 = B.put (0 :: Word8)
  put MF1 = B.put (1 :: Word8)
  put MF2 = B.put (2 :: Word8)
  
  get = do t <- B.get :: Get Word8
           case t of 
             0 -> return MF0           
             1 -> return MF1           
             2 -> return MF2
             _ -> error "invalid binary encoding of MidiFormat"
             
-- NFData instances
instance NFData MidiScore  where rnf = genericRnf
instance NFData TimeSig    where rnf = genericRnf
instance NFData MeterKind  where rnf = genericRnf
instance NFData ScoreEvent where rnf = genericRnf
instance NFData Key        where rnf = genericRnf
instance NFData a => NFData (Timed a) where rnf = genericRnf
instance NFData MidiScaleType where rnf a = a `seq` ()
instance NFData MidiFormat    where rnf a = a `seq` ()

--------------------------------------------------------------------------------
-- JSON import and export
--------------------------------------------------------------------------------
instance ToJSON Beat
instance ToJSON BeatRat
instance ToJSON BarRat

instance (Integral a, ToJSON a) => ToJSON (Ratio a) where
     toJSON r = object [pack "num" .= numerator r, pack "den" .= denominator r]  

instance ToJSON (TimeSig) where
     toJSON (TimeSig n d _ _) = object [pack "ts_num" .= n, pack "ts_den" .= d]
     toJSON NoTimeSig         = object [pack "ts" .= pack "none"]     

instance ToJSON MeterKind where
     toJSON mk = case mk of Duple  -> object [pack "mk" .= pack "dupl"]
                            Triple -> object [pack "mk" .= pack "trpl"]
                            Both   -> object [pack "mk" .= pack "both"]
                            Odd    -> object [pack "mk" .= pack "odd"]
     
instance FromJSON Beat
instance FromJSON BeatRat
instance FromJSON BarRat
     
instance (Integral a, FromJSON a) => FromJSON (Ratio a) where
     parseJSON (Object v) = (%) <$> v .: (pack "num") <*> v .: (pack "den")
     parseJSON _          = mzero
     
     
     -- TODO add NoTimeSig case...
instance FromJSON (TimeSig) where
     parseJSON (Object v) =  
       let ts   = (\n d -> TimeSig n d 0 0) <$> v .: (pack "ts_num") 
                                            <*> v .: (pack "ts_den") 
           nots = let f x = case unpack x of
                              "none" -> NoTimeSig
                              y -> error ("FromJSON TimeSig: unknown json: " ++ y)
                  in f <$> v .: (pack "ts")  
           
       in ts <|> nots 
     parseJSON _          = mzero
     
instance FromJSON MeterKind where
     parseJSON (Object v) = 
       let toMK x = case unpack x of 
                      "dupl" -> Duple
                      "trpl" -> Triple
                      "both" -> Both
                      "odd"  -> Odd
                      y -> error ("FromJSON MeterKind: unknown json: " ++ y)
       in toMK <$> v .:  pack "mk"
     parseJSON _          = mzero
                                        
