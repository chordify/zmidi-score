module ZMidi.Score.BarBeatPos ( getBeatInBar
                              , getBarRat
                              , toRatInBeat
                              , toBarRat 
                              ) where

import ZMidi.Score.Datatypes
import ZMidi.Score.Quantise

import Control.Arrow         ( (***) )
import Data.Ratio            ( (%) )

--------------------------------------------------------------------------------
-- Bar & Beat position
--------------------------------------------------------------------------------

-- Within a 'MidiScore' we can musically describe every (quantised)
-- position in time in 'Bar', Beat, and 'BarRat'. Therefore, we need the 
-- 'TimeSig'nature, the length of a beat ('TPB', in ticks), and the actual
-- 'Time' stamp.
-- TODO move these newtypes to the Quantise module?
getBeatInBar :: TimeSig -> TPB -> Time -> (Bar, Beat, BeatRat)
getBeatInBar NoTimeSig _ _ = error "getBeatInBar applied to noTimeSig"
getBeatInBar (TimeSig num _den _ _) t o = 
  let (Beat bt, rat) = getRatInBeat t o
      (br, bib)      = (succ *** succ) $ bt `divMod` num 
  in (Bar br, Beat bib, rat)
  
-- | Returns the position within a 'Bar', see 'getBeatInBar'.
getRatInBeat :: TPB -> Time -> (Beat, BeatRat)
getRatInBeat (TPB t) (Time o) = 
  ((Beat) *** (BeatRat . (% t))) (o `divMod` t)

-- | Similar to 'getBeatInBar' we can also describe the musical position as the
-- combination of a 'Bar' and a 'BarRat'. The latter denotes the ratio within 
-- a bar, e.g. BarRat (3 % 4) denotes the 4th 'Beat' in the bar.
getBarRat :: TimeSig -> TPB -> Time -> (Bar, BarRat)
getBarRat NoTimeSig _ _ = error "getBeatInBar applied to noTimeSig"
getBarRat (TimeSig num den _ _) (TPB t) (Time o) = 
  let (bt, rest) = o `divMod` t 
      (b , bib)  = bt `divMod` num 
      br         = ((bib * t) + rest) % (den * t) 
  in (Bar (succ b), BarRat br)

-- toRatInBeat allows us to convert a 'BarRat' into a ('Beat','BeatRat')
toRatInBeat :: TimeSig -> BarRat -> (Beat, BeatRat)
toRatInBeat ts br = countBeat (Beat 0, BeatRat . barRat $ br)
  
  where oneBeat = BeatRat (1 % tsNum ts) :: BeatRat
        
        countBeat :: (Beat, BeatRat) -> (Beat, BeatRat)
        countBeat (b,x) | rest < 0   = (b,x)
                         | otherwise = countBeat (succ b, rest)
                              where rest = x - oneBeat

-- | Musically it is sometimes more intuitive to have a 'BarRat', i.e. the
-- onset is defined as the ratio within the bar. For example 1%4 denotes them
-- the position of the second quarter note within a 4/4 meter
toBarRat :: QBins -> TimeSig -> (Beat, BeatRat) -> BarRat
toBarRat _ NoTimeSig _ = error "toBarRat applied to noTimeSig"
toBarRat q@(QBins x) (TimeSig _n d _ _) (Beat b, BeatRat r) = 
           BarRat (((pred b * x) + getNumForQBins q r) % (x * d))
