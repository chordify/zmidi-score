{-# OPTIONS_GHC -Wall                    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
module Ragtime.SelectQBins ( selectQBins 
                           -- , filterByQBinStrength
                           -- , filterByQBinStrengthWith
                           , filterBin
                           , filterToList
                           , printMeterStats
                           , QBinSelection
                           -- * Rotations
                           , Rot (..)
                           ) where

-- import ZMidi.Score.Datatypes  hiding  ( numerator, denominator )
import ZMidi.Score.Datatypes          ( TimeSig (..) , Beat(..) , BeatRat (..) )
import ZMidi.Score.Quantise           ( QBins (..) )
import Ragtime.NSWProf
import Data.List                      ( sort, sortBy )
import Data.Ord                       ( comparing, Down (..) )
import Data.Maybe                     ( fromJust )
import Data.Ratio                as R ( numerator, denominator, (%) )
import qualified Data.Map.Strict as M ( map, lookup )
import Data.Map.Strict                ( Map, toAscList, filterWithKey
                                      , findWithDefault )
import Control.Arrow                  ( second )

-- | A selection of the SWProf bins with the strongest weights                     
type QBinSelection = Map TimeSig [(Beat, BeatRat)]
                             
-- | creates a QBinSelection on a map of averaged NSWProfiles
selectQBins :: Int -> Map TimeSig NSWProf -> QBinSelection
selectQBins bs = M.map select where

  select :: NSWProf -> [(Beat, BeatRat)]
  select = sort . map fst                   -- and sort in regular order
         . take bs                          -- select 
         . sortBy (comparing (Down . snd))  -- sort by weight
         . toAscList . snd . nswprof        -- ignore the nr of bars

-- removes all bins from an 'NSWProf' that are not specified in a 'QBinSelection'
filterBin :: QBins -> Rot -> QBinSelection -> TimeSig -> NSWProf -> NSWProf 
filterBin q r s ts = NSWProf . second (filterWithKey f) . nswprof

  where l = map (rotate q ts r) . fromJust . M.lookup ts $ s 
        
        f :: (Beat, BeatRat) -> a -> Bool
        f k _ = k `elem` l
  
-- Special case of 'filterByQBinStrengthWith' using the 12 most prominent bins
-- filterByQBinStrength :: Map TimeSig NSWProf -> Map TimeSig NSWProf
-- filterByQBinStrength m = filterByQBinStrengthWith (selectQBins 12 m) m
         
-- Selects the bins marked in a 'QBinSelection' for a map of 'NSWProf's
-- filterByQBinStrengthWith ::  QBins -> Rot -> QBinSelection -> Map TimeSig NSWProf -> Map TimeSig NSWProf
-- filterByQBinStrengthWith q r s m = mapWithKey (filterBin q r s) m where

-- Given a selection, time signature selects the selected bins from a 'NSWProf'
-- and returns them in a list. If the selected bin is not present in the 
-- profile 0 is returned
filterToList ::QBins -> Rot -> QBinSelection -> TimeSig -> NSWProf -> [NSWeight]
filterToList q r s ts (NSWProf (_,p)) = -- reverse . sort -- sort by Weight
                                        map fnd . fromJust . M.lookup ts $ s
  
  where fnd :: (Beat, BeatRat) -> NSWeight
        -- N.B. NSWeight is a log of the SWeight, we apply laplacian 
        -- smoothing with alpha is 1, log 1 = 0. See NSWProf.normSWProfByBar
        -- TODO unify the alpha parameter!
        fnd k = findWithDefault (NSWeight 0) (rotate q ts r k) p
  
-- N.B. copied from RagPatIMA
-- Prints the average normalised inner metric analysis profiles to the user
printMeterStats :: Map TimeSig NSWProf -> IO ()
printMeterStats = mapM_ (putStrLn . showNSWProf) . toAscList 

--------------------------------------------------------------------------------
-- Rotations
--------------------------------------------------------------------------------
newtype Rot = Rot { rot :: Int } 
                  deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )

rotate :: QBins -> TimeSig -> Rot -> (Beat, BeatRat) -> (Beat, BeatRat)
rotate (QBins q) (TimeSig n _ _ _) (Rot rot) (Beat b, BeatRat r) =
  let x         = R.numerator r * (q `div` (R.denominator r))
      (a, rot') = (rot + x) `divMod` q
      b'        = succ $ (pred b + a) `mod` n
  in  ( Beat b' , BeatRat ( rot' R.%  q ))
rotate _ _ _ _ = error "SelectQBins.rotate: invalid arguments"

