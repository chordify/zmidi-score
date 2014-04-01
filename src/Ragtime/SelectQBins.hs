{-# OPTIONS_GHC -Wall                   #-}
module Ragtime.SelectQBins ( selectQBins 
                           , filterByQBinStrength
                           -- , filterByQBinStrengthWith
                           -- , filterBin
                           , filterToList
                           , printMeterStats
                           ) where

import ZMidi.Score.Datatypes          ( TimeSig, Beat, BeatRat )
import Ragtime.NSWProf
import Data.List                      ( sort, sortBy )
import Data.Ord                       ( comparing, Down (..) )
import Data.Maybe                     ( fromJust )
import qualified Data.Map.Strict as M ( map, lookup )
import Data.Map.Strict                ( Map, toAscList, filterWithKey
                                      , mapWithKey, findWithDefault )
import Control.Arrow                  ( second )
                             
type QBinSelection = Map TimeSig [(Beat, BeatRat)]
                             
selectQBins :: Int -> Map TimeSig NSWProf -> QBinSelection
selectQBins bs = M.map select where

  select :: NSWProf -> [(Beat, BeatRat)]
  select = sort . map fst . take bs          -- select and sort in regular order
         . sortBy (comparing (Down . snd)) . toAscList-- sort by weight
         . snd . nswprof                     -- ignore the nr of bars
         
filterByQBinStrengthWith :: QBinSelection -> Map TimeSig NSWProf -> Map TimeSig NSWProf
filterByQBinStrengthWith s m = mapWithKey (filterBin s) m where

filterBin :: QBinSelection -> TimeSig -> NSWProf -> NSWProf 
filterBin s ts = NSWProf . second (filterWithKey (\k _ -> k `elem` l)) . nswprof

  where l = fromJust $ M.lookup ts s 
  
filterByQBinStrength :: Map TimeSig NSWProf -> Map TimeSig NSWProf
filterByQBinStrength m = filterByQBinStrengthWith (selectQBins 12 m) m

filterToList :: QBinSelection -> TimeSig -> NSWProf -> [NSWeight]
filterToList s ts (NSWProf (_,p)) = map fnd . fromJust . M.lookup ts $ s
  
  where fnd :: (Beat, BeatRat) -> NSWeight
        fnd k = findWithDefault (NSWeight 0) k p
  
-- N.B. copied from 
-- Prints the average normalised inner metric analysis profiles to the user
printMeterStats :: Map TimeSig NSWProf -> IO ()
printMeterStats = mapM_ (putStrLn . showNSWProf) . toAscList 


