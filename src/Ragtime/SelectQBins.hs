{-# OPTIONS_GHC -Wall                   #-}
module Ragtime.SelectQBins ( selectQBins 
                           , filterByQBinStrength
                           , filterByQBinStrengthWith
                           , printMeterStats
                           ) where

import ZMidi.Score.Datatypes          ( TimeSig, Beat, BeatRat )
import Ragtime.NSWProf
import Data.List                      ( sort, sortBy )
import Data.Ord                       ( comparing, Down (..) )
import Data.Maybe                     ( fromJust )
import qualified Data.Map.Strict as M ( map, lookup )
import Data.Map.Strict                ( Map, toAscList, filterWithKey
                                      , mapWithKey )
                                      
selectQBins :: Int -> Map TimeSig NSWProf -> Map TimeSig [(Beat, BeatRat)]
selectQBins bs = M.map select where

  select :: NSWProf -> [(Beat, BeatRat)]
  select = sort . map fst . take bs          -- select and sort in regular order
         . sortBy (comparing (Down . snd)) . toAscList-- sort by weight
         . snd . nswprof                     -- ignore the nr of bars
         
filterByQBinStrengthWith :: Map TimeSig [(Beat, BeatRat)] -> Map TimeSig NSWProf
                         -> Map TimeSig NSWProf
filterByQBinStrengthWith s m = mapWithKey filter m where

  filter :: TimeSig -> NSWProf -> NSWProf
  filter ts (NSWProf (n, x)) = 
    let l = getBins ts
    in NSWProf (n, filterWithKey (\k _ -> k `elem` l) x)
  
  getBins :: TimeSig -> [(Beat, BeatRat)]
  getBins ts = fromJust $ M.lookup ts s 
  
filterByQBinStrength :: Map TimeSig NSWProf -> Map TimeSig NSWProf
filterByQBinStrength m = filterByQBinStrengthWith (selectQBins 12 m) m
  
-- N.B. copied from 
-- Prints the average normalised inner metric analysis profiles to the user
printMeterStats :: Map TimeSig NSWProf -> IO ()
printMeterStats = mapM_ (putStrLn . showNSWProf) . toAscList 

