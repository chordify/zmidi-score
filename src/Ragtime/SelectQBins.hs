{-# OPTIONS_GHC -Wall                   #-}
module Ragtime.SelectQBins ( selectQBins ) where

import ZMidi.Score.Datatypes          ( TimeSig, Beat, BeatRat )
import Ragtime.NSWProf
import Data.List                      ( sort, sortBy )
import Data.Ord                       ( comparing )
import qualified Data.Map.Strict as M ( map )
import Data.Map.Strict                ( Map, toAscList )
                                      
selectQBins :: Int -> Map TimeSig NSWProf -> Map TimeSig [(Beat, BeatRat)]
selectQBins bs = M.map select where

  select :: NSWProf -> [(Beat, BeatRat)]
  select = sort . map fst . take bs          -- select and sort in regular order
         . sortBy (comparing snd) . toAscList-- sort by weight
         . snd . nswprof                     -- ignore the nr of bars
         
filterByQBinStrength :: Map TimeSig NSWProf -> Map TimeSig NSWProf
filterByQBinStrength m = undefined