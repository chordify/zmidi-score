{-# OPTIONS_GHC -Wall                    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
-- | This module deals with selecting the SWProf bins used to estimate the meter
module ZMidi.IMA.SelectProfBins ( getSel
                                , filterBin
                                , filterToList
                                , QBinSelection
                                -- * Select QBins on avg weight
                                , selectQBins
                                , sumNSWProf                                
                                ) 
                                where

import ZMidi.Score.Datatypes         -- ( TimeSig (..) , Beat(..) , BeatRat (..) )
import ZMidi.Score.Quantise           ( QBins (..) )
import ZMidi.IMA.Internal             ( lookupErr )
import ZMidi.IMA.NSWProf
import ZMidi.IMA.Rotations            ( Rot (..), rotate )
import ZMidi.IMA.GTInfo               ( GTMR (..) )
import Data.List                      ( sort, sortBy )
import Data.Foldable                  ( foldrM )
import Data.Ord                       ( comparing, Down (..) )
import Data.Maybe                     ( fromJust )
import qualified Data.Map.Strict as M ( map, lookup )
import Data.Map.Strict                ( Map, toAscList, filterWithKey
                                      , findWithDefault, unionWith, insertWith )
import Control.Arrow                  ( second )

--------------------------------------------------------------------------------
-- QBinSelection stuff
--------------------------------------------------------------------------------

                   
-- | A selection of the SWProf bins with the strongest weights                     
type QBinSelection = Map TimeSig [(Beat, BeatRat)]
        
getSel :: QBinSelection -> TimeSig -> [(Beat, BeatRat)]
getSel s t = lookupErr ("QBinSelection.getSel: TimeSig not found "++ show t) s t

-- removes all bins from an 'NSWProf' that are not specified in a 'QBinSelection'
filterBin :: QBins -> Rot -> QBinSelection -> TimeSig -> NSWProf -> NSWProf 
filterBin q r s ts = NSWProf . second (filterWithKey f) . nswprof

  where l = map (rotate q ts r) . getSel s $ ts 
        
        f :: (Beat, BeatRat) -> a -> Bool
        f k _ = k `elem` l

-- Given a selection, time signature selects the selected bins from a 'NSWProf'
-- and returns them in a list. If the selected bin is not present in the 
-- profile 0 is returned
filterToList ::QBins -> Rot -> QBinSelection -> TimeSig -> NSWProf -> [NSWeight]
filterToList q r s ts (NSWProf (_,p)) = map fnd . fromJust . M.lookup ts $ s
  
  where fnd :: (Beat, BeatRat) -> NSWeight
        -- N.B. NSWeight is a log of the SWeight, we apply laplacian 
        -- smoothing with alpha is 1, log 1 = 0. See NSWProf.normSWProfByBar
        -- TODO unify the alpha parameter!
        fnd k = findWithDefault (NSWeight 0) (rotate q ts r k) p

--------------------------------------------------------------------------------
-- Create a QBinSelection based on the most heaviest bins
--------------------------------------------------------------------------------
        
-- | Collects all profiles sorted by time signature in one map
sumNSWProf :: Map TimeSig NSWProf -> NSWPStore -> Either String (Map TimeSig NSWProf)
sumNSWProf m n = foldrM doProf m $ nswps n where

  doProf :: (GTMR, Map TimeSig NSWProf) -> Map TimeSig NSWProf 
         -> Either String (Map TimeSig NSWProf)
  doProf (GTMR ts _, ps) x = case M.lookup ts ps of 
    Just p -> Right $ insertWith mergeNSWProf ts p x
    _ -> Left ("sumNSWProf: TimeSignature not found in NSWPStore: " ++ show ts)
                        
  -- | merges two 'NSWProf's by summing its values
  mergeNSWProf :: NSWProf -> NSWProf -> NSWProf
  mergeNSWProf (NSWProf (a, ma)) (NSWProf (b, mb)) 
    = (NSWProf (a + b, unionWith (+) ma mb))
        
-- | creates a QBinSelection on a map of averaged NSWProfiles
selectQBins :: Int -> Map TimeSig NSWProf -> QBinSelection
selectQBins bs = M.map select where

  select :: NSWProf -> [(Beat, BeatRat)]
  select = sort . map fst                   -- and sort in regular order
         . take bs                          -- select 
         . sortBy (comparing (Down . snd))  -- sort by weight
         . toAscList . snd . nswprof        -- ignore the nr of bars

                      
