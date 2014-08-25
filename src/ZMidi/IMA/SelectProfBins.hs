{-# OPTIONS_GHC -Wall                    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
-- | This module deals with selecting the SWProf bins used to estimate the meter
module ZMidi.IMA.SelectProfBins ( getSel
                                , filterBin
                                , filterToList
                                , printMeterStats
                                , QBinSelection
                                -- * Select QBins on avg weight
                                , selectQBins
                                , sumNSWProf                                
                                -- * Rotations
                                , Rot (..)
                                , RPrior (..)
                                , Rotations
                                , getRot
                                , getNumForQBins
                                , stdRotations
                                , threePerNum
                                , normPriors
                                -- , normPriorsPerTS
                                , showRotations
                                -- * JSON import and export
                                , writeJSON
                                , readJSON
                                ) 
                                where

import ZMidi.Score.Datatypes         -- ( TimeSig (..) , Beat(..) , BeatRat (..) )
import ZMidi.Score.Quantise           ( QBins (..), getNumForQBins )
import ZMidi.IMA.Internal             ( lookupErr )
import ZMidi.IMA.NSWProf
import ZMidi.IMA.Constants            ( acceptedTimeSigs )
import Data.List                      ( sort, sortBy, intercalate )
import Data.Foldable                  ( foldrM )
import Data.Ord                       ( comparing, Down (..) )
import Data.Maybe                     ( fromJust )
import Data.Csv                       ( FromField (..) )
import Data.Ratio                     ( numerator, denominator, (%), Ratio) 
import qualified Data.Map.Strict as M ( map, lookup, foldr )
import Data.Map.Strict                ( Map, toAscList, filterWithKey, empty
                                      , findWithDefault, insert, toList, unionWith
                                      , fromList, foldrWithKey, insertWith)
import Data.ByteString.Char8          ( readInt, ByteString )
import qualified Data.ByteString.Char8 as BC ( drop )
import Control.Monad                  ( mzero )
import Control.Arrow                  ( second )
import Control.Applicative            ( pure, (<$>), (<*>) )
import Data.Aeson                     ( ToJSON (..), FromJSON (..), decode
                                      , encode, (.=), (.:), Value (..), object)
import Data.Text                      ( pack )
import Text.Printf                    ( printf, PrintfArg)
import qualified Data.ByteString.Lazy as BL ( readFile, writeFile )
import System.Random                  ( Random (..) )
import Control.DeepSeq                ( NFData )

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

  where l = map (rotate q ts r) . fromJust . M.lookup ts $ s 
        
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
  
-- N.B. copied from RagPatIMA
-- Prints the average normalised inner metric analysis profiles to the user
printMeterStats :: Map TimeSig NSWProf -> IO ()
printMeterStats = mapM_ (putStrLn . showNSWProf) . toAscList 

--------------------------------------------------------------------------------
-- Create a QBinSelection based on the most heaviest bins
--------------------------------------------------------------------------------
        
-- | Collects all profiles sorted by time signature in one map
sumNSWProf :: Map TimeSig NSWProf -> NSWPStore -> Either String (Map TimeSig NSWProf)
sumNSWProf m n = foldrM doProf m $ nswps n where

  doProf :: (TimeSig, Map TimeSig NSWProf) -> Map TimeSig NSWProf 
         -> Either String (Map TimeSig NSWProf)
  doProf (ts, ps) x = case M.lookup ts ps of 
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


--------------------------------------------------------------------------------
-- JSON import and export
--------------------------------------------------------------------------------
instance ToJSON Beat
instance ToJSON BeatRat

instance (Integral a, ToJSON a) => ToJSON (Ratio a) where
     toJSON r = object [pack "num" .= numerator r, pack "den" .= denominator r]  

instance ToJSON (TimeSig) where
     toJSON (TimeSig n d _ _) = object [pack "ts_num" .= n, pack "ts_den" .= d]
     toJSON NoTimeSig         = object [pack "ts" .= pack "none"]     

instance FromJSON Beat
instance FromJSON BeatRat
     
instance (Integral a, FromJSON a) => FromJSON (Ratio a) where
     parseJSON (Object v) = (%) <$> v .: (pack "num") <*> v .: (pack "den")
     parseJSON _          = mzero
     
instance FromJSON (TimeSig) where
     parseJSON (Object v) =  (\n d -> TimeSig n d 0 0) 
                          <$> v .: (pack "ts_num") <*> v .: (pack "ts_den") 
                          
     parseJSON _          = mzero
     
writeJSON :: ToJSON a => FilePath -> Map TimeSig a -> IO ()
writeJSON fp = BL.writeFile fp . encode . toList 

readJSON :: FromJSON a => FilePath -> IO (Map TimeSig a)
readJSON fp = do mr <- BL.readFile fp >>= return . decode
                 case mr of 
                   Just r  -> do putStrLn ("read TimeSig Map: " ++ fp)
                                 return . fromList $ r 
                   Nothing -> error "readRotations: cannot parse rotations JSON"                        
