{-# OPTIONS_GHC -Wall                    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
-- | This module deals with selecting the SWProf bins used to estimate the meter
module ZMidi.IMA.SelectProfBins ( selectQBins
                                , getSel
                                , filterBin
                                , filterToList
                                , printMeterStats
                                , QBinSelection
                                -- * Rotations
                                , Rot (..)
                                , RPrior (..)
                                , Rotations
                                , getRot
                                , getNumForQBins
                                , stdRotations
                                -- * JSON import and export
                                , writeJSON
                                , readJSON
                                ) where

import ZMidi.Score.Datatypes          ( TimeSig (..) , Beat(..) , BeatRat (..) )
import ZMidi.Score.Quantise           ( QBins (..) )
import ZMidi.IMA.Analyse              ( IMAStore ) 
import ZMidi.IMA.NSWProf
import Data.List                      ( sort, sortBy )
import Data.Ord                       ( comparing, Down (..) )
import Data.Maybe                     ( fromJust )
import Data.Csv                       ( FromField (..) )
import Data.Ratio                     ( numerator, denominator, (%), Ratio) 
import qualified Data.Map.Strict as M ( map, lookup )
import Data.Map.Strict                ( Map, toAscList, filterWithKey, empty
                                      , findWithDefault, insert, toList, fromList )
import Data.ByteString.Char8          ( readInt )
import Control.Monad                  ( mzero )
import Control.Arrow                  ( second )
import Control.Applicative            ( pure, (<$>), (<*>) )
import Data.Aeson                     ( ToJSON (..), FromJSON (..), decode
                                      , encode, (.=), (.:), Value (..), object)
import Data.Text                      ( pack )
import qualified Data.ByteString.Lazy as BL ( readFile, writeFile )
import GA                             (Entity(..))


-- | A selection of the SWProf bins with the strongest weights                     
type QBinSelection = Map TimeSig [(Beat, BeatRat)]
        
getSel :: QBinSelection -> TimeSig -> [(Beat, BeatRat)]
getSel rs ts = case M.lookup ts rs of 
  Just r  -> r
  Nothing -> error ("QBinSelection.getSel: TimeSig not found " ++ show ts)
        
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
-- | Returs a list of four 'Rot'ations per time signature numerator:

stdRotations :: QBins -> Rotations
stdRotations q = foldr f empty [ TimeSig 2 2 0 0, TimeSig 2 4 0 0
                               , TimeSig 4 4 0 0, TimeSig 3 4 0 0
                               , TimeSig 6 8 0 0 ]
  where f ts m = insert ts (threePerNum q ts) m

threePerNum :: QBins -> TimeSig -> [(Rot, RPrior)]
threePerNum (QBins q) ts = reverse $ map f [0, 3 .. ((tsNum ts * q) - 3)]
  where len = (2 + 2 + 4 + 3 + 6) * 4
        f x = (Rot (x % q), RPrior (1.0 / len))

getRot :: Rotations -> TimeSig -> [(Rot,RPrior)]
getRot rs ts = case M.lookup ts rs of 
  Just r  -> r
  Nothing -> error ("QBinSelection.getRot: TimeSig not found " ++ show ts)

type Rotations = Map TimeSig [(Rot, RPrior)]
  
-- | The Rotation
newtype Rot = Rot { rot :: Ratio Int } 
                  deriving ( Eq, Show, Num, Ord, Enum, Real, FromJSON, ToJSON )

-- | A prior for the Rotation
newtype RPrior = RPrior { rprior :: Double }
                  deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                           , Fractional, RealFloat, RealFrac, FromJSON, ToJSON )
                  
instance FromField Rot where
  parseField r = case readInt r of 
                  (Just (r',_)) -> pure $ Rot ( r' % 12 ) 
                  _             -> error "FromInt Rot: Invalid rotation"

rotate :: QBins -> TimeSig -> Rot -> (Beat, BeatRat) -> (Beat, BeatRat)
rotate q@(QBins k) (TimeSig n _ _ _) (Rot r) (Beat b, BeatRat x) =
  let nx      = getNumForQBins q x
      nr      = getNumForQBins q r
      (a, r') = (nr + nx) `divMod` k
      b'      = succ $ (pred b + a) `mod` n
  in  ( Beat b' , BeatRat ( r' % k) )
rotate _ _ _ _ = error "SelectQBins.rotate: invalid arguments"

-- | Returns the numerator of a Ratio given a certain 'QBins' as denominator.
-- The standard Ratio implementation simplifies the Ration, e.g. 3 % 12 
-- is converted into 1 % 4. This function reverses that process: 
-- 
-- >>> getNumForQBins 12 (1 % 4) 
-- >>> 3
-- 
-- >>> getNumForQBins 12 (1 % 1) 
-- >>> 12
getNumForQBins :: QBins -> Ratio Int -> Int
getNumForQBins (QBins q) r = numerator r * (q `div` denominator r)

--------------------------------------------------------------------------------
-- GA instances
--------------------------------------------------------------------------------

instance Entity (Map TimeSig [(Rot, RPrior)]) Double [IMAStore] [RPrior] IO where
  genRandom pool seed = undefined

  crossover pool par seed a b = undefined

  mutation pool par seed e = undefined

  score' dat e = undefined

  -- showGeneration ix 
  


--------------------------------------------------------------------------------
-- JSON import and export
--------------------------------------------------------------------------------

instance (Integral a, ToJSON a) => ToJSON (Ratio a) where
     toJSON r = object [pack "num" .= numerator r, pack "den" .= denominator r]  

instance ToJSON (TimeSig) where
     toJSON (TimeSig n d _ _) = object [pack "ts_num" .= n, pack "ts_den" .= d]
     toJSON NoTimeSig         = object [pack "ts" .= pack "none"]     

instance (Integral a, FromJSON a) => FromJSON (Ratio a) where
     parseJSON (Object v) = (%) <$> v .: (pack "num") <*> v .: (pack "den")
     parseJSON _          = mzero
     
instance FromJSON (TimeSig) where
     parseJSON (Object v) =  (\d n -> TimeSig d n 0 0) 
                          <$> v .: (pack "ts_num") <*> v .: (pack "ts_den")  
     parseJSON _          = mzero
     
writeJSON :: ToJSON a => FilePath -> Map TimeSig a -> IO ()
writeJSON fp = BL.writeFile fp . encode . toList 

readJSON :: FromJSON a => FilePath -> IO (Map TimeSig a)
readJSON fp = do mr <- BL.readFile fp >>= return . decode
                 case mr of 
                   Just r  -> return . fromList $ r
                   Nothing -> error "readRotations: cannot parse rotations JSON"                        
