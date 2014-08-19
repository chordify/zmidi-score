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

import ZMidi.Score.Datatypes          ( TimeSig (..) , Beat(..) , BeatRat (..) )
import ZMidi.Score.Quantise           ( QBins (..) )
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
import Data.ByteString.Char8          ( readInt )
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
-- Rotations
--------------------------------------------------------------------------------
-- TODO: move to separate module... 

-- | Returs a list of four 'Rot'ations per time signature numerator:
stdRotations :: QBins -> (QBins -> TimeSig -> [(Rot, RPrior)]) -> Rotations
stdRotations q f = normPriors $ foldr g empty acceptedTimeSigs
  where g ts m = insert ts (f q ts) m

-- Considering twelve bins we rotate at bins 0, 3, 6, etc.
threePerNum :: QBins -> TimeSig -> [(Rot, RPrior)]
threePerNum (QBins q) ts = reverse $ map f [0, 3 .. ((tsNum ts * q) - 3)]
  where f x = (Rot (x % q), RPrior 1.0 )
 
-- | normalises the 'Rotations' such that /all/ priors to sum to 1.0
normPriors :: Rotations -> Rotations
normPriors r = let s = sumPriors r in M.map (map (second (/ s))) r where
               
  sumPriors :: Rotations -> RPrior
  sumPriors = M.foldr perTS 0 where
    
    perTS :: [(Rot, RPrior)] -> RPrior -> RPrior
    perTS l s = s + foldr (\x y -> y + snd x) 0 l

-- | normalises the 'Rotations' such that the priors to sum to 1.0 
-- /per time signature/
{- 
normPriorsPerTS :: Rotations -> Rotations
normPriorsPerTS = M.map norm where

  norm :: [(Rot, RPrior)] -> [(Rot, RPrior)]
  norm x = let s = sum . snd . unzip $ x 
           in map (second (/ s) ) x
-}
           
getRot :: Rotations -> TimeSig -> [(Rot,RPrior)]
getRot r t = lookupErr ("QBinSelection.getRot: TimeSig not found "++ show t) r t

                   
type Rotations = Map TimeSig [(Rot, RPrior)]
  
-- | The Rotation
newtype Rot = Rot { rot :: Ratio Int } 
                  deriving ( Eq, Show, Num, Ord, Enum, Real, Read, FromJSON, ToJSON, NFData )

-- | A prior for the Rotation
newtype RPrior = RPrior { rprior :: Double }
                  deriving ( Eq, Show, Num, Ord, Enum, Real, Floating, Read
                           , Fractional, RealFloat, RealFrac, FromJSON, ToJSON
                           , Random, PrintfArg, NFData)
                  
-- | for Parsing
instance FromField Rot where
  parseField r = case readInt r of 
                  (Just (r',_)) -> pure $ Rot ( r' % 12 ) 
                  _             -> error "FromField Rot: Invalid rotation"

-- | Applies a metrical offset ('Rot') to a ('Beat', 'BeatRat'), basically
-- "rotating" the profile with this offset. We rotate in a forward direction.
-- Hence, say that an upbeat of one beat has been missed, we can correct this
-- with a rotation of 4 * 'QBins' in case of a 4/4 meter.
rotate :: QBins -> TimeSig -> Rot -> (Beat, BeatRat) -> (Beat, BeatRat)
rotate q@(QBins k) (TimeSig n _ _ _) (Rot r) (Beat b, BeatRat x) =
  let -- get a common divider
      nx      = getNumForQBins q x
      nr      = getNumForQBins q r
      -- we rotate forward: the rotation is added to the current beat position
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

showRotations :: Rotations -> String
showRotations = foldrWithKey showRPs "" where

  showRPs :: TimeSig -> [(Rot, RPrior)] -> String -> String
  showRPs t r s = let (rs,ps) = unzip . reverse $ r
                  in intercalate "\n" [s, show t ++ ":", showRs rs, showPs ps]

  showRs :: [Rot] -> String
  showRs rs = intercalate "   " . map (show . rot) $ rs

  showPs :: [RPrior] -> String
  showPs ps = concatMap (\p -> printf "%.5f " p) ps

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
