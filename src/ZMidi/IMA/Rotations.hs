{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ZMidi.IMA.Rotations ( -- * Rotations
                             Rot (..)
                           , RPrior (..)
                           , Rotations
                           , getRot
                           , stdRotations
                           , threePerNum
                           , normPriors
                           , rotate
                           -- , normPriorsPerTS
                           , showRotations
                           ) where

import ZMidi.Score.Datatypes         -- ( TimeSig (..) , Beat(..) , BeatRat (..) )
import ZMidi.Score.Quantise           ( QBins (..), getNumForQBins )
import ZMidi.IMA.Internal             ( lookupErr )
import ZMidi.IMA.Constants            ( acceptedTimeSigs )
import Data.List                      ( intercalate )
import Data.Csv                       ( FromField (..) )
import Data.Ratio                     ( (%), Ratio) 
import qualified Data.Map.Strict as M ( map, foldr )
import Data.Map.Strict                ( Map, empty,  insert, foldrWithKey )
import Data.ByteString.Char8          ( readInt, ByteString )
import Data.Aeson                     ( ToJSON (..), FromJSON (..))
import Data.Binary                    ( Binary )
import qualified Data.ByteString.Char8 as BC ( drop )
import Control.Arrow                  ( second )
import Control.Applicative            ( pure )
import Control.DeepSeq                ( NFData )
import Text.Printf                    ( printf, PrintfArg)
import System.Random                  ( Random (..) )

--------------------------------------------------------------------------------
-- Rotations
--------------------------------------------------------------------------------

-- | Returns a list of four 'Rot'ations per time signature numerator:
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
                  deriving ( Eq, Show, Num, Ord, Enum, Real, Read, FromJSON, ToJSON, NFData, Binary )

-- | A prior for the Rotation
newtype RPrior = RPrior { rprior :: Double }
                  deriving ( Eq, Show, Num, Ord, Enum, Real, Floating, Read
                           , Fractional, RealFloat, RealFrac, FromJSON, ToJSON
                           , Random, PrintfArg, NFData)
                  
-- | for Parsing
instance FromField Rot where
  parseField r = let pInt :: ByteString -> (Int, ByteString)
                     pInt = maybe (error "FromField Rot: Invalid rotation") id . readInt 
                     (num, x) = pInt r
                 in case num of 
                      -- a zero means no rotation
                      0 -> pure $ Rot (0 % 1)
                      -- parse (<Int>)<somthing><Int>
                      _ -> pure $ Rot (num % (fst . pInt $ BC.drop 1 x))
      
                  
-- | Applies a metrical offset ('Rot') to a ('Beat', 'BeatRat'), basically
-- "rotating" the profile with this offset. We rotate in a forward direction.
-- Hence, say that an upbeat of one beat has been missed, we can correct this
-- with a rotation of 3 * 'QBins' in case of a 4/4 meter.
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

showRotations :: Rotations -> String
showRotations = foldrWithKey showRPs "" where

  showRPs :: TimeSig -> [(Rot, RPrior)] -> String -> String
  showRPs t r s = let (rs,ps) = unzip . reverse $ r
                  in intercalate "\n" [s, show t ++ ":", showRs rs, showPs ps]

  showRs :: [Rot] -> String
  showRs rs = intercalate "   " . map (show . rot) $ rs

  showPs :: [RPrior] -> String
  showPs ps = concatMap (\p -> printf "%.5f " p) ps