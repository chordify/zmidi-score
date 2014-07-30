{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
module ZMidi.IMA.RNSWMatch( PMatch (..)
                          , NSWDist (..)
                          , Prob (..)
                          , Result (..)
                          , meterFail
                          , matchNSWPStore
                          , evalMeter
                          , avgResult
                          , pickMeters
                          , dontPickMeters
                          , printPickMeter
                          ) where
                       
import ZMidi.Score 
import ZMidi.IMA.SelectProfBins ( Rot (..), getNumForQBins, QBinSelection
                                , Rotations, getRot, RPrior (..))
import ZMidi.IMA.NSWProf        ( NSWProf, NSWPStore (..), getProf )
import ZMidi.IMA.RNSWProf       ( toDoubles, toRNSWProfWithTS )
import ReadPDF                  ( IMAPDF(..) )

import Data.List                ( intercalate, maximumBy )
import Data.Function            ( on )
import Data.Map.Strict          ( Map )

import Text.Printf              ( printf, PrintfArg )

import GHC.Generics             ( Generic )
import Control.Arrow            ( second )
import Control.DeepSeq          ( NFData (..) )
import Control.DeepSeq.Generics ( genericRnf )

-- | Normalised spectral weights distance, obtained by matching two 'SWProf's
newtype NSWDist = NSWDist { nswdist :: Double }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                             , Fractional, RealFloat, RealFrac, PrintfArg
                             , NFData )

newtype Prob    = Prob { prob :: Double }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                             , Fractional, RealFloat, RealFrac, PrintfArg 
                             , NFData )
                             
data PMatch = PMatch {  pmTimeSig :: TimeSig
                     ,  pmatch    :: NSWDist
                     ,  pmRot     :: Rot
                     ,  pmRPrior  :: RPrior
                     ,  pmQBins   :: QBins
                     ,  pmFile    :: FilePath
                     } deriving (Eq, Generic)

instance NFData PMatch where rnf = genericRnf                     
    
instance Show PMatch where
  show (PMatch ts m r rp q fp) = 
    printf (fp ++ '\t' : show ts ++ ": %1.4f\tR: %d2 \tRp: %f1.4")  
            m (getNumForQBins q . rot $ r) (rprior rp)
    
  showList l s = s ++ (intercalate "\n" . map show $ l)
  
data Result a = Result { meterOk :: a
                       , rotOk   :: a
                       } deriving (Eq, Functor)

instance Show a => Show (Result a) where
  show (Result a b) =    "meter OK:    " ++ show a 
                    ++ "\nrotation OK: " ++ show b

-- | Returns 1 - the results. (It is assumed, but not checked that resultOk
-- returns the percentage of the cases in which the meter is correct)
meterFail :: Result Double -> Double
meterFail r = 1.0 - meterOk r

-- | aggregates a list of results
avgResult :: [Result Bool] -> Result Double
avgResult l = ap (/) (foldr step (Result 0 0) l) (Result len len)

  where len = fromIntegral . length $ l :: Double

        step :: Result Bool -> Result Double -> Result Double
        step a b = ap (+) (fmap toInt a) b

        toInt :: Bool -> Double
        toInt True = 1.0
        toInt _    = 0.0

ap :: (a -> b -> c) -> Result a -> Result b -> Result c
ap f (Result a b) (Result c d) = Result (f a c) (f b d)
  
evalMeter :: [(TimeSig, PMatch)] ->  [Result Bool]
evalMeter = map eval where

  eval :: (TimeSig, PMatch)-> Result Bool
  eval (ts, m) = Result (pmTimeSig m == ts) 
                        (getNumForQBins (pmQBins m) (rot $ pmRot m)== 0)

  
-- | Picks the best matching profile
pickMeters :: [(TimeSig, [PMatch])] -> [(TimeSig, PMatch)]
pickMeters = map (fmap (maximumBy (compare `on` pmatch)))

-- | Copies the TimeSig for every Match for printing
dontPickMeters :: [(TimeSig, [PMatch])] -> [(TimeSig, PMatch)]
dontPickMeters = concatMap f 
  where f (t, ms) = map (\x -> (t, x)) ms

printPickMeter :: (TimeSig, PMatch) -> String
printPickMeter (ts, m) = 
  let est = pmTimeSig m
      s = intercalate "\t" [ shwTs ts
                           , shwTs est
                           , show (ts == est)
                           , "%.2f" 
                           , "r:%2d"
                           , "rp:%.2f"
                           , pmFile m
                           ]
      
      shwTs :: TimeSig -> String
      shwTs x = '\'' : show x ++ "\'"
      
      -- tsEq :: TimeSig -> TimeSig -> Bool
      -- tsEq (TimeSig 4 4 _ _) (TimeSig 2 2 _ _) = True
      -- tsEq a                 b                 = a == b
  
  in printf s (pmatch m) 
              (getNumForQBins (pmQBins m) . rot . pmRot $ m) 
              (rprior . pmRPrior $ m)
  
  
-- | Matches the a segment against it's annotated 'TimeSig'nature
-- match :: Rotations -> QBinSelection -> [IMAPDF] -> IMAStore -> [(TimeSig, [PMatch])]
-- match rs s pdf = map (matchNSWPStore rs s pdf) . toNSWPStore

-- | Matches meter profiles
matchNSWPStore :: Rotations -> QBinSelection -> [IMAPDF] -> NSWPStore -> [(TimeSig, [PMatch])]
matchNSWPStore rs s pdfs (NSWPStore q d fp) = map matchSeg d
  
  where 
        matchSeg :: (TimeSig, Map TimeSig NSWProf) -> (TimeSig, [PMatch])
        matchSeg (gt, x) = (gt, concatMap (matchPDF x) pdfs)
  
        -- matches a single pdf, creating the profile is independent of the 
        -- rotation and happens here
        matchPDF :: Map TimeSig NSWProf -> IMAPDF -> [PMatch]
        matchPDF ps ip = 
          let ts = pdfTimeSig ip 
              pf = getProf ps ts
          in [ getRotProb pf r ts ip | r <- getRot rs ts ]
          
        -- Calculates the match for all rotations  
        getRotProb :: NSWProf -> (Rot, RPrior) -> TimeSig -> IMAPDF -> PMatch
        getRotProb pf (r,rp) ts ip = 
          let d  = toDoubles $ toRNSWProfWithTS q r ts s pf
              p  = NSWDist $ log (pdfPrior ip) + log (pdf ip d) + log (rprior rp)
          in PMatch ts p r rp q fp

         
  
