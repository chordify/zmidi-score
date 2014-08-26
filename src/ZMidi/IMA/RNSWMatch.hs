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
                          , pickMaxRotation
                          , printGtPMatch
                          ) where
                       
import ZMidi.Score 
import ZMidi.IMA.SelectProfBins ( QBinSelection )
import ZMidi.IMA.Rotations      ( Rot (..),  Rotations, getRot, RPrior (..) )
import ZMidi.IMA.GTInfo         -- ( GTMR (..) )
import ZMidi.IMA.NSWProf        ( NSWProf, NSWPStore (..), getProf )
import ZMidi.IMA.RNSWProf       ( toDoubles, toRNSWProfWithTS )
import ReadPDF                  ( IMAPDF(..) )

import Data.List                ( intercalate, maximumBy )
import Data.Function            ( on )
import Data.Foldable            ( foldrM )
import Data.Map.Strict          ( Map, insertWith, adjust )

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
  
evalMeter :: [(GTMR, PMatch)] ->  [Result Bool]
evalMeter = map eval where

  eval :: (GTMR, PMatch)-> Result Bool
  eval (GTMR ts r, m) = Result (pmTimeSig m == ts) (pmRot m == r)

  
-- | Picks the best matching profile
pickMeters :: [(GTMR, [PMatch])] -> [(GTMR, PMatch)]
pickMeters = map (second (maximumBy (compare `on` pmatch)))

-- | Copies the TimeSig for every Match for printing
dontPickMeters :: [(GTMR, [PMatch])] -> [(GTMR, PMatch)]
dontPickMeters = concatMap f 
  where f (t, ms) = map (\x -> (t, x)) ms

-- | Given a list of matched results, we look rotation that yielded the best
-- result using the ground truth time signature.
pickMaxRotation :: [(GTMR, [PMatch])] -> Map TimeSig (Map Rot RPrior) 
                -> Either String (Map TimeSig (Map Rot RPrior))
pickMaxRotation d m = mapM (fmap pick . filtr) d >>= foldrM toRot m 

  where -- picks the maximum rotation (like pickMeters)
        pick :: (TimeSig, [PMatch]) -> (TimeSig, Rot)
        pick (t, ms) = (t, pmRot $ maximumBy (compare `on` pmatch) ms)

        -- filter all rotations that do not match the gt        
        filtr :: (GTMR, [PMatch]) -> Either String (TimeSig,[PMatch])
        filtr (GTMR t _, ms) = case filter (\p -> pmTimeSig p == t) $ ms of
          [] -> Left  ("TimeSignature not supported " ++ show t)
          x  -> Right (t,x)

        -- Store the rotation in a Map
        toRot :: (TimeSig, Rot) -> Map TimeSig (Map Rot RPrior) 
              -> Either String (Map TimeSig (Map Rot RPrior))
        toRot (ts,r) x = return $ adjust (insertWith (+) r (RPrior 1)) ts x 

  
printGtPMatch :: (GTMR, PMatch) -> String
printGtPMatch (GTMR ts r, m) = 
  let est = pmTimeSig m
      
      shwTs :: TimeSig -> String
      shwTs x = '\'' : show x ++ "\'"
  
  in intercalate "\t" [ shwTs ts
                       , shwTs est
                       , show (ts == est)
                       , printf "%.2f" (pmatch m)
                       , (show . rot $ r )
                       , (show . rot . pmRot $ m) 
                       , show (r == pmRot m)
                       , printf "rp:%.2f" (rprior . pmRPrior $ m)
                       , pmFile m
                       ]


-- | Matches meter profiles
matchNSWPStore :: Rotations -> QBinSelection -> [IMAPDF] -> NSWPStore -> [(GTMR, [PMatch])]
matchNSWPStore rs s pdfs (NSWPStore q i fp) = map matchSeg i
  
  where 
        matchSeg :: (GTMR, Map TimeSig NSWProf) -> (GTMR, [PMatch])
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
              -- p  = NSWDist $ log (pdf ip d) + log (rprior rp)
          in PMatch ts p r rp q fp



  
