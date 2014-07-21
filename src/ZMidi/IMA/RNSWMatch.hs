{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor            #-}
module ZMidi.IMA.RNSWMatch( PMatch (..)
                          , NSWDist (..)
                          , Prob (..)
                          , match
                          , evalMeter
                          , avgResult
                          , pickMeters
                          , dontPickMeters
                          , printPickMeter
                          ) where
                       
import ZMidi.Score 
import ZMidi.IMA.TimeSigSeg       ( TimedSeg (..) )
import ZMidi.IMA.SelectProfBins   ( Rot (..), getNumForQBins, QBinSelection
                                  , Rotations, getRot, RPrior (..))
import ZMidi.IMA.Analyse          ( SWMeterSeg, IMAStore (..), toSWProfWithTS )

import ZMidi.IMA.NSWProf          ( NSWProf, normSWProfByBar )
import ZMidi.IMA.RNSWProf         ( toDoubles, toRNSWProfWithTS )
import ReadPDF                    ( IMAPDF(..) )

import Data.List                  ( intercalate, maximumBy )
import Data.Function              ( on )

import Text.Printf                ( printf, PrintfArg )

-- | Normalised spectral weights distance, obtained by matching two 'SWProf's
newtype NSWDist = NSWDist { nswdist :: Double }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                             , Fractional, RealFloat, RealFrac, PrintfArg )

newtype Prob    = Prob { prob :: Double }
                    deriving ( Eq, Show, Num, Ord, Enum, Real, Floating
                             , Fractional, RealFloat, RealFrac, PrintfArg )
                             
data PMatch = PMatch {  pmTimeSig :: TimeSig
                     ,  pmatch    :: NSWDist
                     ,  pmRot     :: Rot
                     ,  pmRPrior  :: RPrior
                     ,  pmQBins   :: QBins
                     ,  pmFile    :: FilePath
                     } deriving (Eq)
                     
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
  
evalMeter :: [TimedSeg TimeSig PMatch] ->  [Result Bool]
evalMeter = map eval where

  eval :: TimedSeg TimeSig PMatch -> Result Bool
  eval (TimedSeg ts m) = Result (pmTimeSig m == getEvent ts) 
                                (getNumForQBins (pmQBins m) (rot $ pmRot m)== 0)

  
-- | Picks the best matching profile
pickMeters :: [TimedSeg TimeSig [PMatch]] -> [TimedSeg TimeSig PMatch]
pickMeters = map (fmap (maximumBy (compare `on` pmatch)))

-- | Copies the TimedSeg for every Match for printing
dontPickMeters :: [TimedSeg TimeSig [PMatch]] -> [TimedSeg TimeSig PMatch]
dontPickMeters = concatMap f 
  where f (TimedSeg t ms) = map (\x -> (TimedSeg t x)) ms

printPickMeter :: TimedSeg TimeSig PMatch -> String
printPickMeter (TimedSeg ts m) = 
  let ann = getEvent ts
      est = pmTimeSig m
      s = intercalate "\t" [ shwTs ann
                           , shwTs est
                           , show (ann == est)
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
match :: Rotations -> QBinSelection -> [IMAPDF] -> IMAStore -> [TimedSeg TimeSig [PMatch]]
match rs s pdfs i = map update . swMeterSeg $ i where
  
  q  = imaQBins i
  tb = imaTPB i

  update :: SWMeterSeg -> TimedSeg TimeSig [PMatch]
  update sg =  fmap (const . concatMap (matchPDF sg) $ pdfs) sg

  -- matches a single pdf, creating the profile is independent of the rotation
  -- and happens here
  matchPDF :: SWMeterSeg -> IMAPDF -> [PMatch]
  matchPDF (TimedSeg _ sg) ip = 
    let ts = pdfTimeSig ip 
        pf = normSWProfByBar . toSWProfWithTS ts tb $ sg
    in [ getRotProb pf r ts ip | r <- getRot rs ts ]
    
  -- Calculates the match for all rotations  
  getRotProb :: NSWProf -> (Rot, RPrior) -> TimeSig -> IMAPDF -> PMatch
  getRotProb pf (r,rp) ts ip = 
    let d  = toDoubles s $ toRNSWProfWithTS q tb r ts s pf
        p  = NSWDist $ log (pdfPrior ip) + log (pdf ip d) + log (rprior rp)
    in PMatch ts p r rp q (imaFile i)

         
  
