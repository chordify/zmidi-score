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
                          , printPickMeter
                          ) where
                       
import ZMidi.Score 
import ZMidi.IMA.TimeSigSeg       ( TimedSeg (..) )
import ZMidi.IMA.SelectProfBins   ( Rot (..) )
import ZMidi.IMA.Analyse          ( SWMeterSeg, IMAStore (..) )

import ZMidi.IMA.RNSWProf         ( toDoubles, toRNSWProf )
import ReadPDF                    ( pdfPrior, multiNormal, ToPDF, pdfTimeSig )

import Data.Map.Strict            ( Map )
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
                     ,  pmFile    :: FilePath
                     } deriving (Eq)
                     
instance Show PMatch where
  show (PMatch ts m r fp) = printf (fp ++ '\t' : show ts ++ ": %1.4f\t R: %2d") m (rot r)
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
  eval (TimedSeg ts pm) = Result (pmTimeSig pm == getEvent ts) 
                                 (pmRot pm == 0)

  
-- | Picks the best matching profile
pickMeters :: [TimedSeg TimeSig [PMatch]] -> [TimedSeg TimeSig PMatch]
pickMeters = map (fmap (maximumBy (compare `on` pmatch)))

printPickMeter :: TimedSeg TimeSig PMatch -> String
printPickMeter (TimedSeg ts m) = 
  let ann = getEvent ts
      est = pmTimeSig m
      s = intercalate "\t" [ shwTs ann
                           , shwTs est
                           , show (ann == est)
                           , "%.3f" 
                           , "r:%2d"]
      
      shwTs :: TimeSig -> String
      shwTs x = '\'' : show x ++ "\'"
      
      -- tsEq :: TimeSig -> TimeSig -> Bool
      -- tsEq (TimeSig 4 4 _ _) (TimeSig 2 2 _ _) = True
      -- tsEq a                 b                 = a == b
  
  in printf s (pmatch m) (rot . pmRot $ m)

-- TODO replace vars variable
-- TODO replace max rotation variable       
-- TODO can probably be simplified      
-- TODO create a prior based on the Rotation, the chances on rotations > 0
--      quite low
match :: Int -> Rot -> Map TimeSig [(Beat, BeatRat)] -> [ToPDF] 
      -> IMAStore -> [TimedSeg TimeSig [PMatch]]
match vars mr s pdfs i = map update . swMeterSeg $ i where

  update :: SWMeterSeg ->  TimedSeg TimeSig [PMatch]
  update x = fmap (const [getProb x r p | r <- [mr, pred mr .. 0], p <- pdfs]) x

  getProb :: SWMeterSeg -> Rot -> ToPDF -> PMatch
  getProb sg r pdf = 
    let ts = pdfTimeSig pdf
        d  = toDoubles vars $ toRNSWProf (imaQBins i) (imaTPB i) r (const ts) s sg
        p  = NSWDist $ log (pdfPrior pdf) + log (multiNormal pdf d)
    in PMatch ts p r (imaFile i)

         
  
