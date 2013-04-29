{-# OPTIONS_GHC -Wall                #-}
module RagPat where

-- import ZMidi.Core         ( readMidi )
import ZMidiBasic
import MidiCommonIO       ( readMidiScore )
import RTCParser          ( RTC (..), getRTCMeta, preciseYear, isPrecise )

import Data.List          ( intercalate )
-- import Data.Ord           ( comparing )
import Control.Arrow      ( first, second )
import Control.Monad      ( when )
import Evaluation
import MelFind            ( findMelodyQuant )

-- import Math.Statistics    ( correl )

-- import Debug.Trace
-- traceShow' a = traceShow a a 

printPatCount :: [RTC] -> FilePath -> IO ()
printPatCount rtc f = 
  do ms <- readMidiScore f 
     let ts = getEvent . head . getTimeSig $ ms
         ps = reGroup ts . segByTimeSig FourtyEighth $ ms
         mt = getRTCMeta rtc f
         yr = year mt
     when (isPrecise yr) ( putStrLn . intercalate "\t"  
              $ ( show (rtcid mt) : f 
                : show ts : preciseYear yr
                : map (show . fst . matchPat ps . upScalePat ts) 
                  [ untied1, untied2, tied1, tied2 ]
                ))

-- | Match the patterns to one file
printFilePatMat :: FilePath -> IO ()
printFilePatMat f =  
  do ms <-readMidiScore f 
     let ts = getEvent . head . getTimeSig $ ms
         ps = reGroup ts . segByTimeSig FourtyEighth $ ms
     putStrLn . showPats $ ps
     putStrLn ("Time Signtaure: " ++ show ts)
     
     print (upScalePat ts untied1)
     print (upScalePat ts untied2)
     putStrLn ("untied1: " ++ (show . matchPat ps . upScalePat ts $ untied1))
     putStrLn ("untied2: " ++ (show . matchPat ps . upScalePat ts $ untied2))
     
     print (upScalePat ts tied1)
     print (upScalePat ts tied2)
     putStrLn ("tied1  : " ++ (show . matchPat ps . upScalePat ts $ tied1  ))
     putStrLn ("tied2  : " ++ (show . matchPat ps . upScalePat ts $ tied2  ))
     
     -- putStrLn ("tiedStr: " ++ (show . matchPat ps . upScalePat ts $ tiedStr))

--------------------------------------------------------------------------------
-- Analysing subdivisions
--------------------------------------------------------------------------------

printSubDiv :: FilePath -> IO ()
printSubDiv f = do ms <- readMidiScore f 
                   let p = segByTimeSig FourtyEighth ms
                       -- r = rankSubDiv p
                       t = percTripGridOnsets p
                   when ( hasValidGridSize ms ) 
                        ( putStrLn . intercalate "\t" $ 
                          [f, show t, show (t <= 0.01) ] )

-- | do stuff with a 'MidiScore' ...
printFileSubDiv :: FilePath -> IO ()
printFileSubDiv f = do p <- readMidiScore f 
                         >>= return . segByTimeSig FourtyEighth
                       putStrLn . showPats $ p
                       -- print . rankSubDiv $ p
                       print . percTripGridOnsets $ p

showPats :: [Pattern] -> String
showPats = intercalate "\n" . map show

--------------------------------------------------------------------------------
-- Converting to patterns
--------------------------------------------------------------------------------

segByTimeSig :: ShortestNote -> MidiScore -> [Pattern]
segByTimeSig q ms = scoreToPatterns (getMinGridSize q ms) (toGridUnit q) 
                  . findMelodyQuant q $ ms

reGroup :: TimeSig -> [Pattern] -> [Pattern]
reGroup ts p = case ts of
  (TimeSig 2 2 _ _ ) -> takeConcat 4 p -- is this right???
  (TimeSig 2 4 _ _ ) -> takeConcat 4 p
  (TimeSig 4 4 _ _ ) -> takeConcat 8 p
  _                  -> error "reGroup: invalid time signature"
  
takeConcat :: Int -> [[a]] -> [[a]]
takeConcat _ [] = []
takeConcat i l  = let (top, rest) = splitAt i l 
                  in concat top : takeConcat i rest

-- | Takes a midiscore, quantises it, finds the melody, and turns it into a 
-- 'Pattern' list, where every 'Pattern' represents a beat
scoreToPatterns :: Time -> Int -> Voice -> [Pattern]
scoreToPatterns ml gu = groupEvery gu . toPat [0, ml .. ] . map onset  where
  
  toPat :: [Time] -> [Time] -> [Onset]
  toPat [] []  = []
  toPat [] _   = error "no more grid?" -- impossible?
  toPat _  []  = []
  toPat (g:gs) (d:ds) | g == d = I : toPat gs ds
                      | g <  d = if d - g < ml
                                 then error "unquantised interval encountered"
                                 else O : toPat gs (d:ds)
                                 
-- | Groups a list of patterns in fixed size lists, if the last list is not 
-- of the same length, the remainder is filled with 'X's
groupEvery :: Int -> Pattern -> [Pattern]
groupEvery x p | glen == x =  g : groupEvery x r
               | otherwise = [g ++ replicate (x - glen) X]
  where (g,r) = splitAt x p 
        glen  = length g
                                 

getMinGridSize :: ShortestNote -> MidiScore -> Time
getMinGridSize q ms = case ticksPerBeat ms `divMod` (toGridUnit q) of
                        (d,0) -> d
                        _     -> error "getMinGridSize: invalid quantisation"
     
--------------------------------------------------------------------------------
-- Important tests for valid midi files
--------------------------------------------------------------------------------

hasValidGridSize :: MidiScore -> Bool
hasValidGridSize ms = (ticksPerBeat ms `mod` toGridUnit FourtyEighth) == 0

-- | has the 'MididScore' a 'Straight' 'SubDiv'ision
isStraight :: MidiScore -> Bool
isStraight = (<= 0.01) . getPercTripGridOnsets 

getPercTripGridOnsets :: MidiScore -> Double
getPercTripGridOnsets = percTripGridOnsets . segByTimeSig FourtyEighth

-- | has the 'MidiScore' a meter we can use for analysis
hasValidTimeSig :: MidiScore -> Bool
-- hasValidTimeSig = or . map isValid . getTimeSig where
hasValidTimeSig ms = case getTimeSig ms of
  [ts] -> isValid ts  -- there should be one valid time signature
  _    -> False


isValid :: Timed TimeSig -> Bool
isValid ts = case getEvent ts of
  (TimeSig 4 4 _ _) -> True
  (TimeSig 2 4 _ _) -> True
  (TimeSig 2 2 _ _) -> True
  _                 -> False

--------------------------------------------------------------------------------
-- Matching beat subdivisions
--------------------------------------------------------------------------------
  
percTripGridOnsets :: [Pattern] -> Double
percTripGridOnsets ps = 
  let (trip,rest) = foldr step (0,0) ps

      step :: Pattern -> (Int, Int) -> (Int, Int)
      step p r = r `seq` foldr doPat r . zipWith (,) [0..11] $ p where 
        
        -- increase the first when off the straight grid
        doPat :: (Int, Onset) -> (Int, Int) -> (Int, Int)
        doPat (1 , I) x = first  succ x
        doPat (2 , I) x = first  succ x
        doPat (4 , I) x = first  succ x
        doPat (5 , I) x = first  succ x
        doPat (7 , I) x = first  succ x
        doPat (8 , I) x = first  succ x
        doPat (10, I) x = first  succ x
        doPat (11, I) x = first  succ x
        doPat _       x = second succ x
        
  in fromIntegral trip / fromIntegral (trip + rest)


--------------------------------------------------------------------------------
-- Matching rhythmic patterns
--------------------------------------------------------------------------------

matchRatio :: (Int, Int) -> Double
matchRatio (mch,mis) = fromIntegral mch / fromIntegral (mch + mis)

-- | Calculates the matches and mismatches, respectively
matchPat :: [Pattern] -> Pattern -> (Int, Int)
matchPat ps p = foldr step (0,0) ps where
  
  step :: Pattern -> (Int,Int) -> (Int, Int)
  step x r | perfect p x = first  succ r -- if perfect increase the first,
           | otherwise   = second succ r -- if mismatch the second

data Onset = X -- | don't care symbol, ignored in all evaluation
           | O -- | No onset
           | I -- | Onset
             deriving Eq
             
instance Show Onset where
  show o = case o of
            X -> "*"
            O -> "_"
            I -> "x"
  showList l s = s ++ (intercalate " " . map show $ l)

instance Matchable Onset where
  -- eqi :: Onset -> Onset -> EqIgnore
  eqi I I = Equal
  eqi O O = Equal
  eqi X _ = Ignore
  eqi _ X = Ignore
  eqi _ _ = NotEq  
  
type Pattern = [Onset]

untied1, untied2, tied1, tied2 :: Pattern

untied1 = [ I, I, O, I,  X, X, X, X,  X, X, X, X,  X, X, X, X ]
untied2 = [ X, X, X, X,  I, I, O, I,  X, X, X, X,  X, X, X, X ]
-- untied3 = [ X, X, X, X,  X, X, X, X,  I, I, O, I,  X, X, X, X ]
-- untied4 = [ X, X, X, X,  X, X, X, X,  X, X, X, X,  I, I, O, I ]

tied1   = [ X, X, I, I,  O, I, I, X,  X, X, X, X,  X, X, X, X ]
-- tied2   = [ X, X, X, X,  X, X, X, X,  X, X, I, I,  O, I, X, X ]
tied2   = [ X, X, X, X,  X, X, I, I,  O, I, X, X,  X, X, X, X ]

upScalePat :: TimeSig -> Pattern -> Pattern
upScalePat ts p = case ts of 
  (TimeSig 2 2 _ _ ) -> scale 2 p
  (TimeSig 2 4 _ _ ) -> scale 2 p
  (TimeSig 4 4 _ _ ) -> scale 5 p
  _                  -> error "upScalePat: invalid time signature"
  
  
scale :: Int -> Pattern -> Pattern
scale fact []     = []
scale fact (X:tl) = X : replicate fact X ++ scale fact tl
scale fact (h:tl) = h : replicate fact O ++ scale fact tl


