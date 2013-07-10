{-# OPTIONS_GHC -Wall                #-}
module RagPat ( getPercTripGridOnsets
              , hasValidGridSize
              , printFileSubDiv
              , printSubDiv 
              , printFilePatMat
              , printPatCount
              )where

import ZMidiBasic
import MidiCommonIO       ( readMidiScore )
import RTCParser          ( RTC (..), getRTCMeta, preciseYear, isPrecise )

import Data.List          ( intercalate )
import Control.Arrow      ( first, second )
import Control.Monad      ( when )
import Evaluation
import MelFind            ( findMelodyQuant )


-- import Debug.Trace
-- traceShow' a = traceShow a a 

printPatCount :: [RTC] -> FilePath -> IO ()
printPatCount rtc f = 
  do ms <- readMidiScore f 
     let ts = getEvent . head . getTimeSig $ ms
         ps = reGroup ts . segByTimeSig FourtyEighth $ ms
         mt = getRTCMeta rtc f
         yr = year mt
     putStrLn . intercalate "\t"  
              $ ( show (rtcid mt) : f 
                : show ts : preciseYear yr
                : map (show . matchRatio . matchPat ps . upScalePat ts) 
                  [ untied1, untied2, tied1, tied2 ]
                )
                
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
  (TimeSig 2 2 _ _ ) -> takeConcatOverlap 8 p
  (TimeSig 2 4 _ _ ) -> takeConcatOverlap 4 p
  (TimeSig 4 4 _ _ ) -> takeConcatOverlap 8 p
  _                  -> error "reGroup: invalid time signature"
  
takeConcatOverlap :: Int -> [[a]] -> [[a]]
takeConcatOverlap _ [] = []
takeConcatOverlap i l  = let (top, rest) = splitAt i l 
                         in concat top 
                          : takeConcatOverlap i (drop (i `div` 2) top ++ rest)

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
                                 
     
--------------------------------------------------------------------------------
-- Important tests for valid midi files
--------------------------------------------------------------------------------

-- | 
hasValidGridSize :: MidiScore -> Bool
hasValidGridSize ms = (ticksPerBeat ms `mod` toGridUnit FourtyEighth) == 0

-- | Returns the percentage of onsets that are not on grid positions 0, 3, 6,
-- and 9 (of 12) for a 'Pattern' (by applying 'percTripGridOnsets)
getPercTripGridOnsets :: MidiScore -> Double
getPercTripGridOnsets = percTripGridOnsets . segByTimeSig FourtyEighth


--------------------------------------------------------------------------------
-- Matching beat subdivisions
--------------------------------------------------------------------------------

-- | Returns the percentage of onsets that are not on grid positions 0, 3, 6,
-- and 9 (of 12) for a 'Pattern'
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

-- | normalises a match by dividing the matches by the total of matched bars
matchRatio :: (Int, Int) -> Double
matchRatio (mch,mis) = fromIntegral mch / fromIntegral (mch + mis)

matchPrint :: (Int, Int) -> (Int, Int) -> [String]
matchPrint (m1,ms1) (m2,ms2) = [ show (fromIntegral (m1 + m2) / fromIntegral (m1 + ms1) :: Double)
                               , show m1, show m2, show (m1 + ms1) ]

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

tied1   = [ X, X, I, I,  O, I, X, X,  X, X, X, X,  X, X, X, X ]
tied2   = [ X, X, X, X,  X, X, I, I,  O, I, X, X,  X, X, X, X ]

upScalePat :: TimeSig -> Pattern -> Pattern
upScalePat ts p = case ts of 
  (TimeSig 2 2 _ _ ) -> scale 5 p
  (TimeSig 2 4 _ _ ) -> scale 2 p
  (TimeSig 4 4 _ _ ) -> scale 5 p
  _                  -> error "upScalePat: invalid time signature"
  
  
scale :: Int -> Pattern -> Pattern
scale _    []     = []
scale fact (X:tl) = X : replicate fact X ++ scale fact tl
scale fact (h:tl) = h : replicate fact O ++ scale fact tl


