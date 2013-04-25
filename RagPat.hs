module RagPat where

-- import ZMidi.Core         ( readMidi )
import ZMidiBasic
import MidiCommonIO       ( readMidiScore )

import Data.List          ( intercalate, genericLength, sortBy )
import Data.Ord           ( comparing )
import Control.Arrow      ( first, second )
import Control.Monad      ( when )
import Evaluation
import MelFind            ( findMelodyQuant )

import Math.Statistics    ( correl )

-- import Debug.Trace
-- traceShow' a = traceShow a a 

-- | do stuff with a 'MidiScore' ...
printFilePatMat :: FilePath -> IO ()
printFilePatMat f = do ms <-readMidiScore f 
                       let ts = getEvent . head . getTimeSig $ ms
                       putStrLn . showPats . reGroup ts 
                                           . segByTimeSig FourtyEighth  $ ms
                       print ts


--------------------------------------------------------------------------------
-- Analysing subdivisions
--------------------------------------------------------------------------------

printSubDiv :: FilePath -> IO ()
printSubDiv f = do ms <- readMidiScore f 
                   let p = segByTimeSig FourtyEighth ms
                       r = rankSubDiv p
                       t = percTripGridOnsets p
                   when ( hasValidGridSize ms ) 
                        ( putStrLn . intercalate "\t" $ 
                          [f, show t, show (t <= 0.01)
                            , show . snd . head $ r, show r] )

-- | do stuff with a 'MidiScore' ...
printFileSubDiv :: FilePath -> IO ()
printFileSubDiv f = do p <- readMidiScore f 
                         >>= return . segByTimeSig FourtyEighth
                       putStrLn . showPats $ p
                       print . rankSubDiv $ p
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

-- ranks different 'Subdiv'isions based on how well the correlate to the data
rankSubDiv :: [Pattern] -> [(Double, DivLab)]
rankSubDiv ps = reverse . sortBy (comparing fst) . map matchBeatDiv $ pats where

  matchBeatDiv :: SubDiv -> (Double, DivLab)
  matchBeatDiv s@(p, _) = let rs = filter (not . isNaN) . map (recall p) $ ps
                          in  first (const $ sum rs / genericLength rs) s

-- calculates the correlation between a template 'Pattern' and a list of data
-- 'Patterns'. The pattern is summarised by 'countMatch's
correlPat :: Pattern -> [Pattern] -> Double
correlPat p s = correl (toDouble p) (countMatch s)
               
-- | Given a template 'Pattern' counts the number of onsets in each position and
-- normalises this list by dividing all number by the highest count, yielding
-- a list of 'Doubles' between 0 and 1.               
countMatch :: [Pattern] -> [Double]
countMatch = normalise . foldr step [ 0, 0, 0,  0, 0, 0,  0, 0, 0,  0, 0, 0  ] where

  step :: Pattern -> [Int] -> [Int]
  step x cs = zipWith doPattern x cs

  doPattern :: Onset -> Int -> Int
  doPattern  I c = succ c
  doPattern  _ c = c
               
  normalise :: [Int] -> [Double]
  normalise i = let m = fromIntegral . maximum $ i 
                in  map (\x -> fromIntegral x / m) i

-- | Labels for the different subdivisions
data DivLab = Straight | Swing | Evenly deriving (Show, Eq)
type SubDiv = (Pattern, DivLab)

-- all patterns
pats :: [SubDiv]
pats =  [(straightGrid, Straight), (swingGrid, Swing), (evenDistGrid, Evenly)]
                
straightGrid, swingGrid, evenDistGrid :: Pattern
--               0  1  2  3  4  5  6  7  8  9 10 11 
straightGrid = [ I, O, O, I, O, O, I, O, O, I, O, O ]
swingGrid    = [ I, O, I, O, I, O, I, O, I, O, I, O ]
evenDistGrid = [ I, I, I, I, I, I, I, I, I, I, I, I ]

toDouble :: Pattern -> [Double]
toDouble = map convert where
  
  convert :: Onset -> Double
  convert I = 0.8
  convert _ = 0.0  
  
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

untied1, untied2, untied3, untied4, tied1, tied2, tiedStr :: Pattern

untied1 = [ I, I, O, I,  X, X, X, X,  X, X, X, X,  X, X, X, X ]
untied2 = [ X, X, X, X,  I, I, O, I,  X, X, X, X,  X, X, X, X ]
untied3 = [ X, X, X, X,  X, X, X, X,  I, I, O, I,  X, X, X, X ]
untied4 = [ X, X, X, X,  X, X, X, X,  X, X, X, X,  I, I, O, I ]

tied1   = [ I, O, I, I,  O, I, I, O,  X, X, X, X,  X, X, X, X ]
tied2   = [ X, X, X, X,  X, X, X, X,  I, O, I, I,  O, I, I, O ]
tiedStr = [ X, X, X, X,  I, O, I, I,  O, I, I, O,  X, X, X, X ]

upScalePat :: Pattern -> TimeSig -> Pattern
upScalePat p ts = case ts of 
  (TimeSig 2 2 _ _ ) -> scale 2 p
  (TimeSig 2 4 _ _ ) -> scale 2 p
  (TimeSig 4 4 _ _ ) -> scale 5 p
  _                  -> error "upScalePat: invalid time signature"
  
  
scale :: Int -> Pattern -> Pattern
scale fact []     = []
scale fact (X:tl) = X : replicate fact X ++ scale fact tl
scale fact (h:tl) = h : replicate fact O ++ scale fact tl


