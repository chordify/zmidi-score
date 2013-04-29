{-# OPTIONS_GHC -Wall                #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Evaluation
-- Copyright   :  (c) 2010-2013 Chordify, Universiteit Utrecht, University of Oxford
-- License     :  LGPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: A module for evaluating chord and key annotations
--------------------------------------------------------------------------------


module Evaluation (
    -- * Evaluation functions
      relCorrectOverlap
    , achievScore
    , chordChangeRatio
    , avgDistToOne
    
    , Matchable (..)
    -- * Chord and key equality functions
    , rootOnlyEq
    , majMinEq
    , triadEq
    , chordClassEq
    -- * Small Evaluation Logic
    , EqIgnore (..)
    , (==*)
    , (&&*)
    , (||*)
    -- * Displaying evaluations 
    -- , printChordRCO
    , printRCO
    -- * Sampling 
    , sample
  ) where

import HarmTrace.Base.MusicTime 
import HarmTrace.Base.MusicRep   

import Data.List                 ( genericLength, intercalate )
import Text.Printf               ( printf )
import Data.Foldable             ( foldrM )
import Control.Monad.State       ( State, execState, modify )
import Control.Arrow             ( first, second )


--------------------------------------------------------------------------------
--  Constants
--------------------------------------------------------------------------------

evaluationSampleRate, displaySampleRate :: NumData
-- | The sample rate used in a normal (non-visual) comparison (in seconds).
evaluationSampleRate = 0.01
-- | The sample rate used when visually comparing a chord annotation with a 
-- ground-truth annotation. Often a higher sample rate is preferred. Although
-- one uses precision, the visual result is easier to read.
displaySampleRate    = 0.3

--------------------------------------------------------------------------------
-- A very small logic for comparing results including a wildcard (Ignore)
--------------------------------------------------------------------------------

-- A datatype for comparisons that can be equal, not equal, or ignored  
data EqIgnore = Equal   -- ^ Equal
              | NotEq   -- ^ Not equal
              | Ignore  -- ^ Ignored
                deriving (Eq, Show)
                
showEqi :: EqIgnore -> String
showEqi Equal  = "==*"
showEqi NotEq  = "/=*"
showEqi Ignore = "***"

-- | Behaves like 'Eq' but returns an 'EqIgnore' ('Equal' or 'NotEq').
(==*) :: Eq a => a -> a -> EqIgnore
a ==* b | a == b    = Equal
        | otherwise = NotEq

infix 4 ==* -- same "infixity" as (==)
        
-- | Behaves like a regular conjunction '(&&)', but when comparing a 'Ignore'
-- will return an 'Ignore' again.
(&&*) :: EqIgnore -> EqIgnore -> EqIgnore
Ignore &&* _      = Ignore
_      &&* Ignore = Ignore
Equal  &&* Equal  = Equal
_      &&* _      = NotEq

infix 3 &&* -- same "infixity" as (&&)

-- | Behaves like a regular conjunction '(&&)', but when comparing a 'Ignore'
-- will return an 'Ignore' again.
(||*) :: EqIgnore -> EqIgnore -> EqIgnore
Ignore ||* _      = Ignore
_      ||* Ignore = Ignore
NotEq  ||* NotEq  = NotEq
_      ||* _      = Equal

infix 3 ||* -- same "infixity" as (||)

class Matchable a where
  eqi       :: a   -> a   -> EqIgnore
  match     :: [a] -> [a] -> (Int, Int)
  matchRest :: [a] -> [a] -> ((Int, Int), ([a],[a]))
  recall    :: [a] -> [a] -> Double
  perfect   :: [a] -> [a] -> Bool
        -- Minimal complete definition:
        --      eqi
  matchRest a b = matchEqIRest eqi a b
  match     a b = matchEqI     eqi a b
  recall    a b = recallEqI    eqi a b
  perfect   a b = perfectEqI   eqi a b
  
-- | Given a particular 'EqIgnore' equality function, returns the Hit and Miss 
-- counts, respectively, i.e. the sum of all 'Equal's and 'NotEq's, when
-- comparing two sequences from the first two ellements on.
matchEqI :: (a -> a -> EqIgnore) -> [a] -> [a] -> (Int, Int)
matchEqI eq a b = fst $ matchEqIRest eq a b
          
-- | Similar to 'matchEqI' but then returns the resulting lists (which may be
-- both empty if the two compared lists are equally long)
matchEqIRest :: (a -> a -> EqIgnore) -> [a] -> [a] -> ((Int, Int), ([a],[a]))
matchEqIRest _  [] y          = ((0,0), ([], y))
matchEqIRest _  x  []         = ((0,0), (x ,[]))
matchEqIRest eq (x:xs) (y:ys) = case eq x y of
     Equal  -> first (first  (+1)) (matchEqIRest eq xs ys)
     NotEq  -> first (second (+1)) (matchEqIRest eq xs ys)
     Ignore ->                     (matchEqIRest eq xs ys)

-- returns True if the two lists are a perfect match
perfectEqI :: (a -> a -> EqIgnore) ->  [a] -> [a] -> Bool
perfectEqI eq a b = case matchEqIRest eq a b of
                      -- no mismatches, and equal in length (no rest)
                      ((_,0),([],[])) -> True
                      _               -> False
     
-- | Returns 'True' if the 'EqIgnore' is 'Ignore'.
ignore :: EqIgnore -> Bool
ignore Ignore = True
ignore _      = False

-- | Returns 'True' if the 'EqIgnore' is 'Equal'.
equal  :: EqIgnore -> Bool
equal  Equal  = True
equal  _      = False

-- Calculates the recall of matching elements in two lists using 'matchEqI'
-- N.B. this function can return a 'NaN' when only 'Ignore's are compared.
recallEqI :: (a -> a -> EqIgnore) -> [a] -> [a] -> Double
recallEqI eq a b = let (hit, mis) = matchEqI eq a b 
                    in  fromIntegral hit / fromIntegral (hit + mis)

-- Todo: better to use Int?
-- Todo: better name?
-- | Foldable 'Equal' couter
countEqual :: EqIgnore -> Double -> Double
countEqual Equal x = succ x -- count the number of matching frames
countEqual _     x = x

--------------------------------------------------------------------------------
-- Chord and key equality functions
--------------------------------------------------------------------------------

rootOnlyEq :: ChordLabel -> ChordLabel -> EqIgnore
rootOnlyEq gt test = chordRoot gt `rootEq` chordRoot test 

-- | enharmonic equality for 'Root' 'Note's, N == N, X == X, and G# == Ab
rootEq :: Root -> Root -> EqIgnore
rootEq (Note Nothing N) (Note Nothing N) = Equal  -- two none roots
rootEq (Note Nothing X) _                = Ignore -- one unknown root in the GT
rootEq _               (Note Nothing X)  = NotEq  -- one unknown root in the MA
rootEq (Note Nothing N) _                = NotEq  -- one none root
rootEq _               (Note Nothing N)  = NotEq
rootEq a               b                 = toSemitone a ==* toSemitone b

-- | Compares a ground-truth 'ChordLabel' (first argument) to a test 
-- 'ChordLabel' (second argument) and returns True if the 'Root's of 
-- the chord's are equal, and both chords are major or minor
--
-- N.B. This equality function is non-associative because comparing a 
-- non-triadic chord is ignored in the evaluation while a non-triadic
-- chord in a evaluated annotation should just be qualified not equal
-- (because if also machine annotated non-triadic chords would be ignored
-- automatic transcription evaluation could be biased by outputting 
-- non-triadic chords at uncertain positions). 
-- 
-- >>> majMinEq (Chord (Note Nothing A) Sus4 [] 0 0) (Chord (Note Nothing A) Maj [] 0 0)
-- >>> Ignore
--
-- >>> majMinEq (Chord (Note Nothing A) Maj [] 0 0) (Chord (Note Nothing A) Sus4 [] 0 0)
-- >>> NotEq
--
majMinEq :: ChordLabel -> ChordLabel -> EqIgnore
majMinEq gt test =   chordRoot gt `rootEq`  chordRoot test
                 &&* toTriad   gt `majMin`  toTriad   test where
   
   -- ingore the NoClass and only return True in case of maj/maj and min/min
   majMin :: Triad -> Triad -> EqIgnore
   majMin x y = case (toMajMin x, toMajMin y) of
                 (MajClass, MajClass) -> Equal
                 (MajClass, MinClass) -> NotEq
                 (MinClass, MinClass) -> Equal
                 (MinClass, MajClass) -> NotEq
                 (NoClass , _       ) -> Ignore
                 _                    -> NotEq

-- | Returns True if both 'ChordLabel's are equal at the triad level: they are
-- either moth major or both minor. "None Chords" match only with other "None
-- Chords" and with nothing else
triadEq :: ChordLabel -> ChordLabel -> EqIgnore
triadEq a b =   chordRoot a  `rootEq` chordRoot b
                 &&* toTriad   a  ==*      toTriad   b

-- | Returns True if both 'ChordLabel's are equal at the chord class level: 
-- A chord is classified as being major, minor, dominant seventh, or dimished
-- seventh. 'chordClassEq' only returns True if the class of compared chords
-- is the same. "None Chords" match only with other None Chords and 
-- with nothing else
chordClassEq :: ChordLabel -> ChordLabel -> EqIgnore
chordClassEq a b =   chordRoot   a `rootEq` chordRoot   b
                 &&* toClassType a  ==*     toClassType b 


{-
am, a, asus, asus4, c, cm:: ChordLabel
am    = Chord (Note Nothing A) Min  [] 0 0 
a     = Chord (Note Nothing A) Maj  [] 0 0 
asus  = Chord (Note Nothing A) Maj  [NoAdd (Note Nothing I3)] 0 0 
asus4 = Chord (Note Nothing A) Sus4 [] 0 0 
cm    = Chord (Note Nothing C) Min  [] 0 0 
c     = Chord (Note Nothing C) Maj  [] 0 0 

gt   = addSimpleTime [a,a,asus,c]
test = addSimpleTime [am,am,am,am]

addSimpleTime :: [a] -> [TimedData a]
addSimpleTime = zipWith timedData [0..] where
  
  timedData :: Double -> a -> TimedData a
  timedData on d = TimedData d [Time on, Time (succ on)]
-}

--------------------------------------------------------------------------------
-- Evaluation functions
--------------------------------------------------------------------------------  
  
-- | Calculates the relative correct overlap, which is the recall
-- of matching frames, and defined as the nr of matching frames (sampled at
-- an 10 milisecond interval) divided by all frames. The first argument 
-- specifies the kind of equality, the second argument should be a
-- reference ground truth annotation, and the third argument specifies the
-- evaluated (machine) annotation.
relCorrectOverlap :: (a -> a -> EqIgnore) -> [TimedData a] -> [TimedData a] 
                  -> Double
relCorrectOverlap eq gt test = foldr countEqual 0 (zipWith eq samt samgt) 
                             / maxCompare eq samgt 
  where samgt = sample gt
        samt  = sample test

-- Returns the maximal number of elements that can be correctly annotated.
-- Given an 'EqIgnore' equality function, it compares a sequence to itself.
-- Next we all 'Ignore's are removed and the length of the list is returned
maxCompare :: Num n => (a -> a -> EqIgnore) -> [a] -> n
maxCompare eq gt = genericLength . filter (not . ignore) $ zipWith eq gt gt
             
-- | Given a chord annotation sample the chord label at every 10 ms
sample :: [TimedData a]-> [a]
sample = sampleWith evaluationSampleRate

-- like sample, but takes a sample rate (seconds :: Float) as argument
sampleWith :: NumData -> [TimedData a] -> [a]
sampleWith rate =  sampleAt [0.00, rate .. ] 

        
-- samples at specific points in time, specified in a list
sampleAt :: [NumData] -> [TimedData a] -> [a]
sampleAt  _  [] = [] -- below, will never occur
sampleAt []  _  = error "Harmtrace.Audio.Evaluation: No sampling grid specified" 
sampleAt (t:ts) (c:cs)
  | t <= offset c = getData c : sampleAt ts (c:cs)
  | otherwise     = sampleAt (t:ts) cs         

-- TODO make eq function a parameter
-- | calculates the maximal achievable score given a ground truth annotation
-- and a chord candidate list.
achievScore :: [TimedData ChordLabel] -> [TimedData [ChordLabel]] -> Double
achievScore a b = sum (zipWith eq sama samb) / len
  where sama = sample a
        samb = sample b
        len  = min (genericLength sama) (genericLength samb)
        eq c cs | equal $ foldr (\x -> (majMinEq c x ||*)) NotEq cs = 1.0
                | otherwise                                         = 0.0  

-- | calculates the number of chord changes in the ground-truth divided 
-- by the number of chord changes in the machine annotation. A number < 1 
-- indicates that the machine annotation misses some chord changes. A number
-- > 1 indicates that the machine annotation finds to many chord sequences.
chordChangeRatio ::  (ChordLabel -> ChordLabel -> EqIgnore) 
                 -> [TimedData ChordLabel] -> [TimedData ChordLabel] -> Double
chordChangeRatio eq gt ma = (fromIntegral . countChordChanges $ gt)
                          / (fromIntegral . countChordChanges $ ma) where

  countChordChanges :: [TimedData ChordLabel] -> Int
  countChordChanges cs = execState (foldrM step [] $ dropTimed cs) 0 

  step :: ChordLabel -> [ChordLabel] -> State Int [ChordLabel]
  step c []      = do modify succ
                      return [c]
  step a ( b : cs ) 
    | equal (a `eq` b)  =    return (a : b : cs)
    | otherwise         = do modify succ
                             return (a : b : cs)

-- | The 'chordChangeRatio' is optimal if it is one, but it can be larger or 
-- smaller than 1. Therefore, calculating the average blurs the actual result.
-- 'avgDistToOne' takes the absolute difference to 1.0 and averages these for a
-- list of Doubles.
avgDistToOne :: [Double] -> Double
avgDistToOne ds = (sum . map absDistToOne $ ds) / genericLength ds where

  absDistToOne :: Double -> Double
  absDistToOne a = abs (1.0 - a)

--------------------------------------------------------------------------------
-- Displaying evaluations (all in IO)
--------------------------------------------------------------------------------    

-- | Takes an 'EqIgnore' equality and a String and returns the same equality
-- function but wrapped in IO. At every evaluation the evaluation is printed
-- to the user. The String is prefixed to this output.
printEqStr ::Show a => (a -> a -> EqIgnore) -> String -> a -> a -> IO (EqIgnore)
printEqStr eq str gt test = 
  do let e = gt `eq` test
     putStrLn . (str ++) . intercalate " " $ [show gt, showEqi e, show test]
     return e
   
-- | Calculates the relative correct overlap, which is the recall
-- of matching frames, and defined as the nr of matching frames (sampled at
-- an interval set in 'ChordTrack.Constants') divided by all frames.
-- This functions differs from 'relCorrectOverlap' in that it runs in IO and
-- prints the comparison to the user.
printRCO :: Show a => (a -> a -> EqIgnore) 
          -> [TimedData a] -> [TimedData a] -> IO (Double)
printRCO eq gt test = 
  do let samgt = sampleWith displaySampleRate gt
         sam   = sampleWith displaySampleRate test
         pEq ts a b = printEqStr eq (printf "%.2f: " ts) a b
         
     matches <- sequence $ zipWith3 pEq [0,displaySampleRate ..] samgt sam
     return (foldr countEqual 0 matches / maxCompare eq samgt)
