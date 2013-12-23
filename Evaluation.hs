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
      -- relCorrectOverlap
    -- , achievScore
    -- , chordChangeRatio
    -- , avgDistToOne
    
      Matchable (..)
    -- * Chord and key equality functions
    -- , rootOnlyEq
    -- , majMinEq
    -- , triadEq
    -- , chordClassEq
    -- * Small Evaluation Logic
    , EqIgnore (..)
    , (==*)
    , (&&*)
    , (||*)
    , showEqi
    , ignore
    , equal
    -- * Displaying evaluations 
    -- , printChordRCO
    -- , printRCO
    -- -- * Sampling 
    -- , sample
  ) where

-- import HarmTrace.Base.MusicTime 
-- import HarmTrace.Base.MusicRep   

import Control.Arrow             ( first, second )

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



