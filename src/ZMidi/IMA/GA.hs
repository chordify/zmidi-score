{-# OPTIONS_GHC -Wall                    #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE TypeSynonymInstances        #-}
module ZMidi.IMA.GA (Entity (..)) where

import ZMidi.Score.Datatypes          ( TimeSig (..) )
import ZMidi.Score.Quantise           ( QBins (..) )
import ZMidi.IMA.SelectProfBins
import ZMidi.IMA.RNSWMatch            ( avgResult, evalMeter, Result (..) )
import ZMidi.IO.IMA                   ( readMatchPutLn, Print (..) )
import Data.List                      ( zipWith4 )
import Data.Maybe                     ( catMaybes )
import Data.Ratio                     ( (%)) 
import Data.Map.Strict                ( Map, elems, fromList, keys, mapAccum)
import GA                             (Entity(..))
import System.Random                  ( Random (..), mkStdGen )
import ZMidi.IO.Common                ( mapDir )
import ReadPDF                        ( IMAPDF )

--------------------------------------------------------------------------------
-- GA configuration
--------------------------------------------------------------------------------

runGA :: IO ()
runGA = undefined

--------------------------------------------------------------------------------
-- GA instances
--------------------------------------------------------------------------------

instance Entity Rotations Double (QBinSelection, [IMAPDF], FilePath) QBins IO where
  genRandom qb seed = return $ stdRotations qb (randomPrior seed) 

  crossover _pool par seed a b = return . Just $ mixRotations seed par a b

  mutation pool par seed e = return . Just $ replaceRotations seed par e

  score (sel, pdfs, dirfp) e = mapDir (readMatchPutLn None sel pdfs e) dirfp 
                       >>= return . Just . meterFail . avgResult 
                                  . evalMeter . concat . catMaybes
       

  -- showGeneration ix 

--------------------------------------------------------------------------------
-- GA helper functions
--------------------------------------------------------------------------------
        
randomPrior :: Int -> QBins -> TimeSig -> [(Rot, RPrior)]
randomPrior s (QBins q) t = reverse $ zipWith f [0, 3 .. ((tsNum t * q) - 3)] r
  -- Hacky, but let's make sure we have different numbers for every timesig
  where r = randoms (mkStdGen (s + tsNum t + tsDen t))
        f x p = (Rot (x % q), RPrior p)    

-- | it assumed the two rotations have the same length.
crossRot :: Int -> [a] -> [a] -> ([a],[a])
crossRot seed a b = 
  let i        = fst $ randomR (0, pred . length $ a) (mkStdGen seed)
      (a1, a2) = splitAt i a
      (b1, b2) = splitAt i b
  in (a1 ++ b2, b1 ++ a2)

mixList :: Int -> Float -> [a] -> [a] -> [a]
mixList seed p a b = zipWith3 select (randBool seed p) a b
  where select :: Bool -> a -> a -> a
        select rand x y | rand      = x
                        | otherwise = y
  
mixRotations :: Int -> Float -> Rotations -> Rotations -> Rotations
-- converting back to lists and then back to maps, not a very nice solutions
-- N.B. it's a pattern that might be useful at other places to...
mixRotations seed p a b = normPriors . fromList 
                        $ zipWith4 mix (randoms $ mkStdGen seed) 
                                       (keys a) (elems a) (elems b) 

  where mix :: Int -> TimeSig -> [(Rot,RPrior)] -> [(Rot,RPrior)] 
            -> (TimeSig, [(Rot,RPrior)])
        mix s ts x y = (ts, mixList s p x y)
        
replaceRotations :: Int -> Float -> Rotations -> Rotations
replaceRotations seed p = mapWithRand seed replace

  where replace :: Int -> [(Rot, RPrior)] -> [(Rot, RPrior)]
        replace s r = let (rs, ps) = unzip r
                          x        = randomRs (0,1) . mkStdGen . succ $ s
                      in zip rs (mixList s p ps x)
                       
randBool :: Int -> Float -> [Bool]
randBool seed p = map toBool . randoms . mkStdGen $ seed
  where k = round (1 / p)
        
        toBool :: Int -> Bool 
        toBool s = s `mod` k == 0
    
mapWithRand :: Int -> (Int -> a -> b) -> Map k a -> Map k b
mapWithRand seed f = snd . mapAccum apply (mkStdGen seed)

  where -- apply :: RandomGen g => g -> a -> (g, b)
        apply g x = let (r, g') = random g in (g', f r x)