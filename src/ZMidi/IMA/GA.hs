{-# OPTIONS_GHC -Wall                    #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE TypeSynonymInstances        #-}
module ZMidi.IMA.GA ( Entity (..)
                    , runGA
                    ) where

import ZMidi.Score.Datatypes    ( TimeSig (..) )
import ZMidi.Score.Quantise     ( QBins (..) )
import ZMidi.IMA.SelectProfBins
import ZMidi.IMA.RNSWMatch      ( avgResult, evalMeter, Result (..), meterFail )
import ZMidi.IO.IMA             ( readMatchPutLn, Print (..) )
import Data.List                ( zipWith4 )
import Data.Maybe               ( catMaybes )
import Data.Ratio               ( (%)) 
import Data.Map.Strict          ( Map, elems, fromList, keys, mapAccum)
import GA                       ( Entity(..), GAConfig (..), evolveVerbose )
import System.Random            ( Random (..), mkStdGen, getStdGen )
import ZMidi.IO.Common          ( mapDir )
import ReadPDF                  ( IMAPDF )

--------------------------------------------------------------------------------
-- GA configuration
--------------------------------------------------------------------------------

runGA :: QBins -> QBinSelection -> [IMAPDF] -> FilePath -> IO ()
runGA qb sel pdfs dir = 
  do let cfg = GAConfig 
                 100 -- population size
                 25  -- archive size (best entities to keep track of)
                 500 -- maximum number of generations
                 0.8 -- crossover rate (% of entities by crossover)
                 0.2 -- mutation rate (% of entities by mutation)
                 0.2 -- parameter for crossover (% of split points)
                 0.2 -- parameter for mutation (% of replaced rotations)
                 False -- whether or not to use checkpointing
                 False -- don't rescore archive in each generation

         -- g = mkStdGen 0 -- random generator

     g <- getStdGen
     -- Do the evolution!
     -- Note: if either of the last two arguments is unused, just use () as a value
     -- evolveVerbose :: StdGen -> GAConfig -> QBins 
     --               -> (QBinSelection, [IMAPDF], FilePath)
     es <- evolveVerbose g cfg qb (sel,pdfs, dir)
     let (s,r) = head es :: (Maybe Double, Rotations)
     
     writeJSON "evolvedRotations.json" r
     
     putStrLn $ "best entity (GA): \n" ++ (showRotations qb r)
     putStrLn $ "score: " ++ show s

--------------------------------------------------------------------------------
-- GA instances
--------------------------------------------------------------------------------

instance Entity Rotations Double (QBinSelection, [IMAPDF], FilePath) QBins IO where
  genRandom pool seed = return $ stdRotations pool (randomPrior seed) 

  crossover _pool par seed a b = return . Just $ mixRotations seed par a b

  mutation _pool par seed e = return . Just $ replaceRotations seed par e

  score (sel, pdfs, dirfp) e = 
    do d <- mapDir (readMatchPutLn None sel pdfs e) dirfp 
       let s = meterFail . avgResult . evalMeter . concat . catMaybes $ d
       s `seq` print s
       return (Just s)

  showGeneration gi (_,archive) = 
    let (Just fitness, e) = head archive
    in  "best entity (gen. "  ++ show gi ++ "): " ++ (showRotations (QBins 12) e) 
                              ++ " [fitness: " ++ show fitness ++ "]"
          
      

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