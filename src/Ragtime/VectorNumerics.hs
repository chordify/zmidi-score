{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Ragtime.VectorNumerics (
                              -- | * Numerical calculation
                                sum
                              , mean
                              , normL2
                              , Num ()
                              ) where

import qualified Data.Vector as V
import Data.Vector ( Vector )
import Prelude hiding (sum)
 
--------------------------------------------------------------------------------
-- Numerical Vectors
--------------------------------------------------------------------------------   

-- using a Matrix
instance Num a => Num (Vector a) where
  va + vb = mergeVectorsSameSize (+) va vb
  va - vb = mergeVectorsSameSize (-) va vb
  va * vb = mergeVectorsSameSize (*) va vb
  signum  = fmap signum
  negate  = fmap negate
  abs     = fmap abs
  fromInteger = V.singleton . fromInteger

-- merges to Vectors by applying a function to index pairs of both Vectors
mergeVectorsSameSize :: Num a => (a -> a -> a) 
                     -> Vector a -> Vector a -> Vector a
mergeVectorsSameSize f va vb 
  | V.length va == V.length vb = V.zipWith f va vb
  | otherwise = error (  "mergeVectorsSameSize: vectors of different sizes: "
                      ++ show (V.length va) ++ " and " ++ show (V.length vb))
                      

-- | Sums the elements of the Vector
sum :: Num a => Vector a -> a
sum = V.foldr (+) 0   

-- | Calculates the mean of the elements of the Vectors
mean :: (Fractional a, Num a) => Vector a -> a 
mean v = sum v / fromIntegral (V.length v)

normL2 :: Floating a => Vector a -> a
normL2 = sqrt . sum . fmap (^ (2 :: Int))
  