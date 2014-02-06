{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Ragtime.VectorNumerics 
                              -- (
                              -- -- | * Numerical calculation
                                -- sum
                              -- , mean
                              -- , normL2
                              -- , dot
                              -- , cosSim
                              -- , Num ()
                              -- ) 
                              where

import qualified Data.Vector as V
import Data.Vector ( Vector )
import Text.Printf ( PrintfArg, printf )
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
  
scale :: Num a => Vector a -> a -> Vector a
scale v s = V.map (s *) v  

add :: Num a => Vector a -> a -> Vector a
add v s = V.map (s +) v  

-- | Sums the elements of the Vector
sum :: Num a => Vector a -> a
sum = V.foldr (+) 0   

-- | Calculates the mean of the elements of the Vectors
mean :: (Fractional a, Num a) => Vector a -> a 
mean v = sum v / fromIntegral (V.length v)

normL2 :: (Floating b) => Vector b -> b
normL2 = sqrt . sum . fmap (^ (2 :: Int))

dot :: Num a => Vector a -> Vector a -> a
dot a b = sum (a * b)
  
cosSim :: Floating a => Vector a -> Vector a -> a
cosSim a b = a `dot` b  /  (normL2 a * normL2 b)

correl :: Floating a => Vector a -> Vector a -> a
correl a b = let a' = add a . negate . mean $ a 
                 b' = add b . negate . mean $ b
                 p2 x = x ^ (2 :: Int)
             in sum (a' * b') 
             / (sqrt ((sum . p2 $ a') * (sum . p2 $ b')))

-- |Pearson's product-moment correlation coefficient
pearson :: Floating a => Vector a -> Vector a -> a
pearson x y = covar x y / (stddev x * stddev y)

-- |Standard deviation of sample
stddev :: Floating a => Vector a -> a
stddev xs = sqrt $ var xs
             
-- |Sample Covariance
covar :: Floating a => Vector a -> Vector a -> a
covar xs ys = sum ((V.map f1 xs) * (V.map f2 ys)) / (n-1)
    where
      n = fromIntegral . V.length $ xs
      m1 = mean xs
      m2 = mean ys
      f1 x = x - m1
      f2 x = x - m2
      
-- | Sample variance
var :: Floating a => Vector a -> a
var v = let m = negate (mean v) 
            v' = add v m 
        in sum (v' * v') / fromIntegral (V.length v - 1)

-- | Displaying a vector
disp :: PrintfArg a => Vector a -> String
disp = concat . V.foldr (\j js -> printf "%.2f\t" j : js ) [] 
           