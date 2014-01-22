{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverlappingInstances       #-}
module IMA.LocalMeter ( Time   (..)
                      , Period (..)
                      , Len    (..)
                      -- , randomOnsets
                      , factors
                      ) where

import Data.List                  ( intercalate )

import Test.QuickCheck
import System.Random
  
newtype Len    = Len    { len    :: Int } 
                        deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )
newtype Period = Period { period :: Int } 
                        deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )
newtype Time   = Time   { time   :: Int }
                        deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )

instance Arbitrary Len where
     arbitrary = choose (2,10) >>= return . Len

instance Arbitrary Period where
     arbitrary = choose (1,20) >>= return . Period
     -- arbitrary = choose (3,30) >>= return . Period

instance Arbitrary Time where
     arbitrary = choose (1,8) >>= return . Time
     -- arbitrary = elements [3, 6 .. 21] >>= return . Time
     
instance Arbitrary [Time] where
     arbitrary = choose (1,100) >>= genOnsets

-- randomOnsets :: Int -> IO [Time]
-- randomOnsets n = do r <- newStdGen 
                    -- let t = generate 1 r (genOnsets n)
                    -- print . map time $ t
                    -- return t
                  
genOnsets :: Int -> Gen [Time]                      
genOnsets n = vector n >>= return . reverse . foldr step [] where

        step :: Time -> [Time] -> [Time]
        step t []     = [t]
        step t (x:xs) = t+x : x : xs
                      

-- | returns the meter ending 'Time'
meterEnd' :: Period -> Len -> Time -> Time
meterEnd' (Period p) (Len l) (Time t) = Time (t + (p * l))

factors :: Period -> [Period]
-- factors p = filter (\x -> p `mod` x == 0) [1 .. p]
factors p = 1 : [x | x <- [2 .. p], p `mod` x == 0] 