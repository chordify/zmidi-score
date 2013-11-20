{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverlappingInstances       #-}
module LocalMeter where

import Data.List                  ( intercalate )

import Test.QuickCheck
import System.Random

data LMeter = LMeter { mStart  :: Time
                     , mPeriod :: Period
                     , mLen    :: Len } deriving (Eq)

instance Show LMeter where
  show (LMeter s p l) = "(O: "  ++ show (time s) ++ 
                        ", P: " ++ show (period p) ++ 
                        ", L: " ++ show (len l) ++ ")"

  showList a b = (intercalate "\n" . map show $ a) ++ b

instance Ord LMeter where
  compare a b = case compare (mPeriod a) (mPeriod b) of
                  EQ -> case compare (mLen a) (mLen b) of
                          EQ -> compare (mStart a) (mStart b)
                          cs -> cs
                  cp -> cp
  
newtype Len    = Len    { len    :: Int } 
                        deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )
newtype Period = Period { period :: Int } 
                        deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )
newtype Time   = Time   { time   :: Int }
                        deriving ( Eq, Show, Num, Ord, Enum, Real, Integral )

instance Arbitrary Len where
     arbitrary = choose (2,10) >>= return . Len

instance Arbitrary Period where
     arbitrary = choose (1,10) >>= return . Period

instance Arbitrary Time where
     arbitrary = choose (1,8) >>= return . Time
     
instance Arbitrary [Time] where
     arbitrary = choose (1,100) >>= genOnsets
     
instance Arbitrary LMeter where
     arbitrary = do s <- arbitrary
                    p <- arbitrary
                    l <- arbitrary
                    return (LMeter s p l)

randomOnsets :: Int -> IO [Time]
randomOnsets n = do r <- newStdGen 
                    let t = generate 1 r (genOnsets n)
                    print . map time $ t
                    return t
                  
genOnsets :: Int -> Gen [Time]                      
genOnsets n = vector n >>= return . reverse . foldr step [] where

        step :: Time -> [Time] -> [Time]
        step t []     = [t]
        step t (x:xs) = t+x : x : xs
                      

-- | returns the meter ending 'Time'
meterEnd' :: Period -> Len -> Time -> Time
meterEnd' (Period p) (Len l) (Time t) = Time (t + (p * l))

factors :: Period -> [Period]
factors p = filter (\x -> p `mod` x == 0) [1 .. p]