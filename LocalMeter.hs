{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LocalMeter where

import Data.List                  ( intercalate )
-- import Control.Monad              ( replicateM )
-- import Data.IntMap                ( IntMap, insertWith, mapWithKey 
                                  -- , split, fromList, foldrWithKey )
-- import qualified Data.IntMap as M ( foldr )

-- import Debug.Trace
import Test.QuickCheck
import System.Random

{-
pprint :: Show a => (LMeter -> LMeter -> a) -> LMeter -> LMeter -> IO ()
pprint f a b = putStrLn (show a ++ " " ++ show b ++ " : " ++ show (f a b) 
                        ++ " " ++ show (getSet a) ++ " " ++ show (getSet b))
 
getSet :: LMeter -> [Int]
getSet m = take (len . mLen $ m) . expMeter $ m
    
expMeter :: LMeter -> [Int]
expMeter (LMeter (Time t) (Period p) (Len l)) = [t, (t+p) .. ]

-- | A matrix is a Vector of Vectors, we could also use one large Vector with
-- another 'ix' function
type LMeters = [(Period, [(Time, Len)])]
type MeterMap = IntMap [(Period, Len)]

insertMeters :: MeterMap -> Period -> [(Time, Len)] -> MeterMap
insertMeters m p l = foldr (insertMeter p) m l
-- insertMeters m p l = trace ("p: " ++ show p ++ show l) (foldr (insertMeter p) m l)

insertMeter :: Period -> (Time, Len) -> MeterMap -> MeterMap
insertMeter p (s,l) m = insertWith (++) (time s) [(p,l)] m


nrOfLMeters :: MeterMap -> Int
nrOfLMeters = M.foldr step 0 where
  
  step :: [(Period, Len)] -> Int -> Int
  step l r = r + length l

-- Converts a 'MeterMap' into a sorted list of 'LMeters'
toLMeters :: MeterMap -> [LMeter]
toLMeters = sort . foldrWithKey step [] where

  step :: Int -> [(Period, Len)] -> [LMeter] -> [LMeter]
  step s x [] =       map (toLMeter s) x 
  step s x ms = ms ++ map (toLMeter s) x 
  
  toLMeter :: Int -> (Period, Len) -> LMeter
  toLMeter s (p, l) = LMeter (Time s) p l
-}
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
     arbitrary = choose (0,12) >>= return . Time
     
instance Arbitrary LMeter where
     arbitrary = do s <- arbitrary
                    p <- arbitrary
                    l <- arbitrary
                    return (LMeter s p l)

randomOnsets :: Int -> IO [Time]
randomOnsets n = do r <- newStdGen
                    let ts = generate 1 r . vector $ n
                    return . reverse . foldr step [] $ ts where

                      step :: Time -> [Time] -> [Time]
                      step t []     = [t]
                      step t (x:xs) = t+x : x : xs
                      

-- | returns the meter ending 'Time'
meterEnd' :: Period -> Len -> Time -> Time
meterEnd' (Period p) (Len l) (Time t) = Time (t + (p * l))

factors :: Period -> [Period]
factors p = filter (\x -> p `mod` x == 0) [1 .. p]