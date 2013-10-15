{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LocalMeter where

import Data.List                  ( intercalate, sort )
import Data.IntMap                ( IntMap, insertWith, mapWithKey 
                                  , split, fromList, foldrWithKey )
import qualified Data.IntMap as M ( foldr )

test :: MeterMap
test = fromList [(1, [(1,2), (2,4)]), (3, [(1,4), (2,2)]), (4, [])]

-- | A matrix is a Vector of Vectors, we could also use one large Vector with
-- another 'ix' function
type LMeters = [(Period, [(Time, Len)])]
type MeterMap = IntMap [(Period, Len)]

insertMeters :: MeterMap -> Period -> [(Time, Len)] -> MeterMap
insertMeters m p l = foldr (insertMeter p) m l

insertMeter :: Period -> (Time, Len) -> MeterMap -> MeterMap
insertMeter p (s,l) m = insertWith (++) (time s) [(p,l)] m

-- filter all local meters with an onset greater then o, and which are in reach 
-- of o
filterLMeters :: Time -> MeterMap -> MeterMap
filterLMeters o = mapWithKey filterInReach . fst . split (succ . time $ o) 

  where filterInReach :: Int -> [(Period, Len)] -> [(Period, Len)]
        filterInReach s = filter (\x -> inReach      (Time s) x 
                                     && matchesPhase (Time s) x)
        
        -- returns true if  onset o lies in reach the meter starting at s
        -- with period p and Len l
        inReach :: Time -> (Period, Len) -> Bool
        inReach s (p, l) = o <= meterEnd p l s
        
        -- given an onset and an 'LMeter' returns True if both have the same 
        -- phase, which means that the onset coincides with the grid of the 'LMeter'
        -- matchesPhase :: Time -> LMeter -> Bool
        -- matchesPhase o (LMeter strt per _len) = (o - strt) `mod` per == 0 
        matchesPhase :: Time -> (Period, Len) -> Bool
        matchesPhase (Time s) (Period p, _ ) = (time o - s) `mod` p ==  0

nrOfLMeters :: MeterMap -> Int
nrOfLMeters = M.foldr step 0 where
  
  step :: [(Period, Len)] -> Int -> Int
  step l r = r + length l

toLMeters :: MeterMap -> [LMeter]
toLMeters = sort . foldrWithKey step [] where

  step :: Int -> [(Period, Len)] -> [LMeter] -> [LMeter]
  step s x [] =       map (toLMeter s) x 
  step s x ms = ms ++ map (toLMeter s) x 
  
  toLMeter :: Int -> (Period, Len) -> LMeter
  toLMeter s (p, l) = LMeter (Time s) p l

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
-- newtype Weight = Int

                     
-- | Indexing a matrix
getStarts :: [(Time,Len)] -> Len -> [Time] 
getStarts m l = map fst $ filter ((== l) . snd) m 

getLens :: [(Time,Len)] -> Time -> [Len] 
getLens m s = map snd $ filter ((== s) . fst) m

-- getLMeters :: LMeters -> Period -> Len -> [LMeter]
-- getLMeters m p l = map (\(o,_) -> LMeter o p l) $ filter ((== l) . snd) (m !! p) 

addLMeter :: [(Time, Len)] -> Period -> Time -> Len -> [(Time,Len)]
addLMeter m p t l | isMaximal (t,l) && (len l) >= 2 = addMeter (t,l)
                  | otherwise                       = m

  where -- adds a meter to the collection, and removes any meters that are 
        -- included in the added meter
        addMeter :: (Time, Len) -> [(Time, Len)]
        addMeter x = x : filter (\y -> not $ isSubSet y x) m
        
        -- being maximal means not being a subset
        isMaximal :: (Time, Len) -> Bool
        isMaximal x = and $ map (not . isSubSet x) m
        
        -- returns true if the first pair is a meter that is a subset of the
        -- second meter pair
        isSubSet :: (Time, Len) -> (Time, Len) -> Bool
        isSubSet (ta, la) (tb, lb) =             ta    >=            tb 
                                   && meterEnd p la ta <= meterEnd p lb tb
                                           
meterEnd :: Period -> Len -> Time -> Time
meterEnd (Period p) (Len l) (Time t) = Time (t + (p * l))
        

test2 :: [(Time, Len)]
test2 = [(Time 0, Len 4)]









