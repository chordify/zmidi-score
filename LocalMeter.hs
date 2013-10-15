{-# OPTIONS_GHC -Wall #-}
module LocalMeter where

import Data.IntMap                ( IntMap, insertWith, mapWithKey 
                                  , split, fromList, showTree )
import qualified Data.IntMap as M ( foldr )

test :: MeterMap
test = fromList [(1, [(1,2), (2,4)]), (3, [(1,4), (2,2)]), (4, [])]

-- | A matrix is a Vector of Vectors, we could also use one large Vector with
-- another 'ix' function
type LMeters = [(Period, [(Time, Length)])]

-- type SMeters = [(Time, [(Time, [(Period, Length)])])]
-- type SMeters = IntMap (IntMap [(Period, Length)])
type MeterMap = IntMap [(Period, Length)]

insertMeter :: MeterMap -> Period -> (Time, Length) -> MeterMap
insertMeter m p (s,l) = insertWith (++) s [(p,l)] m

-- filter all local meters with an onset greater then o, and which are in reach 
-- of o
filterLMeters :: Time -> MeterMap -> MeterMap
filterLMeters o = mapWithKey filterInReach . fst . split (succ o) 

  where filterInReach :: Time -> [(Period, Length)] -> [(Period, Length)]
        filterInReach s = filter (\x -> inReach s x && matchesPhase s x)
        
        -- returns true if  onset o lies in reach the meter starting at s
        -- with period p and length l
        inReach :: Time -> (Period, Length) -> Bool
        inReach s (p, l) = o <= meterEnd p l s
        
        -- given an onset and an 'LMeter' returns True if both have the same 
        -- phase, which means that the onset coincides with the grid of the 'LMeter'
        -- matchesPhase :: Time -> LMeter -> Bool
        -- matchesPhase o (LMeter strt per _len) = (o - strt) `mod` per == 0 
        matchesPhase :: Time -> (Period, Length) -> Bool
        matchesPhase s (p, _ ) = (o - s) `mod` p == 0

nrOfLMeters :: MeterMap -> Int
nrOfLMeters = M.foldr step 0 where
  
  step :: [(Period, Length)] -> Int -> Int
  step l r = r + length l



data LMeter = LMeter { start   :: Time
                     , period  :: Period
                     , mlength :: Length } deriving (Eq)

instance Show LMeter where
  show (LMeter s p l) = '(' : show s ++ ", " ++ show p ++ ", " ++ show l ++ ")"

type Length = Int
type Period = Int
type Time   = Int
type Weight = Int

                     
-- | Indexing a matrix
getStarts :: [(Time,Length)] -> Length -> [Time] 
getStarts m l = map fst $ filter ((== l) . snd) m 

getLengths :: [(Time,Length)] -> Time -> [Length] 
getLengths m s = map snd $ filter ((== s) . fst) m

-- getLMeters :: LMeters -> Period -> Length -> [LMeter]
-- getLMeters m p l = map (\(o,_) -> LMeter o p l) $ filter ((== l) . snd) (m !! p) 

addLMeter :: [(Time, Length)] -> Period -> Time -> Length -> [(Time,Length)]
addLMeter m p t l | isMaximal (t,l) = addMeter (t,l)
                  | otherwise       = m

  where -- adds a meter to the collection, and removes any meters that are 
        -- included in the added meter
        addMeter :: (Time, Length) -> [(Time, Length)]
        addMeter x = x : filter (\y -> not $ isSubSet y x) m
        
        -- being maximal means not being a subset
        isMaximal :: (Time, Length) -> Bool
        isMaximal x = and $ map (not . isSubSet x) m
        
        -- returns true if the first pair is a meter that is a subset of the
        -- second meter pair
        isSubSet :: (Time, Length) -> (Time, Length) -> Bool
        isSubSet (ta, la) (tb, lb) =             ta    >=            tb 
                                   && meterEnd p ta la <= meterEnd p tb lb
                                           
meterEnd :: Period -> Length -> Time -> Time
meterEnd p len tm = tm + (p * len)
        











