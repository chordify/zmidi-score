{-# OPTIONS_GHC -Wall                   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LocalMeter where

import Data.List                  ( intercalate, sort )
import Data.IntMap                ( IntMap, insertWith, mapWithKey 
                                  , split, fromList, foldrWithKey )
import qualified Data.IntMap as M ( foldr )

import Debug.Trace
import Test.QuickCheck

test :: MeterMap
test = fromList [(1, [(1,2), (2,4)]), (3, [(1,4), (2,2)]), (4, [])]

-- a, b, c, d, e :: LMeter
-- a = LMeter (Time 2) (Period 2) (Len 4)
-- b = LMeter (Time 1) (Period 2) (Len 3)
-- c = LMeter (Time 2) (Period 3) (Len 3)
-- d = LMeter (Time 5) (Period 3) (Len 3)
-- e = LMeter (Time 2) (Period 2) (Len 2)

-- l = [a,b,c,d,e]
-- l = [d,e]

-- doTest :: Show a => (LMeter -> LMeter -> a) -> IO ()
-- doTest f = mapM_ (\x -> mapM_ (pprint f x) l) l

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

{-
-- Returns 
--
-- * 'LT' if the first argument is a subset of the second argument, 
-- * 'GT' if the first argument is a superset of the second argument, 
-- * 'EQ' if the two 'LMeters' describe the same set
isSubset :: LMeter -> LMeter -> Ordering
isSubset ma@(LMeter sa pa la) mb@(LMeter sb pb lb) 
  | sa < sb   = GT
  | pa < pb   = GT
  | pa == pb  = if sa == sb then compare la lb
                            else 
  
            case (sa == sb, compare la lb) of 
                   (True , c ) -> c
                   (False, GT) -> GT 
                   (False, _ ) -> LT
  | otherwise = compare (meterEnd' pa la sa) (meterEnd' pb lb sb)


-- given an onset and an 'LMeter' returns True if both have the same 
-- phase, which means that the onset coincides with the grid of the 'LMeter'
matchesPhase :: LMeter -> LMeter -> Bool
-- matchesPhase o (LMeter strt per _len) = (o - strt) `mod` per == 0 
matchesPhase a@(LMeter (Time sa) (Period pa) _la) 
             b@(LMeter (Time sb) (Period pb) _lb) 
               | pa == 1 || pb == 1 = inRange a b
               | d == 1             = matchesPhaseSet a b
               | otherwise          = (sa - sb) `mod` d == 0
                    where d = gcd pa pb
  
inRange :: LMeter -> LMeter -> Bool 
inRange a@(LMeter sa _ _) b@(LMeter sb _ _) 
  | sa <= sb  = meterEnd a >= sb
  | otherwise = meterEnd b >= sa
  
meterEnd :: LMeter -> Time 
meterEnd (LMeter (Time s) (Period p) (Len l)) = Time (s + (l+p))

matchesPhaseSet :: LMeter -> LMeter -> Bool
matchesPhaseSet a b=let -- sa = take 1000 . expMeter $ a 
                        -- sb = take 1000 . expMeter $ b
                        sa = getSet a 
                        sb = getSet b
                    in or . map (\x -> elem x sa) $ sb

-- phaseOffsetSet :: LMeter -> LMeter -> Maybe (Time, Period)
-- phaseOffsetSet a b = let sa = expMeter $ a 
                         -- sb = expMeter $ b
                     -- in case gdc (period a) (period b)
                          -- (Period 1) take 5 $ filter (\x -> elem x sb) sa

-- phaseOffset :: LMeter -> LMeter -> Maybe (Time, Period)
phaseOffset a@(LMeter (Time sa) (Period pa) _la) 
            b@(LMeter (Time sb) (Period pb) _lb) = 
               let d = gcd pa pb
               -- in  quotRem (sa - sb) d
               in  case quotRem (sa - sb) d of
                     -- (d, 0) -> Just (Time (3 * d), Period (pa * pb))
                     (q, 0) -> Just (q * d, d)
                     (_, _) -> Nothing
                    
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
        inReach s (p, l) = o <= meterEnd' p l s
        
        -- given an onset and an 'LMeter' returns True if both have the same 
        -- phase, which means that the onset coincides with the grid of the 'LMeter'
        -- matchesPhase :: Time -> LMeter -> Bool
        -- matchesPhase o (LMeter strt per _len) = (o - strt) `mod` per == 0 
        matchesPhase :: Time -> (Period, Len) -> Bool
        matchesPhase (Time s) (Period p, _ ) = (time o - s) `mod` p ==  0
-}
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
     arbitrary = choose (0,10) >>= return . Time
     
instance Arbitrary LMeter where
     arbitrary = do s <- arbitrary
                    p <- arbitrary
                    l <- arbitrary
                    return (LMeter s p l)

-- pPhase :: LMeter -> LMeter -> Bool
-- pPhase a b = matchesPhase a b == matchesPhaseSet a b

{-                    
-- TODO move to InnerMetricalAnalysis
-- Adds a new local meter to a list of local meters with the same period
addLMeter :: [(Time, Len)] -> Period -> Time -> Len -> [(Time,Len)]
addLMeter m p t l | isMaximal (t,l) && (len l) >= 2 = addMeter (t,l)
                  | otherwise                       = m

  where -- adds a meter to the collection, and removes any meters that are 
        -- included in the added meter
        addMeter :: (Time, Len) -> [(Time, Len)]
        -- addMeter x = x : filter (\y -> not $ isSubSet y x) m
        addMeter x = x : m
        
        -- being maximal means not being a subset
        isMaximal :: (Time, Len) -> Bool
        isMaximal x = and $ map (not . isSubSet x) m
        
        -- returns true if the first pair is a meter that is a subset of the
        -- second meter pair
        isSubSet :: (Time, Len) -> (Time, Len) -> Bool
        isSubSet (ta, la) (tb, lb) =                 ta >=                tb 
                                   && matchPhase     ta                   tb
                                   && meterEnd' p la ta <= meterEnd' p lb tb
        
        p' = period p
        -- Returns True if the two time stamps are in phase
        matchPhase :: Time -> Time -> Bool
        matchPhase (Time ta) (Time tb) = ta `mod` p' == tb `mod` p'

-}        
-- | returns the meter ending 'Time'
meterEnd' :: Period -> Len -> Time -> Time
meterEnd' (Period p) (Len l) (Time t) = Time (t + (p * l))

factors :: Period -> [Period]
factors p = filter (\x -> p `mod` x == 0) [1 .. p]