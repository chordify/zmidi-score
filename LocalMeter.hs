{-# OPTIONS_GHC -Wall #-}
module LocalMeter where

-- | A matrix is a Vector of Vectors, we could also use one large Vector with
-- another 'ix' function
type LMeters = [(Period, [(Time, Length)])]

type SMeters = [(Time, [(Period, Length)])]

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
        isSubSet (ta, la) (tb, lb) =           ta    >=          tb 
                                   && meterEnd ta la <= meterEnd tb lb
                                           
        meterEnd :: Length -> Time -> Time
        meterEnd len tm = tm + (p * len)
        
indexByStart :: LMeters -> SMeters
indexByStart = undefined