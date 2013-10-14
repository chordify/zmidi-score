{-# OPTIONS_GHC -Wall #-}
module LocalMeter where

import qualified Data.Vector as V
import Data.Vector ( Vector )
-- import Data.List   ( intercalate )
-- import Text.Printf ( printf, PrintfArg )
import Prelude hiding (sum)

-- | A matrix is a Vector of Vectors, we could also use one large Vector with
-- another 'ix' function
type LMeters = Vector [(Time, Length)]

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
getStarts :: LMeters -> Period -> Length -> [Time] 
getStarts m p l = map fst $ filter ((== l) . snd) (m V.! p) 

getLengths :: LMeters -> Period -> Time -> [Length] 
getLengths m p s = map snd $ filter ((== s) . fst) (m V.! p) 

getLMeters :: LMeters -> Period -> Length -> [LMeter]
getLMeters m p l = map (\(o,_) -> LMeter o p l) $ filter ((== l) . snd) (m V.! p) 

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
        

{-
-- | Displaying a matrix
disp :: Show a => Matrix a -> String
disp = disp' dispRow

  where dispRow :: Show a => Vector a -> String
        dispRow = intercalate " " . V.foldr (\j js -> show j : js ) [] 

-- | Displaying a matrix
dispf :: PrintfArg a => Matrix a -> String
dispf = disp' dispRow

  where dispRow :: PrintfArg a => Vector a -> String
        dispRow = concat . V.foldr (\j js -> printf "\t%.2f" j : js ) [] 

-- | Displaying a matrix
disp' :: (Vector a -> String) -> Matrix a -> String
disp' dispRow  = intercalate "\n" . V.foldr (\i is -> dispRow i : is) ["\n"] 

-}