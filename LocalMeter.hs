{-# OPTIONS_GHC -Wall #-}
module LocalMeter where

import qualified Data.Vector as V
import Data.Vector ( Vector )
-- import Data.List   ( intercalate )
-- import Text.Printf ( printf, PrintfArg )
import Prelude hiding (sum)

-- | A matrix is a Vector of Vectors, we could also use one large Vector with
-- another 'ix' function
type LMeters = Vector (Vector [Int]) 

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
getOnsets :: LMeters -> Period -> Length -> [Time] 
getOnsets m p l = (m V.! p) V.! l  

getLMeters :: LMeters -> Period -> Length -> [LMeter]
getLMeters m p l = map (\o -> LMeter o p l) $ getOnsets m p l

addLMeter :: LMeters -> Period -> Length -> Time -> LMeters
addLMeter  = undefined

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