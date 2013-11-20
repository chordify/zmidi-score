{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  InnerMetricalAnalysis
-- Copyright   :  (c) 2012--2013 Utrecht University
-- License     :  LGPL-3
--
-- Maintainer  :  W. Bas de Haas <bash@cs.uu.nl>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: implements the Inner Metrical Analysis model 
-- see: 
--------------------------------------------------------------------------------
module Main -- ( -- * Types for Local Meters
                               -- LMeter (..)
                             -- , Time
                             -- , Weight
                             -- , Period
                             -- , Len
                               -- -- * The Inner Metrical Analysis
                             -- , getLocalMeters
                             -- , getMetricWeight
                             -- , getSpectralWeight
                             -- , normalise
                               -- -- * parameters
                             -- -- , maximumPhase
                             -- -- , phaseStepSize
                             -- , main
                             -- , toList
                             -- )
                             where

import Data.List                  ( foldl' )
import Data.IntMap                ( empty, IntMap, insert, toAscList, mapWithKey )
import qualified Data.IntMap as M ( lookup, foldr )
import Data.Vector                ( Vector, (!) )
import qualified Data.Vector as V ( fromList, length )
import LocalMeter
import InnerMetricalAnalysisOld   ( getLocalMetersOld )

-- import Debug.Trace

type Weight = Int
type MeterMap = IntMap [(Time, Len)]

--------------------------------------------------------------------------------
-- Local Meters
--------------------------------------------------------------------------------

-- | Inner Metric Analysis defines the pulses of a piece of music solely on 
-- the base of note onsets. The model considers all the pulses, called 
-- local metres, that can overlay with each other with very different periods 
-- and shifted at all phases.
getLocalMeters :: Period -> [Time] -> MeterMap
getLocalMeters _   []  = empty
getLocalMeters phs ons = foldl' onePeriod empty 
              -- todo change this into a parameter
              ([phs, (2 * phs) .. (Period (time . last $ ons) `div` 2)]) where
              
   v = V.fromList $ onsetGrid [0 .. last ons] ons
   
   onePeriod :: MeterMap -> Period -> MeterMap
   onePeriod m p = insertMeters facts m p . foldl' oneMeter [] $ ons where
   
     facts = factors p
     
     oneMeter :: [(Time, Len)] -> Time -> [(Time, Len)]     
     oneMeter l t = addLMeter l p t $ getLength v p t
     
-- | Creates a grid of 'Bool's where 'True' represents an onset and 'False'
-- no onset
--
-- >>> onsetGrid [0..10] [2,3,6,7]
-- >>> [False,False,True,True,False,False,True,True,False,False,False]
onsetGrid :: [Time] -> [Time] -> [Bool]
onsetGrid []     []     = []
onsetGrid []     os     = error ("onsetGrid: grid to small for onsets" ++ show os)
onsetGrid gs     []     = replicate (length gs) False
onsetGrid (g:gs) (o:os) | g == o = True  : onsetGrid gs    os
                        | g <  o = False : onsetGrid gs (o:os)
                        | o >  g = error "onsetGrid: non-monotone onsets"
onsetGrid _      _     = error "onsetGrid: non-monotone onsets"


        
getLength :: Vector Bool -> Period -> Time -> Len
getLength v (Period p) o = pred $ project o where

  project :: Time -> Len
  project (Time t) | t < V.length v && v ! t = 1 + project (Time (t + p))
                   | otherwise               = 0
        
pLocalMeter :: Period -> [Time] -> Bool
pLocalMeter p ons = getLocalMetersOld p ons == getLocalMeters p ons

-- Adds a new local meter to a list of local meters with the same period
addLMeter :: [(Time, Len)] -> Period -> Time -> Len -> [(Time,Len)]
addLMeter m p t l | isMax p m p (t,l) && (len l) >= 2 = (t,l) : m
                  | otherwise                         =         m
-- addLMeter m p t l | isMax p m p (t,l) && (len l) >= 2 = trace (show m ++ ": add: " ++ show (p,t,l)) ((t,l) : m)
                  -- | otherwise                         = trace (show m ++ ": no add: " ++ show (p,t,l))        m


insertMeters :: [Period] -> MeterMap -> Period -> [(Time, Len)] -> MeterMap
insertMeters fs m p l = case filter (isMaximal fs m p) l of 
                           [] -> m 
                           x  -> insert (period p) x m

isMaximal :: [Period] -> MeterMap -> Period -> (Time, Len) -> Bool
isMaximal fs m p tl = and . map isMaxInMeterMap $ fs where

  isMaxInMeterMap :: Period -> Bool
  isMaxInMeterMap f = case M.lookup (period f) m of
                         Nothing -> True
                         Just l  -> isMax f l p tl

                            
-- being maximal means not being a subset
isMax :: Period -> [(Time, Len)] -> Period -> (Time, Len) -> Bool
isMax f m  p x = and $ map (not . isSubSet p x f) m
-- isMax f m p x =  traceShow (p,x,a) a where a = and $ map (not . isSubSet p x f) m

-- returns true if the first pair is a meter that is a subset of the
-- second meter pair
-- N.B. precondition: pb > pa
isSubSet :: Period -> (Time, Len) -> Period -> (Time, Len) -> Bool
isSubSet (Period pa) (Time ta, Len la) (Period pb) (Time tb, Len lb) = 
-- isSubSet :: LMeter -> LMeter -> Bool
-- isSubSet (LMeter ta pa la) (LMeter tb pb lb) =
     ta             >= tb             -- starts later
  && ta `mod` pb    == tb `mod` pb    -- has the same phase
  && ta + (la * pa) <= tb + (lb * pb) -- ends earlier


showMeterMap :: MeterMap -> String
showMeterMap = concatMap showPer . toAscList

showPer :: (Int, [(Time, Len)]) -> String
showPer (p, l) = "Period: " ++ show p ++ concatMap (showMeter p) l ++ "\n"

showMeter :: Int -> (Time, Len) -> String
showMeter p (Time t, Len l) = " (onset="++ show t++ " per=" ++ show p ++ " len="++ show l ++ ")"

--------------------------------------------------------------------------------
-- Inner Metrical Analysis Weights
--------------------------------------------------------------------------------

-- | Based on the detection of all local metres in a given piece a metric weight 
-- for each onset is defined that reflects the amount of local metres that 
-- coincide at this onset. Hence, the basic idea is similar. Onsets where many 
-- pulses coincide get a greater weight than onsets where fewer pulses coincide. 
-- Moreover, the intuition modelled in the metric weight is that longer 
-- repetitions should contribute more weight than shorter ones.
getMetricWeight :: Period -> [Time] -> [Weight]
getMetricWeight phs ons = map snd $ getWeight partOfLMeter phs ons ons

-- | The spectral weight is based on the extension of each local metre throughout 
-- the entire piece.
getSpectralWeight :: Period -> [Time] -> [Weight]
getSpectralWeight _ []          = []
getSpectralWeight phs ons@(h:t) = map snd $ getWeight matchPhase phs ons 
                                           [h .. (last t)]


getWeight :: (Time -> Int -> (Time,Len) -> Bool)
          -> Period -> [Time] -> [Time] -> [(Time, Weight)]
getWeight f phs ons grid = 
  let ms = getLocalMeters phs ons 
      
      makePair :: Time -> (Time, Weight)
      makePair o = (o, sumPowers2 . mapWithKey ( filterMetersPer (f o) ) $ ms)
      
  in  map makePair grid


filterMetersPer :: (Int -> (Time,Len) -> Bool)
                -> Int -> [(Time, Len)] -> [(Time, Len)]
filterMetersPer f p = filter (f p) 

-- partOfLMeter :: Time -> Period -> (Time, Length) -> Bool
-- partOfLMeter (Time o) (Period p) (Time t, Len l) = 
partOfLMeter :: Time -> Int -> (Time, Len) -> Bool
partOfLMeter (Time o) p (Time t, Len l) = 
  o >= t &&               -- located after the meter starts
  o <= t + (l * p) &&     -- located before the meter ends
  o `mod` p == t `mod` p  -- matches the phase of the meter

matchPhase :: Time -> Int -> (Time, Len) -> Bool
matchPhase (Time o) p (Time t, _) = o `mod` p == t `mod` p

-- getSpectralWeight' :: Period -> [Time] -> [(Time, Weight)]
-- getSpectralWeight' phs ons = 
  -- let ms = getLocalMeters phs ons 
      
      -- getWeight :: Time -> (Time, Weight)
      -- getWeight o = (o, sumPowers2 . Set.filter (matchesPhase o) $ ms )
 
  -- in map getWeight [head ons, (phs + head ons)  .. last ons]
  
-- given an onset and an 'LMeter' returns True if both have the same 
-- phase, which means that the onset coincides with the grid of the 'LMeter'
-- matchesPhase :: Time -> LMeter -> Bool
-- matchesPhase o (LMeter strt per _len) = (o - strt) `mod` per == 0 

    
-- takes a set of 'LMeter's, takes of every Len the power of 2 and sums
-- the results
sumPowers2 :: MeterMap -> Int
sumPowers2 = M.foldr ((+) . sumPower2Len ) 0 where

 sumPower2Len :: [(Time, Len)] -> Int
 sumPower2Len = sum . map ((^ (2 ::Int)) . len . snd)

-- normalise :: [Weight] -> [Float]
-- normalise ws = let mx = fromIntegral (maximum ws) 
               -- in map (\x -> fromIntegral x / mx) ws

-- testing
main :: IO ()
main = print $ getMetricWeight 1 [6,13,16,17,20,26,28,36,44,49,50,56,57,61,64,66,68,73,75,76,83,85,88,91,93,95,99,105,107,110,116,119,124,129,136,137,145,147,151,158,161,166,174,178,184,188,192,200,201,208,213,219,225,230,231,235,238,240,241,242,246,253,259,261,266,267,275,277,281,284,286,290,298,304,311,317,322,325,330,337,340,341,346,352,355,362,370,378,383,389,393,394,399,405,410,416,423,427,435,439,445,450,456,462,469,471,478,481,487,495,501,508,510,515,519,526,532,535,543,550,557,563,566,571,574,576,580,583,589,595,596,599,604,611,619,620,625,627,631,634,640,647,651,659,664,666,669,674,676,679,681,683,690,692,694,695,697,701,702,707,711,716,719,725,732,737,739,740,747,751,755,757,763,767,770,778,784,792,798,806,811,814,818,824,829,830,831,832,839,840,844,852,856,861,862,864,866,871,873,876,877,882,884,892,898,906,908,913,920,924,925,929,931,937,941,946,949,954,962,970,974,978,983,986,989,995,1002,1008,1012,1017,1019,1027,1029,1035,1042,1050,1055,1060,1061,1067,1073,1079,1081,1083,1091,1098,1105,1108,1112,1118,1121,1128,1132,1135,1143,1147,1150,1157,1160,1168,1174,1177,1185,1191,1198,1199,1204,1207,1214,1221,1222,1223,1231,1239,1243,1244,1251,1253,1259,1260,1262,1265,1270,1278,1281,1282,1285,1286,1293,1299,1303,1306,1311,1313,1317,1323,1326,1328,1332,1338,1345,1351,1352,1359,1361,1367,1372,1373,1378,1384,1392,1400,1408,1416,1422,1428,1436,1443,1450,1457,1458,1462,1468,1476,1477,1482,1487,1490,1491,1497,1502,1504,1510,1511,1513,1521,1526,1533,1534,1540,1543,1548,1555,1558,1561,1565,1573,1581,1587,1591,1598,1605,1607,1611,1616,1620,1628,1636,1641,1648,1652,1658,1665,1667,1668,1673,1678,1680,1687,1693,1698,1706,1708,1713,1721,1726,1731,1732,1737,1742,1748,1755,1761,1769,1770,1771,1778,1781,1784,1788,1789,1790,1793,1795,1802,1809,1813,1819,1826,1831,1839,1846,1849,1857,1858,1861,1868,1876,1878,1885,1888,1891,1893,1900,1905,1912,1914,1919,1920,1923,1930,1938,1940,1944,1951,1955,1959,1963,1965,1968,1976,1978,1982,1984,1991,1997,1998,1999,2005,2013,2014,2018,2023,2030,2038,2044,2048,2052,2054,2056,2064,2070,2076,2078,2084,2090,2095,2099,2100,2107,2113,2115,2116,2124,2132,2137,2140,2145,2150,2157,2165,2167,2175,2183,2189,2197,2201,2203,2209,2217,2222,2228,2231,2234,2238,2241,2244,2249,2255,2263,2271,2274,2277,2284,2285,2286,2290,2297,2300,2302,2304,2305,2313,2318,2325,2333,2341,2349,2356,2364,2367,2370,2377,2378,2379,2380,2386,2392,2394,2398,2405,2411,2418,2419,2423,2431,2436,2438,2444,2446,2454,2460,2466,2469,2473,2481,2487,2491,2496,2498,2503,2511,2512,2514,2518,2525,2532,2534,2537,2542,2543,2550,2555,2558,2563,2570,2572,2576,2583,2584,2592,2599,2606,2614,2619,2626,2631,2633,2636,2642,2643,2650,2652,2659,2666,2668,2672,2674,2676,2680,2685,2686,2691,2697,2703,2707,2709,2713,2721,2727,2728,2732,2733,2740,2744,2748,2751,2759,2764,2772,2778,2783,2791,2796,2800,2803,2809,2814,2821,2825,2832,2840,2841,2849,2851,2855,2856,2863,2864,2871,2877,2884,2892,2897,2903,2908,2916,2919,2925,2933,2940,2942,2947,2951,2956,2958,2964,2966,2970,2971,2974,2975,2980,2981,2986,2992,2997,3005,3008,3011,3018,3023,3024,3025,3032,3035,3040,3041,3043,3047,3054,3055,3061,3064,3072,3077,3079,3082,3090,3092,3094,3096,3099,3107,3115,3120,3127,3128,3130,3132,3139,3147,3148,3155,3158,3165,3171,3179,3186,3194,3196,3204,3210,3214,3220,3221,3224,3230,3233,3234,3236,3239,3247,3254,3259,3261,3264,3266,3270,3272,3277,3284,3289,3297,3299,3300,3305,3310,3312,3317,3320,3328,3334,3335,3343,3344,3345,3349,3355,3361,3367,3374,3380,3384,3388,3393,3394,3395,3398,3402,3407,3415,3422,3427,3432,3438,3442,3450,3453,3461,3462,3470,3473,3480,3487,3488,3491,3493,3501,3502,3505,3509,3517,3524,3531,3538,3539,3545,3549,3555,3559,3564,3569,3574,3581,3584,3585,3589,3592,3595,3600,3607,3608,3614,3622,3627,3634,3636,3639,3643,3651,3652,3657,3665,3672,3679,3686,3693,3700,3707,3714,3721,3723,3725,3727,3729,3730,3734,3738,3740,3745,3752,3755,3761,3767,3772,3779,3785,3793,3796,3802,3810,3818,3823,3830,3834,3836,3844,3848,3851,3853,3857,3859,3863,3867,3872,3874,3880,3885,3893,3898,3902,3909,3911,3912,3916,3917,3920,3922,3929,3934,3941,3944,3949,3955,3960,3965,3969,3975,3981,3985,3990,3993,3998,3999,4006,4011,4015,4019,4020,4021,4024,4025,4033,4036,4037,4042,4048,4050,4054,4056,4063,4068,4069,4075,4076,4084,4089,4096,4104,4107,4109,4116,4123,4125,4127,4133,4137,4144,4151,4152,4153,4154,4160,4166,4171,4173,4174,4177,4182,4189,4192,4198,4201,4205,4210,4213,4218,4221,4225,4226,4228,4235,4239,4241,4242,4245,4248,4252,4253,4256,4264,4270,4277,4285,4288,4296,4304,4311,4312,4314,4319,4324,4328,4329,4334,4337,4342,4350,4353,4355,4363,4367,4371,4377,4384,4390,4392,4396,4399,4400,4401,4405,4409,4416,4421,4429,4432,4434,4438,4439,4441,4448,4451,4453,4456,4461,4467,4471,4473,4477,4479,4485,4493,4501,4502,4506,4512,4514,4515,4517,4521,4522,4529,4530,4532,4539,4542,4548,4556]
-- main = print $ getMetricWeight 1 [0..500]
-- main = do ons <- randomOnsets 1000 
          -- print (map time ons)
          -- print (getMetricWeight 1 ons)
               
               
jmrEx :: [Time]
jmrEx = [0,1,2,6,8,9,10,14,16,17,18,22,24,25,26,30]
-- metric weight should be: [17,13,65,57,25,21,65,57,33,21,65,57,25,13,65,57]
