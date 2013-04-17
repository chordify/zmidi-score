{-# OPTIONS_GHC -Wall                #-}
module MatchFile where

import Data.Array
import Data.Tuple        ( swap )
import Data.List         ( stripPrefix, sortBy )
import Data.Text         ( unpack )
import Data.Ord          ( comparing )

import RTCParser


import System.FilePath   ( (</>), splitDirectories, takeFileName
                         , isPathSeparator, dropExtension )

data RTCMidi = RTCMidi { baseDir  :: FilePath
                       , folder   :: RTCFolder 
                       , fileName :: String
                       } deriving (Show, Eq)
  
readRTCMidiPath :: FilePath -> FilePath -> IO (RTCMidi)
readRTCMidiPath bd fp = return $ fromPath bd fp
  
fromPath :: FilePath -> FilePath -> RTCMidi
fromPath bd fp = 
  case stripPrefix (bd </> "") fp of
    Nothing -> error "basedir and filepath do not match"
    Just f  -> let s = head . dropWhile (isPathSeparator . head) . splitDirectories $ f 
               in  case lookup s folderMapSwap of 
                     Nothing   -> error ("folder not found" ++ show s ++ " in " ++ fp)
                     Just rtcf -> RTCMidi bd rtcf (takeFileName fp)

toPath :: RTCMidi -> FilePath
toPath rtcf = case lookup (folder rtcf) folderMap of
  Just f  -> baseDir rtcf </> f </> fileName rtcf
  Nothing -> error ("folder not found" ++ show rtcf)
  
--------------------------------------------------------------------------------
-- Mapping from RTC Folders to sub directories
--------------------------------------------------------------------------------
                        
  
folderMap :: [(RTCFolder, String)]              
folderMap =
  [(Cowles           , "Cowles"                                           )
  ,(MacDonald        , "MacDonald"                                        )
  ,(PittPayne        , "Pitt-Payne"                                       )
  ,(Reublin          , "Reublin"                                          )
  ,(Watanabe         , "Watanabe"                                         )
  ,(Wilson           , "Wilson"                                           )
  ,(MiscUnks         , "Misc Sequencers, unks"                            )
  ,(MiscSZ           , "Misc Sequencers S-Z excl unks"                    )
  ,(MiscMR           , "Misc Sequencers M-R"                              )
  ,(MiscFL           , "Misc Sequencers F-L"                              )
  ,(MiscAE           , "Misc Sequencers  A-E"                             )
  ,(AddedUniques     , "MIDIs added - Uniques"                            )
  ,(AddedHimpsl      , "MIDIs added - Himpsl uniques"                     )
  ,(AddedOther       , "MIDIs added - Have other renditions"              )
  ,(Mezjuev          , "Mezjuev"                                          )
  ,(Keller           , "Keller"                                           )
  ,(Hiawatha         , "Hiawatha & Radna"                                 )
  ,(Mathew           , "ex Mathew"                                        )
  ,(ElectriClef      , "ElectriClef (Witherwax)"                          )
  ,(Edwards          , "Edwards"                                          )
  ,(Decker           , "Decker, Kermit, Laura, Morgan, MSN, Midibiz & bTd")
  ,(CookieJar        , "Cookie Jar, Melody Lane & Crider"                 )
  ,(BokerTov         , "BokerTov, Bowie & Brunk"                          )
  ,(Crausaz          , "Crausaz & Orton"                                  )
  ,(Intartaglia      , "Intartaglia & ex Intartaglia"                     )
  ,(Treemonisha      , "Treemonisha (Davis & Wilson)"                     )
  ,(Trachtman        , "Trachtman"                                        )
  ,(TrachtmanRolls   , "Trachtman rolls"                                  )
  ,(Pianocorder      , "Pianocorder, PG Music, PianoDisc & Live (need to check Pianocorder titles)" )
  ,(PianocorderCheck , "Pianocorder to check"                             )
  ,(Perry            , "Perry, incl ex Perry"                             )
  ,(OldWeb           , "Old-Web & PRT"                                    )
  ,(ODell            , "O'Dell"                                           )
  ,(Summers          , "Summers"                                          )
  ,(Smythe           , "Smythe"                                           )
  ,(Roache           , "Roache"                                           )
  ,(Ranalli          , "Ranalli"                                          )
  ,(Schwartz         , "Schwartz"                                         )
  ,(OldWeb           , "Old Web & PRT"                                    )
  ]
 
folderMapSwap :: [(String, RTCFolder)]
folderMapSwap = map swap folderMap

--------------------------------------------------------------------------------
-- Matching Filenames
--------------------------------------------------------------------------------

noMatchDist :: Int
noMatchDist = 5

match :: RTC -> [RTCMidi] -> (RTCMidi, RTC)
match rtc ms = case  head . sortBy (comparing snd) . map (matchFN rtc) $ ms of 
  (r, 0) -> r
  (r, s) -> error $ show (r, s)
  

matchFN :: RTC -> RTCMidi -> ((RTC, RTCMidi), Int)
matchFN rtc mid = ((rtc, mid), editDistance (preProcRTC rtc) (preProcRTCMidi mid))

preProcRTCMidi :: RTCMidi -> String
preProcRTCMidi = dropExtension . fileName 

preProcRTC :: RTC -> String
preProcRTC = unpack . title

editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = fromIntegral (table ! (m,n))
    where
    (m,n) = (length xs, length ys)
    x     = array (1,m) (zip [1..] xs)
    y     = array (1,n) (zip [1..] ys)
 
    table :: Array (Int,Int) Int
    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds  = ((0,0),(m,n))
 
    dist (0,j) = j
    dist (i,0) = i
    dist (i,j) = minimum [table ! (i-1,j) + 1, table ! (i,j-1) + 1,
        if x ! i == y ! j then table ! (i-1,j-1) else 1 + table ! (i-1,j-1)]