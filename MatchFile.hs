{-# OPTIONS_GHC -Wall                #-}
module MatchFile (readRTCMidis, readRTCMidiPath, match, groupRTCMidis, printMatch ) where

import Data.Array
import Data.Tuple        ( swap )
import Data.List         ( stripPrefix, sortBy, groupBy, intercalate )
import Data.Text         ( unpack, pack, strip, breakOn  )
import Data.Ord          ( comparing )
import Data.Char         ( toLower )
import Data.Function     ( on )

import RTCParser
import MidiCommonIO      ( mapDir, mapDirInDir )

import System.FilePath   ( (</>), splitDirectories, takeFileName
                         , isPathSeparator, dropExtension )

-- | Representing a Midi file path
data RTCMidi = RTCMidi { baseDir  :: FilePath
                       , folder   :: RTCFolder 
                       , fileName :: String
                       } deriving (Show, Eq)
 
readRTCMidis :: FilePath -> IO [RTCMidi]
readRTCMidis d =   mapDirInDir (mapDir (readRTCMidiPath d)) d 
               >>= return . concat
  
readRTCMidiPath :: FilePath -> FilePath -> IO (RTCMidi)
readRTCMidiPath bd fp = return $ fromPath bd fp

-- Path conversions
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
-- Matching Filenames
--------------------------------------------------------------------------------

printMatch :: ((RTC,RTCMidi), Int) -> String
printMatch ((r,m),s) = intercalate "\t" 
  [show (rtcid r), show (title r), show (folders r), show (toPath m), show (s)]


groupRTCMidis :: [RTCMidi] -> [(RTCFolder, [RTCMidi])]
groupRTCMidis m = let grp = groupBy ((==) `on` folder) m
                      ixs = map (folder . head) grp
                   in zip ixs grp

getSubSet :: [RTCFolder] -> [(RTCFolder, [RTCMidi])] -> [RTCMidi]
getSubSet fs = concat . map snd . filter ((flip elem) fs . fst) 
                   
-- | matches 'RTCMidi's to an 'RTC' meta data entry
match :: [RTCMidi] ->  [(RTCFolder, [RTCMidi])] -> RTC -> ((RTC, RTCMidi), Int)
match ms grp r = 

  let doMatch :: [RTCMidi] -> RTC -> ((RTC, RTCMidi), Int)
      doMatch subs r = head . sortBy (comparing snd) . map (matchFN r) $ subs 

  in case folders r of
      [] -> doMatch ms r
      m  -> doMatch (getSubSet m grp) r
  
-- | Matches a filename
matchFN :: RTC -> RTCMidi -> ((RTC, RTCMidi), Int)
matchFN rtc mid = ((rtc, mid), editDistance caseInsEQ (preProcRTC rtc) (preProcRTCMidi mid))

-- preprocessing
preProcRTCMidi :: RTCMidi -> String
preProcRTCMidi = unpack . strip . fst . breakOn (pack " - ") . pack . fileName 

preProcRTC :: RTC -> String
preProcRTC = unpack . strip . title

editDistance :: Eq a => (a -> a-> Bool) -> [a] -> [a] -> Int
editDistance eq xs ys = fromIntegral (table ! (m,n))
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
        if (x ! i) `eq` (y ! j) then table ! (i-1,j-1) else 1 + table ! (i-1,j-1)]

-- case insensitive matching
caseInsEQ :: Char -> Char -> Bool
caseInsEQ a b = toLower a == toLower b
  
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
        