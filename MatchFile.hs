{-# OPTIONS_GHC -Wall                #-}
module MatchFile ( readRTCMidis, readRTCMidiPath, match, groupRTCMidis
                 , printMatch, matchAll, copyRTCMidi, getRTCMeta ) where

import Data.Array
import Data.Tuple        ( swap )
import Data.List         ( stripPrefix, sortBy, groupBy, intercalate
                         , delete, find )
import Data.Text         ( unpack, pack, strip, breakOn )
import qualified Data.Text as T ( filter)
import Data.Ord          ( comparing )
import Data.Char         ( toLower )
import Data.Maybe        ( catMaybes )
import Data.Function     ( on )
import Data.IntMap.Lazy  ( size )

import Control.Arrow     ( second )
import Control.Monad.State

import RTCParser
import MidiCommonIO      ( mapDir, mapDirInDir )
import ZMidi.Core        ( readMidi )
import ZMidiBasic        ( MidiScore (..), buildTickMap, midiFileToMidiScore )

import System.Directory  ( copyFile, createDirectoryIfMissing, doesFileExist )
import System.FilePath   ( (</>), (<.>), splitDirectories, takeFileName
                         , isPathSeparator, takeDirectory, makeValid
                         , dropExtension )

 
--------------------------------------------------------------------------------
-- When a matched subset has been created, Meta info can be retrieved with
-- getRTCMeta
--------------------------------------------------------------------------------
                           
getRTCMeta :: [RTC] -> FilePath -> RTC
getRTCMeta rs f = 
  case find ((dropExtension (takeFileName f) ==) 
            . makeValid . unpack . title) rs of
    Just rtc -> rtc
    Nothing  -> error ("No RTC meta data found for file: " ++ f )
 
--------------------------------------------------------------------------------
-- A Datatype for matching midifiles to the ragtime compendium
--------------------------------------------------------------------------------
                         
-- | Representing a Midi file path
data RTCMidi = RTCMidi { baseDir    :: FilePath
                       , folder     :: RTCFolder 
                       , fileName   :: String
                       , hasMeter   :: Bool
                       , nrOfVoices :: Int
                       , nrOfNoteLen:: Int
                       } deriving (Show, Eq)
 
-- comparse the midi file based on statistics that might reflect the quality 
-- of the data: a) does it has a meter, b) how many different note lengths
-- do the files have (less is better = smaller), c) how many tracks do the files
-- have
compareRTCMidi :: RTCMidi -> RTCMidi -> Ordering
compareRTCMidi a b = 
  case (hasMeter a, hasMeter b) of
    (True , False) -> LT
    (False, True ) -> GT
    _              -> case compare (nrOfNoteLen a) (nrOfNoteLen b) of
                        EQ -> compare (nrOfVoices a) (nrOfVoices b)
                        c  -> c
 
 -- | Prints a match between a compendium entry and a midifile
printMatch :: ((RTC,RTCMidi), Int) -> String
printMatch ((r,m),s) = intercalate "\t" 
  [show (rtcid r), show (title r), show (folders r), show (toPath m), show (s)]
 
--------------------------------------------------------------------------------
-- IO stuff
--------------------------------------------------------------------------------

-- copies a matched midi file if the edit distance is smaller than 2
copyRTCMidi :: FilePath -> ((RTC, RTCMidi), Int) -> IO ()
copyRTCMidi newBase m@((rtc, from), d) 
  | d > 2     = putStrLn ("ignored\t" ++ printMatch m)
  | otherwise = do let to = toPath $ from { baseDir  = newBase
                                          , fileName = makeValid ((unpack . strip . title $ rtc) <.> ".mid")}
                       fr = toPath from
                   createDirectoryIfMissing True . takeDirectory $ to
                   e <- doesFileExist fr
                   if e then do copyFile fr to
                                putStrLn ("created\t" ++ printMatch m ++ "\t"++ to) 
                        else putStrLn ("N.B. File does not exist: " ++ fr)

readRTCMidis :: FilePath -> IO [RTCMidi]
readRTCMidis d =   mapDirInDir (mapDir (readRTCMidiPath d)) d 
               >>= return . catMaybes . concat

-- reads a 'RTCMidi' given a base directory and a path to the file
readRTCMidiPath :: FilePath -> FilePath -> IO (Maybe RTCMidi)
readRTCMidiPath bd fp = -- Path conversions
  case stripPrefix (bd </> "") fp of
    Nothing -> error ("basedir and filepath do not match " ++ bd ++" /=  " ++ fp)
    Just f  -> let s = head . dropWhile (isPathSeparator . head) . splitDirectories $ f 
               in  case lookup s folderMapSwap of 
                     Nothing   -> error ("folder not found" ++ show s ++ " in " ++ fp)
                     Just rtcf -> do m <- readMidi fp 
                                     case m of
                                       Left  _ -> return Nothing -- ignore erroneous files
                                       Right x -> do let sc = midiFileToMidiScore x
                                                         vs = getVoices sc
                                                         k  = null . getTimeSig $ sc   -- does it contain a time signature
                                                         l  = length vs                -- how many tracks
                                                         p  = size . buildTickMap $ vs -- how many different durations
                                                         r  = RTCMidi bd rtcf (takeFileName fp)
                                                                      k  l  p
                                                     k `seq` l `seq` p `seq` r `seq` (return . Just $ r)  

-- | returns the filepath of the RTCMidi
toPath :: RTCMidi -> FilePath
toPath rtcf = case lookup (folder rtcf) folderMap of
  Just f  -> baseDir rtcf </> f </> fileName rtcf
  Nothing -> error ("folder not found" ++ show rtcf)


--------------------------------------------------------------------------------
-- Matching Filenames
--------------------------------------------------------------------------------

-- | groups the files based on 
groupRTCMidis :: [RTCMidi] -> [(RTCFolder, [RTCMidi])]
groupRTCMidis m = let grp = groupBy ((==) `on` folder) m
                      ixs = map (folder . head) grp
                   in zip ixs grp

-- | Returns a subset of the compendium based on pre-annotated folders
getSubSet :: [RTCFolder] -> [(RTCFolder, [RTCMidi])] -> [RTCMidi]
getSubSet fs = concat . map snd . filter ((flip elem) fs . fst) 

type MidiSt = ([RTCMidi],[(RTCFolder, [RTCMidi])])

matchAll :: [RTCMidi] -> [(RTCFolder, [RTCMidi])] -> [RTC] -> [((RTC,RTCMidi),Int)]
matchAll ms grp rs = evalState (mapM match rs) (ms, grp)

-- | matches 'RTCMidi's to an 'RTC' meta data entry
match ::  RTC -> State MidiSt ((RTC, RTCMidi), Int)
match r = do (ms, grp) <- get 
             let m = case folders r of
                       [] -> doMatch ms -- match with the complete corpus
                       f  -> doMatch (getSubSet f grp) -- or a subdir
             modify (deleteMidi (snd . fst $ m))       -- delete the match from the corpus
             return m where

  -- | Deletes an 'RTCMidi' from the 'MidiSt' 
  deleteMidi :: RTCMidi -> MidiSt -> MidiSt
  deleteMidi rtc (l, tab) = (delete rtc l, map (second (delete rtc)) tab)
               
  -- | returns a match      
  doMatch :: [RTCMidi] -> ((RTC, RTCMidi), Int)
  doMatch subs = -- create a ranked list based on the edit distance
                 let ranks =  sortBy (comparing snd) . map (matchFN r) $ subs 
                 in case takeWhile ((== 0) . snd) $ ranks of 
                 -- if there are no results with a zero distance return the best
                    []  -> head ranks  
                 -- else resort the top of the list with a zero distance based 
                 -- on midi file statistics and pick the top file
                    top -> head . sortBy (compareRTCMidi `on` (snd . fst)) $ top

  
-- | Matches a filename
matchFN :: RTC -> RTCMidi -> ((RTC, RTCMidi), Int)
matchFN rtc mid = ( (rtc, mid)
                  , editDistance caseInsEQ (preProcRTC rtc) (preProcRTCMidi mid))
  where -- preprocessing
    preProcRTCMidi :: RTCMidi -> String
    preProcRTCMidi = unpack . T.filter wanted . fst . breakOn (pack " - ") 
                   . pack   . fileName 

    preProcRTC :: RTC -> String
    preProcRTC = unpack . T.filter wanted . title
    
    wanted :: Char -> Bool
    wanted c = not (c `elem` ",.\'\"-_ \t?!()[]" )

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
  ,(OldWeb           , "Old Web & PRT"                                    )
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
        