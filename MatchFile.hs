{-# OPTIONS_GHC -Wall                #-}
module MatchFile ( -- | * Retrieving Meta data (after creating a matched corpus)
                   getRTCMeta 
                   -- | * Creating a matched corpus
                   -- | ** Reading
                 , readRTCMidis
                 , readRTCMidiPath
                   -- | ** Printing & Copying
                 , printMatch 
                 , copyRTCMidi
                   -- | ** Matching
                 , match
                 , matchAll
                 ) where

-- import Data.Array
import Data.Tuple        ( swap )
import Data.List         ( stripPrefix, sortBy, groupBy, intercalate
                         , delete )
import Data.Text         ( unpack, pack, strip, breakOn )
import qualified Data.Text as T ( filter)
import Data.Char         ( toLower )
import Data.Maybe        ( catMaybes, isJust, fromJust )
import Data.Function     ( on )

import Control.Arrow     ( second )
import Control.Monad     ( when )

import RTCParser
import MidiCommonIO      ( mapDir, mapDirInDir )
import ZMidi.Core        ( readMidi )
import ZMidiBasic        ( MidiScore (..), midiFileToMidiScore, TimeSig (..), Timed (..)
                         , GridUnit, ShortestNote, nrOfNotes, quantiseDev )

import RagPat            ( getPercTripGridOnsets, hasValidGridSize)
                         
import System.Directory  ( copyFile, createDirectoryIfMissing, doesFileExist )
import System.FilePath   ( (</>), (<.>), splitDirectories, takeFileName
                         , isPathSeparator, takeDirectory, makeValid )
import Control.Monad.Trans.State.Strict ( evalStateT, StateT (..), get, modify )
import Control.Monad.IO.Class    ( liftIO )
 
--------------------------------------------------------------------------------
-- A Datatype for matching midifiles to the ragtime compendium
--------------------------------------------------------------------------------

selectRTCMidi :: (RTC, Maybe RTCMidi) -> Bool
selectRTCMidi (rtc, mm) = 
  case mm of
    Nothing -> False
    Just m  ->    isValidYear      (year rtc)
               && isValidTimeSig   (rtcTimeSig m)
               && isValidDeviation (gridUnit m) (avgDeviation m)
 
-- | Is the 'TimeSig'natur a meter we can use for analysis?
isValidTimeSig :: [Timed TimeSig] -> Bool
isValidTimeSig [ts] = case getEvent ts of 
                        -- there should be one valid time signature
                        (TimeSig 4 4 _ _) -> True 
                        (TimeSig 2 4 _ _) -> True
                        (TimeSig 2 2 _ _) -> True
                        _                 -> False 
isValidTimeSig _    = False
 
-- | Is the quantisation deviation small enough for analysis?
isValidDeviation :: GridUnit -> Float -> Bool
isValidDeviation gu d = (d / fromIntegral gu) < 0.01
--------------------------------------------------------------------------------
-- A Datatype for matching midifiles to the ragtime compendium
--------------------------------------------------------------------------------
                         
-- | Representing a Midi file path
data RTCMidi = RTCMidi { baseDir      :: FilePath
                       , folder       :: RTCFolder 
                       , fileName     :: String
                       -- some properties of the midifile 
                       -- (We don't store the complete midi files to save space)
                       , nrOfVoices   :: Int
                       , rtcTimeSig   :: [Timed TimeSig]
                       , tripletPerc  :: Double
                       , validGrid    :: Bool
                       , avgDeviation :: Float
                       , gridUnit     :: GridUnit
                       } deriving (Show, Eq)
 
-- compare the midi file based on statistics that might reflect the quality 
-- of the data: a) does it has a meter, b) how many different note lengths
-- do the files have (less is better = smaller), c) how many tracks do the files
-- have
compareRTCMidi :: RTCMidi -> RTCMidi -> Ordering
compareRTCMidi a b =  case compare (tripletPerc a) (tripletPerc b) of
                        EQ  -> compare (nrOfVoices a) (nrOfVoices b) 
                        x   -> x

 -- | Prints a match between a compendium entry and a midifile
printMatch :: (RTC, Maybe RTCMidi) -> String
printMatch (r,m) = let l  = [ show (rtcid r)   , show (title r)
                            , show (folders r) , show (year  r)
                            , approxYear (year r) ]
                       l' = case m of
                              Just p  -> l ++ [ show (selectRTCMidi (r,m))
                                              , show (nrOfVoices    p)
                                              , show (rtcTimeSig    p)
                                              , show (tripletPerc   p)
                                              , show (tripletPerc   p <= 0.01)
                                              , show (validGrid     p)
                                              , show (avgDeviation  p)
                                              , show (gridUnit      p)
                                              , show (avgDeviation  p / 
                                                     (fromIntegral . gridUnit $ p))
                                              , show ((avgDeviation p / 
                                                     (fromIntegral . gridUnit $ p)) <= 0.1)
                                              , show (fileName      p) ]
                              Nothing -> l ++ ["n/a\tn/a\tn/a\tn/a\tn/a\tn/a\tn/a\tn/a\tn/a\tn/a\tn/a\tno matching file found"]
                   in intercalate "\t" l'

--------------------------------------------------------------------------------
-- IO stuff
--------------------------------------------------------------------------------

-- copies a matched midi file if it has a matching compendium entry
copyRTCMidi :: FilePath -> (RTC, Maybe RTCMidi) -> IO ()
copyRTCMidi newBase m@(rtc, mfrom) 
  | selectRTCMidi m = 
      do let from = fromJust mfrom
             to = toPath $ from { baseDir  = newBase
                                , fileName = makeValid ((unpack . strip . title $ rtc) <.> ".mid")}
             fr = toPath from
         createDirectoryIfMissing True . takeDirectory $ to
         e <- doesFileExist fr
         if e then do copyFile fr to
                      putStrLn ("created\t" ++ printMatch m ++ "\t"++ to) 
              else putStrLn ("N.B. File does not exist: " ++ fr)
  -- Here we select if a match between a compendium entry and a midifile
  | otherwise = putStrLn ("ignored\t" ++ printMatch m)
  
  


readRTCMidis :: ShortestNote -> FilePath -> IO [RTCMidi]
readRTCMidis sn d =   mapDirInDir (mapDir (readRTCMidiPath sn d)) d 
                  >>= return . catMaybes . concat

-- reads a 'RTCMidi' given a base directory and a path to the file
readRTCMidiPath :: ShortestNote -> FilePath -> FilePath -> IO (Maybe RTCMidi)
readRTCMidiPath sn bd fp = -- Path conversions
  case stripPrefix (bd </> "") fp of
    Nothing -> error ("basedir and filepath do not match " ++ bd ++" /=  " ++ fp)
    Just f  -> let s = head . dropWhile (isPathSeparator . head) . splitDirectories $ f 
               in  case lookup s folderMapSwap of 
                     Nothing   -> error ("folder not found" ++ show s ++ " in " ++ fp)
                     Just rtcf -> do m <- readMidi fp 
                                     case m of
                                       Left  _ -> return Nothing -- ignore erroneous files
                                       Right x -> let r  = toRTCMidi rtcf bd fp sn (midiFileToMidiScore x)
                                                  in  r `seq` (return . Just $ r)

-- | Strictly extract information needed for the selection of Midifiles from
-- a 'MidiScore'.
toRTCMidi :: RTCFolder -> FilePath -> FilePath -> ShortestNote -> MidiScore 
          -> RTCMidi
toRTCMidi rtcf bd fp sn ms = 
  let n          = length . getVoices $ ms  
      t          = getTimeSig ms
      g          = hasValidGridSize ms
      p          = if g then getPercTripGridOnsets ms else -1
      (_, d, gu) = quantiseDev sn ms
      x          = fromIntegral . nrOfNotes $ ms
      r          = RTCMidi bd rtcf (takeFileName fp) n t p g (fromIntegral d / x) gu
  in n `seq` t `seq` p `seq` g `seq` p `seq` d `seq` gu `seq` x `seq` r

-- | returns the filepath of the RTCMidi
toPath :: RTCMidi -> FilePath
toPath rtcf = case lookup (folder rtcf) folderMap of
  Just f  -> baseDir rtcf </> f </> fileName rtcf
  Nothing -> error ("folder not found" ++ show rtcf)


--------------------------------------------------------------------------------
-- Matching Filenames
--------------------------------------------------------------------------------

-- | Returns a subset of the compendium based on pre-annotated folders
getSubSet :: [RTCFolder] -> [(RTCFolder, [RTCMidi])] -> [RTCMidi]
getSubSet fs = concat . map snd . filter ((flip elem) fs . fst) 

-- State to keep track of the unmatched compendium entries
type MidiSt = ([RTCMidi],[(RTCFolder, [RTCMidi])])

matchAll :: ((RTC, Maybe RTCMidi) -> IO a) -> [RTCMidi] -> [RTC] -> IO [a]
matchAll f ms rs = evalStateT (mapM g rs) (ms, groupRTCMidis ms) where
  
  -- g :: RTC -> StateT MidiSt IO a
  g x = match x >>= liftIO . f 

  -- | groups the files based on 
  groupRTCMidis :: [RTCMidi] -> [(RTCFolder, [RTCMidi])]
  groupRTCMidis m = let grp = groupBy ((==) `on` folder) m
                        ixs = map (folder . head) grp
                     in zip ixs grp

-- | matches 'RTCMidi's to an 'RTC' meta data entry
match ::  RTC -> StateT MidiSt IO (RTC, Maybe RTCMidi)
match r = do (ms, grp) <- get 
             let m = case folders r of
                       [] -> doMatch ms -- match against the complete corpus
                       f  -> case getSubSet f grp of -- or against a subdir
                               [] -> doMatch ms      -- if it's not empty
                               s  -> doMatch s
             -- delete the match from the corpus when it is a good match
             when (isJust . snd $ m) (modify (deleteMidi (fromJust . snd $ m)))
             return m where

  -- | Deletes an 'RTCMidi' from the 'MidiSt', this ensures we cannot match
  -- the same 'RTCMidi' twice.
  deleteMidi :: RTCMidi -> MidiSt -> MidiSt
  deleteMidi rtc (l, tab) = (delete rtc l, map (second (delete rtc)) tab)
               
  -- | returns a match      
  doMatch :: [RTCMidi] -> ((RTC, Maybe RTCMidi))
  doMatch subs = -- filter the matches
                 case filter snd . map (matchFN r) $ subs of
                 -- if there are no matches
                    []  -> (r, Nothing)
                 -- else resort the top of the list with a zero distance based 
                 -- on midi file statistics and pick the top file
                    top -> second Just . head . sortBy (compareRTCMidi `on` snd) 
                                       . map fst $ top

  
-- | Matches a filename
matchFN :: RTC -> RTCMidi -> ((RTC, RTCMidi), Bool)
matchFN rtc mid = ( (rtc, mid)
                  , caseInsStrMatch (preProcRTC rtc) (preProcRTCMidi mid))
  where -- preprocessing
    preProcRTCMidi :: RTCMidi -> String
    preProcRTCMidi = unpack . T.filter wanted . fst . breakOn (pack " - ") 
                   . pack   . fileName 

    preProcRTC :: RTC -> String
    preProcRTC = unpack . T.filter wanted . title
    
    wanted :: Char -> Bool
    wanted c = not (c `elem` ",.\'\"-_ \t?!()[]`{}&~" )
               
        
caseInsStrMatch :: String -> String -> Bool
caseInsStrMatch [] []         = True
caseInsStrMatch [] _          = False
caseInsStrMatch _  []         = False
caseInsStrMatch (x:xs) (y:ys) = x `caseInsEQ` y && caseInsStrMatch xs ys

        
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
        