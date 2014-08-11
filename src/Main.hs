module Main (main) where

import System.Console.ParseArgs
import Data.Maybe                     ( catMaybes )

import ZMidi.Score.Datatypes          ( TimeSig (..))
import ZMidi.Score.Quantise           ( QBins (..) )
import ZMidi.IO.Common                ( mapDir_, mapDir, warning )
import ZMidi.IO.IMA                   ( printIMA, analyseProfile, exportIMAStore
                                      , readIMAScoreGeneric, exportCSVProfs
                                      , writeCSVHeader, printMatchLine
                                      , printMatchAgr, readMatchPutLn, Print (..)
                                      , exportNSWPStore )
import ZMidi.IMA.SelectProfBins       ( selectQBins, Rot (..), QBinSelection
                                      , stdRotations, threePerNum, readJSON )
import ZMidi.IMA.NSWProf              ( readNSWProf )
import ZMidi.IMA.Internal             ( parseTimeSig )
import ZMidi.IMA.TimeSigSeg           ( TimedSeg )
import ZMidi.IMA.RNSWMatch            ( PMatch )
import ZMidi.IMA.MeterGT              ( MeterGT (..), maybeReadGT, setGT )
import ReadPDF                        ( readPDFs )
import ZMidi.IMA.GA                   ( runGA )
import Control.Concurrent.ParallelIO  ( stopGlobalPool )
--------------------------------------------------------------------------------
-- Commandline argument parsing
--------------------------------------------------------------------------------
data MyArgs = Mode | InputFilepath | InputDirFilepath | OutFile | OutDir
            | SelProfFilepath | NrProfBins | RotationPath | TimeSigArg
            | GTFilepath 
                  deriving (Eq, Ord, Show)

myArgs :: [Arg MyArgs]
myArgs = [
           Arg { argIndex = Mode,
                 argAbbr  = Just 'm',
                 argName  = Just "mode",
                 argData  = argDataRequired "mode" ArgtypeString,
                 argDesc  = "The operation mode (train|test|ima|profile|\n" ++
                  "                              store-ima|store-prof|ga-rot)"
               }
        ,  Arg { argIndex = OutFile,
                 argAbbr  = Just 'o',
                 argName  = Just "out-file",
                 argData  = argDataDefaulted "filepath" ArgtypeString "out.csv",
                 argDesc  = "Output file for writing CSV data"
               } 
        ,  Arg { argIndex = OutDir,
                 argAbbr  = Just 'u',
                 argName  = Just "out-dir",
                 argData  = argDataDefaulted "filepath" ArgtypeString "",
                 argDesc  = "Output directory for the IMA files"
               }
         , Arg { argIndex = InputFilepath,
                 argAbbr  = Just 'f',
                 argName  = Just "file",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Input file midi file to analyse"
               }
         , Arg { argIndex = InputDirFilepath,
                 argAbbr  = Just 'd',
                 argName  = Just "dir",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Base directory path to test or train"
               }
         , Arg { argIndex = SelProfFilepath,
                 argAbbr  = Just 's',
                 argName  = Just "prof",
                 argData  = argDataDefaulted "filepath" 
                       ArgtypeString "ragtimeMeterProfilesTrain_2014-03-25.bin",
                 argDesc  = "Base directory path to test or train"
               }
         , Arg { argIndex = GTFilepath,
                 argAbbr  = Just 'g',
                 argName  = Just "gt",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Ground-Truth file"
               }
         , Arg { argIndex = NrProfBins,
                 argAbbr  = Just 'b',
                 argName  = Just "bins",
                 argData  = argDataDefaulted "integer" ArgtypeInt 8,
                 argDesc = "The number of profile bins to be matched"
                }
         , Arg { argIndex = RotationPath,
                 argAbbr  = Just 'r',
                 argName  = Just "rot",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "A JSON file specifying the meter & rotation priors"
                }             
         ]

-- representing the mode of operation
data Mode = Train | Test | Prof | StoreIMA | StoreProf | IMA | GARot deriving (Eq)

parseTimeSigArg :: Args MyArgs -> String -> TimeSig
parseTimeSigArg arg s = either (usageError arg) id $ parseTimeSig s 

-- Run from CL
main :: IO ()
main = do arg <- parseArgsIO ArgsComplete myArgs
          -- check whether we have a usable mode
          let mode   = case (getRequiredArg arg Mode) of
                         "train"      -> Train
                         "test"       -> Test
                         "profile"    -> Prof
                         "store-ima"  -> StoreIMA
                         "store-prof" -> StoreProf
                         "ima"     -> IMA
                         "ga-rot"  -> GARot
                         m         -> usageError arg ("unrecognised mode: " ++ m)
              
              -- get parameters
              out = getRequiredArg arg OutFile    :: FilePath
              od  = getRequiredArg arg OutDir     :: FilePath
              b   = getRequiredArg arg NrProfBins 
                            
              -- the input is either a file (Left) or a directory (Right)
              input :: Either FilePath FilePath
              input = case ( getArg arg InputFilepath
                           , getArg arg InputDirFilepath ) of
                        -- we have two ways of identifying a file: by filename
                       (Just f , Nothing) -> Left f
                        -- or by basepath and id
                       (Nothing, Just d ) -> Right d
                       _                  -> usageError arg "Invalid filepaths" 
                       
              r' = maybe (usageError arg "no rotations found") readJSON 
                 $ getArg arg RotationPath -- :: IO Rotations
              
          s <- readNSWProf (getRequiredArg arg SelProfFilepath) >>= return . selectQBins b
          p <- readPDFs ("prof-fit\\ragtime.fit"++show b++".json" ) -- TODO replace...
          g <- maybeReadGT $ getArg arg GTFilepath :: IO (Maybe [MeterGT [TimeSig]])
          
          -- do the parsing magic
          case (mode, input) of
            (Train, Left  f) -> exportCSVProfs s out f
            (Train, Right d) -> writeCSVHeader s out >> mapDir_ (exportCSVProfs s out) d
       
            (Test , Left  f) -> r' >>= \r -> readMatchPutLn PRot s p r g f >> return ()
            (Test , Right d) -> do r <- r'
                                   x <-mapDir (readMatchPutLn PFile s p r g) d 
                                   printMatchAgr . concat . catMaybes $ x
            
            (StoreIMA, Left  f) -> exportIMAStore od f
            (StoreIMA, Right d) -> mapDir_ (exportIMAStore od) d
            (StoreProf, Left  f) -> exportNSWPStore od f
            (StoreProf, Right d) -> mapDir_ (exportNSWPStore od) d
            
            (IMA  , Left  f) -> readIMAScoreGeneric f >>= either error printIMA
            (IMA  , Right _) -> usageError arg "We can only analyse a file"
            
            (Prof , Left  f) -> readIMAScoreGeneric f >>= either error (analyseProfile 0 s)
            (Prof , Right _) -> usageError arg "We can only profile a file" 
            
            (GARot, Left  _) -> usageError arg "We can only evolve on a directory"
            (GARot, Right d) -> runGA (QBins 12) s p d  
          
          stopGlobalPool -- required for using parallel-IO

       
