{-# OPTIONS_GHC -Wall                    #-}
module Main (main) where

import System.Console.ParseArgs
import Data.Maybe                     ( catMaybes )

import ZMidi.Score.Datatypes
import ZMidi.Score.Quantise           ( QBins (..) )
import ZMidi.IO.Common                ( mapDir_, mapDir, foldrDir, readQMidiScore )
import ZMidi.IO.IMA                   
import ZMidi.IMA.SelectProfBins       ( selectQBins )
import ZMidi.IMA.Rotations            ( normPriors, Rot, RPrior )
import ZMidi.IMA.Internal             ( initMapTSMap )
import ZMidi.IMA.GTInfo               ( GTInfo, maybeReadGT )
import ReadPDF                        ( readPDFs )
import ZMidi.IMA.GA                   ( runGA )
import Data.Map.Strict                ( Map, empty, toList )
import qualified Data.Map.Strict as M ( map )
import Control.Concurrent.ParallelIO  ( stopGlobalPool )
--------------------------------------------------------------------------------
-- Commandline argument parsing
--------------------------------------------------------------------------------
data MyArgs = Mode | InputFilepath | InputDirFilepath | OutFile | OutDir
            | SelProfFilepath | RotationPath | TimeSigArg
            | GTFilepath | FitFilepath
                  deriving (Eq, Ord, Show)

myArgs :: [Arg MyArgs]
myArgs = [
           Arg { argIndex = Mode,
                 argAbbr  = Just 'm',
                 argName  = Just "mode",
                 argData  = argDataRequired "mode" ArgtypeString,
                 argDesc  = "The operation mode (ga-rot|test|ima|profile|\n" ++
                  "                              store-ima|store-prof|csv-prof)"
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
         , Arg { argIndex = FitFilepath,
                 argAbbr  = Just 'p',
                 argName  = Just "fit",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Input json file with the mixture model parameters"
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
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Input json file specifying the IMA profile bins"
               }
         , Arg { argIndex = GTFilepath,
                 argAbbr  = Just 'g',
                 argName  = Just "gt",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Ground-Truth file"
               }
         , Arg { argIndex = RotationPath,
                 argAbbr  = Just 'r',
                 argName  = Just "rot",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "A JSON file specifying the meter & rotation priors"
                }             
         ]

-- representing the mode of operation
data Mode = CSV | Test | Prof | StoreProf | IMA | GARot | SelBin 
          | TrainRot deriving (Eq)

-- | An optional argument that is required by certain program modes
getOptReq :: ArgType a => Args MyArgs -> MyArgs -> String -> (a -> IO b) -> IO b
getOptReq arg a s f = maybe (usageError arg s) f $ getArg arg a

-- Run from CL
main :: IO ()
main = do arg <- parseArgsIO ArgsComplete myArgs
          -- check whether we have a usable mode
          let mode   = case (getRequiredArg arg Mode) of
                         "csv-prof"   -> CSV
                         "test"       -> Test
                         "profile"    -> Prof
                         "store-prof" -> StoreProf
                         "ima"        -> IMA
                         "ga-rot"     -> GARot
                         "select-bin" -> SelBin
                         "train-rot"  -> TrainRot
                         m         -> usageError arg ("unrecognised mode: " ++ m)
              
              -- get parameters
              out = getRequiredArg arg OutFile    :: FilePath
              od  = getRequiredArg arg OutDir     :: FilePath
                            
              -- the input is either a file (Left) or a directory (Right)
              input :: Either FilePath FilePath
              input = case ( getArg arg InputFilepath
                           , getArg arg InputDirFilepath ) of
                        -- we have two ways of identifying a file: by filename
                       (Just f , Nothing) -> Left f
                        -- or by basepath and id
                       (Nothing, Just d ) -> Right d
                       _                  -> usageError arg "Invalid filepaths" 
              
              r' = getOptReq arg RotationPath "no rotation file found" 
                     readJSON :: IO (Map TimeSig [(Rot, RPrior)])
              p' = getOptReq arg FitFilepath "no GMM fit file found" readPDFs
              s' = getOptReq arg SelProfFilepath "no IMA Profile bin selection file found" 
                     readJSON :: IO (Map TimeSig [(Beat, BeatRat)])

          g <- maybeReadGT $ getArg arg GTFilepath :: IO (Maybe [GTInfo])
          
          -- do the parsing magic
          case (mode, input) of
            (CSV  , Left  f) -> s' >>= \s -> exportCSVProfs s out f
            (CSV  , Right d) -> do s <- s'
                                   writeCSVHeader s out >> mapDir_ (exportCSVProfs s out) d
       
            (Test , Left  f) -> do pHeader ; r <- r' ; p <- p' ; s <- s'
                                   readMatchPutLn PRot s p r g f >> return ()
            (Test , Right d) -> do pHeader ; r <- r' ; p <- p' ; s <- s'
                                   x <-mapDir (readMatchPutLn PFile s p r g) d 
                                   printMatchAgr . concat . catMaybes $ x
            
            (StoreProf, Left  f) -> exportNSWPStore od f
            (StoreProf, Right d) -> mapDir_ (exportNSWPStore od) d
            
            (IMA  , Left  f) -> readQMidiScore f >>= printIMA
            (IMA  , Right _) -> usageError arg "We can only analyse a file"
            
            (Prof , Left  f) -> do s <- s'
                                   readNSWPStoreGeneric f >>= either error (analyseProfile s)
            (Prof , Right _) -> usageError arg "We can only profile a file" 
            
            (GARot, Left  _) -> usageError arg "We can only evolve on a directory"
            (GARot, Right d) -> do p <- p' ; s <- s' 
                                   runGA (QBins 12) s p out d  
          
            (SelBin,Right d) -> foldrDir selectMaxWeightBins empty d 
                                   >>= writeJSON out . selectQBins 8
            (SelBin,Left  _) -> usageError arg "We need a directory to select the heaviest bins"

            (TrainRot,Right d) -> do p <- p' ; s <- s' 
                                     r <- foldrDir (trainRotPrior s p) initMapTSMap d 
                                     writeJSON out . normPriors . M.map toList $ r
            (TrainRot,Left  _) -> usageError arg "We need a directory to train the rotation priors"
            
          stopGlobalPool -- required for using parallel-IO

       
