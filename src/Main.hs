module Main (main) where

import System.Console.ParseArgs
import EncodeNSWProf
import ZMidi.Score.Datatypes          ( TimeSig (..))
import ZMidi.Score.Quantise           ( ShortestNote (..) )
import ZMidi.IO.Common                ( mapDir_, readQMidiScoreSafe )
import ZMidi.IO.IMA                   ( printIMA, convertToIMA )
import ZMidi.IMA.SelectProfBins       ( selectQBins, Rot (..) )
import ZMidi.IMA.NSWProf              ( readNSWProf )


--------------------------------------------------------------------------------
-- Commandline argument parsing
--------------------------------------------------------------------------------
data MyArgs = Mode | InputFilepath | InputDirFilepath | OutFilepath 
            | SelProfFilepath | NrProfBins | RotationArg | TimeSigArg
                  deriving (Eq, Ord, Show)

myArgs :: [Arg MyArgs]
myArgs = [
           Arg { argIndex = Mode,
                 argAbbr  = Just 'm',
                 argName  = Just "mode",
                 argData  = argDataRequired "mode" ArgtypeString,
                 argDesc  = "The operation mode (train|test|analyse|select)"
               }
        ,  Arg { argIndex = OutFilepath,
                 argAbbr  = Just 'o',
                 argName  = Just "out",
                 argData  = argDataDefaulted "filepath" ArgtypeString "out.csv",
                 argDesc  = "Output directory for the profile files"
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
         , Arg { argIndex = NrProfBins,
                 argAbbr  = Just 'b',
                 argName  = Just "bins",
                 argData  = argDataDefaulted "integer" ArgtypeInt 8,
                 argDesc = "The number of profile bins to be matched"
                }
         , Arg { argIndex = RotationArg,
                 argAbbr  = Just 'r',
                 argName  = Just "rot",
                 argData  = argDataDefaulted "integer" ArgtypeInt 0,
                 argDesc  = "The rotation of the spectral weight profile"
                }
         , Arg { argIndex = TimeSigArg,
                 argAbbr  = Just 't',
                 argName  = Just "ts",
                 argData  = argDataOptional "TimeSig" ArgtypeString,
                 argDesc  = "The time signature to be used in the analysis"
                }               
         ]

-- representing the mode of operation
data Mode = Train | Test | Profile | Store | IMA deriving (Eq)

parseTimeSig :: Args MyArgs -> String -> TimeSig
parseTimeSig arg s = case s of 
                       "4/4" -> TimeSig 4 4 0 0
                       "2/4" -> TimeSig 2 4 0 0
                       "2/2" -> TimeSig 2 2 0 0
                       "3/4" -> TimeSig 3 4 0 0
                       "6/8" -> TimeSig 6 8 0 0
                       _     -> usageError arg ("Unknown time signature: " ++ s)

-- Run from CL
main :: IO ()
main = do arg <- parseArgsIO ArgsComplete myArgs
          -- check whether we have a usable mode
          let mode   = case (getRequiredArg arg Mode) of
                         "train"   -> Train
                         "test"    -> Test
                         "profile" -> Profile
                         "store"   -> Store
                         "ima"     -> IMA
                         -- "select"  -> Select
                         m         -> usageError arg ("unrecognised mode: " ++ m)
              
              -- get parameters
              out = getRequiredArg arg OutFilepath
              b   = getRequiredArg arg NrProfBins
              r   = Rot $ getRequiredArg arg RotationArg
              ts  = getArg arg TimeSigArg >>= return . parseTimeSig arg
              
              -- the input is either a file (Left) or a directory (Right)
              input = case ( getArg arg InputFilepath
                           , getArg arg InputDirFilepath ) of
                        -- we have two ways of identifying a file: by filename
                       (Just f , Nothing) -> Left f
                        -- or by basepath and id
                       (Nothing, Just d ) -> Right d
                       _                  -> usageError arg "Invalid filepaths" 
              
          s <- readNSWProf (getRequiredArg arg SelProfFilepath) >>= return . selectQBins b
          -- do the parsing magic
          case (mode, input) of
            (Train, Left  f) -> processMidi s out f
            (Train, Right d) -> writeHeader s out >> mapDir_ (processMidi s out) d
            (Test , Left  f) -> matchIO b r s f
            (Test , Right d) -> mapDir_ (matchIO b r s) d
            (Store, Left  f) -> convertToIMA out f
            (Store, Right d) -> undefined
            (IMA  , Left  f) -> readQMidiScoreSafe FourtyEighth f >>= printIMA . either error id
            (IMA  , Right _) -> usageError arg "We can only analyse a file"
            (Profile, Left  f) -> analyseMidi ts r s f
            (Profile, Right _) -> usageError arg "We can only profile a file"

            -- (Select, _       ) -> print s
            

       