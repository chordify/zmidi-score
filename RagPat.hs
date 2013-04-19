module Main where

import ZMidi.Core         ( readMidi )
import ZMidiBasic
import MidiCommonIO       ( mapDirInDir, mapDir, readMidiScore )

import System.Environment ( getArgs )
import Data.List          ( intercalate )

main :: IO ()
main = do arg <- getArgs
          case arg of
            ["-d", d] -> print d -- mapDirInDir (mapDir showMidiStats) d
            ["-f", f] -> doScore f
            _         -> putStrLn "usage:  -f <filename> OR -d <directory>"


-- | do stuff with a 'MidiScore' ...
doScore :: FilePath -> IO ()
doScore f = do mf <- readMidiScore f 
               putStrLn . showMidiScore $ mf
               putStrLn . showMidiScore . quantise FourtyEighth $ mf
               

data Onset = X -- | don't care symbol, ignored in all evaluation
           | O -- | No onset
           | I -- | Onset
             deriving Eq
             
instance Show Onset where
  show o = case o of
            X -> "_"
            O -> "O"
            I -> "I"
  showList l s = s ++ (intercalate " " . map show $ l)

type Pattern = [Onset]

match :: Onset -> Onset -> Bool
match X 

untied1 = [ I, I, O, I,  X, X, X, X,  X, X, X, X,  X, X, X, X ]
untied2 = [ X, X, X, X,  I, I, O, I,  X, X, X, X,  X, X, X, X ]
untied3 = [ X, X, X, X,  X, X, X, X,  I, I, O, I,  X, X, X, X ]
untied4 = [ X, X, X, X,  X, X, X, X,  X, X, X, X,  I, I, O, I ]

tied1   = [ I, O, I, I,  O, I, I, O,  X, X, X, X,  X, X, X, X ]
tied2   = [ X, X, X, X,  I, O, I, I,  O, I, I, O,  X, X, X, X ]
tied3   = [ X, X, X, X,  X, X, X, X,  I, O, I, I,  O, I, I, O ]



-- oeq :: Onset -> Onset 
             
-- type Bar    = [Bool] -- This should be a fixed length list
-- type Rhythm = [Bar]




