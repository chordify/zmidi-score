module RScript where
import System.IO            ( Handle, hGetContents )
import System.Process 


-- | Takes a home directory, an R script to run, and the function to call.
-- 'runRScript' will return @(mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph)@
runRScript :: FilePath -> FilePath -> String 
           -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) 
runRScript home src func =
  createProcess (shell "Rscript --vanilla --verbose -e \"source('D:\\\\R\\\\r-scripts\\\\FitGaus.R')\" -e \"fitAll('D:\\\\R\\\\train.barnorm.sqr.smth.log.8.csv', 'D:\\\\test.haskell.json')\""
  -- createProcess (proc "Rscript" -- --vanilla --verbose -e \"source('D:\\\\R\\\\r-scripts\\\\FitGaus.R')\" -e \"fitAll('D:\\\\R\\\\train.barnorm.sqr.smth.log.8.csv', 'D:\\\\test.haskell.json')\""
                                -- [ "--vannila"
                                -- , "-e \"source('" ++ src ++ "')\""
                                -- , "-e \"" ++ func ++ "\""         
                                -- ] 
                                ) { -- cwd = Just home 
                                     std_out = CreatePipe 
                                   , std_err = CreatePipe 
                                   }