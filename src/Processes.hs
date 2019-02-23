module Processes
       ( executeProcess
       , StartedProcess(..)
       , updateProcess
       ) where

import Prelude hiding (log)
import Control.Exception (catch, IOException)
import GHC.IO.Handle
import System.Exit
import System.Process

data StartedProcess = StartedProcess { handle       :: ProcessHandle
                                     , stdoutHandle :: Handle
                                     , stderrHandle :: Handle
                                     , log          :: String
                                     , outcome      :: Maybe Bool
                                     }

executeProcess :: String -> IO StartedProcess
executeProcess cmd = do
    (_, Just out, Just err, p) <- createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }
    hSetBinaryMode out True
    hSetBinaryMode err True
    pure $ StartedProcess p out err "" Nothing

collectLog :: Handle -> IO String
collectLog h = loop ""
  where
    handleExc :: a -> IOException -> IO a
    handleExc x  = const $ pure x
    loop x       = catch (hWaitForInput h 0) (handleExc False) >>= cont x
    cont x False = pure $ reverse x
    cont x True  = do
      c <- catch (hGetChar h) (handleExc ' ')
      loop (c : x)

updateProcess :: StartedProcess -> IO StartedProcess
updateProcess (p @ StartedProcess { outcome = Just _ }) = pure p
updateProcess (p @ StartedProcess { handle = ph, stdoutHandle = out, stderrHandle = err, log = l }) = do
    outLog <- collectLog out
    errLog <- collectLog err
    m      <- getProcessExitCode ph
    pure $ p { outcome = (==) ExitSuccess <$> m, log = combineLogs outLog errLog }
  where
    combineLogs outLog errLog  = l ++ outLog ++ errLog
