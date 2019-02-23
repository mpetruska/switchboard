module Processes
       ( executeProcess
       , changed
       , hasChanged
       , MayChange
       , resultValue
       , StartedProcess(..)
       , unchanged
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

type MayChange a = (a, Bool)

changed :: a -> MayChange a
changed x = (x, True)

unchanged :: a -> MayChange a
unchanged x = (x, False)

resultValue :: MayChange a -> a
resultValue = fst

hasChanged :: MayChange a -> Bool
hasChanged = snd

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

updateProcess :: StartedProcess -> IO (MayChange StartedProcess)
updateProcess (p @ StartedProcess { outcome = Just _ }) = pure $ unchanged p
updateProcess (p @ StartedProcess { handle = ph, stdoutHandle = out, stderrHandle = err, log = l }) = do
    outLog             <- collectLog out
    errLog             <- collectLog err
    m                  <- getProcessExitCode ph
    let newLogs        =  l ++ outLog ++ errLog
    let logsChanged    =  not $ and (null <$> [outLog, errLog])
    let newOutcome     =  (==) ExitSuccess <$> m
    let outcomeChanged =  newOutcome /= outcome p
    pure $ if or [logsChanged, outcomeChanged]
             then changed $ p { outcome = newOutcome, log = newLogs }
             else unchanged p
