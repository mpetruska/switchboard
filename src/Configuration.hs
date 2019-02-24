{-# LANGUAGE TemplateHaskell #-}

module Configuration
       ( Arguments(..)
       , loadSwitches
       , SwitchConfiguration(..)
       ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Aeson.TH
import Data.Yaml

import Processes
import SwitchboardModel hiding (switches)

data Arguments = Arguments { filename :: String }

data SwitchConfiguration = SwitchConfiguration { title      :: String
                                               , initialize :: String
                                               , on         :: String
                                               , off        :: String
                                               }

data SwitchboardConfiguration = SwitchboardConfiguration { switches :: [SwitchConfiguration] }

$(deriveJSON defaultOptions 'SwitchConfiguration)
$(deriveJSON defaultOptions 'SwitchboardConfiguration)

type Error = String

createSwitch :: SwitchConfiguration -> IO Switch
createSwitch c = do
    sp <- executeProcess $ initialize c
    pure $ Switch { text           = title c
                  , state          = Initializing
                  , startedProcess = Just sp
                  , onCommand      = on c
                  , offCommand     = off c
                  }

createSwitchboard :: SwitchboardConfiguration -> IO Switchboard
createSwitchboard c = do
    sw <- traverse createSwitch (switches c)
    pure $ Switchboard sw 0 False

loadSwitches :: Arguments -> IO (Either Error Switchboard)
loadSwitches a = runExceptT $ withExceptT prettyPrintParseException $
    (ExceptT $ decodeFileEither (filename a)) >>= lift . createSwitchboard
