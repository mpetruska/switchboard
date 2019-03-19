{-# LANGUAGE TemplateHaskell #-}

module Configuration
       ( Arguments(..)
       , loadSwitches
       , SwitchConfiguration(..)
       ) where

import Control.Monad.Trans.Class  ( lift )
import Control.Monad.Trans.Except ( ExceptT(ExceptT), runExceptT, withExceptT )
import Data.Aeson.TH              ( defaultOptions, deriveJSON )
import Data.Yaml                  ( decodeFileEither, prettyPrintParseException )

import Processes                  ( executeProcess )
import SwitchboardModel           ( mkSwitch, mkSwitchboard, Switch, Switchboard )

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
    pure $ mkSwitch  (title c) sp (on c) (off c)

createSwitchboard :: SwitchboardConfiguration -> IO Switchboard
createSwitchboard c = do
    sw <- traverse createSwitch (switches c)
    pure $ mkSwitchboard sw

loadSwitches :: Arguments -> IO (Either Error Switchboard)
loadSwitches a = runExceptT $ withExceptT prettyPrintParseException $
    (ExceptT $ decodeFileEither (filename a)) >>= lift . createSwitchboard
