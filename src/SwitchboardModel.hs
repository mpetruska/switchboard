module SwitchboardModel
       ( flipSwitchState
       , progressSwitchState
       , SwitchState(..)
       , Switch(..)
       , Switchboard(..)
       , updateSwitch
       ) where

import Processes

data SwitchState = Initializing
                 | Off
                 | SwitchingOn
                 | On
                 | SwitchingOff

data Switch = Switch { text           :: String
                     , state          :: SwitchState
                     , startedProcess :: Maybe StartedProcess
                     , onCommand      :: String
                     , offCommand     :: String
                     }

data Switchboard = Switchboard { switches    :: [Switch]
                               , selected    :: Integer
                               , logExtended :: Bool
                               }

flipSwitchState :: SwitchState -> SwitchState
flipSwitchState Off = SwitchingOn
flipSwitchState On  = SwitchingOff
flipSwitchState x   = x

progressSwitchState :: SwitchState -> Bool -> SwitchState
progressSwitchState SwitchingOn  True  = On
progressSwitchState SwitchingOn  False = Off
progressSwitchState SwitchingOff True  = Off
progressSwitchState SwitchingOff False = On
progressSwitchState Initializing True  = On
progressSwitchState Initializing False = Off
progressSwitchState x            _     = x

updateStartedProcess :: Switch -> StartedProcess -> IO Switch
updateStartedProcess sw (sp @ StartedProcess { outcome = Nothing }) = do
    updated <- updateProcess sp
    pure $ (checkOutcome (outcome updated)) { startedProcess = Just updated }
  where
    checkOutcome (Just x) = sw { state = progressSwitchState (state sw) x }
    checkOutcome Nothing  = sw
updateStartedProcess sw _ = pure sw

updateSwitch :: Switch -> IO Switch
updateSwitch (sw @ Switch { startedProcess = Just sp }) = updateStartedProcess sw sp
updateSwitch sw = pure sw
