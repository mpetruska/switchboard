module Data.SwitchboardModel
       ( flipSwitchState
       , progressSwitchState
       , SwitchState(..)
       , Switch(..)
       , Switchboard(..)
       ) where

data SwitchState = Off
                 | SwitchingOn
                 | On
                 | SwitchingOff

data Switch = Switch { text        :: String
                     , state       :: SwitchState
                     , logText     :: String
                     , logExtended :: Bool
                     }

data Switchboard = Switchboard { switches :: [Switch]
                               , selected :: Integer
                               }

flipSwitchState :: SwitchState -> SwitchState
flipSwitchState Off = SwitchingOn
flipSwitchState On  = SwitchingOff
flipSwitchState x   = x

progressSwitchState :: SwitchState -> SwitchState
progressSwitchState SwitchingOn  = On
progressSwitchState SwitchingOff = Off
progressSwitchState x            = x
