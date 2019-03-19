module SwitchboardModel
       ( flipSwitch
       , flipSwitchState
       , MasterSwitch(..)
       , mkSwitch
       , mkSwitchboard
       , progressSwitchState
       , selectNext
       , selectPrevious
       , SelectedSwitch(..)
       , Switch(..)
       , Switchboard(..)
       , SwitchState(..)
       , update
       , updateSwitch
       ) where

import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Maybe ( MaybeT(..), runMaybeT )
import Data.Foldable             ( foldlM )

import Processes                 ( changed
                                 , executeProcess
                                 , hasChanged
                                 , MayChange
                                 , outcome
                                 , resultValue
                                 , StartedProcess(..)
                                 , unchanged
                                 , updateProcess
                                 )

data SwitchState = Initializing
                 | Off
                 | SwitchingOn
                 | On
                 | SwitchingOff
                 deriving (Eq)

data MasterSwitch = MasterSwitch { isOn               :: Bool
                                 , switchesControlled :: [Integer]
                                 }

data Switch = Switch { text           :: String
                     , state          :: SwitchState
                     , startedProcess :: Maybe StartedProcess
                     , onCommand      :: String
                     , offCommand     :: String
                     }

data SelectedSwitch = MasterSelected
                    | SwitchSelected Integer
                    deriving (Eq)

data Switchboard = Switchboard { masterSwitch :: MasterSwitch
                               , switches     :: [Switch]
                               , selected     :: SelectedSwitch
                               , logExtended  :: Bool
                               }

mkMasterSwitch :: MasterSwitch
mkMasterSwitch = MasterSwitch True []

mkSwitch :: String -> StartedProcess -> String -> String -> Switch
mkSwitch t sp on off = Switch t Initializing (Just sp) on off

mkSwitchboard :: [Switch] -> Switchboard
mkSwitchboard sw = Switchboard mkMasterSwitch sw MasterSelected False

selectNext :: Switchboard -> Switchboard
selectNext sw = next (selected sw) (toInteger $ length $ switches sw)
  where
    next MasterSelected l
      | l > 0     = sw { selected = SwitchSelected 0 }
      | otherwise = sw
    next (SwitchSelected i) l
      | i + 1 < l = sw { selected = SwitchSelected (i + 1) }
      | otherwise = sw

selectPrevious :: Switchboard -> Switchboard
selectPrevious sw = prev (selected sw)
  where
    prev MasterSelected = sw
    prev (SwitchSelected i)
      | i > 0     = sw { selected = SwitchSelected (i - 1) }
      | otherwise = sw { selected = MasterSelected         }

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

performSwitch :: Switch -> IO Switch
performSwitch switch = do
    sp <- runProcess $ state switch
    return $ switch  { state          = flipSwitchState $ state switch
                     , startedProcess = firstJust sp (startedProcess switch) }
  where
    runProcess On  = Just <$> (executeProcess $ offCommand switch)
    runProcess Off = Just <$> (executeProcess $  onCommand switch)
    runProcess _   = pure Nothing
    firstJust x @ (Just _) _ = x
    firstJust _            y = y

mayb :: Bool -> a -> Maybe a
mayb True  x  = Just x
mayb False _  = Nothing

maych :: a -> Maybe a -> MayChange a
maych d = maybe (unchanged d) changed

indicesSwitchedOn :: [Switch] -> [Integer]
indicesSwitchedOn = (<$>) fst . filter (\(_, s) -> state s == On) . zip [1..]

switchOff :: Switch -> IO Switch
switchOff switch
  | state switch == On = performSwitch switch
  | otherwise          = return switch

switchOnByIndices :: [Integer] -> [Switch] -> IO ([Integer], [Switch])
switchOnByIndices is s = do
    (is', s') <- foldlM switchy (is, []) $ zip [1..] s
    return (is', reverse s')
  where
    switchy (indices, sws) (index, sw)
      | index `elem` indices && state sw == Off = do
          sw'          <- performSwitch sw
          let indices' =  filter ((/=) index) indices
          let sws'     =  sw' : sws
          return (indices', sws')
      | otherwise = return (indices, sw : sws)

masterSwitchOff :: Switchboard -> IO Switchboard
masterSwitchOff (board @ (Switchboard (MasterSwitch _ is) sw _ _)) = do
    sw' <- traverse switchOff sw
    return $ board { masterSwitch = MasterSwitch False $ is ++ indicesSwitchedOn sw
                   , switches     = sw'
                   }

masterSwitchOn :: Switchboard -> IO Switchboard
masterSwitchOn (board @ (Switchboard (MasterSwitch _ is) sw _ _)) = do
    (is', sw') <- switchOnByIndices is sw
    return $ board { masterSwitch = MasterSwitch True is'
                   , switches     = sw'
                   }

flipSwitch :: Switchboard -> IO Switchboard
flipSwitch (board @ (Switchboard (MasterSwitch True  _) _ MasterSelected _)) = masterSwitchOff board
flipSwitch (board @ (Switchboard (MasterSwitch False _) _ MasterSelected _)) = masterSwitchOn  board
flipSwitch (board @ (Switchboard (MasterSwitch False _) _ _ _))              = return board
flipSwitch (board @ (Switchboard _ sw (SwitchSelected si) _)) = do
    newSwitches <- traverse (uncurry flipSelected) (zip [0..] sw)
    return $ board { switches = newSwitches }
  where
    flipSelected i switch
      | si == i   = performSwitch switch
      | otherwise = pure switch

updateStartedProcess :: Switch -> StartedProcess -> IO (MayChange Switch)
updateStartedProcess sw (sp @ StartedProcess { outcome = Nothing }) = do
    result      <- updateProcess sp
    let updated =  resultValue result
    let ch      =  hasChanged result
    pure $ if ch
             then changed $ (checkOutcome (outcome updated)) { startedProcess = Just updated }
             else unchanged sw
  where
    checkOutcome (Just x) = sw { state = progressSwitchState (state sw) x }
    checkOutcome Nothing  = sw
updateStartedProcess sw _ = pure $ unchanged sw

updateSwitch :: Switch -> IO (MayChange Switch)
updateSwitch (sw @ Switch { startedProcess = Just sp }) = updateStartedProcess sw sp
updateSwitch sw = pure $ unchanged sw

update :: Switchboard -> IO (MayChange Switchboard)
update (board @ (Switchboard m sw _ _)) =
    maych board <$> runMaybeT updateMaybeT
  where
    anyChange  xs = mayb (or $ hasChanged <$> xs) (resultValue <$> xs)
    mstr False b  = masterSwitchOff b
    mstr True  b  = masterSwitchOn  b
    updateMaybeT  = do
      sw'        <- MaybeT (anyChange <$> traverse updateSwitch sw)
      let board' =  board { switches = sw' }
      lift $ mstr (isOn m) board'
