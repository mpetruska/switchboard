module EventLoop
       ( loop
       ) where

import UI.NCurses

import Processes
import Renderer
import SwitchboardModel

flipSwitch :: Switchboard -> IO Switchboard
flipSwitch (board @ Switchboard { switches = sw, selected = si }) = do
    newSwitches <- traverse (uncurry flipSelected) (zip [0..] sw)
    pure $ board { switches = newSwitches }
  where
    runProcess switch On  = Just <$> (executeProcess $ offCommand switch)
    runProcess switch Off = Just <$> (executeProcess $  onCommand switch)
    runProcess _      _   = pure Nothing
    performSwitch switch  = do
      sp <- runProcess switch $ state switch
      pure $ switch  { state = flipSwitchState $ state switch
                     , startedProcess = firstJust sp (startedProcess switch) }
    flipSelected i switch
      | i == si   = performSwitch switch
      | otherwise = pure switch
    firstJust x @ (Just _) _ = x
    firstJust _            y = y

update :: Switchboard -> IO Switchboard
update (board @ Switchboard { switches = sw }) = do
    newSwitches <- traverse updateSwitch sw
    pure $ board { switches = newSwitches }

selectNext :: Switchboard -> Switchboard
selectNext sw
    | (selected sw) + 1 < (toInteger $ length $ switches sw) = sw { selected = (selected sw) + 1 }
    | otherwise                                              = sw

selectPrevious :: Switchboard -> Switchboard
selectPrevious sw
    | (selected sw) > 0 = sw { selected = (selected sw) - 1 }
    | otherwise         = sw

toggleLog :: Switchboard -> Switchboard
toggleLog s = s { logExtended = not (logExtended s) }

handleEvent :: Maybe Event -> Switchboard -> IO (Maybe Switchboard)
handleEvent (Just (EventCharacter 'q'))            = const $ pure Nothing
handleEvent (Just (EventCharacter 'l'))            = pure . Just . toggleLog
handleEvent (Just (EventCharacter ' '))            = (<$>)  Just . flipSwitch
handleEvent (Just (EventSpecialKey KeyEnter))      = (<$>)  Just . flipSwitch
handleEvent (Just (EventSpecialKey KeyLeftArrow))  = (<$>)  Just . flipSwitch
handleEvent (Just (EventSpecialKey KeyRightArrow)) = (<$>)  Just . flipSwitch
handleEvent (Just (EventSpecialKey KeyUpArrow))    = pure . Just . selectPrevious
handleEvent (Just (EventSpecialKey KeyDownArrow))  = pure . Just . selectNext
handleEvent _                                      = (<$>)  Just . update

loop :: Window -> Window -> Window -> Colors -> Switchboard -> IO ()
loop w blw logw colors switchboard = do
    ev <- runCurses $ catchCurses (do
      renderWindow w blw logw colors switchboard
      getEvent w $ Just 500) handleCursesError
    result <- handleEvent ev switchboard
    case result of
      Just new -> loop w blw logw colors new
      Nothing  -> pure ()
  where
    handleCursesError = const $ pure Nothing
