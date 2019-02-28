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

update :: Switchboard -> IO (MayChange Switchboard)
update (board @ Switchboard { switches = sw }) = do
    result          <- traverse updateSwitch sw
    let newSwitches =     resultValue <$> result
    let ch          =  or (hasChanged <$> result)
    pure $ if ch
             then changed $ board { switches = newSwitches }
             else unchanged board

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

handleEvent :: Maybe Event -> Switchboard -> IO (Maybe (MayChange Switchboard))
handleEvent (Just (EventCharacter 'q'))            = const $ pure Nothing
handleEvent (Just (EventCharacter 'l'))            = pure . Just .       changed . toggleLog
handleEvent (Just (EventCharacter ' '))            = (<$>)  Just . (<$>) changed . flipSwitch
handleEvent (Just (EventSpecialKey KeyEnter))      = (<$>)  Just . (<$>) changed . flipSwitch
handleEvent (Just (EventSpecialKey KeyLeftArrow))  = (<$>)  Just . (<$>) changed . flipSwitch
handleEvent (Just (EventSpecialKey KeyRightArrow)) = (<$>)  Just . (<$>) changed . flipSwitch
handleEvent (Just (EventSpecialKey KeyUpArrow))    = pure . Just .       changed . selectPrevious
handleEvent (Just (EventSpecialKey KeyDownArrow))  = pure . Just .       changed . selectNext
handleEvent _                                      = (<$>)  Just .                 update

loop :: Window -> RenderingState -> Switchboard -> IO ()
loop w r switchboard = do
    (r', ev) <- runCurses $ catchCurses (do
      r' <- renderWindow r switchboard
      ev <- getEvent w $ Just 500
      pure (r', ev)) handleCursesError
    result <- handleEvent ev switchboard
    case result of
      Just (new, refresh) -> loop w (updateRenderingState ev r' refresh) new
      Nothing             -> pure ()
  where
    handleCursesError = const $ pure (r, Nothing)
    updateRenderingState (Just EventResized) r' _       = setNeedsResize  True    r'
    updateRenderingState _                   r' refresh = setNeedsRefresh refresh r'
