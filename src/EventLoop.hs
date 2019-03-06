module EventLoop
       ( loop
       ) where

import UI.NCurses

import Processes
import Renderer
import SwitchboardModel

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
