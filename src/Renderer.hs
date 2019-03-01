module Renderer
       ( Colors(..)
       , createColors
       , createWindows
       , mkRenderingState
       , RenderingState
       , renderWindow
       , setNeedsRefresh
       , setNeedsResize
       ) where

import Prelude hiding (log)
import Control.Monad
import Data.List
import Data.Maybe
import UI.NCurses

import Processes
import SwitchboardModel

data Colors = Colors { green  :: ColorID
                     , yellow :: ColorID
                     , red    :: ColorID
                     }

data RenderingState = RenderingState { mainWindow          :: Window
                                     , logBackgroundWindow :: Window
                                     , logForegroundWindow :: Window
                                     , colors              :: Colors
                                     , needsRefresh        :: Bool
                                     , needsResize         :: Bool
                                     }

instance Show RenderingState where
  show r = show (needsRefresh r) <> show (needsResize r)

mkRenderingState :: Window -> Window -> Window -> Colors -> RenderingState
mkRenderingState main logBack logFore clrs = RenderingState main logBack logFore clrs True False

setNeedsRefresh :: Bool -> RenderingState -> RenderingState
setNeedsRefresh x r = r { needsRefresh = x }

setNeedsResize :: Bool -> RenderingState -> RenderingState
setNeedsResize x r = r { needsResize = x }

renderSwitchState :: RenderingState -> Integer -> Switch -> Update ()
renderSwitchState r row switch = do
      moveCursor row 4
      switchy (state switch)
      setColor defaultColorID
  where
    color = colors r
    switchy Off = do setColor $ red    color; drawString "[ | off ]"
    switchy On  = do setColor $ green  color; drawString "[ on  | ]"
    switchy _   = do setColor $ yellow color; drawString "[  | |  ]"


renderSwitch :: RenderingState -> Integer -> Integer -> Switch -> Update ()
renderSwitch r si row switch = do
    moveCursor row 0
    renderSelection $ si == row
    renderSwitchState r row switch
    renderText
  where
    renderSelection True  = drawString "->"
    renderSelection False = drawString "  "
    renderText = do
      moveCursor row 15
      drawString $ text switch

renderSwitchboard :: RenderingState -> Switchboard -> Update ()
renderSwitchboard r (Switchboard { switches = sw, selected = si }) =
    forM_ (zip [0..] sw) $ uncurry $ renderSwitch r si

renderLogBorder :: Update ()
renderLogBorder = drawBorder d n d n d h v n
  where
    d  = Nothing
    n  = Just (Glyph ' ' [])
    h  = Just glyphLineH
    v  = Just glyphLineV

renderLog :: Switchboard -> Update ()
renderLog (Switchboard { switches = sw, selected = si }) = do
    moveCursor 0 0
    (rows, cols) <- windowSize
    drawString $ logText rows cols
  where
    selectedSwitch     = snd <$> find (\(i, _) -> i == si) (zip [0..] sw)
    truncated cols     = take (fromInteger cols)
    extend cols l      = let trunc = truncated cols l
                         in  trunc <> replicate (fromInteger cols - (length trunc)) ' '
    cutLine False cols = extend cols
    cutLine True  cols = extend (cols - 1)
    cutText rows cols  = concat . (<$>) (\(i, l) -> cutLine (i == rows) cols l) . zip [1..]
                       . reverse . take (fromInteger rows) . reverse . lines
    logText rows cols  = cutText rows cols $ fromMaybe "" (log <$> (selectedSwitch >>= startedProcess))

createColors :: Curses Colors
createColors =
    maxColorID >>= assembleColors
  where
    assembleColors m
      | m >= 16 = do
          colorGreen   <- newColorID ColorGreen  ColorDefault (m - 2)
          colorYellow  <- newColorID ColorYellow ColorDefault (m - 1)
          colorRed     <- newColorID ColorRed    ColorDefault m
          pure $ Colors { green  = colorGreen
                        , yellow = colorYellow
                        , red    = colorRed
                        }
      | otherwise =
          pure $ Colors { green  = defaultColorID
                        , yellow = defaultColorID
                        , red    = defaultColorID
                        }

createWindows :: Curses (Window, Window, Window)
createWindows = do
    w      <- defaultWindow
    blw    <- newWindow 0 0 4 30
    logw   <- newWindow 0 0 5 31
    pure (w, blw, logw)

renderWindow :: RenderingState -> Switchboard -> Curses RenderingState
renderWindow (r @ RenderingState { needsResize = True }) s = do
    closeWindow $ logForegroundWindow r
    closeWindow $ logBackgroundWindow r
    (_, blw, logw) <- createWindows
    let r'         =  r { logBackgroundWindow = blw
                        , logForegroundWindow = logw
                        , needsRefresh        = True
                        , needsResize         = False
                        }
    renderWindow r' s
renderWindow (r @ RenderingState { needsRefresh = False }) _ = r <$ render
renderWindow renderingState switchboard = do
    if logExtended switchboard
      then do
        updateWindow w    $ renderSwitchboard renderingState switchboard
        updateWindow blw  $ renderLogBorder
        updateWindow logw $ renderLog switchboard
      else do
        updateWindow blw  $ clear
        updateWindow logw $ clear
        updateWindow w    $ renderSwitchboard renderingState switchboard
    render
    pure $ renderingState { needsRefresh = False }
  where
    w    = mainWindow          renderingState
    blw  = logBackgroundWindow renderingState
    logw = logForegroundWindow renderingState
