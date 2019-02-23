module Renderer
       ( Colors(..)
       , createColors
       , mkRenderingState
       , RenderingState
       , renderWindow
       , setNeedsRefresh
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
                                     }

mkRenderingState :: Window -> Window -> Window -> Colors -> RenderingState
mkRenderingState main logBack logFore clrs = RenderingState main logBack logFore clrs True

setNeedsRefresh :: RenderingState -> Bool -> RenderingState
setNeedsRefresh r x = r { needsRefresh = x }

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
    drawString logText
  where
    selectedSwitch = snd <$> find (\(i, _) -> i == si) (zip [0..] sw)
    takeLastN n    = unlines . reverse . take n . reverse . lines
    logText        = takeLastN 20 $ fromMaybe "" (log <$> (selectedSwitch >>= startedProcess))

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

renderWindow :: RenderingState -> Switchboard -> Curses ()
renderWindow (RenderingState { needsRefresh = False }) _ = render
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
  where
    w    = mainWindow          renderingState
    blw  = logBackgroundWindow renderingState
    logw = logForegroundWindow renderingState
