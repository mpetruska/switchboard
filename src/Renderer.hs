module Renderer
       ( Colors(..)
       , createColors
       , renderWindow
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

renderSwitchState :: Colors -> Integer -> Switch -> Update ()
renderSwitchState color row switch = do
      moveCursor row 4
      switchy (state switch)
      setColor defaultColorID
  where
    switchy Off = do setColor $ red    color; drawString "[ | off ]"
    switchy On  = do setColor $ green  color; drawString "[ on  | ]"
    switchy _   = do setColor $ yellow color; drawString "[  | |  ]"


renderSwitch :: Colors -> Integer -> Integer -> Switch -> Update ()
renderSwitch color si row switch = do
    moveCursor row 0
    renderSelection $ si == row
    renderSwitchState color row switch
    renderText
  where
    renderSelection True  = drawString "->"
    renderSelection False = drawString "  "
    renderText = do
      moveCursor row 15
      drawString $ text switch

renderSwitchboard :: Colors -> Switchboard -> Update ()
renderSwitchboard color (Switchboard { switches = sw, selected = si }) =
    forM_ (zip [0..] sw) $ uncurry $ renderSwitch color si

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
    logText  = fromMaybe "" (log <$> (selectedSwitch >>= startedProcess))

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

renderWindow :: Window -> Window -> Window -> Colors -> Switchboard -> Curses ()
renderWindow w blw logw colors switchboard = do
    if logExtended switchboard
      then do
        updateWindow w    $ renderSwitchboard colors switchboard
        updateWindow blw  $ renderLogBorder
        updateWindow logw $ renderLog switchboard
      else do
        updateWindow blw  $ clear
        updateWindow logw $ clear
        updateWindow w    $ renderSwitchboard colors switchboard
    render
