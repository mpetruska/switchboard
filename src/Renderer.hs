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

import Prelude          hiding ( log )
import Control.Monad    ( forM_ )
import Data.List        ( find )
import Data.Maybe       ( fromMaybe )
import UI.NCurses       ( catchCurses
                        , clear
                        , closeWindow
                        , Color(ColorDefault, ColorGreen, ColorRed, ColorYellow)
                        , ColorID
                        , Curses
                        , defaultColorID
                        , defaultWindow
                        , drawBorder
                        , drawString
                        , Glyph(Glyph)
                        , glyphLineH
                        , glyphLineV
                        , maxColorID
                        , moveCursor
                        , newColorID
                        , newWindow
                        , render
                        , setColor
                        , Update
                        , updateWindow
                        , Window
                        , windowSize
                        )

import Processes        ( log )
import SwitchboardModel ( isOn
                        , MasterSwitch
                        , SelectedSwitch(..)
                        , startedProcess
                        , state
                        , Switch
                        , Switchboard(..)
                        , switchesControlled
                        , SwitchState(..)
                        , text
                        )

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

offString :: String
offString          = "[ | | off ]"

onString :: String
onString           = "[  on | | ]"

intermediateString :: String
intermediateString = "[   | |   ]"

footerString :: String
footerString = " up, down: select switch | left, right, space: flip switch | l: show switch log | q: quit "

mkRenderingState :: Window -> Window -> Window -> Colors -> RenderingState
mkRenderingState main logBack logFore clrs = RenderingState main logBack logFore clrs True False

setNeedsRefresh :: Bool -> RenderingState -> RenderingState
setNeedsRefresh x r = r { needsRefresh = x }

setNeedsResize :: Bool -> RenderingState -> RenderingState
setNeedsResize x r = r { needsResize = x }

createColors :: Curses Colors
createColors =
    maxColorID >>= assembleColors
  where
    createNewColors m = do
      colorGreen   <- newColorID ColorGreen  ColorDefault (m - 2)
      colorYellow  <- newColorID ColorYellow ColorDefault (m - 1)
      colorRed     <- newColorID ColorRed    ColorDefault m
      return $ Colors { green  = colorGreen
                      , yellow = colorYellow
                      , red    = colorRed
                      }
    createFakeColors =
      return $ Colors { green  = defaultColorID
                      , yellow = defaultColorID
                      , red    = defaultColorID
                      }
    assembleColors m
      | m >= 16   = catchCurses (createNewColors m) (const createFakeColors)
      | otherwise = createFakeColors

createWindows :: Curses (Window, Window, Window)
createWindows = do
    w            <- defaultWindow
    (rows, cols) <- updateWindow w $ windowSize
    blw          <- createBlw  rows cols
    logw         <- createLogw rows cols
    return (w, blw, logw)
  where
    createBlw rows cols
      | rows > 5 && cols > 31 = newWindow 0 0 4 30
      | otherwise             = newWindow 0 0 0 0
    createLogw rows cols
      | rows > 5 && cols > 31 = newWindow 0 0 5 31
      | otherwise             = newWindow 0 0 1 1

renderSelection :: Bool -> Update ()
renderSelection True  = drawString "->"
renderSelection False = return ()

renderMasterSwitchState :: RenderingState -> MasterSwitch -> Update ()
renderMasterSwitchState r m = do
    moveCursor 0 4
    switchy (isOn m)
    setColor defaultColorID
  where
    color         = colors r
    switchy False = do setColor $ red    color; drawString offString
    switchy True  = do setColor $ green  color; drawString  onString

renderMasterSwitch :: RenderingState -> SelectedSwitch -> MasterSwitch -> Update ()
renderMasterSwitch r si m = do
    moveCursor 0 0
    renderSelection $ si == MasterSelected
    renderMasterSwitchState r m
    moveCursor 0 17
    drawString "Master"

renderSwitchState :: RenderingState -> Integer -> Switch -> Update ()
renderSwitchState r row switch = do
    moveCursor row 4
    switchy (state switch)
    setColor defaultColorID
  where
    color = colors r
    switchy Off = do setColor $ red    color; drawString          offString
    switchy On  = do setColor $ green  color; drawString           onString
    switchy _   = do setColor $ yellow color; drawString intermediateString

renderSwitch :: RenderingState -> SelectedSwitch -> Integer -> Switch -> Update ()
renderSwitch r si i switch = do
    moveCursor row 0
    renderSelection $ si == SwitchSelected i
    renderSwitchState r row switch
    (_, cols) <- windowSize
    renderText (fromInteger cols)
  where
    row             = i + 2
    title      cols = take (cols - 17) $ text switch
    renderText cols = do
      moveCursor row 17
      drawString $ title cols

renderSwitchboard :: RenderingState -> Switchboard -> Update ()
renderSwitchboard r (Switchboard { masterSwitch = m, switches = sw, selected = si }) = do
    clear
    (rows, cols) <- windowSize
    renderMasterSwitch r si m
    let swcut = take ((fromInteger rows) - 3) sw
    forM_ (zip [0..] swcut) $ uncurry $ renderSwitch r si
    drawFooter (rows > 2) rows cols
  where
    drawFooter True rows cols = do
      moveCursor (rows - 1) 0
      drawString $ take ((fromInteger cols) - 1) footerString
    drawFooter False _ _ =
      return ()

renderLogBorder :: Update ()
renderLogBorder = drawBorder d n d n d h v n
  where
    d  = Nothing
    n  = Just (Glyph ' ' [])
    h  = Just glyphLineH
    v  = Just glyphLineV

renderLog :: Switchboard -> Update ()
renderLog (Switchboard master sw si _) = do
    moveCursor 0 0
    (rows, cols) <- windowSize
    drawString $ logText rows cols $ selectedLog si
  where
    selectedSwitch i               = snd <$> find (\(j, _) -> j == i) (zip [0..] sw)
    selectedLog (SwitchSelected i) = fromMaybe "" (log <$> (selectedSwitch i >>= startedProcess))
    selectedLog MasterSelected     = show $ switchesControlled master
    truncated cols     = take (fromInteger cols)
    extend cols l      = let trunc = truncated cols l
                         in  trunc <> replicate (fromInteger cols - (length trunc)) ' '
    cutLine False cols = extend cols
    cutLine True  cols = extend (cols - 1)
    logText rows cols  = concat . (<$>) (\(i, l) -> cutLine (i == rows) cols l) . zip [1..]
                       . reverse . take (fromInteger rows) . reverse . lines

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
    return $ setNeedsRefresh False renderingState
  where
    w    = mainWindow          renderingState
    blw  = logBackgroundWindow renderingState
    logw = logForegroundWindow renderingState
