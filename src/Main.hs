import Control.Monad
import UI.NCurses

import Data.SwitchboardModel

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

renderWindow :: Window -> Colors -> Switchboard -> Curses ()
renderWindow w colors switchboard = do
    updateWindow w $ renderSwitchboard colors switchboard
    render

flipSwitch :: Switchboard -> Switchboard
flipSwitch (board @ Switchboard { switches = sw, selected = si }) =
    board { switches = uncurry flipSelected <$> zip [0..] sw }
  where
    flipSelected i switch
      | i == si   = switch { state = flipSwitchState $ state switch }
      | otherwise = switch

update :: Switchboard -> Switchboard
update (board @ Switchboard { switches = sw }) =
    board { switches = progress <$> sw }
  where
    progress switch = switch { state = progressSwitchState $ state switch }

handleEvent :: Switchboard -> Maybe Event -> Maybe Switchboard
handleEvent _  (Just (EventCharacter 'q'))       = Nothing
handleEvent sw (Just (EventCharacter ' '))       = Just $ flipSwitch sw
handleEvent sw (Just (EventSpecialKey KeyEnter)) = Just $ flipSwitch sw
handleEvent sw (Just (EventSpecialKey KeyUpArrow))
    | (selected sw) > 0 = Just $ sw { selected = (selected sw) - 1 }
    | otherwise         = Just $ sw
handleEvent sw (Just (EventSpecialKey KeyDownArrow))
    | (selected sw) + 1 < (toInteger $ length $ switches sw) = Just $ sw { selected = (selected sw) + 1 }
    | otherwise                                              = Just $ sw
handleEvent sw _       = Just $ update sw

loop :: Window -> Colors -> Switchboard -> Curses ()
loop w colors switchboard = do
    renderWindow w colors switchboard
    ev <- getEvent w $ Just 1000
    case handleEvent switchboard ev of
      Just new -> loop w colors new
      Nothing  -> pure ()

main :: IO ()
main = runCurses $ do
    setEcho False
    w      <- defaultWindow
    _      <- setCursorMode CursorInvisible
    colors <- createColors
    loop w colors switchboard  
  where
    manyswitches = [ Switch { text = "My VPN" ,   state = On,          logText = "", logExtended = False }
                   , Switch { text = "Vagrant",   state = SwitchingOn, logText = "", logExtended = False }
                   , Switch { text = "Something", state = Off,         logText = "", logExtended = False }
                   ]
    switchboard  = Switchboard { switches = manyswitches, selected = 0 }
