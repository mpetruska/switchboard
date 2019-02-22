import UI.NCurses

import Configuration
import EventLoop
import Renderer
import SwitchboardModel

main :: IO ()
main = loadSwitches >>= either fail runCursesApp

runCursesApp :: Switchboard -> IO ()
runCursesApp switchboard = do
    _      <- runCurses $ do
      setEcho False
      setCursorMode CursorInvisible
    w      <- runCurses defaultWindow
    blw    <- runCurses $ newWindow 0 0 4 30
    logw   <- runCurses $ newWindow 0 0 5 31
    colors <- runCurses createColors
    loop w blw logw colors switchboard
