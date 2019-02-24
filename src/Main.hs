import Options.Applicative
import UI.NCurses

import Configuration
import EventLoop
import Renderer
import SwitchboardModel

argumentsParser :: Parser Arguments
argumentsParser =
    Arguments
      <$> strOption (  long    "file"
                    <> short   'f'
                    <> metavar "FILE"
                    <> help    "The configuration file"
                    <> value   "switchboard.yaml"
                    <> showDefault
                    )

arguments :: ParserInfo Arguments
arguments = info (argumentsParser <**> helper)
    (  fullDesc
    <> progDesc "Allows easy system configuration by shell commands backed switches"
    <> header   "switchboard - toggle everything"
    )

main :: IO ()
main = execParser arguments >>= loadSwitches >>= either fail runCursesApp

runCursesApp :: Switchboard -> IO ()
runCursesApp switchboard = do
    _      <- runCurses $ do
      setEcho False
      setCursorMode CursorInvisible
    w      <- runCurses defaultWindow
    blw    <- runCurses $ newWindow 0 0 4 30
    logw   <- runCurses $ newWindow 0 0 5 31
    colors <- runCurses createColors
    let r  =  mkRenderingState w blw logw colors
    loop w r switchboard
