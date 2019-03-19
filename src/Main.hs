import Options.Applicative ( (<**>)
                           , execParser
                           , fullDesc
                           , header
                           , help
                           , helper
                           , info
                           , long
                           , metavar
                           , Parser
                           , ParserInfo
                           , progDesc
                           , short
                           , showDefault
                           , strOption
                           , value
                           )
import UI.NCurses          ( CursorMode(CursorInvisible), runCurses, setCursorMode, setEcho )

import Configuration       ( Arguments(Arguments), loadSwitches )
import EventLoop           ( loop )
import Renderer            ( createColors, createWindows, mkRenderingState )
import SwitchboardModel    ( Switchboard )

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
    _              <- runCurses $ do
      setEcho False
      setCursorMode CursorInvisible
    (w, blw, logw) <- runCurses createWindows
    colors         <- runCurses createColors
    let r          =  mkRenderingState w blw logw colors
    loop w r switchboard
