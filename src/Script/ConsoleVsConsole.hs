module Script.ConsoleVsConsole where

import Agent.Console as Console
import Chess
import Control.Monad.Freer
import Control.Monad.Freer.Console
import Control.Monad.Freer.State
import Grid.Array

runConsoleVsConsoleIO :: IO ()
runConsoleVsConsoleIO = do
  let
    whiteAgent = Console.agent @ArrayBoard WHITE
    blackAgent = Console.agent @ArrayBoard BLACK
  winner <- runM $
    runConsoleIO $ flip evalState (Console.initialAgentState @ArrayBoard @White) $
    runConsoleIO $ flip evalState (Console.initialAgentState @ArrayBoard @Black) $
    playGame whiteAgent blackAgent
  print winner
