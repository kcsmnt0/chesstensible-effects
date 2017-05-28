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
    -- todo: there should be a way to include these "send all agent effects to IO" functions in the agents themselves
    runConsoleIO $ flip evalState (Console.initialAgentState @ArrayBoard @White) $ -- console agent effects
    runConsoleIO $ flip evalState (Console.initialAgentState @ArrayBoard @Black) $ -- console agent effects
    playGame whiteAgent blackAgent
  print winner
