module Script.ConsoleVsNegamax where

import Agent.Console as Console
import Agent.AlphaBetaNegamax as Negamax
import Chess
import Control.Monad.Freer
import Control.Monad.Freer.Console
import Control.Monad.Freer.State
import Control.Monad.Freer.Time
import Grid.Array

runConsoleVsNegamaxIO :: IO ()
runConsoleVsNegamaxIO = do
  let
    whiteAgent = Console.agent @ArrayBoard WHITE
    blackAgent = Negamax.agent @ArrayBoard BLACK
  winner <- runM $
    -- todo: there should be a way to include these "send all agent effects to IO" functions in the agents themselves
    runConsoleIO $ flip evalState (Console.initialAgentState @ArrayBoard @White) $ -- console agent effects
    runTimeIO $ flip evalState (Negamax.initialAgentState @ArrayBoard @Black) $ -- negamax agent effects
    playGame whiteAgent blackAgent
  print winner
