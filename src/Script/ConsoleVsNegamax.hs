module Script.ConsoleVsNegamax where

import Agent.Console as Console
import Agent.AlphaBetaNegamax as Negamax
import Chess
import Control.Monad.Freer
import Control.Monad.Freer.Console
import Control.Monad.Freer.Rand
import Control.Monad.Freer.State
import Control.Monad.Freer.Time
import Grid.Array

runConsoleVsNegamaxIO :: IO ()
runConsoleVsNegamaxIO = do
  let
    whiteAgent = Console.agent @ArrayBoard WHITE
    blackAgent = Negamax.agent @ArrayBoard BLACK
  winner <- runM $ do
    st <- runRandIO $ Negamax.initialAgentState @ArrayBoard @Black
    runConsoleIO $ flip evalState (Console.initialAgentState @ArrayBoard @White) $ -- console agent effects
      runTimeIO $ flip evalState st $ -- negamax agent effects
        playGame whiteAgent blackAgent
  print winner
