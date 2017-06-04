module Script.NegamaxVsNegamax where

import Agent.AlphaBetaNegamax as Negamax
import Chess
import Control.Monad.Freer
import Control.Monad.Freer.Console
import Control.Monad.Freer.Rand
import Control.Monad.Freer.State
import Control.Monad.Freer.Time
import Grid.Array

runNegamaxVsNegamaxIO :: IO ()
runNegamaxVsNegamaxIO = do
  let
    whiteAgent = Negamax.agent @ArrayBoard 6 WHITE
    blackAgent = Negamax.agent @ArrayBoard 6 BLACK
  winner <- runM $
    runConsoleIO $
    runRandIO $ do
      let
        stb = Negamax.initialAgentState @ArrayBoard @Black
        stw = Negamax.initialAgentState @ArrayBoard @White
      runTimeIO $ flip evalState stb $ flip evalState stw $ playGame whiteAgent blackAgent
  print winner
