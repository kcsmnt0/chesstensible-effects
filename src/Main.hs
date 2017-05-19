module Main where

import Agent.AlphaBetaNegamax as AlphaBetaNegamax
import Agent.Negamax as Negamax
import Chess
import Control.Monad.Freer
import Control.Monad.Freer.Console
import Control.Monad.Freer.State
import Grid
import Grid.Array
import Script.ConsoleVsIMCSOpponent
import Script.ConsoleVsNegamax
import Script.NegamaxVsIMCSOpponent
import Script.NegamaxVsNegamax

main = runNegamaxVsIMCSOpponentIO
-- main = runM $ runConsoleIO $ flip evalState (AlphaBetaNegamax.AgentState @Black @ArrayBoard (makeMove ((1,4),(1,3)) initialBoard)) $ act $ AlphaBetaNegamax.agent @ArrayBoard 6 BLACK
