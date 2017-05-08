{-# language DataKinds, TypeApplications #-}

module Main where

import Agent.Console
import Chess
import Control.Monad.Freer
import Control.Monad.Freer.State
import Data.Proxy
import Game
import Grid
import Grid.Array

main :: IO ()
main = do
  let
    whiteAgent = consoleAgent WHITE
    blackAgent = consoleAgent BLACK
  winner <-
    runM $
      flip evalState (initialConsoleAgentState @ArrayBoard @White Proxy) $
      flip evalState (initialConsoleAgentState @ArrayBoard @Black Proxy) $
      playGame whiteAgent blackAgent
  print winner
