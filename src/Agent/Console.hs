{-# LANGUAGE DataKinds, ExistentialQuantification, UndecidableInstances, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeApplications, KindSignatures, PolyKinds, ScopedTypeVariables, ViewPatterns #-}

module Agent.Console where

import Chess
import Control.Monad.Freer
import Control.Monad.Freer.State
import Data.Char
import Data.Proxy
import Game
import Grid
import Text.Read (readMaybe)

data ConsoleAgentState (p :: Player) = forall b. Board b => ConsoleAgentState { consoleAgentState :: b }

-- just a hacky way to get a partially-appliable class synonym
class (Member (State (ConsoleAgentState p)) e, Member IO e) => ConsoleAgentEffects p e
instance (Member (State (ConsoleAgentState p)) e, Member IO e) => ConsoleAgentEffects p e

initialConsoleAgentState :: forall b p. Board b => Proxy b -> ConsoleAgentState p
initialConsoleAgentState (Proxy :: Proxy b) = ConsoleAgentState (initialBoard :: b)

readColumn :: Char -> Maybe Int
readColumn c = if 'a' <= c && c <= 'e' then Just (ord c - ord 'a') else Nothing

readRow :: Char -> Maybe Int
readRow c = do i <- readMaybe (c:""); if 0 <= i && i <= 5 then Just i else Nothing

readMove :: String -> Maybe Turn
readMove "give up" = Just Lose
readMove [readColumn -> Just x, readRow -> Just y, readColumn -> Just x', readRow -> Just y'] = Just $ Move ((x,y), (x',y'))
readMove _ = Nothing

consoleAct :: ConsoleAgentEffects p e => PlayerSing p -> Eff e Turn
consoleAct (p :: PlayerSing p) = do
  ConsoleAgentState b :: ConsoleAgentState p <- get
  send $ print p
  send $ putStrLn $ showBoard b
  m <- send getLine
  case readMove m of
    Nothing -> send (putStrLn "please take this seriously") >> consoleAct p
    Just Lose -> return Lose
    Just (Move (i, j)) -> do
      case maybeMove b i j of
        -- todo: i'm not using the output of maybeMove? this is just legalMove :: etc. -> Bool
        Just m -> put (ConsoleAgentState @p (makeMove i j b)) >> return (Move (i, j))
        Nothing -> send (putStrLn "that's not allowed") >> consoleAct p

consoleObserve :: ConsoleAgentEffects p e => PlayerSing p -> (Index, Index) -> Eff e ()
consoleObserve (p :: PlayerSing p) (i, j) = modify (\(ConsoleAgentState b) -> ConsoleAgentState @p (makeMove i j b))

consoleAgent :: PlayerSing p -> Agent (ConsoleAgentEffects p)
consoleAgent p = Agent (consoleAct p) (consoleObserve p)
