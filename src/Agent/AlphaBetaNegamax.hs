{-# language UndecidableInstances #-}

module Agent.AlphaBetaNegamax where

import Chess
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Choice
import Control.Monad.Freer.Console
import Control.Monad.Freer.EarlyReturn
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.Freer.Time
import Data.Function
import Data.List
import Data.Time

data AgentState (p :: Player) b = AgentState { turnsLeft :: Int, board :: b }

type AgentEffects p b = [State (AgentState p b), Console, Time]

-- todo: un-hardcode this (softcode?)
maxTime = 5 * 60
maxTurns = 40
turnTime = maxTime / fromIntegral maxTurns

initialAgentState :: forall b p. Board b => AgentState p b
initialAgentState = AgentState @p @b maxTurns initialBoard

compareMoveResult :: MoveResult -> MoveResult -> Ordering
compareMoveResult (Capture King) _ = GT
compareMoveResult _ (Capture King) = LT
compareMoveResult Migrate Migrate = EQ
compareMoveResult (Capture _) Migrate = GT
compareMoveResult Migrate (Capture _) = LT
compareMoveResult (Capture s) (Capture t) = compare (pieceScore s) (pieceScore t)

compareMoveRecord :: MoveRecord -> MoveRecord -> Ordering
compareMoveRecord = compareMoveResult `on` result

-- todo: shortest win prioritization
rank :: Board b => b -> Int -> Int -> Player -> Rank -> Rank -> Rank
rank board t d p alpha beta = if
  | lost p board -> NegativeInfinity -- todo: these won/lost checks might be expensive
  | won p board -> PositiveInfinity
  | (t == 0) -> Rank 0
  | (d == 0) -> Rank $ boardScore p board
  | otherwise ->
      -- todo! explain the effects here
      either id fst $ run $ runEarlyReturn $ flip execState (NegativeInfinity, alpha) $ runChoices $ do
        MoveRecord e m <- choose $ sortBy (flip compareMoveRecord) $ moves p board
        (v' :: Rank, alpha' :: Rank) <- get
        let v = negateRank $ rank (makeMove m board) (t-1) (d-1) (opponent p) (negateRank beta) (negateRank alpha')
        when (v >= beta) $ earlyReturn v
        put (max v' v, max alpha' v)

-- todo: use alpha/beta from previous ID levels
bestMove :: Board b => b -> Player -> Int -> Int -> MoveRecord
bestMove b p t d = fst $ maximumBy (compare `on` snd) $ do
  m@(MoveRecord e m') <- moves p b
  return (m, negateRank $ rank (makeMove m' b) t d (opponent p) NegativeInfinity PositiveInfinity)

negamaxAct :: forall b p effs. (Board b, Members (AgentEffects p b) effs) => PlayerSing p -> Eff effs TurnOutcome
negamaxAct p = do
  st@AgentState{..} :: AgentState p b <- get
  if turnsLeft <= 0 then
    return Tie
  else do
    consoleWrite $ showBoard board
    case moves (playerSing p) board of
      [] -> return Lose
      ms -> do
        -- todo: is this off by one? even-odd thing?
        MoveRecord e m <- timeoutIterateMapLastAfter turnTime (+ 2) 1 $ bestMove board (playerSing p) turnsLeft
        put @(AgentState p b) $ st { turnsLeft = turnsLeft - 1 , board = makeMove m board }
        return $ case e of
          Capture King -> Win m
          _ -> Move m

negamaxObserve :: forall b p effs. (Board b, Members (AgentEffects p b) effs) => PlayerSing p -> Move -> Eff effs ()
negamaxObserve p m = do
  st@AgentState{..} :: AgentState p b <- get
  put @(AgentState p b) $ st { board = makeMove m board }

negamaxRunIO :: forall b p a effs. (Board b, Member IO effs) => Eff (AgentEffects p b ++ effs) a -> Eff effs a
negamaxRunIO = runTimeIO . runConsoleIO . flip evalState initialAgentState

agent :: forall b p. Board b => PlayerSing p -> Agent (AgentEffects p b)
agent p = Agent (negamaxAct @b p) (negamaxObserve @b p)
