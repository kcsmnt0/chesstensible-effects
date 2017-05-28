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
import Data.Function
import Data.List

data AgentState (p :: Player) b = AgentState { turnsLeft :: Int, agentState :: b }

class (Board b, Member (State (AgentState p b)) effs, Member Console effs) => AgentEffects p b effs
instance (Board b, Member (State (AgentState p b)) effs, Member Console effs) => AgentEffects p b effs

data Rank = NegativeInfinity | Rank Int | PositiveInfinity deriving (Show, Eq, Ord)

initialAgentState :: forall b p. Board b => AgentState p b
initialAgentState = AgentState @p @b 40 initialBoard

negateRank :: Rank -> Rank
negateRank (Rank x) = Rank $ negate x
negateRank NegativeInfinity = PositiveInfinity
negateRank PositiveInfinity = NegativeInfinity

compareMoveResult :: MoveResult -> MoveResult -> Ordering
compareMoveResult (Capture King) _ = GT
compareMoveResult _ (Capture King) = LT
compareMoveResult Migrate Migrate = EQ
compareMoveResult (Capture _) Migrate = GT
compareMoveResult Migrate (Capture _) = LT
compareMoveResult (Capture s) (Capture t) = compare (pieceScore s) (pieceScore t)

compareMoveRecord :: MoveRecord -> MoveRecord -> Ordering
compareMoveRecord = compareMoveResult `on` result

rank :: Board b => b -> Int -> Int -> Player -> Rank -> Rank -> Rank
rank board 0 d p alpha beta = Rank 0 -- tie
rank board t 0 p alpha beta = Rank $ boardScore p board
rank board t d p alpha beta
  | lost p board = NegativeInfinity -- todo: these checks might be expensive
  | won p board = PositiveInfinity
  | otherwise =
      either id fst $ run $ runEarlyReturn $ flip execState (NegativeInfinity, alpha) $ runChoices $ do
        MoveRecord e m <- choose $ sortBy (flip compareMoveRecord) $ moves p board
        (v' :: Rank, alpha' :: Rank) <- get
        let v = negateRank $ rank (makeMove m board) (t-1) (d-1) (opponent p) (negateRank beta) (negateRank alpha')
        when (v >= beta) $ earlyReturn v
        put (max v' v, max alpha' v)

negamaxAct :: forall b p e. AgentEffects p b e => Int -> PlayerSing p -> Eff e TurnOutcome
negamaxAct d p = do
  AgentState t b :: AgentState p b <- get
  if (t <= 0) then
    return Tie
  else do
    consoleWrite $ showBoard b
    case moves (playerSing p) b of
      [] -> return Lose
      ms -> do
        MoveRecord e m <- fmap (fst . maximumBy (compare `on` snd)) $ runChoices $ do
          m@(MoveRecord e m') <- choose ms
          return (m, negateRank $ rank (makeMove m' b) t d (opponent (playerSing p)) NegativeInfinity PositiveInfinity)
        put $ AgentState @p @b (t-1) (makeMove m b)
        return $ case e of
          Capture King -> Win m
          _ -> Move m

negamaxObserve :: forall b p e. AgentEffects p b e => PlayerSing p -> Move -> Eff e ()
negamaxObserve p m = do
  AgentState t b :: AgentState p b <- get
  put $ AgentState @p @b t (makeMove m b)

agent :: forall b p. Board b => Int -> PlayerSing p -> Agent (AgentEffects p b)
agent d p = Agent (negamaxAct @b d p) (negamaxObserve @b p)
