module Agent.AlphaBetaNegamax where

import Chess
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Choice
import Control.Monad.Freer.Console
import Control.Monad.Freer.EarlyReturn
import Control.Monad.Freer.Rand
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.Freer.Time
import Data.Function
import Data.List
import Data.Time
import Zobrist

import Debug.Trace

-- todo: un-hardcode this (softcode?)
maxTurns = 40
turnTime = 5

data SearchState = SearchState
  { depth :: Int
  , turnsRemaining :: Int
  , player :: Player
  , alpha :: Rank
  , beta :: Rank
  } deriving (Show, Eq, Ord)

type TTable = Zobrist SearchState

data AgentState (p :: Player) b = AgentState { turnsLeft :: Int, board :: b, ttable :: TTable }

type AgentEffects p b = [State (AgentState p b), Console, Time]

initialAgentState :: forall b p effs. (Board b, Member Rand effs) => Eff effs (AgentState p b)
initialAgentState = AgentState @p @b maxTurns initialBoard <$> initialZobrist (2^20)

compareMoveResult :: MoveResult -> MoveResult -> Ordering
compareMoveResult (Capture King) _ = GT
compareMoveResult _ (Capture King) = LT
compareMoveResult Migrate Migrate = EQ
compareMoveResult (Capture _) Migrate = GT
compareMoveResult Migrate (Capture _) = LT
compareMoveResult (Capture s) (Capture t) = compare (pieceScore s) (pieceScore t)

compareMoveRecord :: MoveRecord -> MoveRecord -> Ordering
compareMoveRecord = compareMoveResult `on` result

-- this is the critical loop so it's worth avoiding Eff and writing it in pure style for the sake of speed
-- (that's the truth, i profiled)
-- todo: this is uglier than it has to be though
rank :: Board b => HashCache -> b -> Hash -> TTable -> SearchState -> (Rank, TTable)
rank hc bd h tt s@SearchState{..} =
  case Zobrist.lookup key tt of
    Just r -> (r, tt)
    Nothing
      | lost player bd -> leaf NegativeInfinity tt
      | won player bd -> leaf PositiveInfinity tt
      | turnsRemaining == 0 -> leaf (Rank 0) tt
      | depth == 0 -> leaf (Rank (boardScore player bd)) tt
      | otherwise -> go (sortBy (flip compareMoveRecord) (moves player bd)) NegativeInfinity alpha beta tt
  where
    key = Key s h

    leaf r tt = (r, Zobrist.insertKey key r tt)

    go [] v al bt tt = (v, tt)
    go ((move -> m) : ms) v al bt tt = if r >= bt then (r, tt') else go ms (max r v) (max r al) bt tt'
      where
        sst = SearchState (depth - 1) (turnsRemaining - 1) (opponent player) (negateRank bt) (negateRank al)
        (negateRank -> r, tt') = rank hc (makeMove m bd) (moveHash hc bd m h) tt sst

bestMove :: Board b => TTable -> b -> Player -> Int -> Int -> (MoveRecord, TTable)
bestMove tt b p t d = fst $ maximumByRank snd $ do
  m@(MoveRecord e m') <- moves p b
  let (negateRank -> r, tt') = rank (hashCache tt) (makeMove m' b) (boardHash (hashCache tt) b) tt $ SearchState d t (opponent p) NegativeInfinity PositiveInfinity
  return ((m, tt'), r)

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
        (MoveRecord e m, tt') <- timeoutIterateMapLastAfter turnTime (+ 2) 1 $ bestMove ttable board (playerSing p) turnsLeft
        put @(AgentState p b) $ st { turnsLeft = turnsLeft - 1 , board = makeMove m board, ttable = tt' }
        return $ case e of
          Capture King -> Win m
          _ -> Move m

negamaxObserve :: forall b p effs. (Board b, Members (AgentEffects p b) effs) => PlayerSing p -> Move -> Eff effs ()
negamaxObserve p m = do
  st@AgentState{..} :: AgentState p b <- get
  put @(AgentState p b) $ st { board = makeMove m board }

negamaxRunIO :: forall b p a effs. (Board b, Member IO effs) => Eff (AgentEffects p b ++ effs) a -> Eff effs a
negamaxRunIO x = runRandIO initialAgentState >>= runTimeIO . runConsoleIO . evalState x

agent :: forall b p. Board b => PlayerSing p -> Agent (AgentEffects p b)
agent p = Agent (negamaxAct @b p) (negamaxObserve @b p)
