{-# language UndecidableInstances #-}

module Agent.Negamax where

import Chess
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Choice
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Data.Function
import Data.List

data AgentState (p :: Player) b = AgentState { agentState :: b } -- todo! turn count

class (Board b, Member (State (AgentState p b)) e) => AgentEffects p b e
instance (Board b, Member (State (AgentState p b)) e) => AgentEffects p b e

-- The derived Ord instance orders the constructors left to right, so it derives the expected infinity inequalities.
data Rank = NegativeInfinity | Rank Int | PositiveInfinity deriving (Show, Eq, Ord)

initialAgentState :: forall b p. Board b => AgentState p b
initialAgentState = AgentState @p @b initialBoard

negateRank :: Rank -> Rank
negateRank (Rank x) = Rank $ negate x
negateRank NegativeInfinity = PositiveInfinity
negateRank PositiveInfinity = NegativeInfinity

-- Negamax rank, depth d, for player p, of a board state readable from the effectful context.
-- todo: is there any point to using the Reader effect instead of just having the board as an argument?
rank :: Board b => Int -> Player -> b -> Rank
rank d p b =
  if d == 0 then
    Rank $ boardScore p b
  else if lost p b then
    NegativeInfinity
  else
    case moves p b of
      [] -> NegativeInfinity -- running out of moves is a loss
      ms -> maximum $
        ms >>= \case
          MoveRecord (Capture King) _ -> return PositiveInfinity
          MoveRecord _ m -> return $ negateRank $ rank (d-1) (opponent p) (makeMove m b)

negamaxAct :: forall b p e. AgentEffects p b e => Int -> PlayerSing p -> Eff e TurnOutcome
negamaxAct d p = do
  AgentState b :: AgentState p b <- get
  case moves (playerSing p) b of
    [] -> return Lose
    ms -> do
      -- maximum of a bounded recursive depth-first search through states for each available move
      MoveRecord e m <- fmap (fst . maximumBy (compare `on` snd)) $ runChoices $ do
        m@(MoveRecord e m') <- choose ms
        let r = rank d (opponent (playerSing p)) (makeMove m' b)
        return (m, negateRank r) -- here's that minus sign!
      modify $ AgentState @p @b . makeMove m . agentState
      return $ case e of
        Capture King -> Win m
        _ -> Move m

negamaxObserve :: forall b p e. AgentEffects p b e => PlayerSing p -> Move -> Eff e ()
negamaxObserve p m = modify (AgentState @p @b . makeMove m . agentState)

agent :: forall b p. Board b => Int -> PlayerSing p -> Agent (AgentEffects p b)
agent d p = Agent (negamaxAct @b d p) (negamaxObserve @b p)
