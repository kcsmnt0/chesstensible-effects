{-# language UndecidableInstances #-}

module Agent.Console where

import Chess
import Control.Monad.Freer
import Control.Monad.Freer.Console
import Control.Monad.Freer.State
import Grid
import Text.Read (readMaybe)

-- In order to avoid implicitly sharing state between two instances of the same agent competing against each other,
-- the state type is parameterized over which player it belongs to. I don't think this is actually necessary for a
-- console agent, since they each just store the board and they should always agree on that anyway, but this makes
-- for a digestible example of the pattern in more complex agents that need to maintain private state in order to
-- ensure fair competition.
newtype AgentState (p :: Player) b = AgentState { agentState :: b }

-- This is just a hacky way to get a partially-appliable constraint synonym: "AgentEffects p b" is a constraint over
-- a list of effects that requires it to contain a state of some board type b for player p and permission to read and
-- write to a console. (This seems to be a known pain point for type-level trickery in Haskell - type synonyms and type
-- families can't be partially applied, and you can't do a newtype over a constraint. I found this somewhere on
-- StackOverflow a while ago and haven't been able to Google it, so I don't have an exact source, but there might be
-- a less ugly way to go about it.)
class (Board b, Member (State (AgentState p b)) e, Member Console e) => AgentEffects p b e
instance (Board b, Member (State (AgentState p b)) e, Member Console e) => AgentEffects p b e

initialAgentState :: forall b p. Board b => AgentState p b
initialAgentState = AgentState @_ @b initialBoard

consoleReadMove :: String -> Maybe TurnOutcome
consoleReadMove "give up" = Just Lose
consoleReadMove s = Move <$> readMove s

-- The PlayerSing argument is because there needs to be something to case over when printing the player, and I'm not
-- sure how to turn a lifted type back into a value - I think the singletons library handles this stuff but I haven't
-- gotten around to looking into that yet.
consoleAct :: forall b p effs. AgentEffects p b effs => PlayerSing p -> Eff effs TurnOutcome
consoleAct p = do
  AgentState b :: AgentState p b <- get
  consoleWrite $ show p
  consoleWrite $ showBoard b
  m <- consoleRead
  case consoleReadMove m of
    Nothing -> consoleWrite "couldn't parse your move" >> consoleAct @b p
    Just Lose -> return Lose
    Just (Move m) -> do
      case maybeMove b m of
        Nothing -> consoleWrite "illegal move" >> consoleAct @b p
        Just (MoveRecord r _) -> do
          let b' = makeMove m b
          put (AgentState @p b')
          consoleWrite $ showBoard b'
          return $ if r == Capture King then Win m else Move m
    _ -> error "no you didn't" -- unreachable case (consoleReadMove never returns Win or Tie)

consoleObserve :: forall b p e. AgentEffects p b e => PlayerSing p -> Move -> Eff e ()
consoleObserve p m = modify (AgentState @p @b . makeMove m . agentState)

agent :: forall b p. Board b => PlayerSing p -> Agent (AgentEffects p b)
agent p = Agent (consoleAct @b p) (consoleObserve @b p)
