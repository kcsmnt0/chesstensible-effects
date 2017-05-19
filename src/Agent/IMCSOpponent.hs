{-# language UndecidableInstances #-}

module Agent.IMCSOpponent where

import Chess

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Console
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Socket
import Data.Maybe
import IMCS

import Debug.Trace

class (Member (Exc IMCSError) effs, Member Socket effs, Member Console effs) => AgentEffects (p :: Player) effs
instance (Member (Exc IMCSError) effs, Member Socket effs, Member Console effs) => AgentEffects (p :: Player) effs

imcsOpponentAct :: AgentEffects p effs => PlayerSing p -> Eff effs MoveOutcome
imcsOpponentAct p = do
  resp <- socketRecv
  socketRecv >>= consoleWrite
  case take 3 resp of
    -- This looks pretty sketchy, but it's okay for now because the game is over in these cases and I never check the
    -- winning move in any of the scripts I'm using so the error terms are never evaluated.
    "231" -> return $ case p of WHITE -> Win (error "todo"); BLACK -> Lose
    "232" -> return $ case p of BLACK -> Win (error "toooodo"); WHITE -> Lose

    _ -> do
      let move = readMove $ take 5 $ drop 2 resp -- e.g. "! a2-a3"
      when (head resp /= '!' || isNothing move) $ throwError $ IMCSError resp -- todo: recovery (possibly)
      return $ Move $ fromJust move

imcsOpponentObserve :: AgentEffects p effs => Move -> Eff effs ()
imcsOpponentObserve = socketSend . (++ "\n") . showMove

agent :: forall p. PlayerSing p -> Agent (AgentEffects p)
agent p = Agent (imcsOpponentAct p) (imcsOpponentObserve @p)
