{-# LANGUAGE DataKinds, ConstraintKinds, FlexibleContexts, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}

module Game where

import Chess
import Control.Monad.Freer
import Control.Monad.Freer.State
import Data.Maybe
import Grid

data Outcome = Win Player | Continue
type Trace = (Index, Index)
data Turn = Move Trace | Lose deriving (Show, Eq) -- todo: also Win (for king capture)

data Agent c = Agent
  { act     :: forall e. c e => Eff e Turn
  , observe :: forall e. c e => Trace -> Eff e ()
  }

tradeTurns :: (c e, c' e) => Agent c -> Agent c' -> Eff e (Maybe Player)
tradeTurns w b = do
  a <- act w
  case a of
    Lose -> return $ Just Black
    Move a' -> do
      observe b a'
      a'' <- act b
      case a'' of
        Lose -> return $ Just White
        Move a''' -> do
          observe w a'''
          return Nothing

playGame :: (c e, c' e) => Agent c -> Agent c' -> Eff e Player
playGame w b = tradeTurns w b >>= maybe (playGame w b) return
