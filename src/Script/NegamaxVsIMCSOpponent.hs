-- todo: this is all just copied and pasted from ConsoleVsIMCSOpponent.hs with slight tweaks
-- (i.e. abstract out the choice of agent for a polymorphic VsIMCSOpponent script)
module Script.NegamaxVsIMCSOpponent where

import Agent.AlphaBetaNegamax as Negamax
import Agent.IMCSOpponent as IMCSOpponent
import Chess
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Console
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Socket
import Control.Monad.Freer.State
import Control.Monad.Freer.Time
import Control.Monad.Loops
import Data.Maybe
import Grid.Array
import IMCS
import Text.Read

clientAgent = Negamax.agent @ArrayBoard 6
serverAgent = IMCSOpponent.agent

prompt :: Member Console effs => String -> Eff effs String
prompt p = consoleWrite (p ++ ": ") >> consoleRead

runGame :: forall p effs. (Member IO effs, IMCSOpponent.AgentEffects p effs) => PlayerSing p -> Eff effs GameOutcome
runGame p =
  runTimeIO $ flip evalState (Negamax.initialAgentState @ArrayBoard @p) $ case p of
    WHITE -> playGame (clientAgent WHITE) (serverAgent BLACK)
    BLACK -> playGame (serverAgent WHITE) (clientAgent BLACK)

runNegamaxVsIMCSOpponentIO :: IO ()
runNegamaxVsIMCSOpponentIO = do
  result <- runM @IO $ runError @SocketError $ runError @IMCSError $ runSocketIO "imcs.svcs.cs.pdx.edu" "3589" $ runConsoleIO $ do
    socketRecvLine >>= ensureResponseCode 100

    user <- prompt "username"
    pass <- prompt "password"
    me user pass

    Just g <- iterateWhile isNothing $ do
      games <- list
      send $ putStrLn $ unlines $ zipWith (++) (map ((++ " ") . show) [0..]) [show g ++ ": " ++ n ++ " " ++ show p | GameOffer g n p <- games]
      fmap readMaybe (send getLine) >>= \case
        Just i | 0 <= i && i < length games -> return $ Just (games !! i)
        _ -> return Nothing

    accept (gameID g) runGame

  putStrLn $ case result of
    Left SocketError -> "socket something error happens"
    Right (Left (IMCSError err)) -> "IMCS error: " ++ err
    Right (Right winner) -> show winner ++ " wins!"
