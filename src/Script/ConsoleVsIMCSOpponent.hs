module Script.ConsoleVsIMCSOpponent where

import Agent.Console as Console
import Agent.IMCSOpponent as IMCSOpponent
import Chess
import Control.Monad.Freer
import Control.Monad.Freer.Console
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Socket
import Control.Monad.Freer.State
import Control.Monad.Loops
import Data.Maybe
import Grid.Array
import IMCS
import Text.Read

-- It's not known at compile-time which agent will play which color.
clientAgent = Console.agent @ArrayBoard
serverAgent = IMCSOpponent.agent

prompt :: Member Console effs => String -> Eff effs String
prompt p = consoleWrite (p ++ ": ") >> consoleRead

runGame :: forall p effs. (Member IO effs, IMCSOpponent.AgentEffects p effs) => PlayerSing p -> Eff effs GameOutcome
runGame p = runConsoleIO $ flip evalState (Console.initialAgentState @ArrayBoard @p) $ case p of
  WHITE -> playGame (clientAgent WHITE) (serverAgent BLACK)
  BLACK -> playGame (serverAgent WHITE) (clientAgent BLACK)

-- Connect to the server, let the user choose a game offer to accept, and run a local game between a console agent and
-- one communicating with the IMCS server to represent the other player.
runConsoleVsIMCSOpponentIO :: IO ()
runConsoleVsIMCSOpponentIO = do
  result <- runM @IO $ runError @SocketError $ runError @IMCSError $ runConsoleIO $ runSocketIO "imcs.svcs.cs.pdx.edu" "3589" $ do
    socketRecv >>= ensureResponseCode 100

    user <- prompt "username"
    pass <- prompt "password"
    me user pass

    Just g <- iterateWhile isNothing $ do
      games <- list
      send $ putStrLn $ unlines $ zipWith (++) (map show [0..]) [show g ++ ": " ++ n ++ " " ++ show p | GameOffer g n p <- games]
      fmap readMaybe (send getLine) >>= \case
        Just i | 0 <= i && i < length games -> return $ Just (games !! i)
        _ -> return Nothing

    accept (gameID g) runGame

  putStrLn $ case result of
    Left SocketError -> "socket something error happens"
    Right (Left (IMCSError err)) -> "IMCS error: " ++ err
    Right (Right winner) -> show winner ++ " wins!"
