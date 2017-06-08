module Script.ConsoleVsIMCSOpponent where

import Agent.Console as Console
import Agent.IMCSOpponent as IMCSOpponent
import Chess
import Control.Monad
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

data Initiation = Offer | Accept

readInitiation :: String -> Maybe Initiation
readInitiation "offer" = Just Offer
readInitiation "accept" = Just Accept
readInitiation _ = Nothing

prompt :: Member Console effs => String -> Eff effs String
prompt p = consoleWrite (p ++ ": ") >> consoleRead

-- Connect to the server, let the user choose a game offer to accept, and run a local game between a console agent and
-- one communicating with the IMCS server to represent the other player.
runConsoleVsIMCSOpponentIO :: IO ()
runConsoleVsIMCSOpponentIO = runM $ runConsoleIO $ do
  result <- runError @SocketError $ runError @IMCSError $ runSocketIO "imcs.svcs.cs.pdx.edu" "3589" $ do
    socketRecvLine >>= ensureResponseCode 100

    user <- prompt "username"
    pass <- prompt "password"
    me user pass

    init <- untilJust (readInitiation <$> prompt "offer/accept")
    player <- case init of
      Offer -> do
        p <- untilJust (readPlayer . head <$> prompt "player (W/B)")
        offer p
        return p

      Accept -> accept <=< untilJust $ do
        games <- list
        consoleWrite $ unlines $ zipWith (++) [show x ++ ": " | x <- [0..]] (map show games)
        fmap readMaybe (send getLine) >>= \case
          Just i | 0 <= i && i < length games -> return $ Just $ gameID (games !! i)
          _ -> return Nothing

    case player of
      White -> evalState (playGame (clientAgent WHITE) (serverAgent BLACK)) $ Console.initialAgentState @ArrayBoard @White
      Black -> evalState (playGame (serverAgent WHITE) (clientAgent BLACK)) $ Console.initialAgentState @ArrayBoard @Black

  consoleWrite $ case result of
    Left SocketError -> "socket something error happens"
    Right (Left (IMCSError err)) -> "IMCS error: " ++ err
    Right (Right winner) -> show winner ++ " wins!"
