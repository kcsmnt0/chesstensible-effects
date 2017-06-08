-- Helper functions for use in scripts that interface with IMCS to set up the local context for a game.
module IMCS where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Choice
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Socket
import Chess
import Data.List

type GameID = Int
data GameOffer = GameOffer { gameID :: GameID, name :: String, opponentPlayer :: Maybe Player }
data IMCSError = IMCSError String

instance Show GameOffer where
  show GameOffer{..} = unwords $ intersperse " " [show gameID, name, maybe "?" show opponentPlayer]

instance Show IMCSError where
  show (IMCSError err) = "IMCS error: " ++ err

ensureResponseCode :: Member (Exc IMCSError) effs => Int -> String -> Eff effs ()
ensureResponseCode c s = when (take 3 s /= show c) $ throwError $ IMCSError s

-- todo: "don't care" player choice, custom max times
offer :: (Member (Exc IMCSError) effs, Member Socket effs) => Player -> Eff effs ()
offer p = do
  socketSend $ "offer " ++ show p ++ "\r\n"
  socketRecvLine >>= ensureResponseCode 103
  resp <- socketRecvLine
  case take 3 resp of
    "105" -> void $ socketRecvCount 10
    "106" -> return ()
    _ -> throwError $ IMCSError resp

accept :: (Member (Exc IMCSError) effs, Member Socket effs) => GameID -> Eff effs Player
accept gid = do
  socketSend $ "accept " ++ show gid ++ "\r\n"
  resp <- socketRecvLine
  case take 3 resp of
    "105" -> socketRecvCount 10 >> return White -- throw away the initial board input (todo: maybe that's useful input?)
    "106" -> return Black
    _ -> throwError $ IMCSError resp

list :: forall effs. (Member (Exc IMCSError) effs, Member Socket effs) => Eff effs [GameOffer]
list = do
  socketSend "list\r\n"
  (l:ls) <- socketRecvUntil "."
  runChoices $ choose (map words ls) >>= \case
    [gid, name, player, opponentTime, ownTime, opponentRank, "[offer]"] -> return $ GameOffer (read gid) name (readPlayer (head player))
    _ -> abandon

me :: (Member (Exc IMCSError) effs, Member Socket effs) => String -> String -> Eff effs ()
me user pass = do
  socketSend $ "me " ++ user ++ " " ++ pass ++ "\r\n"
  socketRecvLine >>= ensureResponseCode 201

quit :: (Member (Exc IMCSError) effs, Member Socket effs) => Eff effs ()
quit = socketSend "quit\r\n" >> socketRecvLine >>= ensureResponseCode 200
