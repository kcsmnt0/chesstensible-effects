-- Helper functions for use in scripts that interface with IMCS to set up the local context for a game.
module IMCS where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Choice
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Socket
import Chess

type GameID = Int
data GameOffer = GameOffer { gameID :: GameID, name :: String, opponentPlayer :: Maybe Player }
data IMCSError = IMCSError String

ensureResponseCode :: Member (Exc IMCSError) effs => Int -> String -> Eff effs ()
ensureResponseCode c s = when (take 3 s /= show c) $ throwError $ IMCSError s

-- This funtcion can't return a "PlayerSing p", because p isn't known at compile time, so it takes in a callback that
-- works for either possible instantiation of p.
accept :: (Member (Exc IMCSError) effs, Member Socket effs) => GameID -> (forall p. Arr effs (PlayerSing p) a) -> Eff effs a
accept gid k = do
  socketSend $ "accept " ++ show gid ++ "\r\n"
  resp <- socketRecv
  case take 3 resp of
    "105" -> socketRecv >> k WHITE
    "106" -> k BLACK
    _ -> throwError $ IMCSError resp

list :: forall effs. (Member (Exc IMCSError) effs, Member Socket effs) => Eff effs [GameOffer]
list = do
  socketSend "list\r\n"
  socketRecv >>= ensureResponseCode 211
  offersResp <- socketRecv
  runChoices $ choose (map words (lines offersResp)) >>= \case
    [gid, name, player, opponentTime, ownTime, opponentRank, "[offer]"] ->
      return $ GameOffer (read gid) name (readPlayer (head player))
    _ -> abandon

me :: (Member (Exc IMCSError) effs, Member Socket effs) => String -> String -> Eff effs ()
me user pass = do
  socketSend $ "me " ++ user ++ " " ++ pass ++ "\r\n"
  socketRecv >>= ensureResponseCode 201

offer :: (Member (Exc IMCSError) effs, Member Socket effs) => Maybe Player -> Eff effs Player
offer player = do
  socketSend $ "offer" ++ maybe "" ((" " ++) . show) player ++ "\r\n"
  socketRecv >>= ensureResponseCode 103
  startResp <- socketRecv
  case take 3 startResp of
    "105" -> socketRecv >> return White -- throw away the board input (todo: maybe that's useful input?)
    "106" -> return Black
    _ -> throwError $ IMCSError startResp

quit :: (Member (Exc IMCSError) effs, Member Socket effs) => Eff effs ()
quit = socketSend "quit\r\n" >> socketRecv >>= ensureResponseCode 200
