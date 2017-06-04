module Control.Monad.Freer.Socket where

import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Data.List
import Data.ByteString.Char8 (pack, unpack)
import Network.Simple.TCP (HostName, ServiceName)
import qualified Network.Simple.TCP as TCP

-- String-based socket IO.
-- todo: Text-based socket IO (-XOverloadedStrings)
data Socket :: * -> * where
  Send :: String -> Socket ()
  Recv :: ([String] -> Bool) -> Socket [String] -- receive until buffer meets predicate

data SocketError = SocketError deriving (Show, Eq)

socketSend s = send $ Send s -- todo: there must be a usable English synonym for Send that would avoid this nonsense
socketRecv p = send $ Recv p
socketRecvCount i = send $ Recv ((i ==) . length)
socketRecvUntil l = send $ Recv ((l ==) . last)

socketRecvLine :: Member Socket effs => Eff effs String
socketRecvLine = fmap head . send $ Recv (const True)

breakAfter :: (a -> Bool) -> [a] -> Maybe ([a], [a])
breakAfter p [] = Nothing
breakAfter p (x:xs)
  | p x = Just ([x], xs)
  | otherwise = do (ys, zs) <- breakAfter p xs; Just (x:ys, zs)

-- network-simple buffers up to newlines automatically
buffer :: (Member IO effs, Member (Exc SocketError) effs) => TCP.Socket -> ([String] -> Bool) -> [String] -> Eff effs ([String], [String])
buffer s p = go
  where
    -- find the shortest prefix that matches the predicate; if there isn't one, receive and try again
    -- todo: this is cute but not super efficient
    go xs = case breakAfter (p . snd) (zip xs (tail (inits xs))) of
      Nothing ->
        send @IO (TCP.recv s 65535) >>= \case
          Nothing -> send @IO (TCP.closeSock s) >> throwError SocketError
          Just xs' -> go (xs ++ lines (filter (/= '\r') (unpack xs')))
      Just (xs, ys) -> return (map fst xs, map fst ys)

-- Interpret socket commands in the IO monad, throwing SocketError if the socket is closed unexpectedly.
runSocketIO :: forall a effs. (Member IO effs, Member (Exc SocketError) effs) => HostName -> ServiceName -> Eff (Socket : effs) a -> Eff effs a
runSocketIO host svc x = do
  s <- fmap fst $ send @IO $ TCP.connectSock host svc
  handleRelayS [] (const pure) (handler s) x <* send @IO (TCP.closeSock s)
  where
    handler :: TCP.Socket -> [String] -> Socket v -> ([String] -> Arr effs v a) -> Eff effs a
    handler s buf (Send x) k = send @IO (TCP.send s (pack x)) >>= k buf
    handler s buf (Recv p) k = do (response, leftover) <- buffer s p buf; k leftover response
