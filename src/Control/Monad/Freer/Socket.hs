module Control.Monad.Freer.Socket where

import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Data.ByteString.Char8 (pack, unpack)
import Network.Simple.TCP (HostName, ServiceName)
import qualified Network.Simple.TCP as TCP

-- String-based socket IO.
data Socket :: * -> * where -- todo: maybe ByteString instead of String? (maybe an arbitrary IsString instance?)
  Send :: String -> Socket ()
  Recv :: Socket String

data SocketError = SocketError deriving (Show, Eq)

socketSend :: Member Socket effs => String -> Eff effs ()
socketSend = send . Send -- todo: there must be a usable English synonym for Send that would avoid this nonsense

socketRecv :: Member Socket effs => Eff effs String
socketRecv = send Recv

-- Interpret socket commands in the IO monad, throwing SocketError if the socket is closed unexpectedly.
runSocketIO :: (Member (Exc SocketError) effs, Member IO effs) => HostName -> ServiceName -> Eff (Socket : effs) a -> Eff effs a
runSocketIO host svc x = do
  s <- fmap fst $ send @IO $ TCP.connectSock host svc
  x' <- handleRelay pure (handler s) x
  send @IO $ TCP.closeSock s
  return x'
  where
    handler :: (Member (Exc SocketError) effs, Member IO effs) => TCP.Socket -> Socket v -> Arr effs v a -> Eff effs a
    handler s (Send x) k = send @IO (TCP.send s (pack x)) >>= k
    handler s Recv k = send @IO (TCP.recv s 4096) >>= \case -- todo: i wonder what i'm supposed to do with this length
      Just l -> k $ unpack l
      Nothing -> send @IO (TCP.closeSock s) >> throwError SocketError
