module Script.IMCSConsole where

import Control.Monad.Freer
import Control.Monad.Freer.Console
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Socket

-- Basically telnet to IMCS implemented in terms of the Socket effect, for testing and debugging.
imcs :: IO ()
imcs = do
  r <- runM $ runError $ runConsoleIO $ runSocketIO "imcs.svcs.cs.pdx.edu" "3589" $ do
    socketRecv >>= consoleWrite
    loop
  putStrLn $ case r of
    Left SocketError -> "socket closed or broke or something"
    Right () -> "who even knows"
  where
    loop :: (Member Console effs, Member Socket effs) => Eff effs ()
    loop = do
      socketSend . (++ "\n") =<< consoleRead
      consoleWrite =<< socketRecv
      loop
