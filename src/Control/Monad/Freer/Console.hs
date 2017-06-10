module Control.Monad.Freer.Console where

import Control.Monad.Freer
import Control.Monad.Freer.Exception

-- String-based console IO.
data Console :: * -> * where
  Read :: Console String
  Write :: String -> Console ()

consoleRead :: Member Console effs => Eff effs String
consoleRead = send Read

consoleWrite :: Member Console effs => String -> Eff effs ()
consoleWrite = send . Write

prompt :: Member Console effs => String -> Eff effs String
prompt p = consoleWrite (p ++ ": ") >> consoleRead

-- Interpret console commands in the IO monad. 
runConsoleIO :: Member IO effs => Eff (Console : effs) a -> Eff effs a
runConsoleIO = handleRelay pure $ flip $ \k -> \case
  Read -> send getLine >>= k
  Write s -> send (putStrLn s) >>= k
  where
    handler :: Member IO effs => Console a -> Arr effs a b -> Eff effs b
    handler Read k = send getLine >>= k
    handler (Write s) k = send (putStrLn s) >>= k

runErrorConsole :: (Show e, Member Console effs) => Eff (Exc e : effs) () -> Eff effs ()
runErrorConsole x = runError x >>= either (consoleWrite . show) return
