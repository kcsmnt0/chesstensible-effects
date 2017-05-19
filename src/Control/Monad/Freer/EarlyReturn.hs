module Control.Monad.Freer.EarlyReturn where

import Control.Monad.Freer

-- basically a synonym for Exc because i don't like typing "throwError" when i mean "return correctly"
newtype EarlyReturn e a = EarlyReturn e

earlyReturn :: Member (EarlyReturn a) effs => a -> Eff effs b
earlyReturn = send . EarlyReturn

runEarlyReturn :: Eff (EarlyReturn a : effs) a -> Eff effs a
runEarlyReturn = handleRelay pure $ \(EarlyReturn e) k -> pure e
