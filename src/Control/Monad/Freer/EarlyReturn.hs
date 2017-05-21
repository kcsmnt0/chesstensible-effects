module Control.Monad.Freer.EarlyReturn where

import Control.Monad.Freer

-- basically a synonym for Exc because i don't like typing "throwError" when i mean "return correctly"
newtype EarlyReturn e a = EarlyReturn e

earlyReturn :: Member (EarlyReturn a) effs => a -> Eff effs b
earlyReturn = send . EarlyReturn

runEarlyReturnLocal :: Eff (EarlyReturn a : effs) a -> Eff effs a
runEarlyReturnLocal = handleRelay pure $ \(EarlyReturn e) k -> pure e

runEarlyReturn :: Eff (EarlyReturn a : effs) b -> Eff effs (Either a b)
runEarlyReturn = handleRelay (pure . pure) $ \(EarlyReturn e) k -> pure (Left e)
