module Control.Monad.Freer.Rand where

import Control.Monad.Freer
import System.Random

-- todo! explain this
data Rand a where Rand :: forall r. Random r => Rand r

rand :: forall a effs. (Random a, Member Rand effs) => Eff effs a
rand = send Rand

-- todo: generate an IO seed at the start and feed it through the rest of the computation purely
runRandIO :: forall a effs. Member IO effs => Eff (Rand : effs) a -> Eff effs a
runRandIO = handleRelay pure $ \(Rand :: Rand v) k -> send (randomIO @v) >>= k
