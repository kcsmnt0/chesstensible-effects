module Control.Monad.Freer.Rand where

import Control.Monad.Freer
import System.Random

-- An effect for generating random values. The trick here is that the Rand constructor is only valid for types with
-- a Random instance, so pattern-matching on the constructor in runRandIO seeds the instance context with the instance
-- it needs to call randomIO.
data Rand a where Rand :: forall r. Random r => Rand r

rand :: forall a effs. (Random a, Member Rand effs) => Eff effs a
rand = send Rand

-- todo: generate an IO seed at the start and feed it through the rest of the computation purely
runRandIO :: forall a effs. Member IO effs => Eff (Rand : effs) a -> Eff effs a
runRandIO = handleRelay pure $ \Rand k -> send randomIO >>= k
