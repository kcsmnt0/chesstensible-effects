module Control.Monad.Freer.Choice where

import Control.Monad
import Control.Monad.Freer

-- Nondeterministic choice. (Really just the list monad, but a little more convenient to use with Eff.)
newtype Choice a = Choose [a]

choose xs = send $ Choose xs

-- Abandon a branch of computation.
abandon :: Member Choice effs => Eff effs a
abandon = choose []

-- Basically equivalent to Control.Monad.guard.
abandonIf :: Member Choice effs => Bool -> Eff effs ()
abandonIf True = abandon
abandonIf False = return ()

-- Collect the results of all branches into a list.
-- todo: i think this actually works for any monad (with minor tweaks)
runChoices :: Eff (Choice : effs) a -> Eff effs [a]
runChoices = handleRelay (pure . pure) $ \(Choose bs) k -> join <$> mapM k bs
  where
    handler :: Choice a -> Arr effs a [b] -> Eff effs [b]
    handler (Choose bs) k = join <$> mapM k bs
