module Control.Monad.Freer.Time where

import Control.Monad.Freer
import Data.Maybe
import Data.Time
import qualified System.Timeout as Sys

type Seconds = Double

-- todo! document
data Time :: * -> * where
  Now :: Time UTCTime
  Timeout :: UTCTime -> a -> Time (Maybe a)

now :: Member Time effs => Eff effs UTCTime
now = send Now

timeout :: Member Time effs => UTCTime -> a -> Eff effs (Maybe a)
timeout t x = send $ Timeout t x

timeoutAfter :: Member Time effs => Seconds -> a -> Eff effs (Maybe a)
timeoutAfter t x = after t >>= flip timeout x

after :: Member Time effs => Seconds -> Eff effs UTCTime
after t = addUTCTime (realToFrac t) <$> now

timeoutTakeAt :: Member Time effs => UTCTime -> [a] -> Eff effs [a]
timeoutTakeAt t [] = return []
timeoutTakeAt t (x:xs) = timeout t x >>= \case
  Just x' -> (x' :) <$> timeoutTakeAt t xs
  Nothing -> return []

timeoutTakeAfter :: Member Time effs => Seconds -> [a] -> Eff effs [a]
timeoutTakeAfter t xs = after t >>= flip timeoutTakeAt xs

runTimeIO :: Member IO effs => Eff (Time : effs) a -> Eff effs a
runTimeIO = handleRelay pure $ \m k -> case m of
  Now -> send getCurrentTime >>= k
  Timeout t x -> do
    d <- diffUTCTime t <$> send getCurrentTime
    if d > 0 then
      -- $! is strict application so x is guaranteed to get evaluated during the IO action and not lazily afterwards
      send (Sys.timeout (floor (realToFrac d * 1000000)) (return $! x)) >>= k
    else
      k Nothing
