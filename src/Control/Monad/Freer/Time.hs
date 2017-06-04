module Control.Monad.Freer.Time where

import Control.Monad.Freer
import Data.Maybe
import Data.Time
import qualified System.Timeout as Sys

import Debug.Trace

type Seconds = Double

-- todo! document
data Time :: * -> * where
  Now :: Time UTCTime
  Timeout :: UTCTime -> a -> Time (Maybe a)

now :: Member Time effs => Eff effs UTCTime
now = send Now

timeout :: Member Time effs => UTCTime -> a -> Eff effs (Maybe a)
timeout t x = send $ Timeout t x

after :: Member Time effs => Seconds -> Eff effs UTCTime
after t = addUTCTime (realToFrac t) <$> now

timeoutAfter :: Member Time effs => Seconds -> a -> Eff effs (Maybe a)
timeoutAfter t x = do t' <- after t; timeout t' x

timeoutFix :: Member Time effs => UTCTime -> (a -> a) -> a -> Eff effs a
timeoutFix t f x = timeout t (f x) >>= \case
  Nothing -> return x
  Just x' -> timeoutFix t f x'

timeoutFixAfter :: Member Time effs => Seconds -> (a -> a) -> a -> Eff effs a
timeoutFixAfter t f x = do t' <- after t; timeoutFix t' f x

-- todo: is there a good name/abstraction for this? (timeoutFold?)
timeoutIterateMapLast :: (Show e, Member Time effs) => UTCTime -> (e -> e) -> e -> (e -> a) -> Eff effs a
timeoutIterateMapLast t f x g = go x
  where
    go x = traceShow x $ timeout t (g (f x)) >>= \case
      Nothing -> return (g x)
      Just x' -> go (f x)

timeoutIterateMapLastAfter :: (Show e, Member Time effs) => Seconds -> (e -> e) -> e -> (e -> a) -> Eff effs a
timeoutIterateMapLastAfter t f x g = do t' <- after t; timeoutIterateMapLast t' f x g

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
