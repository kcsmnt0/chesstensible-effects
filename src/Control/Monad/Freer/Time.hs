module Control.Monad.Freer.Time where

import Control.Monad.Freer
import Data.Maybe
import Data.Time
import qualified System.Timeout as Sys

type Seconds = Double

-- This is a deceptively delicate thing. The second Timeout argument is a lazy computation and of course wrapping
-- it up in the Timeout constructor doesn't force its evaluation, so it goes unevaluated on to runTimeIO, which then
-- calls Sys.timeout with an IO computation that forces the argument in an IO context (which is what makes the timeout
-- possible). Forcing it at any point before then will evaluate it untimed and likely keep the result around to
-- short-circuit the timed computation with, and failing to force it in the IO context will just time-limit the
-- nearly instantaneous process of making an IO thunk out of the lazy computation and return it to be dealt with later.
data Time :: * -> * where
  Now :: Time UTCTime -- get the current time
  Timeout :: UTCTime -> a -> Time (Maybe a) -- timeout at the specified time

now :: Member Time effs => Eff effs UTCTime
now = send Now

timeout :: Member Time effs => UTCTime -> a -> Eff effs (Maybe a)
timeout t x = send $ Timeout t x

after :: Member Time effs => Seconds -> Eff effs UTCTime
after t = addUTCTime (realToFrac t) <$> now

timeoutAfter :: Member Time effs => Seconds -> a -> Eff effs (Maybe a)
timeoutAfter t x = after t >>= flip timeout x

-- take from a (presumably lazy and probably infinite) list until time is up
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
    -- $! is strict application
    if d > 0 then send (Sys.timeout (floor (realToFrac d * 1000000)) (return $! x)) >>= k else k Nothing
