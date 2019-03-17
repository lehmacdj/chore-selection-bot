module Timers where

import Data.Time
import Control.Concurrent

type Hour = Integer

data HourTime
  = Monday Hour
  | Tuesday Hour

-- The times at which chores need to be selected.
schedule :: [HourTime]
schedule =
    [ Monday 10
    , Monday 11
    , Monday 12
    , Monday 13
    , Monday 14
    , Monday 15
    , Monday 16
    , Monday 17
    , Monday 18
    , Monday 19
    , Monday 20
    , Monday 21
    , Monday 22
    , Tuesday 10
    , Tuesday 11
    , Tuesday 12
    , Tuesday 13
    , Tuesday 14
    , Tuesday 15
    , Tuesday 16
    , Tuesday 17
    , Tuesday 18
    , Tuesday 19
    , Tuesday 20
    , Tuesday 21
    , Tuesday 22
    ]

instance Show HourTime where
    show (Monday x)
      | x < 12 = "Monday at " ++ show x ++ "am"
      | x == 12 = "Monday at " ++ show x ++ "pm"
      | x > 12 = "Monday at " ++ show (x - 12) ++ "pm"
    show (Tuesday x)
      | x < 12 = "Tuesday at " ++ show x ++ "am"
      | x == 12 = "Tuesday at " ++ show x ++ "pm"
      | x > 12 = "Tuesday at " ++ show (x - 12) ++ "pm"

startDate :: Day
Just startDate = fromGregorianValid 2019 3 17

-- hours to seconds + 1 min grace period
hours :: Integer -> DiffTime
hours x = secondsToDiffTime (60 * 60 * (x + 11) + 30 * 60 + 60)

toUTCTime :: HourTime -> UTCTime
toUTCTime (Monday x) = UTCTime startDate (hours x)
toUTCTime (Tuesday x) = nominalDay `addUTCTime` UTCTime startDate (hours x)

delay timeAct time = fromInteger $ diffTimeToPicoseconds (realToFrac (timeAct `diffUTCTime` time)) `div` 1000000

scheduleJob :: UTCTime -> IO () -> IO ThreadId
scheduleJob timeAct action = forkIO go where
    go = do
        time <- getCurrentTime
        print (delay timeAct time)
        threadDelay (delay timeAct time)
        action
