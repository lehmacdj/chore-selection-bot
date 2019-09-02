module Timers where

import Data.Time
import Control.Concurrent

data Hour = At Integer | HalfPast Integer
  deriving (Eq)

-- | the difftime selects the hour, in the proper timezone
-- the timezone is selected using the utcTimeZone below
data HourTime
  = Monday Hour
  | Tuesday Hour

-- the timezone to use
-- EDT is UTC-4 so -4 is used
-- EST is UTC-5 so -5 is used when not in daylight savings time
utcTimeZone :: Integer
utcTimeZone = -4

-- hours to seconds + 1 min grace period + account for UTC time
hours :: Integer -> DiffTime
hours x = secondsToDiffTime (60 * 60 * (x - utcTimeZone) + 60)

-- hours + an extra half hour
hoursPlusHalf :: Integer -> DiffTime
hoursPlusHalf x = hoursPlusHalf x + secondsToDiffTime (30 * 60)

hourToDiffTime :: Hour -> DiffTime
hourToDiffTime (At x) = hours x
hourToDiffTime (HalfPast x) = hoursPlusHalf x

-- The times at which chores need to be selected.
schedule :: [HourTime]
schedule =
    [ Monday (At 10)
    , Monday (At 11)
    , Monday (At 12)
    , Monday (At 13)
    , Monday (At 14)
    , Monday (At 15)
    , Monday (At 16)
    , Monday (At 17)
    , Monday (At 18)
    , Monday (At 19)
    , Monday (At 20)
    , Monday (At 21)
    , Monday (At 22)
    , Tuesday (At 10)
    , Tuesday (At 11)
    , Tuesday (At 12)
    , Tuesday (At 13)
    , Tuesday (At 14)
    , Tuesday (At 15)
    , Tuesday (At 16)
    , Tuesday (At 17)
    , Tuesday (At 18)
    , Tuesday (At 19)
    , Tuesday (At 20)
    , Tuesday (At 21)
    , Tuesday (At 22)
    ]

-- TODO: make sure that time doesn't start too late, i.e. on the next day
-- based on the way the scheduler works
compressedSchedule :: [HourTime]
compressedSchedule =
    [ Tuesday (HalfPast 9)
    , Tuesday (At 10)
    , Tuesday (HalfPast 10)
    , Tuesday (At 11)
    , Tuesday (HalfPast 11)
    , Tuesday (At 12)
    , Tuesday (HalfPast 12)
    , Tuesday (At 13)
    , Tuesday (HalfPast 13)
    , Tuesday (At 14)
    , Tuesday (HalfPast 14)
    , Tuesday (At 15)
    , Tuesday (HalfPast 15)
    , Tuesday (At 16)
    , Tuesday (HalfPast 16)
    , Tuesday (At 17)
    , Tuesday (HalfPast 17)
    , Tuesday (At 18)
    , Tuesday (HalfPast 18)
    , Tuesday (At 19)
    , Tuesday (HalfPast 19)
    , Tuesday (At 20)
    , Tuesday (HalfPast 20)
    , Tuesday (At 21)
    , Tuesday (HalfPast 21)
    , Tuesday (At 22)
    ]

instance Show Hour where
    show (At x)
      | x < 12 = show x ++ ":00am"
      | x == 12 = show x ++ ":00pm"
      | otherwise = show (x - 12) ++ ":00pm"
    show (HalfPast x)
      | x < 12 = show x ++ ":30am"
      | x == 12 = show x ++ ":30pm"
      | otherwise = show (x - 12) ++ ":30pm"

instance Show HourTime where
    show (Monday x) = "Monday at " ++ show x
    show (Tuesday x) = "Tuesday at " ++ show x

startDate :: Day
Just startDate = fromGregorianValid 2019 9 1

toUTCTime :: HourTime -> UTCTime
toUTCTime (Monday x) = nominalDay `addUTCTime` daytime where
  daytime = UTCTime startDate (hourToDiffTime x)
toUTCTime (Tuesday x) = (nominalDay + nominalDay) `addUTCTime` daytime where
  daytime = UTCTime startDate (hourToDiffTime x)

delay :: Num a => UTCTime -> UTCTime -> a
delay timeAct time = fromInteger $
  diffTimeToPicoseconds (realToFrac (timeAct `diffUTCTime` time)) `div` 1000000

scheduleJob :: UTCTime -> IO () -> IO ThreadId
scheduleJob timeAct action = forkIO go where
    go = do
        time <- getCurrentTime
        print (delay timeAct time)
        threadDelay (delay timeAct time)
        action
