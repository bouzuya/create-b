module DateTimeFormatter
  ( toDateString
  , toDateString'
  , toDateTimeString
  , toDayString
  , toDayString'
  , toMonthString
  , toMonthString'
  , toTimeString
  , toTimeString'
  , toYearString
  , toYearString'
  ) where

import Prelude

import Bouzuya.DateTime.Formatter.Date as DateFormatter
import Bouzuya.DateTime.Formatter.DateTime as DateTimeFormatter
import Bouzuya.DateTime.Formatter.Time as TimeFormatter
import Data.DateTime (Date, DateTime, Time)
import Data.DateTime as DateTime
import Data.String as String

-- YYYY-MM-DD
toDateString :: DateTime -> String
toDateString = toDateString' <<< DateTime.date

-- YYYY-MM-DD
toDateString' :: Date -> String
toDateString' = DateFormatter.toString

-- YYYY-MM-DDTHH:MM:SS
toDateTimeString :: DateTime -> String
toDateTimeString = DateTimeFormatter.toString

-- DD
toDayString :: DateTime -> String
toDayString = toDayString' <<< DateTime.date

-- DD
toDayString' :: Date -> String
toDayString' =
  (String.take (String.length "DD"))
  <<< (String.drop (String.length "YYYY-MM-"))
  <<< DateFormatter.toString

-- MM
toMonthString :: DateTime -> String
toMonthString = toMonthString' <<< DateTime.date

-- MM
toMonthString' :: Date -> String
toMonthString' =
  (String.take (String.length "MM"))
  <<< (String.drop (String.length "YYYY-"))
  <<< DateFormatter.toString

-- HH:MM:SS
toTimeString :: DateTime -> String
toTimeString = toTimeString' <<< DateTime.time

-- HH:MM:SS
toTimeString' :: Time -> String
toTimeString' = TimeFormatter.toString

-- YYYY
toYearString :: DateTime -> String
toYearString = toYearString' <<< DateTime.date

-- YYYY
toYearString' :: Date -> String
toYearString' =
  (String.take (String.length "YYYY"))
  <<< DateFormatter.toString
