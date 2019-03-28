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

import Data.DateTime (Date, DateTime(..), Time)
import Data.Formatter.DateTime as Formatter
import Data.List as List
import Prelude (bottom, (<>))

-- YYYY-MM-DD
toDateString :: DateTime -> String
toDateString =
  Formatter.format
    (List.fromFoldable
      [ Formatter.YearFull
      , Formatter.Placeholder "-"
      , Formatter.MonthTwoDigits
      , Formatter.Placeholder "-"
      , Formatter.DayOfMonthTwoDigits
      ])

-- YYYY-MM-DD
toDateString' :: Date -> String
toDateString' d = toDateString (DateTime d bottom)

-- YYYY-MM-DDTHH:MM:SS
toDateTimeString :: DateTime -> String
toDateTimeString dt = (toDateString dt) <> "T" <> (toTimeString dt)

-- DD
toDayString :: DateTime -> String
toDayString =
  Formatter.format
    (List.fromFoldable
      [ Formatter.DayOfMonthTwoDigits
      ])

-- DD
toDayString' :: Date -> String
toDayString' d = toDayString (DateTime d bottom)

-- MM
toMonthString :: DateTime -> String
toMonthString =
  Formatter.format
    (List.fromFoldable
      [ Formatter.MonthTwoDigits
      ])

-- MM
toMonthString' :: Date -> String
toMonthString' d = toMonthString (DateTime d bottom)

-- HH:MM:SS
toTimeString :: DateTime -> String
toTimeString =
  Formatter.format
    (List.fromFoldable
      [ Formatter.Hours24
      , Formatter.Placeholder ":"
      , Formatter.MinutesTwoDigits
      , Formatter.Placeholder ":"
      , Formatter.SecondsTwoDigits
      ])

-- HH:MM:SS
toTimeString' :: Time -> String
toTimeString' t = toTimeString (DateTime bottom t)

-- YYYY
toYearString :: DateTime -> String
toYearString =
  Formatter.format
    (List.fromFoldable
      [ Formatter.YearFull
      ])

-- YYYY
toYearString' :: Date -> String
toYearString' d = toYearString (DateTime d bottom)
