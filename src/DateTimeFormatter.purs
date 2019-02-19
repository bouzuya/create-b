module DateTimeFormatter
  ( toDateString
  , toDateTimeString
  , toTimeString
  ) where

import Data.DateTime (DateTime)
import Data.Formatter.DateTime as Formatter
import Data.List as List
import Prelude ((<>))

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

-- YYYY-MM-DDTHH:MM:SS
toDateTimeString :: DateTime -> String
toDateTimeString dt = (toDateString dt) <> "T" <> (toTimeString dt)

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
