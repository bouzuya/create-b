module WeekDateFormat
  ( toISOString
  , toYearWeekString
  ) where

import Bouzuya.DateTime.Formatter.WeekDate as WeekDateFormatter
import Bouzuya.DateTime.WeekDate (WeekDate)
import Data.String as String

-- | YYYY-Www-D
toISOString :: WeekDate -> String
toISOString = WeekDateFormatter.toString

-- | YYYY-Www
toYearWeekString :: WeekDate -> String
toYearWeekString wd =
  String.take (String.length "YYYY-Www") (WeekDateFormatter.toString wd)
