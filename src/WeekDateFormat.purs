module WeekDateFormat
  ( toISOString
  , toYearWeekString
  ) where

import Bouzuya.DateTime.WeekDate (WeekDate)
import Bouzuya.DateTime.WeekDate as WeekDate
import Data.Enum as Enum
import Data.Foldable as Foldable
import Prelude (otherwise, show, (<), (<>))

-- | YYYY-Www-D
toISOString :: WeekDate -> String
toISOString wd =
  toYearWeekString wd <> "-" <> show (Enum.fromEnum (WeekDate.weekday wd))

-- | YYYY-Www
toYearWeekString :: WeekDate -> String
toYearWeekString wd =
  let
    y = Enum.fromEnum (WeekDate.weekYear wd)
    woy = Enum.fromEnum (WeekDate.week wd)
    pad2 n
      | n < 10 = "0" <> show n
      | otherwise = show n
    pad4 n
      | n < 10 = "000" <> show n
      | n < 100 = "00" <> show n
      | n < 1000 = "0" <> show n
      | otherwise = show n
  in Foldable.intercalate "" [pad4 y, "-W", pad2 woy]
