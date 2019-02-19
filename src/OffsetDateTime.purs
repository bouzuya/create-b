module OffsetDateTime
  ( OffsetDateTime
  , inOffset
  , inUTC
  , offsetDateTime
  , toDateString
  , toString
  , toTimeString
  ) where

import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Formatter.DateTime as Formatter
import Data.List as List
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds)
import Prelude (class Eq, class Show, bind, map, pure, show, (<>))
import TimeZoneOffset (TimeZoneOffset)
import TimeZoneOffset as TimeZoneOffset

newtype LocalDateTime = LocalDateTime DateTime

derive instance eqLocalDateTime :: Eq LocalDateTime

instance showLocalDateTime :: Show LocalDateTime where
  show (LocalDateTime dt)= "(LocalDateTime " <> show dt <> ")"

data OffsetDateTime = OffsetDateTime DateTime TimeZoneOffset LocalDateTime

derive instance eqOffsetDateTime :: Eq OffsetDateTime

instance showOffsetDateTime :: Show OffsetDateTime where
  show o = "(OffsetDateTime " <> toString o <> ")"

inOffset :: TimeZoneOffset -> OffsetDateTime -> Maybe OffsetDateTime
inOffset offset (OffsetDateTime utc _ _) = offsetDateTime offset utc

inUTC :: OffsetDateTime -> OffsetDateTime
inUTC (OffsetDateTime utc _ _) =
  OffsetDateTime utc TimeZoneOffset.utc (LocalDateTime utc)

localDateTime' :: TimeZoneOffset -> DateTime -> Maybe LocalDateTime
localDateTime' offset utc =
  map
    LocalDateTime
    (DateTime.adjust (TimeZoneOffset.toDuration offset :: Milliseconds) utc)

offsetDateTime :: TimeZoneOffset -> DateTime -> Maybe OffsetDateTime
offsetDateTime offset utc = do
  local <- localDateTime' offset utc
  pure (OffsetDateTime utc offset local)

timeZoneOffset :: OffsetDateTime -> TimeZoneOffset
timeZoneOffset (OffsetDateTime _ offset _) = offset

toDateString :: OffsetDateTime -> String
toDateString (OffsetDateTime _ _ (LocalDateTime local)) = toDateString' local

-- YYYY-MM-DD
toDateString' :: DateTime -> String
toDateString' =
  Formatter.format
    (List.fromFoldable
      [ Formatter.YearFull
      , Formatter.Placeholder "-"
      , Formatter.MonthTwoDigits
      , Formatter.Placeholder "-"
      , Formatter.DayOfMonthTwoDigits
      ])

-- YYYY-MM-DDTHH:MM:SS
toDateTimeString' :: DateTime -> String
toDateTimeString' dt = (toDateString' dt) <> "T" <> (toTimeString' dt)

-- YYYY-MM-DDTHH:MM:SSZ or YYYY-MM-DDTHH:MM:SS+HH:MM
toString :: OffsetDateTime -> String
toString (OffsetDateTime _ offset (LocalDateTime local)) =
  (toDateTimeString' local) <> (TimeZoneOffset.toString offset)

toTimeString :: OffsetDateTime -> String
toTimeString (OffsetDateTime _ _ (LocalDateTime local)) = toTimeString' local

-- HH:MM:SS
toTimeString' :: DateTime -> String
toTimeString' =
  Formatter.format
    (List.fromFoldable
      [ Formatter.Hours24
      , Formatter.Placeholder ":"
      , Formatter.MinutesTwoDigits
      , Formatter.Placeholder ":"
      , Formatter.SecondsTwoDigits
      ])
