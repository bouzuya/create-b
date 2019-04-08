module OffsetDateTime
  ( OffsetDateTime
  , inOffset
  , inUTC
  , offsetDateTime
  , toDateString
  , toDateTime
  , toString
  , toTimeString
  ) where

import Bouzuya.DateTime.Formatter.TimeZoneOffset as TimeZoneOffsetFormatter
import Bouzuya.DateTime.TimeZoneOffset (TimeZoneOffset)
import Bouzuya.DateTime.TimeZoneOffset as TimeZoneOffset
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds)
import DateTimeFormatter as DateTimeFormatter
import Prelude (class Eq, class Show, bind, map, pure, show, (<>))

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
toDateString (OffsetDateTime _ _ (LocalDateTime local)) =
  DateTimeFormatter.toDateString local

-- | local date time
toDateTime :: OffsetDateTime -> DateTime
toDateTime (OffsetDateTime _ _ (LocalDateTime local)) = local

-- YYYY-MM-DDTHH:MM:SSZ or YYYY-MM-DDTHH:MM:SS+HH:MM
toString :: OffsetDateTime -> String
toString (OffsetDateTime _ offset (LocalDateTime local)) =
  (DateTimeFormatter.toDateTimeString local) <>
    (TimeZoneOffsetFormatter.toString offset)

toTimeString :: OffsetDateTime -> String
toTimeString (OffsetDateTime _ _ (LocalDateTime local)) =
  DateTimeFormatter.toTimeString local
