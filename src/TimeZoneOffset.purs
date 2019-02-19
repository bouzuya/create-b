module TimeZoneOffset
  ( TimeZoneOffset
  , fromDuration
  , fromString
  , toDuration
  , toString
  , utc
  ) where

import Control.MonadZero (guard, join, map, pure)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either as Either
import Data.Formatter.Parser.Number (parseInteger)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord, (<), (>))
import Data.Ord as Ord
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Time.Duration (class Duration, Minutes(..))
import Data.Time.Duration as Duration
import Prelude (class Bounded, class Eq, class Ring, class Show, between, bind, bottom, discard, div, identity, mod, negate, otherwise, show, top, (*), (+), (<>), (==))
import Text.Parsing.Parser (runParser)

newtype TimeZoneOffset = TimeZoneOffset Int

instance boundedTimeZoneOffset :: Bounded TimeZoneOffset where
  top = TimeZoneOffset (23 * 60 + 59)
  bottom = TimeZoneOffset (negate (23 * 60 + 59))

derive instance eqTimeZoneOffset :: Eq TimeZoneOffset

derive instance ordTimeZoneOffset :: Ord TimeZoneOffset

instance showTimeZoneOffset :: Show TimeZoneOffset where
  show o = "(TimeZoneOffset " <> toString o <> ")"

fromDuration :: forall a. Duration a => a -> Maybe TimeZoneOffset
fromDuration d = do
  let ms = Duration.fromDuration d
  guard (between (toDuration bottom) (toDuration top) ms)
  let (Minutes minutesNumber) = Duration.convertDuration ms :: Minutes
  minutesInt <- Int.fromNumber minutesNumber
  pure (TimeZoneOffset minutesInt)

fromString :: String -> Maybe TimeZoneOffset
fromString "Z" = Just utc
fromString s = do
  regex <-
    Either.hush
      (Regex.regex "^([-+])([0-2][0-9]):([0-5][0-9])$" RegexFlags.noFlags)
  matches <- map NonEmptyArray.toArray (Regex.match regex s)
  signString <- join (Array.index matches 1)
  hoursString <- join (Array.index matches 2)
  minutesString <- join (Array.index matches 3)
  let
    sign :: forall a. Ring a => a -> a
    sign = if signString == "+" then identity else negate
  hours <- Either.hush (runParser hoursString parseInteger)
  guard (between 0 23 hours)
  minutes <- Either.hush (runParser minutesString parseInteger)
  guard (between 0 59 minutes)
  pure (TimeZoneOffset (sign (hours * 60 + minutes)))

toDuration :: forall a. Duration a => TimeZoneOffset -> a
toDuration (TimeZoneOffset offset) =
  Duration.convertDuration (Minutes (Int.toNumber offset))

toString :: TimeZoneOffset -> String
toString (TimeZoneOffset offset)
  | offset == 0 = "Z"
  | otherwise =
      let
        hours = ((Ord.abs offset) `div` 60)
        minutes = ((Ord.abs offset) `mod` 60)
      in
        Array.fold
          [ if offset > 0 then "+" else "-"
          , (if hours < 10 then "0" else "") <> show hours
          , ":"
          , (if minutes < 10 then "0" else "") <> show minutes
          ]

utc :: TimeZoneOffset
utc = TimeZoneOffset 0