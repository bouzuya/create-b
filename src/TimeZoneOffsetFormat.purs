module TimeZoneOffsetFormat
  ( fromString
  , toString
  ) where

import Bouzuya.DateTime.TimeZoneOffset (TimeZoneOffset)
import Bouzuya.DateTime.TimeZoneOffset as TimeZoneOffset
import Control.MonadZero (guard, join, map)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either as Either
import Data.Formatter.Parser.Number (parseInteger)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Ord ((<), (>))
import Data.Ord as Ord
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Time.Duration (Minutes(..))
import Partial.Unsafe as Unsafe
import Prelude (class Ring, between, bind, discard, div, identity, mod, negate, otherwise, show, (*), (+), (<>), (==))
import Text.Parsing.Parser (runParser)

fromString :: String -> Maybe TimeZoneOffset
fromString "Z" = Just TimeZoneOffset.utc
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
  TimeZoneOffset.fromDuration
    (Minutes (Int.toNumber (sign (hours * 60 + minutes))))

toString :: TimeZoneOffset -> String
toString timeZoneOffset
  | timeZoneOffset == TimeZoneOffset.utc = "Z"
  | otherwise =
      let
        (Minutes offset') = TimeZoneOffset.toDuration timeZoneOffset
        offset = Unsafe.unsafePartial (Maybe.fromJust (Int.fromNumber offset'))
        hours = ((Ord.abs offset) `div` 60)
        minutes = ((Ord.abs offset) `mod` 60)
      in
        Array.fold
          [ if offset > 0 then "+" else "-"
          , (if hours < 10 then "0" else "") <> show hours
          , ":"
          , (if minutes < 10 then "0" else "") <> show minutes
          ]
