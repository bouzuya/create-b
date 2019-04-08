module TemplateVariables
  ( build
  ) where

import Prelude

import Bouzuya.DateTime.Formatter.OffsetDateTime as OffsetDateTimeFormatter
import Bouzuya.DateTime.Formatter.TimeZoneOffset as TimeZoneOffsetFormatter
import Bouzuya.DateTime.OffsetDateTime (OffsetDateTime)
import Bouzuya.DateTime.OffsetDateTime as OffsetDateTime
import Bouzuya.DateTime.WeekDate as WeekDate
import Bouzuya.TemplateString as TemplateString
import Data.Array as Array
import Data.Date (Date)
import Data.Date as Date
import Data.DateTime (DateTime(..))
import Data.DateTime as DateTime
import Data.Formatter.DateTime as Formatter
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Data.Time.Duration (Days(..))
import Data.Traversable as Traversable
import Data.Tuple (Tuple(..))
import DateTimeFormatter as DateTimeFormatter
import Effect (Effect)
import Effect.Exception as Exception
import Effect.Now as Now
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Simple.JSON as SimpleJSON
import WeekDateFormat as WeekDateFormat

type Post = { date :: String, title :: String }

build :: String -> Effect (Object String)
build directory = do
  nowInJp <- nowOffsetDateTimeInJp
  postsMaybe <- readPosts directory nowInJp
  pure (build' nowInJp postsMaybe)

build' :: OffsetDateTime -> Maybe (Array Post) -> Object String
build' nowInJp postsMaybe =
  let
    localDateTime = OffsetDateTime.toLocalDateTime nowInJp
    localDate = DateTime.date localDateTime
    -- YYYY-MM-DDTHH:MM:SSZ -> YYYYMMDDTHHMMSSZ
    toBasic =
      (String.replaceAll (String.Pattern ":") (String.Replacement "")) <<<
      (String.replaceAll (String.Pattern "-") (String.Replacement ""))
    utcOffsetDateTime = OffsetDateTime.inUTC nowInJp
    utcDateTime = OffsetDateTime.toUTCDateTime utcOffsetDateTime
    utcDateTimeString = OffsetDateTimeFormatter.toString utcOffsetDateTime
    wd = WeekDate.fromDate localDate
  in
    Object.fromFoldable
      [ -- YYYY-MM-DDTHH:MM:SS+09:00
        Tuple "date_time" (OffsetDateTimeFormatter.toString nowInJp)
      , -- YYYY-MM-DD (local)
        Tuple "date" (DateTimeFormatter.toDateString localDateTime)
      , -- DD (local)
        Tuple "day" (DateTimeFormatter.toDayString localDateTime)
      , -- MM (local)
        Tuple "month" (DateTimeFormatter.toMonthString localDateTime)
      , -- YYYY (local)
        Tuple "year" (DateTimeFormatter.toYearString localDateTime)
      , -- YYYY-MM-DDTHH:MM:SSZ
        Tuple "utc_date_time" utcDateTimeString
      , -- YYYYMMDDTHHMMSSZ
        Tuple "utc_date_time_basic" (toBasic utcDateTimeString)
      , -- DD (UTC)
        Tuple "utc_day" (DateTimeFormatter.toDayString utcDateTime)
      , -- MM (UTC)
        Tuple "utc_month" (DateTimeFormatter.toMonthString utcDateTime)
      , -- YYYY (UTC)
        Tuple "utc_year" (DateTimeFormatter.toYearString utcDateTime)
      , -- YYYY-Www
        Tuple "year_week" (WeekDateFormat.toYearWeekString wd)
      , -- - [YYYY-MM-DD title][YYYY-MM-DD]\n...
        Tuple "week_posts" (Maybe.maybe "" buildWeekPosts postsMaybe)
      ]

buildWeekPosts :: Array Post -> String
buildWeekPosts posts = String.joinWith "\n" (map postToLine posts)
  where
    lineTmpl = "- [{{date}} {{title}}][{{date}}]"
    postToVars { date, title } =
      Object.fromFoldable
        [ Tuple "date" date
        , Tuple "title" title
        ]
    postToLine post =
      TemplateString.template lineTmpl (postToVars post)

-- [(d - 1), (d - 2), ..., (d - 7)]
datesInWeekBefore :: Date -> Array Date
datesInWeekBefore date =
  Array.catMaybes do
    days <- map (Days <<< negate <<< Int.toNumber) (Array.range 1 7)
    pure (Date.adjust days date)

formatPath :: Date -> String
formatPath d = Formatter.format pathFormatter (DateTime d bottom)

nowOffsetDateTimeInJp :: Effect OffsetDateTime
nowOffsetDateTimeInJp = do
  dateTimeInUTC <- Now.nowDateTime
  jpOffset <-
    Maybe.maybe
      (Exception.throw "invalid time zone offset")
      pure
      (TimeZoneOffsetFormatter.fromString "+09:00")
  Maybe.maybe
    (Exception.throw "invalid offset date time")
    pure
    (OffsetDateTime.fromUTCDateTime jpOffset dateTimeInUTC)

pathFormatter :: Formatter.Formatter
pathFormatter =
  List.fromFoldable
    [ Formatter.Placeholder "/"
    , Formatter.YearFull
    , Formatter.Placeholder "/"
    , Formatter.MonthTwoDigits
    , Formatter.Placeholder "/"
    , Formatter.YearFull
    , Formatter.Placeholder "-"
    , Formatter.MonthTwoDigits
    , Formatter.Placeholder "-"
    , Formatter.DayOfMonthTwoDigits
    ]

readPost :: String -> Date -> Effect (Maybe Post)
readPost directory d = do
  let metaPath = directory <> (formatPath d) <> ".json"
  exists <- FS.exists metaPath
  if exists
    then do
      text <- FS.readTextFile Encoding.UTF8 metaPath
      { title } <-
        Maybe.maybe
          (Exception.throw "invalid meta data")
          pure
          (SimpleJSON.readJSON_ text :: _ { title :: String })
      pure (Maybe.Just { date: DateTimeFormatter.toDateString' d, title })
    else pure Maybe.Nothing

readPosts :: String -> OffsetDateTime -> Effect (Maybe (Array Post))
readPosts directory offsetDateTime = do
  dates <-
    pure
      (datesInWeekBefore
        (DateTime.date (OffsetDateTime.toLocalDateTime offsetDateTime)))
  postMaybes <- Traversable.traverse (readPost directory) dates
  pure (Traversable.traverse identity postMaybes)
