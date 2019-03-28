module TemplateVariables
  ( build
  ) where

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
import Data.Maybe (maybe)
import Data.String as String
import Data.Time.Duration (Days(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import DateTimeFormatter as DateTimeFormatter
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Now (nowDateTime)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import OffsetDateTime (OffsetDateTime)
import OffsetDateTime as OffsetDateTime
import Prelude (bind, bottom, map, negate, pure, (<<<), (<>))
import Simple.JSON as SimpleJSON
import TimeZoneOffsetFormat as TimeZoneOffsetFormat
import WeekDateFormat as WeekDateFormat

type Post = { date :: String, title :: String }

build :: String -> Effect (Object String)
build directory = do
  nowInJp <- nowOffsetDateTimeInJp
  posts <- readPosts directory (OffsetDateTime.toDateTime nowInJp)
  pure (build' nowInJp posts)

build' :: OffsetDateTime -> Array Post -> Object String
build' nowInJp posts =
  let
    localDateTime = OffsetDateTime.toDateTime nowInJp
    localDate = DateTime.date localDateTime
    -- YYYY-MM-DDTHH:MM:SSZ -> YYYYMMDDTHHMMSSZ
    toBasic =
      (String.replaceAll (String.Pattern ":") (String.Replacement "")) <<<
      (String.replaceAll (String.Pattern "-") (String.Replacement ""))
    utcOffsetDateTime = OffsetDateTime.inUTC nowInJp
    utcDateTime = OffsetDateTime.toDateTime utcOffsetDateTime
    utcDateTimeString = OffsetDateTime.toString utcOffsetDateTime
    wd = WeekDate.fromDate localDate
  in
    Object.fromFoldable
      [ -- YYYY-MM-DDTHH:MM:SS+09:00
        Tuple "date_time" (OffsetDateTime.toString nowInJp)
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
        Tuple "week_posts" (buildWeekPosts posts)
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
  dateTimeInUTC <- nowDateTime
  jpOffset <-
    maybe
      (throw "invalid time zone offset")
      pure
      (TimeZoneOffsetFormat.fromString "+09:00")
  maybe
    (throw "invalid offset date time")
    pure
    (OffsetDateTime.offsetDateTime jpOffset dateTimeInUTC)

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

readPost :: String -> Date -> Effect Post
readPost directory d = do
  let metaPath = directory <> (formatPath d) <> ".json"
  text <- FS.readTextFile Encoding.UTF8 metaPath
  { title } <-
    maybe
      (throw "invalid meta data")
      pure
      (SimpleJSON.readJSON_ text :: _ { title :: String })
  pure { date: DateTimeFormatter.toDateString' d, title }

readPosts :: String -> DateTime -> Effect (Array Post)
readPosts directory localDateTime =
  traverse
    (readPost directory)
    (datesInWeekBefore (DateTime.date localDateTime))
