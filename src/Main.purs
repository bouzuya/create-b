module Main
  ( main
  ) where

import Bouzuya.DateTime (Date, DateTime(..))
import Bouzuya.DateTime as DateTime
import Data.Array as Array
import Data.Date as Date
import Data.Formatter.DateTime as Formatter
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Options (Option, Options)
import Data.Options as Options
import Data.Time.Duration (Days(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import DateTimeFormatter as DateTimeFormatter
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Now (nowDateTime)
import Foreign.Object as Object
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Process as Process
import OffsetDateTime as OffsetDateTime
import Prelude (class Show, Unit, bind, bottom, discard, map, mempty, negate, pure, (<<<), (<>))
import Simple.JSON as SimpleJSON
import TemplateString as TemplateString
import TimeZoneOffset as TimeZoneOffset
import WeekDate as WeekDate

data Template
  = BlogPostWeekday
  | BlogPostWeekend

instance showTemplate :: Show Template where
  show t = "(Template " <> templateToString t <> ")"

templateFromString :: String -> Maybe Template
templateFromString "blog-post-weekday" = Just BlogPostWeekday
templateFromString "blog-post-weekend" = Just BlogPostWeekend
templateFromString _ = Nothing

templateToString :: Template -> String
templateToString BlogPostWeekday = "blog-post-weekday"
templateToString BlogPostWeekend = "blog-post-weekend"

type OptionsRecord =
  { directory :: Maybe String
  , template :: Maybe String
  }
data CommandLineOptions

directoryOption :: Option CommandLineOptions String
directoryOption = Options.opt "directory"

templateOption :: Option CommandLineOptions String
templateOption = Options.opt "template"

optionsToRecord :: Options CommandLineOptions -> Maybe OptionsRecord
optionsToRecord = SimpleJSON.read_ <<< Options.options

parseOptions :: Array String -> Options CommandLineOptions
parseOptions args = Tuple.snd (Array.foldl go (Tuple Nothing mempty) args)
  where
    go (Tuple Nothing options) arg =
      case arg of
        "--directory" -> Tuple (Just directoryOption) options
        "--template" -> Tuple (Just templateOption) options
        _ -> Tuple Nothing options
    go (Tuple (Just option) options) arg =
      Tuple Nothing (options <> Options.assoc option arg)

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

formatPath :: Date -> String
formatPath d = Formatter.format pathFormatter (DateTime d bottom)

readTemplate :: String -> Effect String
readTemplate s = FS.readTextFile Encoding.UTF8 ("./templates/" <> s)

-- [(d - 1), (d - 2), ..., (d - 7)]
datesInWeekBefore :: Date -> Array Date
datesInWeekBefore date =
  Array.catMaybes do
    days <- map (Days <<< negate <<< Int.toNumber) (Array.range 1 7)
    pure (Date.adjust days date)

main :: Effect Unit
main = do
  args <- map (Array.drop 2) Process.argv
  optionsMaybe <- pure ((optionsToRecord <<< parseOptions) args)
  options <- maybe (throw "invalid options") pure optionsMaybe
  directory <- maybe (throw "directory is required") pure options.directory
  template <-
    maybe
      (throw "invalid template")
      pure
      (maybe
        (Just BlogPostWeekday)
        templateFromString
        options.template)
  dateTimeInUTC <- nowDateTime
  inJp <-
    maybe
      (throw "invalid time zone offset")
      pure
      (TimeZoneOffset.fromString "+09:00")
  nowInJp <-
    maybe
      (throw "invalid offset date time")
      pure
      (OffsetDateTime.offsetDateTime inJp dateTimeInUTC)
  localDateTime <- pure (OffsetDateTime.toDateTime nowInJp)
  wd <- pure (WeekDate.toWeekDate (DateTime.date localDateTime))
  let
    wy = WeekDate.weekYear wd
    woy = WeekDate.weekOfYear wd
    dates = datesInWeekBefore (DateTime.date localDateTime)
  posts <-
    traverse
      (\d -> do
        text <-
          FS.readTextFile Encoding.UTF8 (directory <> (formatPath d) <> ".json")
        { title } <-
          maybe
            (throw "invalid meta data")
            pure
            (SimpleJSON.readJSON_ text :: _ { title :: String })
        pure { date: DateTimeFormatter.toDateString' d, title })
      dates
  let
    weekPosts =
      Array.intercalate
        "\n"
        (map
          (\{ date, title } ->
            TemplateString.template
              "- [{{date}} {{title}}][{{date}}]"
              (Object.fromFoldable
                [ Tuple "date" date
                , Tuple "title" title
                ]))
          posts)
    templateVariables =
      Object.fromFoldable
        [ -- YYYY-MM-DDTHH:MM:SS+09:00
          Tuple "local_date_time" (OffsetDateTime.toString nowInJp)
          -- YYYY-Www
        , Tuple "year_week" (WeekDate.toYearWeekString wd)
          -- /YYYY/MM/YYYY-MM-DD
        , Tuple "blog_post_path" (Formatter.format pathFormatter localDateTime)
          -- - [YYYY-MM-DD title][YYYY-MM-DD]\n...
        , Tuple "week_posts" weekPosts
        ]
  generated <-
    case template of
      BlogPostWeekday -> do
        content <- readTemplate "blog_post_weekday.md"
        meta <- readTemplate "blog_post_weekday.json"
        pure { content, meta, path: "{{blog_post_path}}" }
      BlogPostWeekend -> do
        content <- readTemplate "blog_post_weekend.md"
        meta <- readTemplate "blog_post_weekend.json"
        pure { content, meta, path: "{{blog_post_path}}" }
  let
    content = TemplateString.template generated.content templateVariables
    meta = TemplateString.template generated.meta templateVariables
    path = TemplateString.template generated.path templateVariables
  FS.writeTextFile Encoding.UTF8 (directory <> path <> ".md") content
  FS.writeTextFile Encoding.UTF8 (directory <> path <> ".json") meta
