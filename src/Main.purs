module Main
  ( main
  ) where

import Bouzuya.DateTime (date)
import Data.Array as Array
import Data.Formatter.DateTime as Formatter
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Options (Option, Options)
import Data.Options as Options
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Now (nowDateTime)
import Foreign.Object as Object
import Node.Process as Process
import OffsetDateTime as OffsetDateTime
import Prelude (class Show, Unit, bind, discard, map, mempty, pure, (<<<), (<>))
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

main :: Effect Unit
main = do
  args <- map (Array.drop 2) Process.argv
  Console.logShow args
  optionsMaybe <- pure ((optionsToRecord <<< parseOptions) args)
  options <- maybe (throw "invalid options") pure optionsMaybe
  template <-
    maybe
      (throw "invalid template")
      pure
      (maybe
        (Just BlogPostWeekday)
        templateFromString
        options.template)
  Console.logShow template
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
  wd <- pure (WeekDate.toWeekDate (date localDateTime))
  let
    templateVariables =
      Object.fromFoldable
        [ -- YYYY-MM-DDTHH:MM:SS+09:00
          Tuple "local_date_time" (OffsetDateTime.toString nowInJp)
          -- YYYY-Www
        , Tuple "year_week" (WeekDate.toYearWeekString wd)
          -- /YYYY/MM/YYYY-MM-DD
        , Tuple "blog_post_path" (Formatter.format pathFormatter localDateTime)
        ]
  generated <-
    case template of
      BlogPostWeekday -> do
        pure
          { content: ""
          , meta:
              SimpleJSON.writeJSON
                { minutes: 0
                , pubdate: "{{local_date_time}}"
                , tags: [] :: Array String
                , title: ""
                }
          , path: "{{blog_post_path}}"
          }
      BlogPostWeekend ->
        pure
          { content:
              Array.fold
                [ "{{year_week}} をふりかえる。"
                , ""
                , "# [{{year_week}} の目標][] とその記事"
                , ""
                , "目標。"
                , ""
                , "記事。"
                , ""
                , "# つくったもの"
                , ""
                , "# よんだもの"
                , ""
                , "# みたもの"
                , ""
                , "# その他"
                , ""
                , "ゲーム。"
                , ""
                , "買い物。"
                , ""
                , "体調。"
                , ""
                , "育児。"
                , ""
                , "# の目標"
                , ""
                ]
          , meta:
              SimpleJSON.writeJSON
                { minutes: 0
                , pubdate: "{{local_date_time}}"
                , tags: ["weekly report"]
                , title: "{{year_week}} ふりかえり"
                }
          , path: "{{blog_post_path}}"
          }
  Console.log (TemplateString.template generated.content templateVariables)
  Console.log (TemplateString.template generated.meta templateVariables)
  Console.log (TemplateString.template generated.path templateVariables)
