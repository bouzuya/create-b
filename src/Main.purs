module Main
  ( main
  ) where

import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Options (Option, Options)
import Data.Options as Options
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Now (nowDateTime)
import Node.Process as Process
import OffsetDateTime as OffsetDateTime
import Prelude (class Show, Unit, bind, discard, map, mempty, pure, (<<<), (<>))
import Simple.JSON as SimpleJSON
import TimeZoneOffset as TimeZoneOffset

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
  generated <-
    case template of
      BlogPostWeekday -> do
        pure
          { content: ""
          , meta:
            { minutes: 0
            , pubdate: OffsetDateTime.toString nowInJp
            , tags: []
            , title: ""
            }
          , path: "/2019/01/2019-01-01"
          }
      BlogPostWeekend ->
        pure
          { content: ""
          , meta:
            { minutes: 0
            , pubdate: OffsetDateTime.toString nowInJp
            , tags: ["weekly report"]
            , title: "2019-W01 ふりかえり"
            }
          , path: "/2019/01/2019-01-01"
          }
  Console.logShow generated
