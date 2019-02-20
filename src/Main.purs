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
import Effect.Exception (throw)
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Process as Process
import Prelude (class Show, Unit, bind, discard, map, mempty, pure, (<<<), (<>))
import Simple.JSON as SimpleJSON
import TemplateString as TemplateString
import TemplateVariables as TemplateVariables

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

readTemplate :: String -> Effect String
readTemplate s = FS.readTextFile Encoding.UTF8 ("./templates/" <> s)

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
  templateVariables <- TemplateVariables.build directory
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
