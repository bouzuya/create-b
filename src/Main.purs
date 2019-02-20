module Main
  ( main
  ) where

import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Exception (throw)
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Process as Process
import Options as Options
import Prelude (Unit, bind, discard, map, pure, (<>))
import Template as Template
import TemplateString as TemplateString
import TemplateVariables as TemplateVariables

readTemplate :: String -> Effect String
readTemplate s = FS.readTextFile Encoding.UTF8 ("./templates/" <> s)

main :: Effect Unit
main = do
  args <- map (Array.drop 2) Process.argv
  optionsMaybe <- pure (Options.parseOptions args)
  options <- maybe (throw "invalid options") pure optionsMaybe
  directory <- maybe (throw "directory is required") pure options.directory
  template <-
    maybe
      (throw "invalid template")
      pure
      (maybe
        (Just Template.BlogPostWeekday)
        Template.fromString
        options.template)
  templateVariables <- TemplateVariables.build directory
  generated <-
    case template of
      Template.BlogPostWeekday -> do
        content <- readTemplate "blog_post_weekday.md"
        meta <- readTemplate "blog_post_weekday.json"
        pure { content, meta, path: "/{{year}}/{{month}}/{{date}}" }
      Template.BlogPostWeekend -> do
        content <- readTemplate "blog_post_weekend.md"
        meta <- readTemplate "blog_post_weekend.json"
        pure { content, meta, path: "/{{year}}/{{month}}/{{date}}" }
  let
    content = TemplateString.template generated.content templateVariables
    meta = TemplateString.template generated.meta templateVariables
    path = TemplateString.template generated.path templateVariables
  FS.writeTextFile Encoding.UTF8 (directory <> path <> ".md") content
  FS.writeTextFile Encoding.UTF8 (directory <> path <> ".json") meta
