module Main
  ( main
  ) where

import Data.Array as Array
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Exception (throw)
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Process as Process
import Options as Options
import Prelude (Unit, bind, discard, map, pure, (<>))
import TemplateString as TemplateString
import TemplateVariables as TemplateVariables

readTemplate :: String -> Effect String
readTemplate = FS.readTextFile Encoding.UTF8

main :: Effect Unit
main = do
  args <- map (Array.drop 2) Process.argv
  optionsMaybe <- pure (Options.parseOptions args)
  options <- maybe (throw "invalid options") pure optionsMaybe
  directory <- maybe (throw "directory is required") pure options.directory
  contentTemplate <- maybe (pure "") readTemplate options.contentTemplate
  metaTemplate <- maybe (pure "") readTemplate options.metaTemplate
  templateVariables <- TemplateVariables.build directory
  let
    content = TemplateString.template contentTemplate templateVariables
    meta = TemplateString.template metaTemplate templateVariables
    path =
      TemplateString.template "/{{year}}/{{month}}/{{date}}" templateVariables
    contentPath = directory <> path <> ".md"
    metaPath = directory <> path <> ".json"
  FS.writeTextFile Encoding.UTF8 contentPath content
  FS.writeTextFile Encoding.UTF8 metaPath meta
