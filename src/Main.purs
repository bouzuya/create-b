module Main
  ( main
  ) where

import Prelude

import Bouzuya.TemplateString as TemplateString
import Data.Array as Array
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Exception (throw)
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Path as Path
import Node.Process as Process
import Options as Options
import TemplateVariables as TemplateVariables

readTemplate :: String -> Effect String
readTemplate = FS.readTextFile Encoding.UTF8

mkdirp :: String -> Effect Unit
mkdirp path = do
  let parent = Path.dirname path
  if parent == path
    then pure unit
    else do
      existsParent <- FS.exists parent
      if existsParent then pure unit else mkdirp parent
  exists <- FS.exists path
  if exists then pure unit else FS.mkdir path

saveTextFile :: String -> String -> Effect Unit
saveTextFile path content = do
  mkdirp (Path.dirname path)
  FS.writeTextFile Encoding.UTF8 path content

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
  saveTextFile contentPath content
  saveTextFile metaPath meta
