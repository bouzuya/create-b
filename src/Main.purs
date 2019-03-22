module Main
  ( main
  ) where

import Prelude

import Bouzuya.TemplateString as TemplateString
import Data.Array as Array
import Data.Maybe (maybe)
import Effect (Effect)
import Effect as Effect
import Effect.Exception (throw)
import Foreign.Object (Object)
import Node.Encoding as Encoding
import Node.FS.Stats as Stats
import Node.FS.Sync as FS
import Node.Path as Path
import Node.Process as Process
import Options as Options
import TemplateVariables as TemplateVariables

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

readTextFile :: String -> Effect String
readTextFile = FS.readTextFile Encoding.UTF8

writeTextFile :: String -> String -> Effect Unit
writeTextFile path content = do
  mkdirp (Path.dirname path)
  FS.writeTextFile Encoding.UTF8 path content

processDirectory :: String -> String -> Object String -> Effect Unit
processDirectory outputDirectory inputPath variables = do
  files <- map (map (append inputPath)) (FS.readdir inputPath)
  Effect.foreachE files \file -> do
    isDirectory <- map Stats.isDirectory (FS.stat file)
    if isDirectory
      then processDirectory outputDirectory file variables
      else processFile outputDirectory file variables

processFile :: String -> String -> Object String -> Effect Unit
processFile outputDirectory inputPath variables = do
  contentTemplate <- readTextFile inputPath
  let
    pathTemplate = Path.concat [outputDirectory, inputPath]
    outputPath = TemplateString.template pathTemplate variables
    outputContent = TemplateString.template contentTemplate variables
  writeTextFile outputPath outputContent

main :: Effect Unit
main = do
  args <- map (Array.drop 2) Process.argv
  optionsMaybe <- pure (Options.parseOptions args)
  options <- maybe (throw "invalid options") pure optionsMaybe
  directory <- maybe (throw "directory is required") pure options.directory
  contentTemplate <- maybe (pure "") readTextFile options.contentTemplate
  metaTemplate <- maybe (pure "") readTextFile options.metaTemplate
  templateVariables <- TemplateVariables.build directory
  let
    content = TemplateString.template contentTemplate templateVariables
    meta = TemplateString.template metaTemplate templateVariables
    path =
      TemplateString.template "/{{year}}/{{month}}/{{date}}" templateVariables
    contentPath = directory <> path <> ".md"
    metaPath = directory <> path <> ".json"
  writeTextFile contentPath content
  writeTextFile metaPath meta
  -- TODO: processDirectory directory templateDirectory templateVariables
