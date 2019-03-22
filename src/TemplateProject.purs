module TemplateProject
  ( template
  ) where

import Prelude

import Bouzuya.TemplateString as TemplateString
import Data.Maybe as Maybe
import Data.String as String
import Data.Traversable as Traversable
import Effect (Effect)
import Effect as Effect
import Effect.Exception as Exception
import FS as FS
import Foreign.Object (Object)
import Node.Path as Path

processDirectory :: String -> String -> Object String -> Effect Unit
processDirectory outputDirectory inputDirectory variables = do
  outputRoot <- Path.resolve [] outputDirectory
  inputRoot <- Path.resolve [] inputDirectory
  let
    go :: String -> Effect Unit
    go inputPath = do
      files <- FS.readDirectory inputPath
      Effect.foreachE files \file -> do
        isDirectory <- FS.isDirectory file
        if isDirectory
          then go file
          else do
            file' <-
              Maybe.maybe
                (Exception.throw ("invalid directory"))
                pure
                (String.stripPrefix (String.Pattern inputRoot) file)
            processFile outputRoot inputRoot file' variables
  go inputRoot

processFile :: String -> String -> String -> Object String -> Effect Unit
processFile outputDirectory inputDirectory inputPath variables = do
  contentTemplate <- FS.readTextFile (Path.concat [inputDirectory, inputPath])
  let
    pathTemplate = Path.concat [outputDirectory, inputPath]
    outputPath = TemplateString.template pathTemplate variables
    outputContent = TemplateString.template contentTemplate variables
  FS.writeTextFile outputPath outputContent

template :: String -> String -> Object String -> Effect Unit
template = processDirectory
