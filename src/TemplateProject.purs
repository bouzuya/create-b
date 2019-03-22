module TemplateProject
  ( template
  ) where

import Prelude

import Bouzuya.TemplateString as TemplateString
import Effect (Effect)
import Effect as Effect
import FS as FS
import Foreign.Object (Object)
import Node.Path as Path

processDirectory :: String -> String -> Object String -> Effect Unit
processDirectory outputDirectory inputPath variables = do
  files <- FS.readDirectory inputPath
  Effect.foreachE files \file -> do
    isDirectory <- FS.isDirectory file
    if isDirectory
      then processDirectory outputDirectory file variables
      else processFile outputDirectory file variables

processFile :: String -> String -> Object String -> Effect Unit
processFile outputDirectory inputPath variables = do
  contentTemplate <- FS.readTextFile inputPath
  let
    pathTemplate = Path.concat [outputDirectory, inputPath]
    outputPath = TemplateString.template pathTemplate variables
    outputContent = TemplateString.template contentTemplate variables
  FS.writeTextFile outputPath outputContent

template :: String -> String -> Object String -> Effect Unit
template = processDirectory
