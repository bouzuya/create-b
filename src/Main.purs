module Main
  ( main
  ) where

import Prelude

import Bouzuya.TemplateString as TemplateString
import Data.Array as Array
import Data.Either as Either
import Data.Maybe as Maybe
import Effect (Effect)
import Effect as Effect
import Effect.Exception as Exception
import FS as FS
import Foreign.Object (Object)
import Node.Path as Path
import Node.Process as Process
import Options as Options
import TemplateVariables as TemplateVariables

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

main :: Effect Unit
main = do
  args <- map (Array.drop 2) Process.argv
  { arguments, options } <-
    Either.either
      (const (Exception.throw "invalid options"))
      pure
      (Options.parse args)
  directory <- pure (Maybe.fromMaybe "." options.directory)
  templateDirectory <-
    Maybe.maybe
      (Exception.throw "template is required")
      pure
      (Array.head arguments)
  templateVariables <- TemplateVariables.build directory
  processDirectory directory templateDirectory templateVariables
