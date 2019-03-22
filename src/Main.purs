module Main
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Either as Either
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Exception as Exception
import Node.Process as Process
import Options as Options
import TemplateProject as TemplateProject
import TemplateVariables as TemplateVariables

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
  TemplateProject.template directory templateDirectory templateVariables
