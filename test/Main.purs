module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.DateTimeFormatter as DateTimeFormatter
import Test.Options as Options
import Test.TemplateVariables as TemplateVariables
import Test.Unit.Main (runTest)
import Test.WeekDateFormat as WeekDateFormat

main :: Effect Unit
main = runTest do
  DateTimeFormatter.tests
  Options.tests
  TemplateVariables.tests
  WeekDateFormat.tests
