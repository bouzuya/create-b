module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.DateTimeFormatter as DateTimeFormatter
import Test.OffsetDateTime as OffsetDateTime
import Test.Options as Options
import Test.TemplateVariables as TemplateVariables
import Test.TimeZoneOffsetFormat as TimezoneOffsetFormat
import Test.Unit.Main (runTest)
import Test.WeekDateFormat as WeekDateFormat

main :: Effect Unit
main = runTest do
  DateTimeFormatter.tests
  OffsetDateTime.tests
  Options.tests
  TemplateVariables.tests
  TimezoneOffsetFormat.tests
  WeekDateFormat.tests
