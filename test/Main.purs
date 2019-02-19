module Test.Main where

import Prelude

import Effect (Effect)
import Test.OffsetDateTime as OffsetDateTime
import Test.TemplateString as TemplateString
import Test.TimeZoneOffset as TimezoneOffset
import Test.Unit.Main (runTest)
import Test.WeekDate as WeekDate

main :: Effect Unit
main = runTest do
  OffsetDateTime.tests
  TemplateString.tests
  TimezoneOffset.tests
  WeekDate.tests
