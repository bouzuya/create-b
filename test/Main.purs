module Test.Main where

import Prelude

import Effect (Effect)
import Test.DateTimeFormatter as DateTimeFormatter
import Test.OffsetDateTime as OffsetDateTime
import Test.TimeZoneOffset as TimezoneOffset
import Test.Unit.Main (runTest)
import Test.WeekDate as WeekDate

main :: Effect Unit
main = runTest do
  DateTimeFormatter.tests
  OffsetDateTime.tests
  TimezoneOffset.tests
  WeekDate.tests
