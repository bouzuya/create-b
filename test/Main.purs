module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.DateTimeFormatter as DateTimeFormatter
import Test.OffsetDateTime as OffsetDateTime
import Test.TimeZoneOffsetFormat as TimezoneOffsetFormat
import Test.Unit.Main (runTest)
import Test.WeekDateFormat as WeekDateFormat

main :: Effect Unit
main = runTest do
  DateTimeFormatter.tests
  OffsetDateTime.tests
  TimezoneOffsetFormat.tests
  WeekDateFormat.tests
