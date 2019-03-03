module Test.WeekDateFormat
  ( tests
  ) where

import Bouzuya.DateTime.WeekDate as WeekDate
import Data.Date as Date
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, discard, ($))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import WeekDateFormat as WeekDateFormat

tests :: TestSuite
tests = suite "WeekDate" do
  let
    -- 2019-01-02 = 2019-W01-3
    d1 = unsafePartial $ fromJust do
      year <- toEnum 2019
      month <- toEnum 1
      dom <- toEnum 2
      Date.exactDate year month dom

  test "toISOString" do
    Assert.equal
      "2019-W01-3"
      (WeekDateFormat.toISOString (WeekDate.fromDate d1))

  test "toYearWeekString" do
    Assert.equal
      "2019-W01"
      (WeekDateFormat.toYearWeekString (WeekDate.fromDate d1))
