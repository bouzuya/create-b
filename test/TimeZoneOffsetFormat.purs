module Test.TimeZoneOffsetFormat
  ( tests
  ) where

import Data.Maybe (Maybe(..))
import Prelude (discard, map)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import TimeZoneOffsetFormat as TimeZoneFormat

tests :: TestSuite
tests = suite "TimeZoneOffset" do
  test "fromString / toString" do
    Assert.equal
      (Just "Z")
      (map TimeZoneFormat.toString (TimeZoneFormat.fromString "Z"))
    Assert.equal
      (Just "+09:00")
      (map TimeZoneFormat.toString (TimeZoneFormat.fromString "+09:00"))
    Assert.equal
      (Just "-09:30")
      (map TimeZoneFormat.toString (TimeZoneFormat.fromString "-09:30"))
