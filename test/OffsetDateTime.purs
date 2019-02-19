module Test.OffsetDateTime
  ( tests
  ) where

import Data.Either (fromRight)
import Data.Formatter.Parser.Interval as ParserInterval
import Data.Maybe (fromJust)
import OffsetDateTime (inUTC, offsetDateTime, toDateString, toString, toTimeString)
import Partial.Unsafe (unsafePartial)
import Prelude (discard, show, (<<<))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Text.Parsing.Parser as Parser
import TimeZoneOffset as TimeZoneOffset

tests :: TestSuite
tests = suite "OffsetDateTime" do
  let
    jpOffset = unsafePartial (fromJust (TimeZoneOffset.fromString "+09:00"))
    dateTime1 =
      unsafePartial
        (fromRight
          (Parser.runParser
            "2000-01-02T03:04:05Z"
            ParserInterval.parseDateTime))
    offsetDateTime1 =
      unsafePartial (fromJust (offsetDateTime jpOffset dateTime1))
  test "Show TimeZoneOffset" do
    Assert.equal
      "(OffsetDateTime 2000-01-02T12:04:05+09:00)"
      (show offsetDateTime1)
    Assert.equal
      "(OffsetDateTime 2000-01-02T03:04:05Z)"
      ((show <<< inUTC) offsetDateTime1)
  test "toDateString" do
    Assert.equal "2000-01-02" (toDateString offsetDateTime1)
    Assert.equal "2000-01-02" ((toDateString <<< inUTC) offsetDateTime1)
  test "toString" do
    Assert.equal
      "2000-01-02T12:04:05+09:00"
      (toString offsetDateTime1)
    Assert.equal
      "2000-01-02T03:04:05Z"
      ((toString <<< inUTC) offsetDateTime1)
  test "toTimeString" do
    Assert.equal "12:04:05" (toTimeString offsetDateTime1)
    Assert.equal "03:04:05" ((toTimeString <<< inUTC) offsetDateTime1)
