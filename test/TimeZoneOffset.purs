module Test.TimeZoneOffset
  ( tests
  ) where

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Hours(..), Minutes(..))
import Prelude ((<), discard, map, negate, show)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import TimeZoneOffset (fromString, toDuration, toString, utc)

tests :: TestSuite
tests = suite "TimeZoneOffset" do
  test "Eq TimeZoneOffset" do
    Assert.equal (fromString "Z") (fromString "Z")
  test "Ord TimeZoneOffset" do
    Assert.assert "<" ((fromString "-09:00") < (fromString "+09:00"))
  test "Show TimeZoneOffset" do
    Assert.equal
      (Just "(TimeZoneOffset +09:00)")
      (map show (fromString "+09:00"))
  test "fromString / toString" do
    Assert.equal (Just "Z") (map toString (fromString "Z"))
    Assert.equal (Just "+09:00") (map toString (fromString "+09:00"))
    Assert.equal (Just "-09:30") (map toString (fromString "-09:30"))
  test "toDuration" do
    Assert.equal
      (Just (Minutes 0.0))
      ((map toDuration (fromString "Z")))
    Assert.equal
      (Just (Hours 9.0))
      ((map toDuration (fromString "+09:00")))
    Assert.equal
      (Just (Minutes 540.0))
      ((map toDuration (fromString "+09:00")))
    Assert.equal
      (Just (Minutes (negate 570.0)))
      ((map toDuration (fromString "-09:30")))
  test "utc" do
    Assert.equal "Z" (toString utc)
