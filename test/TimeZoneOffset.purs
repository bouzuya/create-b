module Test.TimeZoneOffset
  ( tests
  ) where

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Hours(..), Milliseconds, Minutes(..), Seconds(..))
import Data.Time.Duration as Duration
import Prelude (bottom, discard, map, negate, show, top, (<))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import TimeZoneOffset (fromDuration, fromString, toDuration, toString, utc)

tests :: TestSuite
tests = suite "TimeZoneOffset" do
  test "Bounded TimeZoneOffset" do
    Assert.equal (fromString "-23:59") (Just bottom)
    Assert.equal (fromString "+23:59") (Just top)
  test "Eq TimeZoneOffset" do
    Assert.equal (fromString "Z") (fromString "Z")
  test "Ord TimeZoneOffset" do
    Assert.assert "<" ((fromString "-09:00") < (fromString "+09:00"))
  test "Show TimeZoneOffset" do
    Assert.equal
      (Just "(TimeZoneOffset +09:00)")
      (map show (fromString "+09:00"))
  test "fromDuration / toDuration" do
    Assert.equal (fromString "Z") (fromDuration (Hours 0.0))
    Assert.equal (fromString "+09:00") (fromDuration (Minutes 540.0))
    for_
      [ Duration.fromDuration (Hours 0.0)
      , Duration.fromDuration (Hours 9.0)
      , Duration.fromDuration (Hours (negate 9.0))
      ]
      \d -> do
        Assert.equal (Just d) (map toDuration (fromDuration d))
    for_
      [ Duration.fromDuration (Hours (negate 24.0))
      , Duration.fromDuration (Hours 24.0)
      , Duration.fromDuration (Seconds 1.0)
      ]
      \d -> do
        Assert.equal
          (Nothing :: _ Milliseconds)
          (map toDuration (fromDuration d))
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
  test "fromString / toString" do
    Assert.equal (Just "Z") (map toString (fromString "Z"))
    Assert.equal (Just "+09:00") (map toString (fromString "+09:00"))
    Assert.equal (Just "-09:30") (map toString (fromString "-09:30"))
  test "utc" do
    Assert.equal "Z" (toString utc)
