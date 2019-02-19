module Test.DateTimeFormatter
  ( tests
  ) where

import Data.Date (exactDate)
import Data.DateTime (DateTime(..))
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Data.Time (Time(..))
import DateTimeFormatter (toDateString, toDateTimeString, toTimeString)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, discard, pure, (<$>), (<*>))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "DateTimeFormatter" do
  let
    dateMaybe1 = do
      y <- toEnum 2000
      m <- toEnum 1
      d <- toEnum 2
      exactDate y m d
    timeMaybe1 = do
      h <- toEnum 3
      m <- toEnum 4
      s <- toEnum 5
      ms <- toEnum 6
      pure (Time h m s ms)
    dt1 = unsafePartial (fromJust (DateTime <$> dateMaybe1 <*> timeMaybe1))
  test "toDateString" do
    Assert.equal "2000-01-02" (toDateString dt1)
  test "toDateTimeString" do
    Assert.equal "2000-01-02T03:04:05" (toDateTimeString dt1)
  test "toTimeString" do
    Assert.equal "03:04:05" (toTimeString dt1)
