module Test.Options
  ( tests
  ) where

import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Options" do
  test "dummy" do
    Assert.equal 1 1
