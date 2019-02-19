module Test.TemplateString
  ( tests
  ) where

import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import TemplateString (template)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "TemplateString" do
  test "template" do
    let
      obj =
        Object.fromFoldable
          [ Tuple "foo" "FOO"
          , Tuple "bar" "BAR"
          ]
    Assert.equal
      "FOOBAR"
      (template "{{foo}}{{bar}}" obj)
