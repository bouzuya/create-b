module Test.TemplateString
  ( tests
  ) where

import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Prelude (discard)
import TemplateString (template)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "TemplateString" do
  test "template" do
    let
      obj1 =
        Object.fromFoldable
          [ Tuple "foo" "FOO"
          , Tuple "bar" "BAR"
          ]
    Assert.equal
      "FOOBAR"
      (template "{{foo}}{{bar}}" obj1)
    let
      obj2 =
        Object.fromFoldable
          [ Tuple "foo" "{{bar}}"
          , Tuple "bar" "{{foo}}"
          ]
    Assert.equal
      "{{bar}}{{foo}}"
      (template "{{foo}}{{bar}}" obj2)
