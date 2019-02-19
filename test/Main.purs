module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Main as Main
import Test.OffsetDateTime as OffsetDateTime
import Test.TimeZoneOffset as TimezoneOffset
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  Main.main
  log "You should add some tests."
  runTest do
    OffsetDateTime.tests
    TimezoneOffset.tests
