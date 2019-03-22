module Options
  ( parseOptions
  ) where

import Prelude

import Bouzuya.CommandLineOption as CommandLineOption
import Data.Either as Either
import Data.Maybe (Maybe(..))

type OptionsRecord =
  { contentTemplate :: Maybe String
  , directory :: Maybe String
  , metaTemplate :: Maybe String
  }

parseOptions :: Array String -> Maybe OptionsRecord
parseOptions args =
  map
    _.options
    (Either.hush
      (CommandLineOption.parse
        { contentTemplate:
            CommandLineOption.maybeStringOption
              "content-template" Nothing "FILE" "" Nothing
        , directory:
            CommandLineOption.maybeStringOption
              "directory" Nothing "DIR" "" Nothing
        , metaTemplate:
            CommandLineOption.maybeStringOption
              "meta-template" Nothing "FILE" "" Nothing
        }
        args))
