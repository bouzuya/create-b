module Options
  ( parse
  ) where

import Bouzuya.CommandLineOption as CommandLineOption
import Data.Either (Either)
import Data.Maybe (Maybe(..))

type Options =
  { directory :: Maybe String
  }

parse ::
  Array String
  -> Either String { arguments :: Array String, options :: Options }
parse args =
  CommandLineOption.parse
    { directory:
        CommandLineOption.maybeStringOption
          "directory" Nothing "DIR" "" Nothing
    }
    args
