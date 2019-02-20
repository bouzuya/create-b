module Options
  ( parseOptions
  ) where

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Options (Option, Options)
import Data.Options as Options
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Prelude (mempty, (<<<), (<>))
import Simple.JSON as SimpleJSON

type OptionsRecord =
  { directory :: Maybe String
  , template :: Maybe String
  }
data CommandLineOptions

directoryOption :: Option CommandLineOptions String
directoryOption = Options.opt "directory"

templateOption :: Option CommandLineOptions String
templateOption = Options.opt "template"

optionsToRecord :: Options CommandLineOptions -> Maybe OptionsRecord
optionsToRecord = SimpleJSON.read_ <<< Options.options

parseOptions :: Array String -> Maybe OptionsRecord
parseOptions = optionsToRecord <<< parseOptions'

parseOptions' :: Array String -> Options CommandLineOptions
parseOptions' args = Tuple.snd (Array.foldl go (Tuple Nothing mempty) args)
  where
    go (Tuple Nothing options) arg =
      case arg of
        "--directory" -> Tuple (Just directoryOption) options
        "--template" -> Tuple (Just templateOption) options
        _ -> Tuple Nothing options
    go (Tuple (Just option) options) arg =
      Tuple Nothing (options <> Options.assoc option arg)
