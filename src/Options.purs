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
  { contentTemplate :: Maybe String
  , directory :: Maybe String
  , metaTemplate :: Maybe String
  , template :: Maybe String
  }
data CommandLineOptions

contentTemplateOption :: Option CommandLineOptions String
contentTemplateOption = Options.opt "contentTemplate"

directoryOption :: Option CommandLineOptions String
directoryOption = Options.opt "directory"

metaTemplateOption :: Option CommandLineOptions String
metaTemplateOption = Options.opt "metaTemplate"

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
        "--content-template" -> Tuple (Just contentTemplateOption) options
        "--directory" -> Tuple (Just directoryOption) options
        "--meta-template" -> Tuple (Just metaTemplateOption) options
        "--template" -> Tuple (Just templateOption) options
        _ -> Tuple Nothing options
    go (Tuple (Just option) options) arg =
      Tuple Nothing (options <> Options.assoc option arg)
