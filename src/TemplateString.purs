module TemplateString
  ( template
  ) where

import Data.String as String
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude ((<>))

template :: String -> Object String -> String
template = Object.fold go
  where
    go :: String -> String -> String -> String
    go s k v =
      String.replaceAll
        (String.Pattern ("{{" <> k <> "}}"))
        (String.Replacement v)
        s

