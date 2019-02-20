module Template
  ( Template(..)
  , fromString
  , toString
  ) where

import Data.Maybe (Maybe(..))
import Prelude (class Show, (<>))

data Template
  = BlogPostWeekday
  | BlogPostWeekend

instance showTemplate :: Show Template where
  show t = "(Template " <> toString t <> ")"

fromString :: String -> Maybe Template
fromString "blog-post-weekday" = Just BlogPostWeekday
fromString "blog-post-weekend" = Just BlogPostWeekend
fromString _ = Nothing

toString :: Template -> String
toString BlogPostWeekday = "blog-post-weekday"
toString BlogPostWeekend = "blog-post-weekend"
