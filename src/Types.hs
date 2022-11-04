{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Types (Bookmark(..), asRow) where

import Data.Text (Text)
import Data.Set (Set, toList)
import Data.Int (Int64)
import Data.String (fromString)
import Data.List (intersperse)

data Bookmark = Bookmark
  { tagID :: Int64
  , link :: Text
  , description :: Text
  , tags :: Set Text
  }

asRow :: Bookmark -> Text
asRow Bookmark { tagID, link, description, tags } =
  mconcat $ intersperse "\t" [fromString (show tagID), link, description, mconcat $ intersperse ", " $ toList tags]
