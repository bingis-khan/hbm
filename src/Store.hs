{-# LANGUAGE LambdaCase, NamedFieldPuns, OverloadedStrings, OverloadedLabels, DeriveGeneric #-}
module Store (initDB, addBookmark, listBookmarks, Bookmark(..), rmBookmark) where

import Data.Text (Text)

import Database.Selda hiding (Set)
import Database.Selda.SQLite (withSQLite) 
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map ((!?))
import qualified Data.Map as M
import Data.Functor ((<&>))
import Data.Foldable (traverse_)
import Data.Int (Int64)
import qualified Types as BK


data Bookmark = Bookmark
  { bkID :: ID Bookmark
  , link :: Text
  , desc :: Text
  } deriving Generic

instance SqlRow Bookmark

bookmarks :: Table Bookmark
bookmarks = table "bookmarks" [#bkID :- autoPrimary]


data Tag = Tag
  { bkTagID :: ID Tag
  , tagBKID :: ID Bookmark
  , tag :: Text
  } deriving Generic

instance SqlRow Tag

tags :: Table Tag
tags = table "tags" [#bkTagID :- autoPrimary]

initDB :: FilePath -> IO ()
initDB path = withSQLite path $ do
  tryCreateTable bookmarks
  tryCreateTable tags


addTags :: ID Bookmark -> Set Text -> SeldaM s ()
addTags bk = insert_ tags . map (\tag -> Tag { bkTagID = def, tagBKID = bk, tag = tag }) . S.toList

addBookmark' :: Text -> Text -> SeldaM s (ID Bookmark)
addBookmark' link desc = insertWithPK bookmarks [Bookmark { bkID = def, link = link, desc = desc }] 


addBookmark :: FilePath -> Text -> [Text] -> Text -> IO (ID Bookmark)
addBookmark path link tags desc = withSQLite path $ do
  bk <- addBookmark' link desc
  addTags bk (S.fromList tags)
  return bk


listBookmarks :: FilePath -> IO [BK.Bookmark]
listBookmarks path = withSQLite path $ do
  -- I'm stupid, so I'll do this the stupid way.
  tags' <- query $ select tags
  bks' <- query $ select bookmarks

  let tags = foldr (\t -> M.insertWith (<>) (tagBKID t) (S.singleton (tag t))) mempty tags'
  let bks = map (\Bookmark { bkID, link, desc } -> BK.Bookmark { BK.tagID = fromId bkID, BK.link = link, BK.description = desc, BK.tags = M.findWithDefault mempty bkID tags }) bks'
  return bks


rmBookmark :: FilePath -> Int64 -> IO Bool
rmBookmark path id' = withSQLite path $ do
  let id = toId id'
  transaction $ do
    num <- deleteFrom bookmarks (\bk -> bk ! #bkID .== literal id)
    case num of
      0 -> -- Bad. Nothing was deleted. Invalid ID.
        return False
      _ -> do
        deleteFrom_ tags (\t -> t ! #tagBKID .== literal id)
        return True
