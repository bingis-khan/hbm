{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where


import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Options.Applicative (Parser, argument, command, subparser, str, many)
import Options.Applicative.Extra (execParser)
import Options.Applicative.Builder (info)
import Options.Applicative (helper)
import Control.Applicative ((<**>))
import Options.Applicative (metavar)
import Options.Applicative (option)
import Options.Applicative (short)
import Options.Applicative (long)
import Options.Applicative (value)
import Store (addBookmark, initDB, listBookmarks, rmBookmark)
import Data.Int (Int64)
import Data.Foldable (traverse_)
import Types (asRow)
import Options.Applicative (auto)
import Options.Applicative (progDesc)
import Options.Applicative (idm)
import System.Environment.XDG.BaseDir (getUserDataFile, getUserDataDir)
import System.Directory (createDirectoryIfMissing)




xdgDirName, dbName :: String
xdgDirName = "hbm"
dbName = "bms.sqlite"

data Bookmark = Bookmark
    {   link :: Text
    ,   tags :: [Text]
    ,   description :: Text
    } deriving Show

data Action
    =   AddBK Bookmark
    |   ListBK
    |   RmBK Int64
    deriving Show

addBK :: Parser Bookmark
addBK = Bookmark
    <$> argument str (metavar "LINK")
    <*> many (argument str (metavar "TAG"))
    <*> option str 
        (   short 'd'
        <>  long "description"
        <>  value mempty
        <>  metavar "DESCRIPTION"
        )

listBK :: Parser Action
listBK = pure ListBK

rmBK :: Parser Int64
rmBK = argument auto (metavar "ID")

opts :: Parser Action
opts = subparser 
    $   command "add"   (info (AddBK <$> addBK) idm)
    <>  command "list"  (info listBK idm)
    <>  command "rm"    (info (RmBK <$> rmBK) idm)



printBookmarks :: FilePath -> IO ()
printBookmarks path = do
    bks <- listBookmarks path
    traverse_ (TIO.putStrLn . asRow) bks 

secureFilePath :: IO FilePath
secureFilePath = do
    -- Ensure directory exists.
    dir <- getUserDataDir xdgDirName
    createDirectoryIfMissing True dir

    getUserDataFile xdgDirName dbName

main :: IO ()
main = do
    action <- execParser $ info (opts <**> helper) mempty

    db <- secureFilePath
    initDB db
    case action of
        AddBK Bookmark { link, tags, description } -> do
            bk <- addBookmark db link tags description
            putStrLn $ "Bookmark added (" <> show bk <> ")."

        ListBK ->
            printBookmarks db

        RmBK bk -> do
            didSucceed <- rmBookmark db bk
            if didSucceed
                then putStrLn $ "Successfully removed bookmark (" <> show bk <> ")." 
                else putStrLn $ "Bookmark (" <> show bk <> ") does not exist."
