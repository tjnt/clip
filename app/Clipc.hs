{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception      (bracket)
import           Control.Monad
import qualified Data.Text              as T
import           Database.SQLite.Simple (Connection (..), FromRow (..),
                                         Only (..), close, execute, field, open,
                                         query, query_)
import           System.Clipboard       (setClipboardString)
import           System.Console.GetOpt
import           System.Directory       (XdgDirectory (..),
                                         createDirectoryIfMissing,
                                         getXdgDirectory)
import           System.Environment     (getArgs, getProgName)
import           Text.Printf            (printf)

data Flag = FlagNone | FlagList | FlagSelect Int
    deriving Show

data Options = Options {
      optFlag     :: Flag
    , optDatabase :: String
    , optVerbose  :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options {
      optFlag     = FlagNone
    , optDatabase = ""
    , optVerbose  = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [
      Option ['l'] ["list"]
        (NoArg (\opts -> opts { optFlag = FlagList }))
        "list clipboard history"
    , Option ['s'] ["select"]
        (ReqArg ((\v opts -> opts { optFlag = FlagSelect v }) . read) "ID")
        "select clipboard record"
    , Option ['d'] ["database"]
        (ReqArg (\v opts -> opts { optDatabase = v }) "FILE")
        "database file path"
    , Option ['v'] ["verbose"]
        (NoArg (\opts -> opts { optVerbose = True }))
        "verbose output"
    ]

parseArgs :: IO Options
parseArgs = do
    argv <- getArgs
    progName <- getProgName
    let header = "Usage: " ++ progName ++ " [OPTION...]"
        helpMessage = usageInfo header options
    case getOpt RequireOrder options argv of
        (opts, [], []) -> do
            cache <- getXdgDirectory XdgCache "clip"
            let defOpts =
                    defaultOptions {
                        optDatabase = cache ++ "/clip.db"
                    }
            return (foldl (flip id) defOpts opts)
        (_, _, errs)   -> ioError (userError (concat errs ++ helpMessage))

data History = History Int T.Text T.Text deriving (Show)

instance FromRow History where
    fromRow = History <$> field <*> field <*> field

list :: Connection -> IO ()
list conn = do
    rs <- query_ conn "\
        \ SELECT * FROM history ORDER BY time desc;" :: IO [History]
    mapM_ (\(History id time value) ->
        printf "%d %s\n" id (replaceCrLf value)) rs
  where
    replaceCrLf = T.replace "\r" "\\r" . T.replace "\n" "\\n"

delete :: Connection -> Int -> IO ()
delete conn n =
    execute conn "\
        \ DELETE FROM history WHERE id = ?;"
        $ Only n

select :: Connection -> Int -> IO ()
select conn n = do
    rs <- query conn "\
        \ SELECT value FROM history WHERE id = ?;"
        $ Only n :: IO [Only T.Text]
    case rs of
        [] -> return ()
        _  -> let Only r = head rs
               in setClipboardString $ T.unpack r

main :: IO ()
main = do
    opts <- parseArgs
    when (optVerbose opts) $ print opts
    bracket
        (open $ optDatabase opts)
        (\conn -> do
            when (optVerbose opts) $ putStrLn "close ..."
            close conn)
        (\conn ->
            case optFlag opts of
                FlagNone     -> return ()
                FlagList     -> list conn
                FlagSelect n -> select conn n)
