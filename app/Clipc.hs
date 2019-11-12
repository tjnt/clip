{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Database.SQLite.Simple (FromRow (..), Only (..), close, field,
                                         open, query, query_)
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

data History = History Int String String deriving (Show)

instance FromRow History where
    fromRow = History <$> field <*> field <*> field

list :: Options -> IO ()
list opts = do
    conn <- open $ optDatabase opts
    rs <- query_ conn "\
        \ SELECT * FROM history ORDER BY time desc;" :: IO [History]
    mapM_ (\(History id time value) ->
        printf "%d %s\n" id (delCrLf value)) rs
    close conn
  where
    delCrLf = map (\c ->
        case c of
            '\r' -> ' '
            '\n' -> ' '
            _    -> c)

select :: Options -> Int -> IO ()
select opts n = do
    conn <- open $ optDatabase opts
    rs <- query conn "\
        \ SELECT value FROM history WHERE id = ?;"
        $ Only n :: IO [Only String]
    case rs of
        [] -> return ()
        _  -> let Only r = head rs
               in setClipboardString r
    close conn

main :: IO ()
main = do
    opts <- parseArgs
    when (optVerbose opts) $ print opts
    case optFlag opts of
        FlagNone     -> return ()
        FlagList     -> list opts
        FlagSelect n -> select opts n
