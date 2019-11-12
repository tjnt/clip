{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent     (threadDelay)
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe             (fromJust, fromMaybe)
import           Database.SQLite.Simple (Only (..), close, execute, execute_,
                                         open)
import           System.Clipboard       (getClipboardString)
import           System.Console.GetOpt
import           System.Directory       (XdgDirectory (..),
                                         createDirectoryIfMissing,
                                         getXdgDirectory)
import           System.Environment     (getArgs, getProgName)
import           System.FilePath        (takeDirectory)

data Options = Options {
      optDatabase :: String
    , optInterval :: Int
    , optVerbose  :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options {
      optDatabase = ""
    , optInterval = 1000
    , optVerbose  = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [
      Option ['d'] ["database"]
        (ReqArg (\v opts -> opts { optDatabase = v }) "FILE")
        "database file path"
    , Option ['i'] ["interval"]
        (ReqArg ((\v opts -> opts { optInterval = v }) . read) "MSEC")
        "interval (msec)"
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

insertRecord :: Options -> String -> IO ()
insertRecord opts s = do
    conn <- open $ optDatabase opts
    execute conn "\
        \ INSERT INTO history (time, value) VALUES (CURRENT_TIMESTAMP, ?);"
        $ Only s
    close conn

initialize :: Options -> IO ()
initialize opts = do
    createDirectoryIfMissing True $ takeDirectory $ optDatabase opts
    conn <- open $ optDatabase opts
    execute_ conn "\
        \ CREATE TABLE IF NOT EXISTS history ( \
        \   id INTEGER NOT NULL PRIMARY KEY, \
        \   time TEXT NOT NULL, \
        \   value BLOB NOT NULL \
        \ );"
    close conn

run :: Options -> String -> IO ((), String)
run opts s = do
    s' <- fromMaybe "" <$> getClipboardString
    when (s /= s') $ do
        when (optVerbose opts) $ putStrLn s'
        insertRecord opts s'
    threadDelay $ optInterval opts * 1000
    return ((), s')

main :: IO ()
main = do
    opts <- parseArgs
    when (optVerbose opts) $ print opts
    initialize opts
    s <- fromMaybe "" <$> getClipboardString
    runStateT (forever (StateT (run opts))) s
    return ()
