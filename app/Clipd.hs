{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent              (threadDelay)
import           Control.Exception               (catch, throw)
import           Control.Monad                   (forever, void, when)
import           Control.Monad.State.Strict      (StateT (StateT), runStateT)
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text                       as T
import           Database.SQLite.Simple          (Connection, Only (Only),
                                                  SQLError (sqlError), execute,
                                                  execute_, withConnection)
import           Database.SQLite3.Bindings.Types (Error (ErrorBusy))
import           System.Clipboard                (getClipboardString)
import           System.Console.GetOpt           (ArgDescr (NoArg, ReqArg),
                                                  ArgOrder (RequireOrder),
                                                  OptDescr (Option), getOpt,
                                                  usageInfo)
import           System.Directory                (XdgDirectory (XdgCache),
                                                  createDirectoryIfMissing,
                                                  getXdgDirectory)
import           System.Environment              (getArgs, getProgName)
import           System.FilePath                 (takeDirectory)
import           System.IO                       (hPrint, stderr)

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

insertRecord :: Connection -> T.Text -> IO ()
insertRecord conn s =
    execute conn "\
        \ INSERT INTO history (time, value) VALUES (CURRENT_TIMESTAMP, ?);"
        $ Only s

create :: Connection -> IO ()
create conn =
    execute_ conn "\
        \ CREATE TABLE IF NOT EXISTS history ( \
        \   id INTEGER NOT NULL PRIMARY KEY, \
        \   time TEXT NOT NULL, \
        \   value BLOB NOT NULL \
        \ );"

run :: Options -> Connection -> T.Text -> IO ((), T.Text)
run opts conn s = do
    cb <- getClipboardString
    let s' = T.pack . fromMaybe "" $! cb
    when (check s s') $ do
        when (optVerbose opts) $ putStrLn $ T.unpack s'
        insertRecord conn s' `catch` sqlErrorHandler
    threadDelay $ optInterval opts * 1000
    return ((), s')
  where
    check old new = (not . T.null) new
                 && (not . T.null . T.strip) new
                 && (T.length old /= T.length new || old /= new)
    sqlErrorHandler :: SQLError -> IO ()
    sqlErrorHandler e =
        case sqlError e of
            ErrorBusy -> hPrint stderr e
            _         -> throw e

main :: IO ()
main = do
    opts <- parseArgs
    when (optVerbose opts) $ print opts
    createDirectoryIfMissing True $ takeDirectory $ optDatabase opts
    withConnection
        (optDatabase opts)
        (\conn -> do
            create conn
            s <- T.pack . fromMaybe "" <$> getClipboardString
            void $ runStateT (forever (StateT (run opts conn))) s)
