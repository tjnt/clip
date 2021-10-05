{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Safe (SomeException, catch)
import           Control.Monad          (when)
import           Data.Functor           ((<&>))
import qualified Data.Text              as T
import           Database.SQLite.Simple (Connection, FromRow (fromRow),
                                         Only (Only, fromOnly), execute,
                                         execute_, field, query, query_,
                                         withConnection)
import           System.Clipboard       (getClipboardString, setClipboardString)
import           System.Console.GetOpt  (ArgDescr (NoArg, ReqArg),
                                         ArgOrder (RequireOrder),
                                         OptDescr (Option), getOpt, usageInfo)
import           System.Directory       (XdgDirectory (XdgCache),
                                         getXdgDirectory)
import           System.Environment     (getArgs, getProgName)
import           System.IO              (hPrint, stderr)
import           Text.Printf            (printf)

data Flag = FlagNone | FlagList | FlagSelect Int | FlagDelete Int | FlagClear
    deriving Show

data Options = Options {
      optFlag     :: Flag
    , optDatabase :: String
    , optVerbose  :: Bool
    } deriving Show

defaultOptions :: IO Options
defaultOptions = do
    cacheDir <- (++ "/clip.db") <$> getXdgDirectory XdgCache "clip"
    return Options
        { optFlag     = FlagNone
        , optDatabase = cacheDir
        , optVerbose  = False
        }

options :: [OptDescr (Options -> Options)]
options =
    [
      Option ['l'] ["list"]
        (NoArg (\opts -> opts { optFlag = FlagList }))
        "list clipboard history"
    , Option ['c'] ["clear"]
        (NoArg (\opts -> opts { optFlag = FlagClear }))
        "clear clipboard history"
    , Option ['s'] ["select"]
        (ReqArg ((\v opts -> opts { optFlag = FlagSelect v }) . read) "ID")
        "select clipboard record"
    , Option ['d'] ["delete"]
        (ReqArg ((\v opts -> opts { optFlag = FlagDelete v }) . read) "ID")
        "select clipboard record"
    , Option ['D'] ["database"]
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
        (opts, [], []) -> defaultOptions <&> \d -> foldl (flip id) d opts
        (_, _, errs)   -> ioError (userError (concat errs ++ helpMessage))

data History = History Int T.Text T.Text deriving (Show)

instance FromRow History where
    fromRow = History <$> field <*> field <*> field

list :: Connection -> IO ()
list conn = do
    rs <- query_ conn "\
        \ SELECT * FROM history ORDER BY time desc;" :: IO [History]
    mapM_ (\(History id' _ value) ->
        printf "%d %s\n" id' (replaceCrLf value)) rs
  where
    replaceCrLf = T.replace "\r" "\\r" . T.replace "\n" "\\n"

clear :: Connection -> IO ()
clear conn = do
    execute_ conn "DELETE FROM history;"
    execute_ conn "VACUUM;"

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
        _  -> let r = T.unpack . fromOnly . head $ rs
               in getClipboardString >>= \m ->
                     when (Just r /= m) $ do
                         setClipboardString r
                         delete conn n

main :: IO ()
main = do
    opts <- parseArgs
    when (optVerbose opts) $ print opts
    withConnection (optDatabase opts)
        (\conn -> case optFlag opts of
            FlagNone     -> return ()
            FlagList     -> list conn
            FlagClear    -> clear conn
            FlagSelect n -> select conn n
            FlagDelete n -> delete conn n)
    `catch` (hPrint stderr :: SomeException -> IO ())
