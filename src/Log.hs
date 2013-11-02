--
-- Project: SHWS - Simple Haskell Web Log
-- Author:  Petr Zemek <s3rvac@gmail.com>
--

-- | Logging.
module Log(Logger,
           initLogger,
           logInfo,
           logError) where

import Control.Concurrent
import Data.Time
import Data.Time.Format
import qualified Data.ByteString.Char8 as B
import IO
import System.Locale

-- | Logger.
type Logger = Chan B.ByteString

{-|
    Initializes a logger from the selected handle and returns it.
-}
initLogger :: Handle -> IO Logger
initLogger h = do hSetBuffering h NoBuffering
                  logChan <- newChan
                  forkIO $ loggingFunc h logChan
                  return logChan

{-|
    Logging function that can run in a standalone thread.
-}
loggingFunc :: Handle -> Logger -> IO ()
loggingFunc h msgs = do msg <- readChan msgs
                        B.hPutStrLn h msg
                        hFlush h
                        loggingFunc h msgs

{-|
    Logs the selected information message.
-}
logInfo :: Logger -> String -> IO ()
logInfo l s = do time <- currentTime
                 logMessage l $ time ++ ": info: " ++ s

{-|
    Logs the selected error message.
-}
logError :: Logger -> String -> IO ()
logError l s = do time <- currentTime
                  logMessage l $ time ++ ": error: " ++ s

{-|
    Logs the selected message.
-}
logMessage :: Logger -> String -> IO ()
logMessage l s = do writeChan l $ B.pack s

{-|
    Returns current time in format "YYYY-MM-DD\/HH:MM:SS" in a string.
-}
currentTime :: IO String
currentTime = do now <- getCurrentTime
                 return $ formatTime defaultTimeLocale "%Y-%m-%d/%H:%M:%S" now

-- End of file
