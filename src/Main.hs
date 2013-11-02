--
-- Project: SHWS - Simple Haskell Web Server
-- Author:  Petr Zemek <s3rvac@gmail.com>
--

-- | Main project module.
module Main where

import Prelude hiding (catch)
import Control.Exception
import Directory
import IO hiding (catch)
import Network
import System.Environment
import System.Exit

import Common
import ConfigParser
import Log
import qualified ServerConfig
import qualified Server

{-|
    Returns true if the selected log file means that the logger should
    be logging on standard output.
-}
logToStdOut :: String -> Bool
logToStdOut "stdout" = True
logToStdOut _        = False

{-|
    Main function.
-}
main :: IO ()
main = do args <- getArgs
          confPath <- parseArgs args
          confHandle <- openFile confPath ReadMode
              `catch` (\e -> do error $ show e)
          confContents <- hGetContents confHandle
              `catch` (\e -> do hClose confHandle
                                error $ show e)
          -- Parse the configuration file
          config <- getConfig confContents
          port <- getPort config
          serverRoot <- getServerRoot config
          indexFile <- getIndexFile config
          logFile <- getLogFile config
          timeout <- getTimeout config
          hClose confHandle

          -- Open the log file and create a logger
          logFileMode <- do fileExists <- doesFileExist logFile
                            if fileExists then return AppendMode else return WriteMode
          logh <- do if logToStdOut logFile
                         then return stdout
                         else openFile logFile logFileMode
                                  `catch` (\e -> do error $ show e)
          hSetBuffering logh NoBuffering
          logger <- initLogger logh

          -- Start the server
          Server.run port serverRoot indexFile logger timeout
              `catch` (\e -> do error $ show e
                                die)
              `finally` (do if logToStdOut logFile then hClose logh else return ())
          exit

{-|
    Returns the configuration file from the selected string. If there
    is an error, it calls error().
-}
getConfig :: String -> IO ParsedConfig
getConfig s = do let parsedConfig = parseConfig s
                 if isEitherRight parsedConfig
                     then do error $ getEitherRight parsedConfig
                     else do return $ getEitherLeft parsedConfig

{-|
    Returns the port number from the selected config. If there
    is an error, it calls error().
-}
getPort :: ParsedConfig -> IO PortID
getPort pc = do let portNum = ServerConfig.getPort pc
                if isEitherRight portNum
                    then do error $ getEitherRight portNum
                    else return $ getEitherLeft portNum

{-|
    Returns the WWW directory path from the selected config.
-}
getServerRoot :: ParsedConfig -> IO String
getServerRoot pc = do return $ ServerConfig.getServerRoot pc

{-|
    Returns the index file from the selected config.
-}
getIndexFile :: ParsedConfig -> IO String
getIndexFile pc = do return $ ServerConfig.getIndexFile pc

{-|
    Returns the log file from the selected config.
-}
getLogFile :: ParsedConfig -> IO String
getLogFile pc = do return $ ServerConfig.getLogFile pc

{-|
    Returns the timeout from the selected config. If there
    is an error, it calls error().
-}
getTimeout :: ParsedConfig -> IO Int
getTimeout pc = do let timeout = ServerConfig.getTimeout pc
                   if isEitherRight timeout
                       then do error $ getEitherRight timeout
                       else return $ getEitherLeft timeout

{-|
    Parses program arguments. If there is a help request or there are not
    enough arguments, it prints the usage and exits. Otherwise, it returns
    the configuration file path.
-}
parseArgs :: [String] -> IO String
parseArgs ["-h"]     = printUsage >> exit
parseArgs ["--help"] = printUsage >> exit
parseArgs ["-f", confPath] = return confPath
parseArgs ["--config-file", confPath] = return confPath
parseArgs []         = printUsage >> die
parseArgs _  = printUsage >> die

-- | Prints program usage.
printUsage :: IO ()
printUsage = putStrLn "Usage: shws -f|--config-file CONFIG_FILE_PATH"

-- | Exists with a success exit code.
exit :: IO a
exit = exitWith ExitSuccess

-- | Exits with a failure exit code.
die :: IO a
die = exitWith (ExitFailure 1)

-- End of file
