--
-- Project: SHWS - Simple Haskell Web Server
-- Author:  Petr Zemek <s3rvac@gmail.com>
--

-- | Server configuration.
module ServerConfig(getPort,
                    getServerRoot,
                    getIndexFile,
                    getLogFile,
                    getTimeout) where

import Network
import qualified Data.Map as Map

import ConfigParser(ParsedConfig)
import Common

-- Configuration option names.
portCName :: String
portCName = "port"
serverRootCName :: String
serverRootCName = "server-root"
indexFileCName :: String
indexFileCName = "index-file"
logFileCName :: String
logFileCName = "log-file"
timeoutCName :: String
timeoutCName = "timeout"

-- Defaults
defaultPort :: PortID
defaultPort = PortNumber 12001
defaultServerRoot :: String
defaultServerRoot = "wwwroot"
defaultIndexFile :: String
defaultIndexFile  = "index.html"
defaultLogFile :: String
defaultLogFile = "log/shws.log"
defaultTimeout :: Int
defaultTimeout = 0

-- Bounds
minPort :: Integer
minPort = 1
maxPort :: Integer
maxPort = 65535
minTimeout :: Integer
minTimeout = 0
maxTimeout :: Integer
maxTimeout = 480

{-|
    Returns port number (Left) from the selected config. If there is no port
    number specified, it returns default port number. If the port
    number is invalid, it returns Right with the error message.
-}
getPort :: ParsedConfig -> Either PortID String
getPort pc
    | Map.member portCName pc = parsePort $ pc Map.! portCName
    | otherwise               = Left $ defaultPort
    where isValidPort p = p >= minPort && p <= maxPort
          parsePort p = if isEitherLeft parsedPort && isValidPort (getEitherLeft parsedPort)
                            then Left $ PortNumber $ fromInteger $ getEitherLeft parsedPort
                            else Right $ "Invalid port number: '" ++ p ++ "'"
                        where parsedPort = stringToInteger p

{-|
    Returns server root from the selected config. If there is no
    WWW directory specified, it returns default server root.
-}
getServerRoot :: ParsedConfig -> String
getServerRoot pc = getStringOptionValue pc serverRootCName defaultServerRoot

{-|
    Returns index file from the selected config. If there is no
    index file specified, it returns default index file.
-}
getIndexFile :: ParsedConfig -> String
getIndexFile pc = getStringOptionValue pc indexFileCName defaultIndexFile

{-|
    Returns log file from the selected config. If there is no
    log file specified, it returns default log file.
-}
getLogFile :: ParsedConfig -> String
getLogFile pc = getStringOptionValue pc logFileCName defaultLogFile

{-|
    Returns timeout (Left) from the selected config. If there is no timeout
    specified, it returns default timeout. If the timeout is invalid,
    it returns Right with the error message.
-}
getTimeout :: ParsedConfig -> Either Int String
getTimeout pc
    | Map.member timeoutCName pc = parseTimeout $ pc Map.! timeoutCName
    | otherwise               = Left $ defaultTimeout
    where isValidTimeout t = t >= minTimeout && t <= maxTimeout
          parseTimeout t = if isEitherLeft parsedTimeout && isValidTimeout (getEitherLeft parsedTimeout)
                               then Left $ fromInteger $ getEitherLeft parsedTimeout
                               else Right $ "Invalid timeout value: '" ++ t ++ "'"
                           where parsedTimeout = stringToInteger t

{-|
    Returns the value of the selected option name (on) from the
    selected config (pc). If there is no such option, default value (def)
    will be returned.
-}
getStringOptionValue :: ParsedConfig -> String -> String -> String
getStringOptionValue pc on def
    | Map.member on pc = pc Map.! on
    | otherwise        = def

-- End of file
