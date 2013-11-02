--
-- Project: SHWS - Simple Haskell Web Server
-- Author:  Petr Zemek <s3rvac@gmail.com>, 2009
--

-- | ServerConfig module tests.
module ServerConfigTest(tests) where

import Test.HUnit
import Network
import qualified Data.Map as Map

import Common
import ServerConfig

-- | Returns prefix (label) for all tests.
testLabelPrefix :: String
testLabelPrefix = "ServerConfig: "

-- | Returns module tests.
tests :: [Test]
tests = [
    TestLabel (testLabelPrefix ++ "default port number") $ TestCase $ assertBool ""
        ((\_ -> True) $ getEitherLeft $ getPort $ Map.fromList []),
    TestLabel (testLabelPrefix ++ "minimal port number") $ TestCase $ assertBool ""
        ((\(PortNumber x) -> x == 1) $ getEitherLeft $ getPort $ Map.fromList [("port", "1")]),
    TestLabel (testLabelPrefix ++ "maximal port number") $ TestCase $ assertBool ""
        ((\(PortNumber x) -> x == 65535) $ getEitherLeft $ getPort $ Map.fromList [("port", "65535")]),
    TestLabel (testLabelPrefix ++ "too low port number") $ TestCase $assertBool ""
        ((\_ -> True) $ getEitherRight $ getPort $ Map.fromList [("port", "0")]),
    TestLabel (testLabelPrefix ++ "too high port number") $ TestCase $assertBool ""
        ((\_ -> True) $ getEitherRight $ getPort $ Map.fromList [("port", "65536")]),
    TestLabel (testLabelPrefix ++ "invalid port number") $ TestCase $assertBool ""
        ((\_ -> True) $ getEitherRight $ getPort $ Map.fromList [("port", "dsfds")]),
    TestLabel (testLabelPrefix ++ "default server root") $ TestCase $ assertBool ""
        ((\_ -> True) $ getServerRoot $ Map.fromList []),
    TestLabel (testLabelPrefix ++ "custom server root") $ TestCase $ assertEqual ""
        "/home/user/mywww"
        (getServerRoot $ Map.fromList [("server-root", "/home/user/mywww")]),
    TestLabel (testLabelPrefix ++ "default index file") $ TestCase $ assertBool ""
        ((\_ -> True) $ getIndexFile $ Map.fromList []),
    TestLabel (testLabelPrefix ++ "custom index file") $ TestCase $ assertEqual ""
        "index.htm"
        (getIndexFile $ Map.fromList [("index-file", "index.htm")]),
    TestLabel (testLabelPrefix ++ "default log file") $ TestCase $ assertBool ""
        ((\_ -> True) $ getLogFile $ Map.fromList []),
    TestLabel (testLabelPrefix ++ "custom log file") $ TestCase $ assertEqual ""
        "stdout"
        (getLogFile $ Map.fromList [("log-file", "stdout")]),
    TestLabel (testLabelPrefix ++ "default timeout") $ TestCase $ assertBool ""
        ((\_ -> True) $ getEitherLeft $ getTimeout $ Map.fromList []),
    TestLabel (testLabelPrefix ++ "minimal timeout") $ TestCase $ assertBool ""
        ((\x -> x == 0) $ getEitherLeft $ getTimeout $ Map.fromList [("timeout", "0")]),
    TestLabel (testLabelPrefix ++ "maximal timeout") $ TestCase $ assertBool ""
        ((\x -> x == 480) $ getEitherLeft $ getTimeout $ Map.fromList [("timeout", "480")]),
    TestLabel (testLabelPrefix ++ "too low timeout") $ TestCase $assertBool ""
        ((\_ -> True) $ getEitherRight $ getTimeout $ Map.fromList [("timeout", "-1")]),
    TestLabel (testLabelPrefix ++ "too high timeout") $ TestCase $assertBool ""
        ((\_ -> True) $ getEitherRight $ getTimeout $ Map.fromList [("timeout", "435353")]),
    TestLabel (testLabelPrefix ++ "invalid timeout") $ TestCase $assertBool ""
        ((\_ -> True) $ getEitherRight $ getTimeout $ Map.fromList [("timeout", "dsfds")])
    ]

-- End of file
