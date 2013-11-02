--
-- Project: SHWS - Simple Haskell Web Server
-- Author:  Petr Zemek <s3rvac@gmail.com>, 2009
--

-- | ConfigParser module tests.
module ConfigParserTest(tests) where

import Test.HUnit
import qualified Data.Map as Map

import Common
import ConfigParser

-- | Returns prefix (label) for all tests.
testLabelPrefix :: String
testLabelPrefix = "ConfigParser: "

-- | Returns module tests.
tests :: [Test]
tests = [
    TestLabel (testLabelPrefix ++ "empty lines are removed") $ TestCase $ assertEqual ""
        (Left $ Map.fromList [])
        (parseConfig "\t  \n\n"),
    TestLabel (testLabelPrefix ++ "comments are removed") $ TestCase $ assertEqual ""
        (Left $ Map.fromList [])
        (parseConfig "# comment1\n #\tcomment2  \n"),
    TestLabel (testLabelPrefix ++ "single option") $ TestCase $ assertEqual ""
        (Left $ Map.fromList [("port", "80")])
        (parseConfig "port 80\n"),
    TestLabel (testLabelPrefix ++ "multiple options") $ TestCase $ assertEqual ""
        (Left $ Map.fromList [("interface", "/dev/ttyS0"), ("speed", "9600"), ("data_bits", "5"), ("field-length", "4")])
        (parseConfig "interface /dev/ttyS0\nspeed   9600\ndata_bits\t5\nfield-length\t\t4\n"),
    TestLabel (testLabelPrefix ++ "two options with the same name") $ TestCase $ assertEqual ""
        (Left $ Map.fromList [("port", "90")])
        (parseConfig "port 80\nport 90"),
    TestLabel (testLabelPrefix ++ "missing value") $ TestCase $ assertBool ""
        ((\_ -> True) $ getEitherRight $ parseConfig "port\n")
    ]

-- End of file
