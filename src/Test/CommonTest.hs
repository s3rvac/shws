--
-- Project: SHWS - Simple Haskell Web Server
-- Author:  Petr Zemek <s3rvac@gmail.com>, 2009
--

-- | Common module tests.
module CommonTest(tests) where

import Test.HUnit

import Common

-- | Returns prefix (label) for all tests.
testLabelPrefix :: String
testLabelPrefix = "Common: "

-- | Returns module tests.
tests :: [Test]
tests = [
    TestLabel (testLabelPrefix ++ "stringToInteger +0") $ TestCase $ assertEqual ""
        0
        (getEitherLeft $ stringToInteger "0"),
    TestLabel (testLabelPrefix ++ "stringToInteger -0") $ TestCase $ assertEqual ""
        0
        (getEitherLeft $ stringToInteger "-0"),
    TestLabel (testLabelPrefix ++ "stringToInteger -1679633352") $ TestCase $ assertEqual ""
        (0-1679633352)
        (getEitherLeft $ stringToInteger "-1679633352"),
    TestLabel (testLabelPrefix ++ "stringToInteger 849358349534") $ TestCase $ assertEqual ""
        849358349534
        (getEitherLeft $ stringToInteger "849358349534"),
    TestLabel (testLabelPrefix ++ "stringToInteger redundant white-spaces") $ TestCase $ assertEqual ""
        110
        (getEitherLeft $ stringToInteger " \t110\n"),
    TestLabel (testLabelPrefix ++ "stringToInteger invalid number") $ TestCase $ assertBool ""
        (isEitherRight $ stringToInteger "5fdsd"),
    TestLabel (testLabelPrefix ++ "isEitherLeft true") $ TestCase $ assertEqual ""
        True
        (isEitherLeft $ Left "left"),
    TestLabel (testLabelPrefix ++ "isEitherLeft false") $ TestCase $ assertEqual ""
        False
        (isEitherLeft $ Right "right"),
    TestLabel (testLabelPrefix ++ "isEitherRight true") $ TestCase $ assertEqual ""
        True
        (isEitherRight $ Right "right"),
    TestLabel (testLabelPrefix ++ "isEitherRight false") $ TestCase $ assertEqual ""
        False
        (isEitherRight $ Left "left"),
    TestLabel (testLabelPrefix ++ "getEitherLeft valid") $ TestCase $ assertEqual ""
        ("left")
        (getEitherLeft $ Left "left"),
    TestLabel (testLabelPrefix ++ "getEitherRight valid") $ TestCase $ assertEqual ""
        ("right")
        (getEitherRight $ Right "right")
    ]

-- End of file
