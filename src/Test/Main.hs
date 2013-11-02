--
-- Project: SHWS - Simple Haskell Web Server
-- Author:  Petr Zemek <s3rvac@gmail.com>, 2009
--

-- | Main module.
module Main where

import Test.HUnit

import qualified CommonTest
import qualified ConfigParserTest
import qualified LogTest
import qualified MimeTest
import qualified ServerConfigTest
import qualified ServerTest

{-|
    Main function.
-}
main :: IO ()
main = do runTestTT tests
          return ()
    where tests = TestList $
        CommonTest.tests ++
        ConfigParserTest.tests ++
        LogTest.tests ++
        MimeTest.tests ++
        ServerConfigTest.tests ++
        ServerTest.tests

-- End of file
