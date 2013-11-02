--
-- Project: SHWS - Simple Haskell Web Server
-- Author:  Petr Zemek <s3rvac@gmail.com>
--

-- | Mime module tests.
module MimeTest(tests) where

import Test.HUnit

import Mime

-- | Returns prefix (label) for all tests.
testLabelPrefix :: String
testLabelPrefix = "Mime: "

-- | Returns module tests.
tests :: [Test]
tests = [
    TestLabel (testLabelPrefix ++ "no suitable MIME type for a file extension") $ TestCase $ assertEqual ""
        defMimeTypeByExt
        (getMimeByExt "-"),
    TestLabel (testLabelPrefix ++ "MIME type for a .html file") $ TestCase $ assertEqual ""
        "text/html"
        (getMimeByExt ".html"),
    TestLabel (testLabelPrefix ++ "case-insensitiveness of the extension") $ TestCase $ assertEqual ""
        "text/html"
        (getMimeByExt ".HTmL")
    ]

-- End of file
