--
-- Project: SHWS - Simple Haskell Web Server
-- Author:  Petr Zemek <s3rvac@gmail.com>, 2009
--

-- | Configuration parsing.
module ConfigParser(ParsedConfig,
                    parseConfig) where

import List
import qualified Data.Map as Map
import Text.Regex.Posix

import Common

-- | Parsed configuration in format option name -> option value.
type ParsedConfig = Map.Map String String

{-|
    Parses the passed string and returns the parsed configuration in Left.
    If there is some error, it returns the error message in Right.

    The string must be in the following format.

    > # Comment
    > option1 value1

    > # Another comment
    > option2 value2

    Comments are not be parsed. Option values must be separated from the option
    name by whitespaces and are parsed until an end of line ('\n').
    This means that each option-value pair must be ended with a new line.
    Option names can contain only alphanumeric characters and the following
    characters: '_', '-'. Option values can contain any characters except '\n'.

    If there are more than one option with the same name, only the last
    value of this option will be taken into account.
-}
parseConfig :: String -> Either ParsedConfig String
parseConfig str
    | errorLines == [] =
        -- The configuration was parsed correctly
        Left $ Map.fromList $ map getEitherLeft nonErrorLines
    | otherwise =
        -- There was an error, so return the first error message
        Right $ getEitherRight $ head errorLines
    where parsedLines = parseLines (lines str)
          nonErrorLines = filter isEitherLeft parsedLines
          errorLines = parsedLines \\ nonErrorLines -- \\ is a list difference

{-|
    Parses all of the selected lines.
-}
parseLines :: [String] -> [Either (String, String) String]
parseLines ls = [parseLine line | line <- ls, not $ isComment line, not $ isEmpty line]
    where isComment l = l =~ "^[ \t]*#.*$" :: Bool
          isEmpty l = l =~ "^[ \t]*$" :: Bool

{-|
    Parses the selected line. The line must NOT be a comment NOR an empty line!
-}
parseLine :: String -> Either (String, String) String
parseLine line
    | line =~ "^[ \t]*([-_a-zA-Z0-9]+)[ \t]+(.+)$" =
        -- Option -> value found
        Left (head lineWords, concat $ tail lineWords)
    | otherwise =
        -- The line has invalid syntax
        Right $ "Invalid syntax on line: '" ++ line ++ "'"
    where lineWords = words line

-- End of file
