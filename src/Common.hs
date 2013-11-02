--
-- Project: SHWS - Simple Haskell Web Server
-- Author:  Petr Zemek <izemek@fit.vutbr.cz>
--

-- | Variable functions for common usage.
module Common(stringToInteger,
              isEitherLeft,
              isEitherRight,
              getEitherLeft,
              getEitherRight) where

import Text.Regex.Posix

{-|
	Converts the selected string into an integer and returns it in Left.
	If there is a parsing error, error message will be returned in Right.
-}
stringToInteger :: String -> Either Integer String
stringToInteger s
	| s =~ "^[ \t\n\r]*[-]?[0-9]+[ \t\n\r]*$" = Left (read s :: Integer)
	| otherwise = Right $ "Selected number has invalid format ('" ++ s ++ "')"

{-|
	Returns true if the selected Either contains a Left item, false otherwise.
-}
isEitherLeft :: Either a b -> Bool
isEitherLeft (Left  _) = True
isEitherLeft (Right _) = False

{-|
	Returns true if the selected Either contains a Right item, false otherwise.
-}
isEitherRight :: Either a b -> Bool
isEitherRight x = not $ isEitherLeft x

{-|
	Returns the Left item from the selected Either object.
-}
getEitherLeft :: Either a b -> a
getEitherLeft (Left x) = x
getEitherLeft (Right _) = error "This Either contains Left item."

{-|
	Returns the Right item from the selected Either object.
-}
getEitherRight :: Either a b -> b
getEitherRight (Right x) = x
getEitherRight (Left _) = error "This Either contains Right item."

-- End of file
