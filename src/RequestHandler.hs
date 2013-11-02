--
-- Project: SHWS - Simple Haskell Web Server
-- Author:  Petr Zemek <s3rvac@gmail.com>
--

-- | Client request handler.
module RequestHandler(RequestHandler,
                      getRequestHandler) where

import Directory
import qualified Data.ByteString.Char8 as B
import IO
import Log
import Mime
import Network.HTTP
import Network.HTTP.Headers
import System.FilePath.Posix
import Text.Html

-- | Client request handler (request and host name).
type RequestHandler = Request -> String -> IO Response

{-|
    Returns the request handler. Parameters are the path to the web server root
    directory, where requested files will be searched, index file and logger.
-}
getRequestHandler :: String -> String -> Logger -> RequestHandler
getRequestHandler serverRoot indexFile logger (Request reqUri reqMethod _ _) hn =
    do logInfo logger $ hn ++ " -- " ++ (show reqMethod) ++ " " ++ (show reqUri)
       handle reqMethod
    where -- Returns complete file path according to the requested URI
          filePath
              | (last uriStr) == '/' = serverRoot ++ "/" ++ indexFile
              | otherwise            = serverRoot ++ uriStr
              where uriStr = show reqUri

          -- Tries to send the selected file
          readAndSendFile = do
              -- Open the file and read its contents (use ByteString because
              -- hReadContents would eat up all available memory for)
              fc <- B.readFile filePath
              -- File was read correctly, so send 200
              send200 (B.unpack fc) $ takeExtension filePath

          -- GET method
          handle GET = do
              -- Check whether the file exists
              fileExists <- doesFileExist filePath
              if not fileExists
                   then send404 filePath
                   else do
              -- The file exists, so if the file cannot be opened or if there
              -- is another problem, return 403 (Forbidden)
              response <- readAndSendFile
                  `catch` (\_ -> send403 filePath)
              return response

          -- HEAD method
          handle HEAD = do
              response <- handle GET
              return $ removeBody response

          -- Other methods (not implemented)
          handle m = do
              send501 $ show m

          -- Removes body from the selected response
          removeBody (Response rc rs headers _) = Response rc rs headers ""

{-|
    Returns common headers for all responses.
-}
commonHeaders :: [Header]
commonHeaders = [
    Header HdrServer "SHWS - Simple Haskell Web Server"
    ]

{-|
    Returns content type based on the selected extension.
-}
contentTypeHdr :: String -> Header
contentTypeHdr ext = Header HdrContentType $ getMimeByExt ext

{-|
    Returns Content-length header.
-}
contentLengthHdr :: String -> Header
contentLengthHdr c = Header HdrContentLength (show $ length c)

{-|
    Returns the 200 (OK) response header.
-}
send200 :: String -> String -> IO Response
send200 msgBody ext = do return $ Response (2, 0, 0) "OK" headers msgBody
    where headers = commonHeaders ++ [contentLengthHdr msgBody] ++ [contentTypeHdr ext]

{-|
    Returns the 403 (Forbidden) response header.
-}
send403 :: String -> IO Response
send403 url = do return $ Response (4, 0, 3) "Forbidden" headers msgBody
    where msgBody = genResponseMsgBody "403 Forbidden" "Forbidden"
                        ("You don't have permission to access " ++ url ++ " on this server.")
          headers = commonHeaders ++ [contentLengthHdr msgBody] ++ [contentTypeHdr ".html"]

{-|
    Returns the 404 (Not Found) response header.
-}
send404 :: String -> IO Response
send404 url = do return $ Response (4, 0, 4) "Not Found" headers msgBody
    where msgBody = genResponseMsgBody "404 Not Found" "Not Found"
                        ("The requested URL " ++ url ++ " was not found on this server.")
          headers = commonHeaders ++ [contentLengthHdr msgBody] ++ [contentTypeHdr ".html"]

{-|
    Returns the 501 (Not Implemented) response header.
-}
send501 :: String -> IO Response
send501 methodName = do return $ Response (5, 0, 1) "Not Implemented" headers msgBody
    where msgBody = genResponseMsgBody "501 Not Implemented" "Not Implemented"
                        ("The requested method " ++ methodName ++ " is not implemented.")
          headers = commonHeaders ++ [contentLengthHdr msgBody] ++ [contentTypeHdr ".html"]

{-|
    Returns message body for server generated (non 200) responses.
-}
genResponseMsgBody :: String -> String -> String -> String
genResponseMsgBody t h txt = prettyHtml $
    header << thetitle << t +++ body << h1 << h +++ paragraph << txt

-- End of file
