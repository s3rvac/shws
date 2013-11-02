--
-- Project: SHWS - Simple Haskell Web Server
-- Author:  Petr Zemek <s3rvac@gmail.com>, 2009
--

-- | HTTP server.
module Server(run) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Maybe
import IO hiding (catch)
import Network
import Network.HTTP
import Network.HTTP.Headers
import Text.Regex.Posix
import System.Posix.Signals
import System.Timeout

import Common
import Log
import qualified RequestHandler

-- | Handle for HTTP connections.
instance Stream Handle where
    readLine h     = hGetLine h >>= \l -> return $ Right $ l ++ "\n"
    readBlock h n  = replicateM n (hGetChar h) >>= return . Right
    writeBlock h s = mapM_ (hPutChar h) s >>= return . Right
    close          = hClose

-- | Put Header class into the Eq class (this is not done in the HTTP module).
instance Eq Header where
    (Header hn1 hv1) == (Header hn2 hv2) = hn1 == hn2 && hv1 == hv2

{-|
    Starts the HTTP server. Parameters are the port number on which the server
    will listen, path to the web server root directory where requested files
    will be searched, indexFile, logger and timeout.
-}
run :: PortID -> String -> String -> Logger -> Int -> IO ()
run portNum serverRoot indexFile logger connTimeout = do
    -- Ignore SIGPIPE signal (to prevent server from stopping when
    -- some client connection is closed)
    installHandler sigPIPE Ignore $ Just fullSignalSet
    -- Accept connections
    withSocketsDo $ do socket <- listenOn portNum
                       serverLoop socket
                           `finally` (sClose socket)
    where serverLoop socket =
        forever $ acceptConnection socket connTimeout $ handleConnection $
           RequestHandler.getRequestHandler serverRoot indexFile logger

{-|
    Accepts a new connection and creates a thread for handling that connection.
    The connection will be handled by the selected HTTP connection handler.
-}
acceptConnection :: Socket -> Int -> (Handle -> String -> Int -> IO ()) -> IO ThreadId
acceptConnection socket to ch = do (conn, hostName, _) <- accept socket
                                   -- Disable buffering (this causes problems when
                                   -- handling persistent connections)
                                   hSetBuffering conn NoBuffering
                                   -- Create a thread for that connection handling
                                   forkIO $ ch conn hostName to

{-|
    Handles the selected connection (conn, hn is the client host name) with
    the selected request handler (rh). If to (timeout) is > 0, then timeout
    specified in to is used (in seconds).
-}
handleConnection :: RequestHandler.RequestHandler -> Handle -> String -> Int -> IO ()
handleConnection rh conn hn to = impl `catch` (\_ -> return ()) -- Ignore errors
                                   `finally` (close conn)
    where impl = do -- Receive request
                    requestOrErr <- receiveRequest to conn
                    if (isTimeoutOrErr requestOrErr)
                        then do close conn
                        else do
                    let request = getEitherRight $ fromJust requestOrErr

                    -- Handle request
                    response <- rh request hn

                    -- Send response
                    respondHTTP conn response

                    -- If the client set the connection to be kept alive,
                    -- try to receive another request
                    if keepAlive request
                        then handleConnection rh conn hn to
                        else close conn
          isTimeoutOrErr r = isNothing r || (isEitherLeft $ fromJust r)
          isHttp11 r = (show r) =~ "^[^\n]*HTTP/1.1\r\n.*$"
          hasKeepAlive (Request _ _ hs _) = elem (Header HdrConnection "keep-alive") hs
          hasConnClose (Request _ _ hs _) = elem (Header HdrConnection "close") hs
          keepAlive r = hasKeepAlive r || (isHttp11 r && (not $ hasConnClose r))
          receiveRequest t c = do if t > 0
                                      -- Timeout is enabled (it must be in microsecs,
                                      -- so multiply it by 10^6)
                                      then timeout (t * 1000000) $ receiveHTTP c
                                      -- Timeout is disabled
                                      else do r <- receiveHTTP c
                                              return $ Just r

-- End of file
