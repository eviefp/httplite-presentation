module Server where

import           Conduit
                 ( (.|) )
import qualified Conduit as C
import           Control.Applicative
                 ( (<|>) )
import           Control.Concurrent
                 ( threadDelay )
import qualified Control.Concurrent.Async as Async
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as PC
import           Data.ByteString
                 ( ByteString )
import qualified Data.ByteString as BS
import           Data.ByteString.Char8
                 ( unpack )
import qualified Data.Conduit.Attoparsec as CP
import qualified Data.Conduit.Network as CN
import           Data.List
                 ( isPrefixOf )
import           Data.Maybe
                 ( catMaybes, listToMaybe )
import           Helpers
                 ( string_ )
import           Prelude
import           System.Directory
                 ( canonicalizePath, doesPathExist )
import           System.FilePath
                 ( (</>) )

--------------------------------------------------------------------------------
-- runServer

runServer :: IO ()
runServer = CN.runTCPServer settings handler
  where
    settings = CN.serverSettings port ipAddr
    port = 5000
    ipAddr = "*"

    handler appData = do
      request <- C.runConduit
        $ CN.appSource appData
        .| C.iterMC (putStrLn . unpack)
        .| CP.sinkParser requestParser
      print request
      response <- respond request
      C.runConduit
         $ C.yield response
         .| CN.appSink appData

--------------------------------------------------------------------------------
-- Parser
data Request
  = GetRequest FilePath (Maybe (Int, Int))
  | NotImplemented
  deriving Show

requestParser :: P.Parser Request
requestParser = getRequestParser <|> pure NotImplemented

getRequestParser :: P.Parser Request
getRequestParser = do
    string_ "GET /"
    path <- PC.takeWhile1 (not . PC.isSpace)
    -- [Maybe (Int, Int)]
    headers <- catMaybes <$> P.many' headerParser
    let range' = listToMaybe headers
    pure $ GetRequest (unpack path) range'

headerParser :: P.Parser (Maybe (Int, Int))
headerParser = rangeParser <|> emptyHeader

rangeParser :: P.Parser (Maybe (Int, Int))
rangeParser = do
    string_ "Range: bytes="
    start <- PC.decimal
    string_ "-"
    end <- PC.decimal
    string_ "\r\n"
    pure $ Just (start, end)

emptyHeader :: P.Parser (Maybe (Int, Int))
emptyHeader = do
    _ <- PC.takeWhile1 (/= '\r')
    string_ "\r\n"
    pure Nothing

--------------------------------------------------------------------------------
-- respond
basePath :: FilePath
basePath = "/home/vlad/http-home/"

-- Range: bytes=1-10

respond :: Request -> IO ByteString
respond NotImplemented =
  pure "HTTP/1.1 501 Not Implemented\r\n"
respond (GetRequest path range) = do
  let filepath = basePath </> path
  fileExists <- doesPathExist filepath
  if fileExists
     then do
        content <- readFile' filepath range
        pure
            $ "HTTP/1.1 200 OK\r\n"
            <> "Connection: close\r\n"
            <> "\r\n"
            <> content
      else
        pure "HTTP/1.1 404 Not Found\r\n"
  where
    readFile' path Nothing = BS.readFile path
    readFile' path (Just (start, end)) = do
        content <- BS.readFile path
        pure $ BS.take (end - start) $ BS.drop start $ content
