module Server2 where

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
runServer = CN.runTCPServer settings safeHandler
  where
    settings = CN.serverSettings port ipAddr
    port = 5000
    ipAddr = "*"

    safeHandler appData =
      Async.runConcurrently
        $ Async.Concurrently (threadDelay (1000000 * 60)) <|> Async.Concurrently (handler appData)

    handler appData = do
      request <-
        C.runConduit
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
data GetRequest = GetRequest
  { path :: FilePath
  , range :: Maybe (Int, Int)
  } deriving (Show)

data Request = Get GetRequest | Unknown
  deriving (Show)

requestParser :: P.Parser Request
requestParser = (Get <$> getRequestParser) <|> pure Unknown

getRequestParser :: P.Parser GetRequest
getRequestParser = do
  path <- parseVerb
  headers <- catMaybes <$> P.many' parseHeader
  let range' = listToMaybe headers
  pure $ GetRequest path range'

parseVerb :: P.Parser String
parseVerb = do
  string_ "GET /"
  path <- PC.takeWhile1 (not . PC.isSpace)
  string_ " HTTP/1.1\r\n"
  pure $ unpack path

parseHeader :: P.Parser (Maybe (Int, Int))
parseHeader =
  (Just <$> parseRangeHeader)
  <|>
  (Nothing <$ skipHeader)

skipHeader :: P.Parser ()
skipHeader =
  PC.letter_ascii
    *> PC.takeWhile1 (not . (== '\r'))
    *> string_ "\r\n"
    *> pure ()

parseRangeHeader :: P.Parser (Int, Int)
parseRangeHeader = do
  string_ "Range: bytes="
  start <- PC.decimal
  string_ "-"
  end <- PC.decimal
  string_ "\r\n"
  pure $ (start, end)

--------------------------------------------------------------------------------
-- respond
basePath :: FilePath
basePath = "/home/vlad/http-home"

respond :: Request -> IO ByteString
respond (Get (GetRequest {path, range})) = do
  let filePath = basePath </> path
  fileExists <- checkFile filePath
  if fileExists
    then do
      contents <- readFile' filePath range
      pure
        $ "HTTP/1.1 200 OK\r\n"
        <> "Connection: close\r\n"
        <> "\r\n"
        <> contents
    else
      pure "HTTP/1.1 404 Not Found\r\n"
  where
    readFile' f Nothing = BS.readFile f
    readFile' f (Just (s, e)) = do
      contents <- BS.readFile f
      pure $ BS.take (e - s) $ BS.drop s $ contents
    checkFile f = do
      canonicPath <- canonicalizePath f
      if basePath `isPrefixOf` canonicPath
        then doesPathExist canonicPath
        else pure False
respond Unknown =
  pure "HTTP/1.1 501 Not Implemented\r\n"
