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
    ipAddr = "127.0.0.1"

    handler appData = do
      C.runConduit
        $ CN.appSource appData
        .| C.stdoutC

--------------------------------------------------------------------------------
-- Parser


--------------------------------------------------------------------------------
-- respond
