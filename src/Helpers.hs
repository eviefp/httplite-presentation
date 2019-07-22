module Helpers where

import qualified Data.Attoparsec.ByteString as P
import           Data.ByteString
                 ( ByteString )
import           Data.Functor
                 ( void )
import           Prelude

string_ :: ByteString -> P.Parser ()
string_ = void . P.string
