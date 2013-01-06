module System.IO.Streams.Logging.Types
  ( Logger
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder                 (Builder)
import qualified Blaze.ByteString.Builder                 as B
import           Control.Concurrent.MVar                  (MVar)
import           System.IO.Streams                        (OutputStream)
import qualified System.IO.Streams                        as Streams
import           System.IO.Streams.Logging.Internal.Types
------------------------------------------------------------------------------
