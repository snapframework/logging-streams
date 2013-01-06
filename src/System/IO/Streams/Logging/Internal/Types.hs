module System.IO.Streams.Logging.Internal.Types
  ( Logger(..)
  , LoggerState(..)
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder (Builder)
import           Control.Concurrent.MVar  (MVar)
import           Data.ByteString.Char8    (ByteString)
import           System.IO.Streams        (OutputStream)
------------------------------------------------------------------------------

newtype Logger = Logger { unLogger :: MVar LoggerState }


------------------------------------------------------------------------------
data LoggerState = LoggerState {
      _output      :: {-# UNPACK #-} !(OutputStream Builder)

      -- | Some logger types must be reset for log rotation, e.g. closed and
      -- re-opened again.
    , _resetOutput :: LoggerState -> IO LoggerState

      -- | Error handler to be called to print a secondary message (e.g. to
      -- stderr) if the output stream or reset functions throw an exception.
    , _onError     :: ByteString -> IO ()
    }
