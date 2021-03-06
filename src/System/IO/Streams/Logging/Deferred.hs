module System.IO.Streams.Logging.Deferred
  ( DeferredLogger
  , makeDeferredLogger
  , stopDeferredLogger
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder                 (Builder)
import qualified Blaze.ByteString.Builder                 as B
import           Control.Concurrent                       (ThreadId)
import           Control.Concurrent.MVar                  (MVar, newEmptyMVar,
                                                           newMVar, readMVar)
import           System.IO.Streams                        (OutputStream)
import qualified System.IO.Streams                        as Streams
------------------------------------------------------------------------------
import           System.IO.Streams.Logging.Internal.Types (Logger (..),
                                                           LoggerState (..))

data DeferredLogger = DeferredLogger
    { _dataWaiting   :: !(MVar ())
    , _loggingThread :: !(MVar ThreadId)
    , _wrappedLogger :: !Logger
    }

makeDeferredLogger :: Logger
                   -> IO Logger
makeDeferredLogger wrappedLogger = do
    dataWaitingMVar   <- newEmptyMVar
    loggingThreadMVar <- newEmptyMVar

    (LoggerState output reset onerror) <- readMVar $ unLogger wrappedLogger

    newLogger <- newMVar $! LoggerState output (wrapReset reset) onerror
    undefined

  where
    wrapReset = undefined


stopDeferredLogger :: Logger -> IO ()
stopDeferredLogger _ = undefined
