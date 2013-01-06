module System.IO.Streams.Logging.File where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder.Internal        (defaultBufferSize)
import           Blaze.ByteString.Builder.Internal.Buffer (allocBuffer)
import           Control.Applicative                      ((<$>))
import           Control.Concurrent.MVar                  (newMVar)
import qualified Data.ByteString.Char8                    as S
import           System.IO                                (BufferMode (..),
                                                           IOMode (..), hClose,
                                                           hSetBuffering,
                                                           openBinaryFile,
                                                           stderr)
import qualified System.IO.Streams                        as Streams
import           System.IO.Streams.Logging.Internal.Types (Logger (..),
                                                           LoggerState (..))
------------------------------------------------------------------------------

fileLogger :: FilePath -> IO Logger
fileLogger fp = Logger <$> (mkLoggerState >>= newMVar)
  where
    mkBuffer = allocBuffer defaultBufferSize

    mkLoggerState = do
        (handle, stream) <- open
        return $! LoggerState stream (reset handle) $ S.hPutStr stderr

    reset handle (LoggerState out _ err) = do
        Streams.write Nothing out
        hClose handle
        (handle', stream) <- open
        return $! LoggerState stream (reset handle') err

    open = do
        handle <- openBinaryFile fp AppendMode
        hSetBuffering handle NoBuffering
        stream <- Streams.handleToOutputStream handle >>=
                  Streams.unsafeBuilderStream mkBuffer
        return (handle, stream)
