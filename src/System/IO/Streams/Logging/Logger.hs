{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.Streams.Logging.Logger where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder                 (Builder)
import           Blaze.ByteString.Builder.Char8           (fromChar)
import           Control.Concurrent.MVar                  (modifyMVar)
import           Control.Exception                        (SomeException, catch,
                                                           handle, throwIO)
import           Data.Monoid                              (mappend)
import qualified Data.Text                                as T
import qualified Data.Text.Encoding                       as T
#if !MIN_VERSION_base(4,6,0)
import           Prelude                                  hiding (catch)
#endif
import qualified System.IO.Streams                        as Streams
import           System.IO.Streams.Logging.Internal.Types (Logger (..),
                                                           LoggerState (..))
------------------------------------------------------------------------------

------------------------------------------------------------------------------
logInternalException :: LoggerState -> SomeException -> IO ()
logInternalException ls@(LoggerState _ reset err) e =
    (err $! T.encodeUtf8 $ T.pack $ show e ++ "\n")
        `catch` \(_::SomeException) -> return ls


------------------------------------------------------------------------------
-- | Write a log message to a 'Logger'. Adds a newline to the log message.
log :: Builder -> Logger -> IO ()
log b (Logger mvar) = modifyMVar mvar w >>= maybe (return $! ()) throwIO
  where
    w ls@(LoggerState stream _ _) = handle (h ls) $ do
        Streams.write (Just $! b `mappend` fromChar '\n') stream
        return $! (ls, Nothing)

    h ls@(LoggerState _ reset err) e = do
        ls' <- (do
                   logInternalException ls e
                   reset ls) `catch` (\(_ :: SomeException) -> return ls)
        return $! (ls', Just e)


------------------------------------------------------------------------------
-- | Resets a 'Logger'.
reset :: Logger -> IO ()
reset (Logger mvar) = modifyMVar_ mvar go
  where
    go ls = (_resetOutput ls ls) `catch` \e -> do
                logInternalException ls e
                throwIO e
