
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A module designed to handle potentially extremely verbose logging.

module System.Log.MonadLogger.Truncated
       (
         runTruncatedLoggingT
       , mkLogFmt
       , LogFmt
       )
       where

import           Control.Monad.IO.Class (MonadIO ())
import           Control.Monad.Logger
import           Data.ByteString.Char8  (ByteString)
import qualified Data.ByteString.Char8  as BS8
import           System.IO
import           System.Log.FastLogger  (fromLogStr)


data LogFmt = LogFmt (Maybe Int) -- LogFunc
-- type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

mkLogFmt :: Maybe Int -> LogFmt
mkLogFmt = LogFmt


-- | Run a logging monad with default output.
--
-- Truncate the logging according to the 'LogFmt'
runTruncatedLoggingT :: MonadIO m => LogFmt -> LoggingT m a -> m a
runTruncatedLoggingT fmt = (`runLoggingT` defaultOutput fmt stdout)

defaultOutput
  :: LogFmt -> Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
defaultOutput (LogFmt tr) h loc src level msg =
    BS8.hPutStr h $ logStrBS (maybe msg truncated tr)
  where
    logStrBS = fromLogStr . defaultLogStr loc src level
    truncated t = toLogStr (shorten t (fromLogStr msg))


-- | Shorten a 'ByteString' and elide it, if necessary.
shorten :: Int -> ByteString -> ByteString
shorten n xs
  | BS8.length xs < n = xs
  | otherwise = BS8.concat [ BS8.take (n - BS8.length end) xs, end ]
 where
    end = BS8.pack "...\n"
