
# Truncated Monad Logger

A logging backend for `monad-logger` designed to handle potentially
verbose logging output.


## Usage (example)

*This library is still experimental*

This usage description is the planned implementation. Currently, a
moderately inneficient implementation is used due to converting the
log string back and forth before concatenating the source info

    > runTruncatedLoggingT (mkLogFmt (Just 10)) (logDebugN "Hello, world")
    [Debug] Hello,... @(monad-logger-truncated-0.1.0.0:System.Log.MonadLogger.Truncated /path/to/monad-logger-truncated/System/Log/MonadLogger/Truncated.hs:26:52)

The ideal use-case is to conditionally truncate logging with any
backend formatter.

    import Control.Monad.Logger           (logDebugN)
    import Control.Monad.Logger.Truncated (runTruncatedLoggingT)

    main :: IO ()
    main = let fmt = mkLogFmt (Just 100) Nothing
	       in  runTruncatedLoggingT (logDebugN "HELLO!")


`monad-logger-truncated` can use existing `monad-logger` backends:

    import Control.Monad.Logger.Syslog (defaultSyslogOutput)

    main = let fmt = mkLogFmt (Just 100) (Just defaultSyslogOutput)
	       in  runTruncatedLoggingT fmt (logDebugN "HELLO!")
