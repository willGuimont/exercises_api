{-# LANGUAGE TemplateHaskell #-}

module Logging
  ( Logging (..),
    runLoggingOnLogger,
    logTrace,
    logDebug,
    logInfo,
    logWarning,
    logError,
    logCritical,
  )
where

import Data.Text
import Polysemy
import System.Log.FastLogger

data Severity
  = Trace
  | Debug
  | Info
  | Warning
  | Error
  | Critical

instance ToLogStr Severity where
  toLogStr Trace = "[TRACE]"
  toLogStr Debug = "[DEBUG]"
  toLogStr Info = "[INFO]"
  toLogStr Warning = "[WARN]"
  toLogStr Error = "[ERROR]"
  toLogStr Critical = "[CRITICAL]"

data LogMessage = LogMessage
  { logSeverity :: !Severity,
    logMessage :: !Text
  }

mkLogMessage :: Severity -> Text -> LogMessage
mkLogMessage logSeverity logMessage = LogMessage {logSeverity, logMessage}

instance ToLogStr LogMessage where
  toLogStr LogMessage {logSeverity = sev, logMessage = msg} =
    toLogStr sev <> toLogStr (" " :: Text) <> toLogStr msg <> toLogStr ("\n" :: Text)

data Logging m a where
  LogMsg :: LogMessage -> Logging m ()

makeSem ''Logging

runLoggingOnLogger :: Member (Embed IO) r => FastLogger -> Sem (Logging ': r) a -> Sem r a
runLoggingOnLogger logger = interpret $ \case
  LogMsg msg -> embed . logger . toLogStr $ msg

logTrace :: Member Logging r => Text -> Sem r ()
logTrace msg = logMsg $ mkLogMessage Trace msg

logDebug :: Member Logging r => Text -> Sem r ()
logDebug msg = logMsg $ mkLogMessage Debug msg

logInfo :: Member Logging r => Text -> Sem r ()
logInfo msg = logMsg $ mkLogMessage Info msg

logWarning :: Member Logging r => Text -> Sem r ()
logWarning msg = logMsg $ mkLogMessage Warning msg

logError :: Member Logging r => Text -> Sem r ()
logError msg = logMsg $ mkLogMessage Error msg

logCritical :: Member Logging r => Text -> Sem r ()
logCritical msg = logMsg $ mkLogMessage Critical msg
