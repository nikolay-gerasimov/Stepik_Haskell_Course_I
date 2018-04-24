import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info 
logLevelToString a = case a of
    Error -> "Error"
    Warning -> "Warning"
    Info -> "Info"

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logEntryToString :: LogEntry -> String
logEntryToString a = timeToString(timestamp a) ++ ": " ++ logLevelToString(logLevel a) ++ ": " ++ message a