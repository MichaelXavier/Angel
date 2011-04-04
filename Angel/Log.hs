module Angel.Log where

import Text.Printf (printf)
import System.Time (getClockTime, toCalendarTime, CalendarTime(..), formatCalendarTime)
import System.Locale (defaultTimeLocale)
import System.Posix.Syslog

-- |provide a clean, ISO-ish format for timestamps in logs
--cleanCalendar :: CalendarTime -> String
--cleanCalendar ct = formatCalendarTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" ct

-- |log a line to stdout; indented for use with partial application for 
-- |"local log"-type macroing
--logger :: String -> String -> IO ()
--logger lname msg = do 
--    tm <- getClockTime
--    ct <- toCalendarTime tm
--    printf "[%s] {%s} %s\n" (cleanCalendar ct) lname msg

logger :: String -> String -> IO ()
logger lname msg = do
  syslog Notice $ printf "{%s} %s" lname msg
