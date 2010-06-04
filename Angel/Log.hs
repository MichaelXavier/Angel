module Angel.Log where

import Text.Printf (printf)
import System.Time (getClockTime, toCalendarTime, CalendarTime(..), formatCalendarTime)
import System.Locale (defaultTimeLocale)

cleanCalendar ct = formatCalendarTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" ct

logger lname msg = do tm <- getClockTime
                      ct <- toCalendarTime tm
                      printf "[%s] {%s} %s\n" (cleanCalendar ct) lname msg
