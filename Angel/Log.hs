module Angel.Log where

import Text.Printf (printf)
import Text.PrettyPrint.HughesPJ
import System.Time (getClockTime, toCalendarTime, CalendarTime(..), formatCalendarTime)
import System.Locale (defaultTimeLocale)

cleanCalendar ct = formatCalendarTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" ct

--makePpDoc ct = date 
--    where
--        date = (makeText $ ctYear ct) <> slash <> (makeText $ ctMonth ct) <> slash <> (makeText $ ctDay ct)
--        makeText = text . show
--        slash = char '/'

logger lname msg = do tm <- getClockTime
                      ct <- toCalendarTime tm
                      printf "[%s] {%s} %s\n" (cleanCalendar ct) lname msg
