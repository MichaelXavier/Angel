module Angel.Log ( cleanCalendar
                 , logger
                 , programLogger ) where

import Data.Time.LocalTime (ZonedTime,
                            getZonedTime)
import Data.Time.Format (formatTime)

import Text.Printf (printf)
import System.Locale (defaultTimeLocale)

-- |provide a clean, ISO-ish format for timestamps in logs
cleanCalendar :: ZonedTime -> String
cleanCalendar = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S"

-- |log a line to stdout; indented for use with partial application for 
-- |"local log"-type macroing
logger :: String -> String -> IO ()
logger lname msg = do 
    zt <- getZonedTime
    printf "[%s] {%s} %s\n" (cleanCalendar zt) lname msg

programLogger :: String -> (String -> IO ())
programLogger id' = logger $ "- program: " ++ id' ++ " -"
