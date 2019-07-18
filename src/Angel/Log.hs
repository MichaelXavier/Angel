{-# LANGUAGE NoImplicitPrelude #-}
module Angel.Log ( cleanCalendar
                 , logger
                 , programLogger ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Time.LocalTime (ZonedTime,
                            getZonedTime)
import Data.Time.Format ( formatTime
                        , defaultTimeLocale)

import Text.Printf (printf)
import Angel.Data
import Angel.Prelude

-- |provide a clean, ISO-ish format for timestamps in logs
cleanCalendar :: ZonedTime -> String
cleanCalendar = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S"

-- |log a line to stdout; indented for use with partial application for
-- |"local log"-type macroing
logger :: String -> Verbosity -> String -> AngelM ()
logger lname v msg = do
    chk <- shouldLog v
    when chk $ liftIO  $ do
      zt <- getZonedTime
      printf "[%s] {%s} %s\n" (cleanCalendar zt) lname msg

programLogger :: String -> Verbosity -> String -> AngelM ()
programLogger id' = logger $ "- program: " ++ id' ++ " -"


shouldLog :: Verbosity -> AngelM Bool
shouldLog v = do
  maxV <- asks verbosity
  return $ v <= maxV
