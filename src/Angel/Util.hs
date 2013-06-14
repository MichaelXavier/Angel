-- |various utility functions
module Angel.Util where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Concurrent (threadDelay, forkIO, forkOS)
import System.Posix.User (getEffectiveUserName,
                          UserEntry(homeDirectory),
                          getUserEntryForName)

-- |sleep for `s` seconds in an thread
sleepSecs :: Int -> IO ()
sleepSecs s = threadDelay $ s * 1000000

-- |wait for the STM TVar to be non-nothing
waitForWake :: TVar (Maybe Int) -> IO ()
waitForWake wakeSig = atomically $ do 
    state <- readTVar wakeSig
    case state of
        Just x -> writeTVar wakeSig Nothing
        Nothing -> retry

expandPath :: FilePath -> IO FilePath
expandPath ('~':rest) = do home <- getHome =<< getUser
                           return $ home ++ relativePath
  where (userName, relativePath) = span (/= '/') rest
        getUser                  = if null userName
                                     then getEffectiveUserName
                                     else return userName
        getHome user             = homeDirectory `fmap` getUserEntryForName user
expandPath path = return path
