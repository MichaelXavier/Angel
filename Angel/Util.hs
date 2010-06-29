-- |various utility functions
module Angel.Util where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Concurrent (threadDelay, forkIO, forkOS)

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
