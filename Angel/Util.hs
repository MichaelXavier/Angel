module Angel.Util where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Concurrent (threadDelay, forkIO, forkOS)

sleepSecs s = threadDelay $ s * 1000000

waitForWake wakeSig = atomically $ do state <- readTVar wakeSig
                                      case state of
                                          Just x -> writeTVar wakeSig Nothing
                                          Nothing -> retry
