{-# LANGUAGE ScopedTypeVariables #-}
module Angel.Process ( getProcessHandleStatus
                     , isProcessHandleDead
                     , softKillProcessHandle
                     , hardKillProcessHandle
                     , signalProcessHandle ) where

import Control.Applicative ((<$>))
import Control.Monad ( join
                     , void )
import Data.Maybe (isJust)
import System.Process (ProcessHandle(..))
import System.Process.Internals ( ProcessHandle(..)
                                , ProcessHandle__(..)
                                , withProcessHandle_
                                , withProcessHandle )
import System.Posix.Types (ProcessID)
import System.Posix.Process ( ProcessStatus
                            , getProcessStatus )
import System.Posix.Signals ( Signal
                            , sigTERM
                            , sigINT --debug
                            , sigKILL
                            , signalProcess )

withPid :: (ProcessID -> IO a) -> ProcessHandle -> IO (Maybe a)
withPid action ph = withProcessHandle ph callback
  where callback ph_@(ClosedHandle _) = return (ph_, Nothing)
        callback ph_@(OpenHandle pid) = do res <- action pid
                                           return (ph_, Just res)

getProcessHandleStatus :: ProcessHandle -> IO (Maybe ProcessStatus)
getProcessHandleStatus = fmap join . withPid (getProcessStatus shouldBlock includeStopped) 
  where shouldBlock    = False
        includeStopped = True

signalProcessHandle :: Signal -> ProcessHandle -> IO ()
signalProcessHandle sig = void . withPid (signalProcess sig) 

softKillProcessHandle :: ProcessHandle -> IO ()
softKillProcessHandle = signalProcessHandle sigTERM

hardKillProcessHandle :: ProcessHandle -> IO ()
hardKillProcessHandle = signalProcessHandle sigKILL

isProcessHandleDead :: ProcessHandle -> IO Bool
isProcessHandleDead = fmap isJust . getProcessHandleStatus
