module Main where 

import Control.Concurrent (threadDelay, forkIO, forkOS)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Monad (unless, when, forever)
import System.Environment (getArgs)
import System.Posix.Signals
import qualified Data.Map as M

import Angel.Log (logger)
import Angel.Config (monitorConfig)
import Angel.Data
import Angel.Job (pollStale)

handleHup wakeSig = atomically $ writeTVar wakeSig $ Just 1

main = do let log = logger "main" 
          log "Angel started"
          args <- getArgs
          unless (length args == 1) $ error "failsafe takes exactly one argument: config file"
          let configPath = head args
          log $ "Using config file: " ++ configPath
          sharedGroupConfig <- newTVarIO $ GroupConfig M.empty M.empty
          wakeSig <- newTVarIO Nothing
          installHandler sigHUP (Catch $ handleHup wakeSig) Nothing
          forkIO $ pollStale sharedGroupConfig
          runInUnboundThread $ forever $ monitorConfig configPath sharedGroupConfig wakeSig
