module Main where 

import Control.Concurrent (threadDelay, forkIO, forkOS)
import Control.Concurrent
import Control.Concurrent.MVar (newEmptyMVar, MVar, takeMVar, putMVar)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Concurrent.STM.TChan (newTChan)
import Control.Monad (unless, when, forever)
import System.Environment (getArgs)
import System.Posix.Signals
import System.IO (hSetBuffering, BufferMode(..), stdout, stderr)

import qualified Data.Map as M

import Angel.Log (logger)
import Angel.Config (monitorConfig)
import Angel.Data (GroupConfig(..))
import Angel.Job (pollStale, syncSupervisors)
import Angel.Files (startFileManager)

-- |Signal handler: when a HUP is trapped, write to the wakeSig Tvar
-- |to make the configuration monitor loop cycle/reload
handleHup :: TVar (Maybe Int) -> IO ()
handleHup wakeSig = atomically $ writeTVar wakeSig $ Just 1

handleExit :: MVar Bool -> IO ()
handleExit mv = putMVar mv True

main = do 
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    let log = logger "main" 
    log "Angel started"
    args <- getArgs

    -- Exactly one argument required for the `angel` executable
    unless (length args == 1) $ error "exactly one argument required: config file"
    let configPath = head args
    log $ "Using config file: " ++ configPath

    -- Create the TVar that represents the "global state" of running applications
    -- and applications that _should_ be running
    fileReqChan <- atomically $ newTChan
    sharedGroupConfig <- newTVarIO $ GroupConfig M.empty M.empty fileReqChan

    -- The wake signal, set by the HUP handler to wake the monitor loop
    wakeSig <- newTVarIO Nothing
    installHandler sigHUP (Catch $ handleHup wakeSig) Nothing

    -- Handle dying
    bye <- newEmptyMVar
    installHandler sigTERM (Catch $ handleExit bye) Nothing
    installHandler sigINT (Catch $ handleExit bye) Nothing

    -- Fork off an ongoing state monitor to watch for inconsistent state
    forkIO $ pollStale sharedGroupConfig
    forkIO $ startFileManager fileReqChan

    -- Finally, run the config load/monitor thread
    forkIO $ forever $ monitorConfig configPath sharedGroupConfig wakeSig

    _ <- takeMVar bye
    log "INT | TERM received; initiating shutdown..."
    log "  1. Clearing config"
    atomically $ do
        cfg <- readTVar sharedGroupConfig
        writeTVar sharedGroupConfig cfg {spec = M.empty}
    log "  2. Forcing sync to kill running going"
    syncSupervisors sharedGroupConfig
    log "That's all folks!"
