module Main (main) where 

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar,
                                MVar,
                                takeMVar,
                                putMVar)
import Control.Concurrent.STM (TVar,
                               atomically,
                               writeTVar,
                               newTChan,
                               readTVar,
                               newTVarIO)
import Control.Monad (forever)
import System.Environment (getArgs)
import System.Exit (exitFailure,
                    exitSuccess)
import System.Posix.Signals (installHandler,
                             sigHUP,
                             sigTERM,
                             sigINT,
                             Handler(Catch))
import System.IO (hSetBuffering,
                  hPutStrLn,
                  BufferMode(LineBuffering),
                  stdout,
                  stderr)

import qualified Data.Map as M

import Angel.Log (logger)
import Angel.Config (monitorConfig)
import Angel.Data (GroupConfig(GroupConfig),
                   spec)
import Angel.Job (pollStale,
                  syncSupervisors)
import Angel.Files (startFileManager)

-- |Signal handler: when a HUP is trapped, write to the wakeSig Tvar
-- |to make the configuration monitor loop cycle/reload
handleHup :: TVar (Maybe Int) -> IO ()
handleHup wakeSig = atomically $ writeTVar wakeSig $ Just 1

handleExit :: MVar Bool -> IO ()
handleExit mv = putMVar mv True

main :: IO ()
main = handleArgs =<< getArgs

handleArgs :: [String] -> IO ()
handleArgs ["--help"]   = printHelp
handleArgs ["-h"]       = printHelp
handleArgs []           = printHelp
handleArgs [configPath] = runWithConfigPath configPath
handleArgs _            = errorExit "expected a single config file. Run with --help for usasge."

printHelp :: IO ()
printHelp = putStrLn "Usage: angel [--help] CONFIG_FILE" >> exitSuccess

runWithConfigPath :: FilePath -> IO ()
runWithConfigPath configPath = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    let logger' = logger "main" 
    logger' "Angel started"

    logger' $ "Using config file: " ++ configPath

    -- Create the TVar that represents the "global state" of running applications
    -- and applications that _should_ be running
    fileReqChan <- atomically newTChan
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
    logger' "INT | TERM received; initiating shutdown..."
    logger' "  1. Clearing config"
    atomically $ do
        cfg <- readTVar sharedGroupConfig
        writeTVar sharedGroupConfig cfg {spec = M.empty}
    logger' "  2. Forcing sync to kill running going"
    syncSupervisors sharedGroupConfig
    logger' "That's all folks!"

errorExit :: String -> IO ()
errorExit msg = hPutStrLn stderr msg >> exitFailure
