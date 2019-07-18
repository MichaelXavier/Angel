{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Monoid ( (<>) )
import Control.Monad.Reader
import Options.Applicative (ParserInfo, ReadM)
import qualified Options.Applicative as O
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

import qualified System.Posix.User as U (setUserID,
                          getUserEntryForName,
                          UserEntry(userID) )

import qualified Data.Map as M

import Angel.Log (logger)
import Angel.Config (monitorConfig, loadInitialUserFromConfig)
import Angel.Data (GroupConfig(GroupConfig),
                   Options(..),
                   spec,
                   Verbosity(..),
                   AngelM,
                   runAngelM)
import Angel.Job (pollStale,
                  syncSupervisors)
import Angel.Prelude

-- |Signal handler: when a HUP is trapped, write to the wakeSig Tvar
-- |to make the configuration monitor loop cycle/reload
handleHup :: TVar (Maybe Int) -> IO ()
handleHup wakeSig = atomically $ writeTVar wakeSig $ Just 1

handleExit :: MVar Bool -> IO ()
handleExit mv = putMVar mv True

main :: IO ()
main = runWithOpts =<< O.execParser opts

opts :: ParserInfo Options
opts = O.info (O.helper <*> opts')
       (O.fullDesc <> O.header "angel - Process management and supervision daemon")
  where
    opts' = Options
            <$> O.strArgument (O.metavar "CONFIG_FILE")
            <*> O.option readUserOpt (O.short 'u' <>
                                 O.value Nothing <>
                                 O.metavar "USER" <>
                                 O.help "Execute as user")
            <*> O.option readVOpt (O.short 'v' <>
                                 O.value V2 <>
                                 O.showDefaultWith vOptAsNumber <>
                                 O.metavar "VERBOSITY" <>
                                 O.help "Verbosity from 0-2")


vOptAsNumber :: Verbosity -> String
vOptAsNumber V2 = "2"
vOptAsNumber V1 = "1"
vOptAsNumber V0 = "0"

readUserOpt :: ReadM (Maybe String)
readUserOpt = O.eitherReader $ (return . Just)

readVOpt :: ReadM Verbosity
readVOpt = O.eitherReader $ \s ->
    case s of
      "0" -> return V0
      "1" -> return V1
      "2" -> return V2
      _   -> Left "Expecting 0-2"

runWithOpts :: Options -> IO ()
runWithOpts os = runAngelM os runWithConfigPath

switchUser :: String -> IO ()
switchUser name = do
    userEntry <- U.getUserEntryForName name
    U.setUserID $ U.userID userEntry

switchRunningUser :: AngelM ()
switchRunningUser = do
    username <- asks userargument

    case username of
        Just user -> do
            logger "main" V2 $ "Running as user: " ++ user
            liftIO $ switchUser user
        Nothing -> do
            configPath <- asks configFile
            userFromConfig <- liftIO $ loadInitialUserFromConfig configPath
            case userFromConfig of
                Just configUser -> do
                    logger "main" V2 $ "Running as user: " ++ configUser
                    liftIO $ switchUser configUser
                Nothing -> return ()

runWithConfigPath :: AngelM ()
runWithConfigPath = do
    configPath <- asks configFile
    liftIO $ hSetBuffering stdout LineBuffering
    liftIO $ hSetBuffering stderr LineBuffering
    let logger' = logger "main"
    logger' V2 "Angel started"

    -- Switch to the specified user if one has been chosen
    switchRunningUser

    logger' V2 $ "Using config file: " ++ configPath

    -- Create the TVar that represents the "global state" of running applications
    -- and applications that _should_ be running
    fileReqChan <- liftIO $ atomically newTChan
    sharedGroupConfig <- liftIO $ newTVarIO $ GroupConfig M.empty M.empty fileReqChan

    -- The wake signal, set by the HUP handler to wake the monitor loop
    wakeSig <- liftIO $ newTVarIO Nothing
    liftIO $ installHandler sigHUP (Catch $ handleHup wakeSig) Nothing

    -- Handle dying
    bye <- liftIO newEmptyMVar
    liftIO $ installHandler sigTERM (Catch $ handleExit bye) Nothing
    liftIO $ installHandler sigINT (Catch $ handleExit bye) Nothing

    -- Fork off an ongoing state monitor to watch for inconsistent state
    forkIO' $ pollStale sharedGroupConfig

    -- Finally, run the config load/monitor thread
    forkIO' $ forever $ monitorConfig configPath sharedGroupConfig wakeSig

    liftIO $ takeMVar bye

    logger' V2 "INT | TERM received; initiating shutdown..."
    logger' V2 "  1. Clearing config"
    liftIO $ atomically $ do
        cfg <- readTVar sharedGroupConfig
        writeTVar sharedGroupConfig cfg {spec = M.empty}
    logger' V2 "  2. Forcing sync to kill running processes"
    syncSupervisors sharedGroupConfig
    logger' V2 "That's all folks!"

errorExit :: String -> IO ()
errorExit msg = hPutStrLn stderr msg >> exitFailure


forkIO' :: AngelM () -> AngelM ()
forkIO' f = do
    r <- ask
    void $ liftIO $ forkIO $ runAngelM r f
