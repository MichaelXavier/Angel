module Angel.Job ( syncSupervisors
                 , killProcess -- for testing
                 , pollStale ) where

import Control.Exception ( finally )
import Control.Monad ( unless
                     , when
                     , forever)
import Data.Maybe ( mapMaybe
                  , fromMaybe
                  , fromJust )
import System.Process ( createProcess
                      , proc
                      , waitForProcess
                      , ProcessHandle
                      , CreateProcess
                      , std_out
                      , std_err
                      , std_in
                      , cwd
                      , env
                      , StdStream(UseHandle, CreatePipe) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( TVar
                              , writeTVar
                              , readTVar
                              , atomically )
import qualified Data.Map as M
import Angel.Log ( logger
                 , programLogger )
import Angel.Data ( Program( delay
                           , exec
                           , logExec
                           , name
                           , pidFile
                           , stderr
                           , stdout
                           , termGrace
                           , workingDir )
                  , ProgramId
                  , GroupConfig
                  , spec
                  , running
                  , KillDirective(SoftKill, HardKill)
                  , RunState(..)
                  , defaultProgram
                  , defaultDelay
                  , defaultStdout
                  , defaultStderr )
import Angel.Process ( isProcessHandleDead
                     , softKillProcessHandle
                     , hardKillProcessHandle )
import qualified Angel.Data as D
import Angel.Util ( sleepSecs
                  , strip
                  , split
                  , nnull )
import Angel.Files ( getFile )
import Angel.PidFile ( startMaybeWithPidFile
                     , clearPIDFile )

ifEmpty :: String -> IO () -> IO () -> IO ()
ifEmpty s ioa iob = if s == "" then ioa else iob

-- |launch the program specified by `id`, opening (and closing) the
-- |appropriate fds for logging.  When the process dies, either b/c it was
-- |killed by a monitor, killed by a system user, or ended execution naturally,
-- |re-examine the desired run config to determine whether to re-run it.  if so,
-- |tail call.
supervise :: TVar GroupConfig -> String -> IO ()
supervise sharedGroupConfig id' = do
    logger' "START"
    cfg <- atomically $ readTVar sharedGroupConfig
    let my_spec = find_me cfg
    ifEmpty (name my_spec)

        (logger' "QUIT (missing from config on restart)")

        (do
            (attachOut, attachErr, lHandle) <- makeFiles my_spec cfg

            let (cmd, args) = cmdSplit . fromJust . exec $ my_spec

            let procSpec = (proc cmd args) {
              std_out = attachOut,
              std_err = attachErr,
              cwd = workingDir my_spec,
              env = Just $ D.env my_spec
            }

            let mPfile = pidFile my_spec
            let onPidError lph ph = do logger' "Failed to create pidfile"
                                       killProcess $ toKillDirective my_spec ph lph

            logger' $ "Spawning process with env " ++ show (env procSpec)

            startMaybeWithPidFile procSpec mPfile (\pHandle -> do
              updateRunningPid my_spec (Just pHandle) lHandle
              logProcess logger' pHandle
              updateRunningPid my_spec Nothing Nothing) (onPidError lHandle)

            cfg' <- atomically $ readTVar sharedGroupConfig
            if M.notMember id' (spec cfg')
              then logger'  "QUIT"
              else do
                logger'  "WAITING"
                sleepSecs . fromMaybe defaultDelay . delay $ my_spec
                logger'  "RESTART"
                supervise sharedGroupConfig id'
        )

    where
        logger' = programLogger id'
        cmdSplit fullcmd = (head parts, tail parts)
            where parts = (filter (/="") . map strip . split ' ') fullcmd

        find_me cfg = M.findWithDefault defaultProgram id' (spec cfg)
        updateRunningPid my_spec mpid mlpid = atomically $ do
            wcfg <- readTVar sharedGroupConfig
            let rstate = RunState { rsProgram = my_spec
                                  , rsHandle = mpid
                                  , rsLogHandle = mlpid
                                  }
            writeTVar sharedGroupConfig wcfg{
              running=M.insertWith' const id' rstate (running wcfg)
            }

        makeFiles my_spec cfg =
            case logExec my_spec of
                Just path -> logWithExec path
                Nothing -> logWithFiles

          where
            logWithFiles = do
                let useout = fromMaybe defaultStdout $ stdout my_spec
                attachOut <- UseHandle `fmap` getFile useout cfg

                let useerr = fromMaybe defaultStderr $ stderr my_spec
                attachErr <- UseHandle `fmap` getFile useerr cfg

                return (attachOut, attachErr, Nothing)

            logWithExec path = do
                let (cmd, args) = cmdSplit path

                attachOut <- UseHandle `fmap` getFile "/dev/null" cfg

                logger' "Spawning logger process"
                (inPipe, _, _, logpHandle) <- createProcess (proc cmd args){
                  std_out = attachOut,
                  std_err = attachOut,
                  std_in  = CreatePipe,
                  cwd     = workingDir my_spec
                }

                forkIO $ logProcess logExecLogger logpHandle

                return (UseHandle (fromJust inPipe),
                        UseHandle (fromJust inPipe),
                        Just logpHandle)
              where
                logExecLogger = programLogger $ "logger for " ++ id'

logProcess :: (String -> IO ()) -> ProcessHandle -> IO ()
logProcess logSink pHandle = do
  logSink "RUNNING"
  waitForProcess pHandle
  logSink "ENDED"

--TODO: paralellize
killProcesses :: [KillDirective] -> IO ()
killProcesses = mapM_ killProcess

killProcess :: KillDirective -> IO ()
killProcess (SoftKill n pid lpid) = do
  logger' $ "Soft killing " ++ n
  softKillProcessHandle pid
  where logger' = logger "process-killer"
killProcess (HardKill n pid lpid grace) = do
  logger' $ "Attempting soft kill " ++ n ++ " before hard killing"
  softKillProcessHandle pid
  logger' $ "Waiting " ++ show grace ++ " seconds for " ++ n ++ " to die"
  sleepSecs grace

  -- Note that this means future calls to get exits status will fail
  dead <- isProcessHandleDead pid

  unless dead $ logger' ("Hard killing " ++ n) >> hardKillProcessHandle pid
  where logger' = logger "process-killer"

cleanPidfiles :: [Program] -> IO ()
cleanPidfiles progs = mapM_ clearPIDFile pidfiles
  where pidfiles = mapMaybe pidFile progs

-- |fire up new supervisors for new program ids
startProcesses :: TVar GroupConfig -> [String] -> IO ()
startProcesses sharedGroupConfig = mapM_ spawnWatcher
    where
        spawnWatcher s = forkIO $ wrapProcess sharedGroupConfig s

wrapProcess :: TVar GroupConfig -> String -> IO ()
wrapProcess sharedGroupConfig id' = do
    run <- createRunningEntry
    when run $ finally (supervise sharedGroupConfig id') deleteRunning
  where
    deleteRunning = atomically $ do
        wcfg <- readTVar sharedGroupConfig
        writeTVar sharedGroupConfig wcfg{
            running=M.delete id' (running wcfg)
        }

    createRunningEntry =
        atomically $ do
            cfg <- readTVar sharedGroupConfig
            let specmap = spec cfg
            case M.lookup id' specmap of
                Nothing -> return False
                Just target -> do
                    let runmap = running cfg
                    case M.lookup id' runmap of
                        Just _ -> return False
                        Nothing -> do
                            let rstate = RunState { rsProgram = target
                                                  , rsHandle = Nothing
                                                  , rsLogHandle = Nothing
                                                  }
                            writeTVar sharedGroupConfig cfg{running=
                                M.insert id' rstate runmap}
                            return True

-- |diff the requested config against the actual run state, and
-- |do any start/kill action necessary
syncSupervisors :: TVar GroupConfig -> IO ()
syncSupervisors sharedGroupConfig = do
   let logger' = logger "process-monitor"
   cfg <- atomically $ readTVar sharedGroupConfig
   let (killProgs, killHandles) = mustKill cfg
   let starts = mustStart cfg
   when (nnull killHandles || nnull starts) $ logger' (
         "Must kill=" ++ show (length killHandles)
                ++ ", must start=" ++ show (length starts))
   killProcesses killHandles
   cleanPidfiles killProgs
   startProcesses sharedGroupConfig starts

--TODO: make private
mustStart :: GroupConfig -> [String]
mustStart cfg = map fst $ filter (isNew $ running cfg) $ M.assocs (spec cfg)
  where isNew isRunning (id', _) = M.notMember id' isRunning

--TODO: make private
mustKill :: GroupConfig -> ([Program], [KillDirective])
mustKill cfg = unzip targets
  where runningAndDifferent :: (ProgramId, RunState) -> Maybe (Program, KillDirective)
        runningAndDifferent (_, RunState {rsHandle = Nothing})    = Nothing
        runningAndDifferent (id', RunState {rsProgram = pg, rsHandle = Just pid, rsLogHandle = lpid})
         | M.notMember id' specMap || M.findWithDefault defaultProgram id' specMap /= pg = Just (pg, toKillDirective pg pid lpid)
         | otherwise                                                                     = Nothing
        targets = mapMaybe runningAndDifferent allRunning
        specMap = spec cfg
        allRunning = M.assocs $ running cfg

toKillDirective :: Program -> ProcessHandle -> Maybe ProcessHandle -> KillDirective
toKillDirective D.Program { name = n
                          , termGrace = Just g } ph lph = HardKill n ph lph g
toKillDirective D.Program { name = n } ph lph           = SoftKill n ph lph

-- |periodically run the supervisor sync independent of config reload,
-- |just in case state gets funky b/c of theoretically possible timing
-- |issues on reload
pollStale :: TVar GroupConfig -> IO ()
pollStale sharedGroupConfig = forever $ sleepSecs 10 >> syncSupervisors sharedGroupConfig
