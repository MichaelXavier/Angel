module Angel.Job ( syncSupervisors
                 , killProcess -- for testing
                 , pollStale ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (tryTakeMVar, newEmptyMVar, putMVar)
import Control.Exception ( finally )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad (unless)
import Data.Conduit.Process.Unix (signalProcessHandle)
import Data.String.Utils ( split
                         , strip )
import Data.Maybe ( mapMaybe
                  , fromMaybe
                  , isJust
                  , fromJust )
import System.Posix.Signals ( sigTERM
                            , sigKILL )
import System.Process ( createProcess
                      , proc
                      , waitForProcess
                      , getProcessExitCode
                      , ProcessHandle
                      , CreateProcess(..)
                      , StdStream(..) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( TVar
                              , writeTVar
                              , readTVar
                              , atomically )
import qualified Data.Map as M
import Control.Monad ( when
                     , guard
                     , void
                     , forever )
import Angel.Log ( logger )
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
                  , GroupConfig(..)
                  , KillDirective(..)
                  , defaultProgram
                  , defaultDelay
                  , defaultStdout
                  , defaultStderr )
import Angel.Process ( isProcessHandleDead
                     , softKillProcessHandle
                     , hardKillProcessHandle )
import qualified Angel.Data as D
import Angel.Util ( sleepSecs
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
supervise sharedGroupConfig id = do 
    log "START"
    cfg <- atomically $ readTVar sharedGroupConfig
    let my_spec = find_me cfg
    ifEmpty (name my_spec) 

        (log "QUIT (missing from config on restart)") 
        
        (do
            (attachOut, attachErr) <- makeFiles my_spec cfg

            let (cmd, args) = cmdSplit $ (fromJust $ exec my_spec)

            let procSpec = (proc cmd args) {
              std_out = attachOut,
              std_err = attachErr,
              cwd = workingDir my_spec,
              env = Just $ D.env my_spec
            }

            let mPfile = pidFile my_spec

            log $ "Spawning process with env " ++ show (env procSpec)

            
            startMaybeWithPidFile procSpec mPfile $ \pHandle -> do
              updateRunningPid my_spec (Just pHandle)
              log "RUNNING"
              waitForProcess pHandle
              log "ENDED"
              updateRunningPid my_spec (Nothing)
            
            cfg <- atomically $ readTVar sharedGroupConfig
            if M.notMember id (spec cfg) 

                then do 
                log  "QUIT"

                else do 
                log  "WAITING"
                sleepSecs $ (fromMaybe defaultDelay $ delay my_spec)
                log  "RESTART"
                supervise sharedGroupConfig id
        )
        
    where
        log = logger $ "- program: " ++ id ++ " -"
        cmdSplit fullcmd = (head parts, tail parts) 
            where parts = (filter (/="") . map strip . split " ") fullcmd

        find_me cfg = M.findWithDefault defaultProgram id (spec cfg)
        updateRunningPid my_spec mpid = atomically $ do 
            wcfg <- readTVar sharedGroupConfig
            writeTVar sharedGroupConfig wcfg{
              running=M.insertWith' (\n o-> n) id (my_spec, mpid) (running wcfg)
            }
                
        makeFiles my_spec cfg = do
            case (logExec my_spec) of
                Just path -> logWithExec path
                Nothing -> logWithFiles

          where
            logWithFiles = do
                let useout = fromMaybe defaultStdout $ stdout my_spec
                attachOut <- UseHandle `fmap` getFile useout cfg

                let useerr = fromMaybe defaultStderr $ stderr my_spec
                attachErr <- UseHandle `fmap` getFile useerr cfg

                return $ (attachOut, attachErr)

            logWithExec path = do
                let (cmd, args) = cmdSplit path
                
                attachOut <- UseHandle `fmap` getFile "/dev/null" cfg

                log "Spawning process"
                (inPipe, _, _, p) <- createProcess (proc cmd args){
                  std_out = attachOut,
                  std_err = attachOut,
                  std_in  = CreatePipe,
                  cwd     = workingDir my_spec
                }

                return $ (UseHandle (fromJust inPipe), 
                    UseHandle (fromJust inPipe))

--TODO: paralellize
killProcesses :: [KillDirective] -> IO ()
killProcesses = mapM_ killProcess

killProcess :: KillDirective -> IO ()
killProcess (SoftKill n pid) = do
  log $ "Soft killing " ++ n
  softKillProcessHandle pid
  where log = logger "process-killer"
killProcess (HardKill n pid grace) = do
  log $ "Attempting soft kill " ++ n ++ " before hard killing"
  softKillProcessHandle pid
  log $ "Waiting " ++ show grace ++ " seconds for " ++ n ++ " to die"
  sleepSecs grace

  -- Note that this means future calls to get exits status will fail
  dead <- isProcessHandleDead pid

  unless dead $ log ("Hard killing " ++ n) >> hardKillProcessHandle pid
  where log = logger "process-killer"

cleanPidfiles :: [Program] -> IO ()
cleanPidfiles progs = mapM_ clearPIDFile pidfiles
  where pidfiles = mapMaybe pidFile progs

-- |fire up new supervisors for new program ids
startProcesses :: TVar GroupConfig -> [String] -> IO ()
startProcesses sharedGroupConfig starts = mapM_ spawnWatcher starts
    where
        spawnWatcher s = forkIO $ wrapProcess sharedGroupConfig s

wrapProcess :: TVar GroupConfig -> String -> IO ()
wrapProcess sharedGroupConfig id = do
    run <- createRunningEntry
    when run $ finally (supervise sharedGroupConfig id) deleteRunning
  where
    deleteRunning = atomically $ do 
        wcfg <- readTVar sharedGroupConfig
        writeTVar sharedGroupConfig wcfg{
            running=M.delete id (running wcfg)
        }

    createRunningEntry =
        atomically $ do
            cfg <- readTVar sharedGroupConfig
            let specmap = spec cfg 
            case M.lookup id specmap of
                Nothing -> return False
                Just target -> do
                    let runmap = running cfg
                    case M.lookup id runmap of
                        Just _ -> return False
                        Nothing -> do
                            writeTVar sharedGroupConfig cfg{running=
                                M.insert id (target, Nothing) runmap}
                            return True

-- |diff the requested config against the actual run state, and
-- |do any start/kill action necessary
syncSupervisors :: TVar GroupConfig -> IO ()
syncSupervisors sharedGroupConfig = do 
   let log = logger "process-monitor"
   cfg <- atomically $ readTVar sharedGroupConfig
   let (killProgs, killHandles) = mustKill cfg
   let starts = mustStart cfg
   when (nnull killHandles || nnull starts) $ log (
         "Must kill=" ++ (show $ length killHandles)
                ++ ", must start=" ++ (show $ length starts))
   killProcesses killHandles
   cleanPidfiles killProgs
   startProcesses sharedGroupConfig starts

--TODO: make private
mustStart :: GroupConfig -> [String]
mustStart cfg = map fst $ filter (isNew $ running cfg) $ M.assocs (spec cfg)
  where isNew running (id, pg) = M.notMember id running

--TODO: make private
mustKill :: GroupConfig -> ([Program], [KillDirective])
mustKill cfg = unzip targets
  where runningAndDifferent :: (ProgramId, (Program, Maybe ProcessHandle)) -> Maybe (Program, KillDirective)
        runningAndDifferent (id, (pg, Nothing))    = Nothing
        runningAndDifferent (id, (pg, (Just pid)))
         | (M.notMember id specMap || M.findWithDefault defaultProgram id specMap /= pg) = Just (pg, toKillDirective pg pid)
         | otherwise                                                                     = Nothing
        targets = mapMaybe runningAndDifferent allRunning
        specMap = spec cfg
        allRunning = M.assocs $ running cfg

toKillDirective :: Program -> ProcessHandle -> KillDirective
toKillDirective D.Program { name = n
                          , termGrace = Just g } ph = HardKill n ph g
toKillDirective D.Program { name = n } ph           = SoftKill n ph

-- |periodically run the supervisor sync independent of config reload,
-- |just in case state gets funky b/c of theoretically possible timing
-- |issues on reload
pollStale :: TVar GroupConfig -> IO ()
pollStale sharedGroupConfig = forever $ sleepSecs 10 >> syncSupervisors sharedGroupConfig
