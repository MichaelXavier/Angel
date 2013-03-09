module Angel.Job where

import Control.Exception (finally)
import Data.String.Utils (split, strip)
import Data.Maybe (isJust, fromJust, fromMaybe)
import System.Process (createProcess, proc, waitForProcess, ProcessHandle)
import System.Process (terminateProcess, CreateProcess(..), StdStream(..))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import qualified Data.Map as M
import Control.Monad (unless, when, forever)

import Angel.Log (logger)
import Angel.Data
import Angel.Util (sleepSecs)
import Angel.Files (getFile)

ifEmpty :: String -> IO () -> IO () -> IO ()
ifEmpty s ioa iob = if s == "" then ioa else iob
-- |launch the program specified by `id`, opening (and closing) the
-- |appropriate fds for logging.  When the process dies, either b/c it was
-- |killed by a monitor, killed by a system user, or ended execution naturally,
-- |re-examine the desired run config to determine whether to re-run it.  if so,
-- |tail call.
supervise sharedGroupConfig id = do 
    let log = logger $ "- program: " ++ id ++ " -"
    log "START"
    cfg <- atomically $ readTVar sharedGroupConfig
    let my_spec = find_me cfg
    ifEmpty (name my_spec) 

        (log "QUIT (missing from config on restart)") 
        
        (do
            (attachOut, attachErr) <- makeFiles my_spec cfg

            let (cmd, args) = cmdSplit $ (fromJust $ exec my_spec)
            
            (_, _, _, p) <- createProcess (proc cmd args){
            std_out = attachOut,
            std_err = attachErr,
            cwd = workingDir my_spec 
            }
            
            updateRunningPid my_spec (Just p)
            log "RUNNING"
            waitForProcess p
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

                (inPipe, _, _, p) <- createProcess (proc cmd args){
                std_out = attachOut,
                std_err = attachOut,
                std_in = CreatePipe,
                cwd = workingDir my_spec 
                }

                return $ (UseHandle (fromJust inPipe), 
                    UseHandle (fromJust inPipe))

-- |send a TERM signal to all provided process handles
killProcesses :: [ProcessHandle] -> IO ()
killProcesses pids = mapM_ terminateProcess pids

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
   let kills = mustKill cfg
   let starts = mustStart cfg
   when (length kills > 0 || length starts > 0) $ log (
         "Must kill=" ++ (show $ length kills)
                ++ ", must start=" ++ (show $ length starts))
   killProcesses kills
   startProcesses sharedGroupConfig starts
                                         
    where
        mustKill cfg = map (fromJust . snd . snd) $ filter (runningAndDifferent $ spec cfg) $ M.assocs (running cfg)
        runningAndDifferent spec (id, (pg, pid)) = (isJust pid && (M.notMember id spec 
                                           || M.findWithDefault defaultProgram id spec `cmp` pg))
            where cmp one two = one /= two

        mustStart cfg = map fst $ filter (isNew $ running cfg) $ M.assocs (spec cfg)
        isNew running (id, pg) = M.notMember id running

-- |periodically run the supervisor sync independent of config reload,
-- |just in case state gets funky b/c of theoretically possible timing
-- |issues on reload
pollStale :: TVar GroupConfig -> IO ()
pollStale sharedGroupConfig = forever $ sleepSecs 10 >> syncSupervisors sharedGroupConfig
