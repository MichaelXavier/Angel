module Angel.Job where

import Data.String.Utils (split, strip)
import Data.Maybe (isJust, fromJust)
import System.Process (createProcess, proc, waitForProcess, ProcessHandle)
import System.Process (terminateProcess, CreateProcess(..), StdStream(..))
import System.IO (hClose, openFile, IOMode(..), hIsClosed)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import qualified Data.Map as M
import Control.Monad (unless, when, forever)

import Angel.Log (logger)
import Angel.Data
import Angel.Util (sleepSecs)

supervise sharedGroupConfig id = do let log = logger $ "- program: " ++ id ++ " -"
                                    log "START"
                                    cfg <- atomically $ readTVar sharedGroupConfig
                                    let my_spec = M.findWithDefault defaultProgram id (spec cfg)
                                    case name my_spec of
                                         "" -> log "QUIT (missing from config on restart)"
                                         otherwise ->  do
                                                            attachOut <- case stdout my_spec of
                                                                              ""    -> return Inherit
                                                                              other -> UseHandle `fmap` openFile other AppendMode 
                                                            attachErr <- case stderr my_spec of
                                                                              ""    -> return Inherit
                                                                              other -> if other == stdout my_spec 
                                                                                       then return attachOut
                                                                                       else UseHandle `fmap` openFile other AppendMode 
  
                                                            let (cmd, args) = cmdSplit $ exec my_spec
                                                            (_, _, _, p) <- createProcess (proc cmd args){
                                                                  std_out = attachOut,
                                                                  std_err = attachOut
                                                                  }
                                                            atomically $ do wcfg <- readTVar sharedGroupConfig
                                                                            writeTVar sharedGroupConfig wcfg{
                                                               running=M.insertWith' updateRunning id (my_spec, Just p) (running wcfg)
                                                                            }
                                                            log "RUNNING"
                                                            waitForProcess p
                                                            log "ENDED"
  
                                                            -- close processes we made
                                                            case attachOut of 
                                                                UseHandle h -> hIsClosed h >>= \c-> unless c (hClose h)
                                                                otherwise -> return ()
                                                            case attachErr of 
                                                                UseHandle h -> hIsClosed h >>= \c-> unless c (hClose h)
                                                                otherwise -> return ()
  
                                                            atomically $ do lcfg <- readTVar sharedGroupConfig
                                                                            writeTVar sharedGroupConfig lcfg{
                                                               running=M.insertWith' updateRunning id (my_spec, Nothing) (running lcfg)
                                                                            }
  
                                                            cfg <- atomically $ readTVar sharedGroupConfig
                                                            if M.notMember id (spec cfg) 
                                                                then    log  "QUIT"
  
                                                                else do log  "WAITING"
                                                                        sleepSecs $ delay my_spec
                                                                        log  "RESTART"
                                                                        supervise sharedGroupConfig id
    where
        updateRunning new old = new
        cmdSplit fullcmd = (head parts, tail parts) 
            where parts = (filter (/="") . map strip . split " ") fullcmd

-- |send a TERM signal to all provided process handles
killProcesses :: [ProcessHandle] -> IO ()
killProcesses pids = mapM_ terminateProcess pids

-- |fire up new supervisors for new program ids
startProcesses :: TVar GroupConfig -> [String] -> IO ()
startProcesses sharedGroupConfig starts = mapM_ spawnWatcher starts
    where
        spawnWatcher s = forkIO $ supervise sharedGroupConfig s

-- |diff the requested config against the actual run state, and
-- |do any start/kill action necessary
syncSupervisors :: TVar GroupConfig -> IO ()
syncSupervisors sharedGroupConfig = do let log = logger "process-monitor"
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
