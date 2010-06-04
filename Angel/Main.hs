module Main where 

import System.Process (createProcess, proc, waitForProcess, ProcessHandle)
import System.Process (terminateProcess, CreateProcess(..), StdStream(..))
import Data.String.Utils (split, strip)
import Data.Maybe (isJust, fromJust)
import Control.Concurrent (threadDelay, forkIO, forkOS)
import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Monad (unless, when, forever)
import Control.Monad.Trans (liftIO)
import System.Environment (getArgs)
import System.Posix.Signals
import System.IO (hClose, openFile, IOMode(..), hIsClosed)
import Language.Haskell.Pretty (prettyPrint)
import Debug.Trace (trace)

import Angel.Parse (parseConfig, Spec, Program(..), defaultProgram)
import Angel.Log (logger)

data GroupConfig = GroupConfig {
    spec :: SpecKey,
    running :: RunKey
}

type SpecKey = M.Map ProgramId Program
type RunKey = M.Map ProgramId (Program, Maybe ProcessHandle)
type ProgramId = String

babysit sharedGroupConfig id = do let log = logger $ "- program: " ++ id ++ " -"
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
                                                                      babysit sharedGroupConfig id
    where
        updateRunning new old = new
        cmdSplit fullcmd = (head parts, tail parts) 
            where parts = (filter (/="") . map strip . split " ") fullcmd

sleepSecs s = threadDelay $ s * 1000000

buildConfigMap cfg = M.fromList [(name p, p) | p <- cfg]

processConfig configPath = do c <- readFile configPath
                              let cfg = parseConfig c
                              case cfg of
                                  Left e -> return $ Left e
                                  Right cfg -> do return $ Right $ buildConfigMap cfg

updateSpecConfig sharedGroupConfig spec = do cfg <- readTVar sharedGroupConfig
                                             writeTVar sharedGroupConfig cfg{spec=spec}

waitForWake wakeSig = atomically $ do state <- readTVar wakeSig
                                      case state of
                                          Just x -> writeTVar wakeSig Nothing
                                          Nothing -> retry

killProcesses pids = mapM_ terminateProcess pids

startProcesses sharedGroupConfig starts = mapM_ spawnWatcher starts
    where
        spawnWatcher s = forkIO $ babysit sharedGroupConfig s

{- compare state of running apps to non-running ones
-} 
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

pollStale sharedGroupConfig = forever $ sleepSecs 10 >> syncSupervisors sharedGroupConfig

monitorConfig configPath sharedGroupConfig wakeSig = do let log = logger "config-monitor"
                                                        mspec <- processConfig configPath
                                                        case mspec of 
                                                            Left e     -> do log $ " <<<< Config Error >>>>\n" ++ show e
                                                                             log " <<<< Config Error: Skipping reload >>>>"
                                                            Right spec -> do atomically $ updateSpecConfig sharedGroupConfig spec
                                                                             syncSupervisors sharedGroupConfig
                                                        waitForWake wakeSig
                                                        log "HUP caught, reloading config"

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
