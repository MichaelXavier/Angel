module Angel.Config where

import qualified Data.Map as M
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (readTVar, writeTVar)

import Angel.Parse (parseConfig)
import Angel.Job (syncSupervisors)
import Angel.Data
import Angel.Log (logger)
import Angel.Util (waitForWake)

buildConfigMap cfg = M.fromList [(name p, p) | p <- cfg]

processConfig configPath = do c <- readFile configPath
                              let cfg = parseConfig c
                              case cfg of
                                  Left e -> return $ Left e
                                  Right cfg -> do return $ Right $ buildConfigMap cfg

updateSpecConfig sharedGroupConfig spec = do cfg <- readTVar sharedGroupConfig
                                             writeTVar sharedGroupConfig cfg{spec=spec}
monitorConfig configPath sharedGroupConfig wakeSig = do let log = logger "config-monitor"
                                                        mspec <- processConfig configPath
                                                        case mspec of 
                                                            Left e     -> do log $ " <<<< Config Error >>>>\n" ++ show e
                                                                             log " <<<< Config Error: Skipping reload >>>>"
                                                            Right spec -> do atomically $ updateSpecConfig sharedGroupConfig spec
                                                                             syncSupervisors sharedGroupConfig
                                                        waitForWake wakeSig
                                                        log "HUP caught, reloading config"
