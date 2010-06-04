module Angel.Config where

import qualified Data.Map as M
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Text.ParserCombinators.Parsec.Error (ParseError)

import Angel.Parse (parseConfig)
import Angel.Job (syncSupervisors)
import Angel.Data
import Angel.Log (logger)
import Angel.Util (waitForWake)

-- |produce a mapping of name -> program for every program
buildConfigMap :: Spec -> SpecKey
buildConfigMap cfg = M.fromList [(name p, p) | p <- cfg]

-- |invoke the parser to process the file at configPath
-- |produce a SpecKey
processConfig :: String -> IO (Either ParseError SpecKey)
processConfig configPath = do c <- readFile configPath
                              let cfg = parseConfig c
                              case cfg of
                                  Left e -> return $ Left e
                                  Right cfg -> do return $ Right $ buildConfigMap cfg

-- |given a new SpecKey just parsed from the file, update the 
-- |shared state TVar
updateSpecConfig :: TVar GroupConfig -> SpecKey -> STM ()
updateSpecConfig sharedGroupConfig spec = do cfg <- readTVar sharedGroupConfig
                                             writeTVar sharedGroupConfig cfg{spec=spec}

-- |read the config file, update shared state with current spec, 
-- |re-sync running supervisors, wait for the HUP TVar, then repeat!
monitorConfig :: String -> TVar GroupConfig -> TVar (Maybe Int) -> IO ()
monitorConfig configPath sharedGroupConfig wakeSig = do let log = logger "config-monitor"
                                                        mspec <- processConfig configPath
                                                        case mspec of 
                                                            Left e     -> do log $ " <<<< Config Error >>>>\n" ++ show e
                                                                             log " <<<< Config Error: Skipping reload >>>>"
                                                            Right spec -> do atomically $ updateSpecConfig sharedGroupConfig spec
                                                                             syncSupervisors sharedGroupConfig
                                                        waitForWake wakeSig
                                                        log "HUP caught, reloading config"
