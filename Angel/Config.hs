module Angel.Config where

import Control.Exception (try, SomeException)
import qualified Data.Map as M
import Control.Monad (when, mapM_)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Data.Configurator (load, getMap, Worth(..))
import Data.Configurator.Types (Config, Value(..), Name)
import qualified Data.HashMap.Lazy as HM
import Data.String.Utils (split)
import Data.List (foldl')
import qualified Data.Text as T

import Angel.Job (syncSupervisors)
import Angel.Data
import Angel.Log (logger)
import Angel.Util (waitForWake)

import Debug.Trace (trace)

void :: Monad m => m a -> m ()
void m = m >> return ()

-- |produce a mapping of name -> program for every program
buildConfigMap :: HM.HashMap Name Value -> IO SpecKey
buildConfigMap cfg = 
    return $! HM.foldlWithKey' addToMap M.empty $ cfg
  where
    addToMap :: SpecKey -> Name -> Value -> SpecKey
    addToMap m 
             (split "." . T.unpack -> [basekey, localkey])
             value =
        let !newprog = case M.lookup basekey m of
                          Just prog -> modifyProg prog localkey value
                          Nothing   -> modifyProg defaultProgram{name=basekey} localkey value
            in
        M.insert basekey newprog m
    addToMap m _ _ = m

checkConfigValues :: SpecKey -> IO SpecKey
checkConfigValues progs = (mapM_ checkProgram $ M.elems progs) >> (return progs)
  where
    checkProgram p = void $ when (exec p == "") $ error $ name p ++ " does not have an 'exec' specification"

modifyProg :: Program -> String -> Value -> Program
modifyProg prog "exec" (String s) = prog{exec = (T.unpack s)}
modifyProg prog "exec" _ = error "wrong type for field 'exec'; string required"

modifyProg prog "delay" (Number n) | n < 0     = error "delay value must be >= 0"
                                   | otherwise = prog{delay = round n}
modifyProg prog "delay" _ = error "wrong type for field 'delay'; integer"

modifyProg prog "stdout" (String s) = prog{stdout = (T.unpack s)}
modifyProg prog "stdout" _ = error "wrong type for field 'stdout'; string required"

modifyProg prog "stderr" (String s) = prog{stderr = (T.unpack s)}
modifyProg prog "stderr" _ = error "wrong type for field 'stderr'; string required"

modifyProg prog "directory" (String s) = prog{workingDir = (Just $ T.unpack s)}
modifyProg prog "directory" _ = error "wrong type for field 'directory'; string required"

modifyProg prog n _ = prog


-- |invoke the parser to process the file at configPath
-- |produce a SpecKey
processConfig :: String -> IO (Either String SpecKey)
processConfig configPath = do 
    mconf <- try $ load [Required configPath] >>= getMap >>= buildConfigMap >>= checkConfigValues

    case mconf of
        Right config -> return $ Right config
        Left (e :: SomeException) -> return $ Left $ show e
    

-- |given a new SpecKey just parsed from the file, update the 
-- |shared state TVar
updateSpecConfig :: TVar GroupConfig -> SpecKey -> STM ()
updateSpecConfig sharedGroupConfig spec = do 
    cfg <- readTVar sharedGroupConfig
    writeTVar sharedGroupConfig cfg{spec=spec}

-- |read the config file, update shared state with current spec, 
-- |re-sync running supervisors, wait for the HUP TVar, then repeat!
monitorConfig :: String -> TVar GroupConfig -> TVar (Maybe Int) -> IO ()
monitorConfig configPath sharedGroupConfig wakeSig = do 
    let log = logger "config-monitor"
    mspec <- processConfig configPath
    case mspec of 
        Left e     -> do 
            log $ " <<<< Config Error >>>>\n" ++ e
            log " <<<< Config Error: Skipping reload >>>>"
        Right spec -> do 
            print spec
            atomically $ updateSpecConfig sharedGroupConfig spec
            syncSupervisors sharedGroupConfig
    waitForWake wakeSig
    log "HUP caught, reloading config"
