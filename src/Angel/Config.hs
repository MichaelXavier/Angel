{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Angel.Config ( monitorConfig
                    , modifyProg
                    , expandByCount
                    , loadInitialUserFromConfig
                    -- for testing
                    , processConfig) where

import Control.Exception ( try
                         , SomeException )
import qualified Data.Map as M
import Control.Monad ( when
                     , (>=>) )
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM ( STM
                              , TVar
                              , writeTVar
                              , readTVar
                              , atomically )
import qualified Data.Configurator as C ( load
                         , getMap
                         , lookup
                         , Worth(Required) )
import Data.Configurator.Types ( Value(Number, String, Bool)
                               , Name )
import qualified Data.Traversable as T
import qualified Data.HashMap.Lazy as HM
import Data.List ( nub
                 , foldl' )
import Data.Maybe ( isNothing
                  , isJust )
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import Angel.Job ( syncSupervisors )
import Angel.Data ( Program( exec
                           , delay
                           , stdout
                           , stderr
                           , logExec
                           , pidFile
                           , workingDir
                           , name
                           , termGrace
                           , env )
                  , SpecKey
                  , AngelM
                  , GroupConfig
                  , Verbosity(..)
                  , spec
                  , defaultProgram )
import Angel.Log ( logger )
import Angel.Prelude
import Angel.Util ( waitForWake
                  , nnull
                  , expandPath )

-- |produce a mapping of name -> program for every program
buildConfigMap :: HM.HashMap Name Value -> SpecKey
buildConfigMap = HM.foldlWithKey' addToMap M.empty . addDefaults
  where
    addToMap :: SpecKey -> Name -> Value -> SpecKey
    addToMap m n value
      | nnull basekey && nnull localkey =
        let !newprog = case M.lookup basekey m of
                          Just prog -> modifyProg prog localkey value
                          Nothing   -> modifyProg defaultProgram {name = basekey, env = []} localkey value
            in
        M.insert basekey newprog m
      | otherwise = m
      where (basekey, '.':localkey) = break (== '.') $ T.unpack n

addDefaults :: HM.HashMap Name Value -> HM.HashMap Name Value
addDefaults conf = foldl' addDefault conf progs
  where
    graceKey prog = prog <> ".grace"
    progs = programNames conf
    addDefault conf' prog
      | HM.member (graceKey prog) conf' = conf'
      | otherwise = HM.insert (graceKey prog) defaultGrace conf
    defaultGrace = Bool False

programNames :: HM.HashMap Name a -> [Name]
programNames = nub . filter nnullN . map extractName . HM.keys
  where
    extractName = T.takeWhile (/= '.')

checkConfigValues :: SpecKey -> IO SpecKey
checkConfigValues progs = mapM_ checkProgram (M.elems progs) >> return progs
  where
    checkProgram p = do
        when (isNothing $ exec p) $ error $ name p ++ " does not have an 'exec' specification"
        when (isJust (logExec p) &&
            (isJust (stdout p) || isJust (stderr p) )) $ error $ name p ++ " cannot have both a logger process and stderr/stdout"

modifyProg :: Program -> String -> Value -> Program
modifyProg prog "exec" (String s) = prog {exec = Just (T.unpack s)}
modifyProg _ "exec" _ = error "wrong type for field 'exec'; string required"

modifyProg prog "delay" (Number n) | n < 0     = error "delay value must be >= 0"
                                   | otherwise = prog{delay = Just $ round n}
modifyProg _ "delay" _ = error "wrong type for field 'delay'; integer"

modifyProg prog "stdout" (String s) = prog{stdout = Just (T.unpack s)}
modifyProg _ "stdout" _ = error "wrong type for field 'stdout'; string required"

modifyProg prog "stderr" (String s) = prog{stderr = Just (T.unpack s)}
modifyProg _ "stderr" _ = error "wrong type for field 'stderr'; string required"

modifyProg prog "directory" (String s) = prog{workingDir = Just (T.unpack s)}
modifyProg _ "directory" _ = error "wrong type for field 'directory'; string required"

modifyProg prog "logger" (String s) = prog{logExec = Just (T.unpack s)}
modifyProg _ "logger" _ = error "wrong type for field 'logger'; string required"

modifyProg prog "pidfile" (String s) = prog{pidFile = Just (T.unpack s)}
modifyProg _ "pidfile" _ = error "wrong type for field 'pidfile'; string required"

modifyProg prog ('e':'n':'v':'.':envVar) (String s) = prog{env = envVar'}
  where envVar' = (envVar, T.unpack s):env prog
modifyProg _ ('e':'n':'v':'.':_) _ = error "wrong type for env field; string required"

modifyProg prog "termgrace" (Bool False) = prog{termGrace = Nothing}
modifyProg prog "termgrace" (Number n) | n < 0 = error "termgrace if it is a number must be >= 1"
                                       | n == 0 = prog{termGrace = Nothing}
                                       | otherwise = prog { termGrace = Just $ round n}
modifyProg _ "termgrace" _ = error "wrong type for field 'termgrace'; number or boolean false required"

modifyProg prog _ _ = prog

loadInitialUserFromConfig :: FilePath -> IO (Maybe String)
loadInitialUserFromConfig configPath = do
    C.load [C.Required configPath] >>= flip C.lookup "user"

-- |invoke the parser to process the file at configPath
-- |produce a SpecKey
processConfig :: String -> IO (Either String SpecKey)
processConfig configPath = do
    mconf <- try $ process =<< C.load [C.Required configPath]

    case mconf of
        Right config -> return $ Right config
        Left (e :: SomeException) -> return $ Left $ show e
  where process = C.getMap                >=>
                  return . expandByCount  >=>
                  return . buildConfigMap >=>
                  expandPaths             >=>
                  checkConfigValues

-- |preprocess config into multiple programs if "count" is specified
expandByCount :: HM.HashMap Name Value -> HM.HashMap Name Value
expandByCount cfg = HM.unions expanded
  where expanded :: [HM.HashMap Name Value]
        expanded         = concat $ HM.foldlWithKey' expand' [] groupedByProgram
        expand'  :: [[HM.HashMap Name Value]] -> Name -> HM.HashMap Name Value -> [[HM.HashMap Name Value]]
        expand' acc      = fmap (:acc) . expand
        groupedByProgram :: HM.HashMap Name (HM.HashMap Name Value)
        groupedByProgram = HM.foldlWithKey' binByProg HM.empty cfg
        binByProg h fullKey v
          | prog /= "" && localKey /= "" = HM.insertWith HM.union prog (HM.singleton localKey v) h
          | otherwise                    = h
          where (prog, localKeyWithLeadingDot) = T.breakOn "." fullKey
                localKey                 = T.drop 1 localKeyWithLeadingDot
        expand :: Name -> HM.HashMap Name Value -> [HM.HashMap Name Value]
        expand prog pcfg = maybe [reflatten prog pcfg]
                                 expandWithCount
                                 (HM.lookup "count" pcfg)
          where expandWithCount (Number n)
                  | n >= 0    = [ reflatten (genProgName i) (rewriteConfig i pcfg) | i <- [1..n] ]
                  | otherwise = error "count must be >= 0"
                expandWithCount _ = error "count must be a number or not specified"
                genProgName i     = prog <> "-" <> textNumber i

rewriteConfig :: Rational -> HM.HashMap Name Value -> HM.HashMap Name Value
rewriteConfig n = HM.insert "env.ANGEL_PROCESS_NUMBER" procNumber . HM.adjust rewritePidfile "pidfile"
  where procNumber                   = String n'
        n'                           = textNumber n
        rewritePidfile (String path) = String $ rewrittenFilename <> extension
          where rewrittenFilename     = filename <> "-" <> n'
                (filename, extension) = T.breakOn "." path
        rewritePidfile x              = x

textNumber :: Rational -> T.Text
textNumber = T.pack . show . (truncate :: Rational -> Integer)

reflatten :: Name -> HM.HashMap Name Value -> HM.HashMap Name Value
reflatten prog pcfg = HM.fromList asList
  where asList            = map prependKey $ filter notCount $ HM.toList pcfg
        prependKey (k, v) = (prog <> "." <> k, v)
        notCount          = not . (== "count") . fst

-- |given a new SpecKey just parsed from the file, update the
-- |shared state TVar
updateSpecConfig :: TVar GroupConfig -> SpecKey -> STM ()
updateSpecConfig sharedGroupConfig newSpec = do
    cfg <- readTVar sharedGroupConfig
    writeTVar sharedGroupConfig cfg{spec=newSpec}

-- |read the config file, update shared state with current spec,
-- |re-sync running supervisors, wait for the HUP TVar, then repeat!
monitorConfig :: String -> TVar GroupConfig -> TVar (Maybe Int) -> AngelM ()
monitorConfig configPath sharedGroupConfig wakeSig = do
    let logger' = logger "config-monitor"
    mspec <- liftIO $ processConfig configPath
    case mspec of
        Left e     -> do
            logger' V1 $ " <<<< Config Error >>>>\n" ++ e
            logger' V2 " <<<< Config Error: Skipping reload >>>>"
        Right newSpec -> do
            liftIO $ atomically $ updateSpecConfig sharedGroupConfig newSpec
            syncSupervisors sharedGroupConfig
    liftIO $ waitForWake wakeSig
    logger' V2 "HUP caught, reloading config"

expandPaths :: SpecKey -> IO SpecKey
expandPaths = T.mapM expandProgramPaths

expandProgramPaths :: Program -> IO Program
expandProgramPaths prog = do exec'       <- maybeExpand $ exec prog
                             stdout'     <- maybeExpand $ stdout prog
                             stderr'     <- maybeExpand $ stderr prog
                             workingDir' <- maybeExpand $ workingDir prog
                             pidFile'    <- maybeExpand $ pidFile prog
                             return prog { exec       = exec',
                                           stdout     = stdout',
                                           stderr     = stderr',
                                           workingDir = workingDir',
                                           pidFile    = pidFile' }
    where maybeExpand = T.traverse expandPath

nnullN :: Name -> Bool
nnullN = not . T.null
