{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Angel.Data ( GroupConfig(..)
                  , SpecKey
                  , RunKey
                  , ProgramId
                  , FileRequest
                  , Program(..)
                  , RunState(..)
                  , Spec
                  , KillDirective(..)
                  , Verbosity(..)
                  , Options(..)
                  , AngelM(..)
                  , defaultProgram
                  , defaultDelay
                  , defaultStdout
                  , defaultStderr
                  , runAngelM
                  ) where

import qualified Data.Map as M
import System.Process (ProcessHandle)
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Concurrent.STM.TChan (TChan)
import System.IO (Handle)

-- |the whole shared state of the program; spec is what _should_
-- |be running, while running is what actually _is_ running_ currently
data GroupConfig = GroupConfig {
    spec :: SpecKey,
    running :: RunKey,
    fileRequest :: TChan FileRequest
}

-- |map program ids to relevant structure
type SpecKey = M.Map ProgramId Program
type RunKey = M.Map ProgramId RunState

data RunState = RunState {
  rsProgram :: Program,
  rsHandle :: Maybe ProcessHandle,
  rsLogHandle :: Maybe ProcessHandle
}

type ProgramId = String
type FileRequest = (String, TChan (Maybe Handle))

-- |the representation of a program is these 6 values,
-- |read from the config file
data Program = Program {
  name       :: String,
  exec       :: Maybe String,
  user       :: Maybe String,
  delay      :: Maybe Int,
  stdout     :: Maybe String,
  stderr     :: Maybe String,
  workingDir :: Maybe FilePath,
  logExec    :: Maybe String,
  pidFile    :: Maybe FilePath,
  env        :: [(String, String)],
  termGrace  :: Maybe Int -- ^ How long to wait after sending a SIGTERM before SIGKILL. Nothing = never SIGKILL. Default Nothing
} deriving (Show, Eq, Ord)

-- |represents all the data needed to handle terminating a process
data KillDirective = SoftKill String ProcessHandle (Maybe ProcessHandle) |
                     HardKill String ProcessHandle (Maybe ProcessHandle) Int

-- instance Show KillDirective where
--   show (SoftKill _)       = "SoftKill"
--   show (HardKill _ grace) = "HardKill after " ++ show grace ++ "s"

-- |Lower-level atoms in the configuration process
type Spec = [Program]


data Verbosity = V0
               -- ^ Failures only
               | V1
               -- ^ Failures + program starts/stops
               | V2
               -- ^ Max verbosity. Default. Logs all of the above as well as state changes and other debugging info.
               deriving (Show, Eq, Ord)


data Options = Options {
      configFile :: FilePath
    , userargument :: Maybe String
    , verbosity  :: Verbosity
    }


newtype AngelM a = AngelM {
      unAngelM :: ReaderT Options IO a
    } deriving (Functor, Applicative, Monad, MonadReader Options, MonadIO)


runAngelM :: Options -> AngelM a -> IO a
runAngelM o (AngelM f) = runReaderT f o

-- |a template for an empty program; the variable set to ""
-- |are required, and must be overridden in the config file
defaultProgram :: Program
defaultProgram = Program "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing

defaultDelay :: Int
defaultDelay = 5

defaultStdout :: FilePath
defaultStdout = "/dev/null"

defaultStderr :: FilePath
defaultStderr = "/dev/null"
