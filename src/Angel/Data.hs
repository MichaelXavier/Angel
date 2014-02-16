module Angel.Data ( GroupConfig(..)
                  , SpecKey
                  , RunKey
                  , ProgramId
                  , FileRequest
                  , Program(..)
                  , Spec
                  , KillDirective(..)
                  , defaultProgram
                  , defaultDelay
                  , defaultStdout
                  , defaultStderr
                  ) where

import qualified Data.Map as M
import System.Process ( createProcess
                      , proc
                      , waitForProcess
                      , ProcessHandle )
import System.Process ( terminateProcess
                      , CreateProcess(..)
                      , StdStream(..) )
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
type RunKey = M.Map ProgramId (Program, Maybe ProcessHandle)
type ProgramId = String
type FileRequest = (String, TChan (Maybe Handle))

-- |the representation of a program is these 6 values, 
-- |read from the config file
data Program = Program {
  name       :: String,
  exec       :: Maybe String,
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
data KillDirective = SoftKill String ProcessHandle |
                     HardKill String ProcessHandle Int

-- instance Show KillDirective where
--   show (SoftKill _)       = "SoftKill"
--   show (HardKill _ grace) = "HardKill after " ++ show grace ++ "s"

-- |Lower-level atoms in the configuration process
type Spec = [Program]

-- |a template for an empty program; the variable set to ""
-- |are required, and must be overridden in the config file
defaultProgram :: Program
defaultProgram = Program "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing

defaultDelay :: Int
defaultDelay = 5

defaultStdout :: FilePath
defaultStdout = "/dev/null"

defaultStderr :: FilePath
defaultStderr = "/dev/null"
